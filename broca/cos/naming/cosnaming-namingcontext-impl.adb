------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--         C O S N A M I N G . N A M I N G C O N T E X T . I M P L          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;
with CORBA.Impl;
with CORBA.ORB;

with Broca.Server_Tools;
with Broca.Exceptions;
with Broca.Debug;

with PortableServer.POA;

with CosNaming; use CosNaming;

with CosNaming.BindingIterator;
with CosNaming.BindingIterator.Impl;
with CosNaming.BindingIterator.Helper;
with CosNaming.NamingContext.Helper;
with CosNaming.NamingContext.Skel;
pragma Elaborate (CosNaming.NamingContext.Skel);

with GNAT.HTable;
with GNAT.Task_Lock; use GNAT.Task_Lock;

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body CosNaming.NamingContext.Impl is

   Flag : constant Natural
     := Broca.Debug.Is_Active ("cosnaming.namingcontext");
   procedure O is new Broca.Debug.Output (Flag);

   type String_Access is access String;

   package Names renames IDL_SEQUENCE_CosNaming_NameComponent;

   Null_NC : constant NameComponent
     := (Istring (Ada.Strings.Unbounded.Null_Unbounded_String),
         Istring (Ada.Strings.Unbounded.Null_Unbounded_String));

   Null_Name : constant Name := Name (Names.Null_Sequence);

   --  Each naming context has its own internal id (Key). Bindings
   --  from local naming contexts are stored in the same hash table
   --  (BOHT). Each binding is encoded using its naming context
   --  internal id, its name component name and its name component
   --  type (Encode).

   subtype Hash_Header is Natural range 0 .. 30;

   function Hash  (F : String_Access) return Hash_Header;
   function Equal (F1, F2 : String_Access) return Boolean;

   package BOHT is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Header,
      Element    => Bound_Object_Ptr,
      No_Element => null,
      Key        => String_Access,
      Hash       => Hash,
      Equal      => Equal);

   function Encode
     (Ctx : Object_Ptr;
      N   : NameComponent)
     return String;
   --  Encode this name component using the naming context internal
   --  id, the name component name and name component type.

   procedure Append_BO_To_NC
     (NC  : in Object_Ptr;
      Key : in String;
      BN  : in NameComponent;
      BT  : in BindingType;
      Obj : in CORBA.Object.Ref);
   --  Append a bound object to a naming context (NC). This bound
   --  object is composed of a binding (BN, BT) and an object Obj.
   --  Set a new entry in the hash table using its Key.

   procedure Display_NC
     (Text : in String;
      NC   : in Object_Ptr);
   --  Display the list of bound objects of naming context NC with a
   --  output title Text.

   procedure Get_Ctx_And_Last_NC
     (Self : access Object;
      N    : in     Name;
      Len  : in out Natural;
      Ctx  : out    NamingContext.Ref;
      NC   : out    NameComponent);
   --  Resolve N from a given naming context Self: split a name N into
   --  its naming context Ctx and the last name component NC. Len is
   --  the length of N. If Len = 1, then Ctx must be ignored. To avoid
   --  concurrent issues, we get a copy of the bound object lists
   --  (thread safe).

   function Look_For_BO_In_NC
     (NC  : Object_Ptr;
      Key : String)
     return Bound_Object_Ptr;
   --  Look for a bound object in a naming context NC using its Key.

   procedure Remove_BO_From_NC
     (NC : in     Object_Ptr;
      BO : in out Bound_Object_Ptr);
   --  Remove a bound object from a naming context NC.

   function To_Name (NC : NameComponent) return Name;
   --  Basic function which returns a sequence of one name component.

   procedure Valid
     (NC     : Object_Ptr;
      Locked : Boolean := False);
   --  Check whether NC is null. If null, raise an exception and
   --  unlock global lock if locked.

   procedure Free is
      new Ada.Unchecked_Deallocation (Bound_Object, Bound_Object_Ptr);

   Seed : Key_Type := (others => 'A');

   --------------
   -- Allocate --
   --------------

   function Allocate return Key_Type is
      N : Natural  := Key_Size;
      K : Key_Type := Seed;

   begin
      while N > 0 loop
         if Seed (N) /= 'Z' then
            Seed (N) := Character'Succ (Seed (N));
            exit;
         end if;
         N := N - 1;
      end loop;

      if N = 0 then
         raise Program_Error;
      end if;

      while N < Key_Size loop
         N := N + 1;
         Seed (N) := 'A';
      end loop;

      return K;
   end Allocate;

   ---------------------
   -- Append_BO_To_NC --
   ---------------------

   procedure Append_BO_To_NC
     (NC  : in Object_Ptr;
      Key : in String;
      BN  : in NameComponent;
      BT  : in BindingType;
      Obj : in CORBA.Object.Ref)
   is
      BO : Bound_Object_Ptr := new Bound_Object;

   begin
      Valid (NC, True);

      Display_NC ("register """ & Key & """ in naming context", NC);

      --  Append to the tail of the double linked list.

      BOHT.Set (new String'(Key), BO);

      BO.BN  := BN;
      BO.BT  := BT;
      BO.Obj := Obj;
      BO.NC  := NC;

      if NC.Head = null then
         NC.Head := BO;
         NC.Tail := BO;

      else
         BO.Prev      := NC.Tail;
         BO.Prev.Next := BO;
         NC.Tail      := BO;
      end if;

      Display_NC ("append """ & Key & """ to naming context", NC);
   end Append_BO_To_NC;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Self : access Object;
      N    : in Name;
      Obj  : in CORBA.Object.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         NamingContext.Bind (Ctx, To_Name (Last), Obj);

      else
         declare
            BON : String := Encode (Self.Self, Last);

         begin
            Lock;
            if Look_For_BO_In_NC (Self.Self, BON) /= null then
               Unlock;
               raise AlreadyBound;
            end if;

            Append_BO_To_NC (Self.Self, BON, Last, NObject, Obj);
            Unlock;
         end;
      end if;
   end Bind;

   ------------------
   -- Bind_Context --
   ------------------

   procedure Bind_Context
     (Self : access Object;
      N    : in Name;
      NC   : in NamingContext.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      pragma Debug (O ("Bind_Context: enter"));
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);
      pragma Debug (O ("Bind_Context: len is" & Len'Img));

      if Len /= 1 then
         pragma Debug (O ("Bind_Context: binding relative name " & To_String (Last.Id)));
         NamingContext.Bind_Context (Ctx, To_Name (Last), NC);
      else
         declare
            BON : String := Encode (Self.Self, Last);

         begin
            Lock;
            if Look_For_BO_In_NC (Self.Self, BON) /= null then
               Unlock;
               raise AlreadyBound;
            end if;

            Append_BO_To_NC
              (Self.Self, BON, Last, NContext, CORBA.Object.Ref (NC));
            Unlock;
         end;
      end if;
   end Bind_Context;

   ----------------------
   -- Bind_New_Context --
   ----------------------

   function Bind_New_Context
     (Self : access Object;
      N    : in Name)
     return NamingContext.Ref
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         return Bind_New_Context (Ctx, To_Name (Last));

      else
         Ctx := New_Context (Self);
         Bind_Context (Self, N, Ctx);
         return Ctx;
      end if;
   end Bind_New_Context;

   ------------
   -- Create --
   ------------

   function Create
     return Object_Ptr
   is
      Obj : Object_Ptr;

   begin
      Obj      := new Object;
      Obj.Self := Obj;
      Obj.Key  := Allocate;
      return Obj;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object) is
   begin
      Valid (Self.Self);
      if Self.Head /= null then
         raise NotEmpty;
      end if;
   end Destroy;

   ----------------
   -- Display_NC --
   ----------------

   procedure Display_NC
     (Text : in String;
      NC   : in Object_Ptr)
   is
      BO : Bound_Object_Ptr;

   begin
      if Flag = 0 then
         return;
      end if;

      Ada.Text_IO.Put_Line (Text);

      BO := NC.Head;
      while BO /= null loop
         Ada.Text_IO.Put (String (NC.Key));
         Ada.Text_IO.Put (" ... ");
         Ada.Text_IO.Put (To_Standard_String (BO.BN.Id));
         Ada.Text_IO.Put (Ascii.HT);
         Ada.Text_IO.Put (To_Standard_String (BO.BN.Kind));
         Ada.Text_IO.Put (Ascii.HT);
         Ada.Text_IO.Put (BO.BT'Img);
         Ada.Text_IO.New_Line;
         BO := BO.Next;
      end loop;
   end Display_NC;

   ------------
   -- Encode --
   ------------

   function Encode
     (Ctx : Object_Ptr;
      N   : NameComponent)
     return String
   is
      Len : Natural;
      NI  : Natural := Length (N.Id);
      NK  : Natural := Length (N.Kind);

   begin
      Len := Key_Size + 1 + NI + 1 + NK + 1;

      declare
         BON : String (1 .. Len);

      begin
         BON (1 .. Key_Size) := String (Ctx.Key);

         Len := Key_Size + 1;
         BON (Len) := Ascii.HT;

         BON (Len + 1 .. Len + NI) := To_String (N.Id);

         Len := Len + NI + 1;
         BON (Len) := Ascii.HT;

         BON (Len + 1 .. Len + NK) := To_String (N.Kind);

         Len := Len + NK + 1;
         BON (Len) := ';';

         return BON;
      end;
   end Encode;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   -------------------------
   -- Get_Ctx_And_Last_NC --
   -------------------------

   procedure Get_Ctx_And_Last_NC
     (Self : access Object;
      N    : in     Name;
      Len  : in out Natural;
      Ctx  : out    NamingContext.Ref;
      NC   : out    NameComponent)
   is
      use Names;

   begin
      pragma Debug (O ("Get_Ctx_And_Last_NC: enter"));
      Valid (Self.Self);

      Lock;
      declare
         NCA         : Element_Array := To_Element_Array (Sequence (N));
         Current_Obj : CORBA.Object.Ref;
         Current_Ctx : NamingContext.Ref;
         Current_Idx : Natural;

      begin
         Unlock;

         Len := NCA'Length;
         if Len = 0 then
            raise InvalidName;
         end if;

         if Len > 1 then
            Current_Idx := NCA'First;
            pragma Debug (O ("Get_Ctx_And_Last_NC: resolve " & To_String (NCA (Current_Idx).Id)));
            Current_Obj := Resolve (Self, To_Name (NCA (Current_Idx)));
            Current_Ctx := NamingContext.Helper.To_Ref (Current_Obj);

            Current_Idx := Current_Idx + 1;
            while Current_Idx < NCA'Last loop
               pragma Debug (O ("Get_Ctx_And_Last_NC: resolve " & To_String (NCA (Current_Idx).Id)));
               Current_Obj := Resolve (Current_Ctx,
                                       To_Name (NCA (Current_Idx)));
               Current_Ctx := NamingContext.Helper.To_Ref (Current_Obj);
               Current_Idx := Current_Idx + 1;
            end loop;

            Ctx := Current_Ctx;
         end if;

         NC  := NCA (NCA'Last);
      exception when CORBA.Bad_Param =>
         declare
            Member : NotFound_Members;

         begin
            --  Cannot cast the current name component into a
            --  naming context.

            Member.Why          := Not_Context;
            Member.Rest_Of_Name := To_Sequence
              (NCA (Current_Idx + 1 .. NCA'Last));
            Broca.Exceptions.User_Raise_Exception
              (NotFound'Identity, Member);
         end;
      end;
   end Get_Ctx_And_Last_NC;

   ----------
   -- Hash --
   ----------

   function Hash (F : String_Access) return Hash_Header is
      N : Natural := 0;

   begin
      --  Add up characters of name, mod our table size

      for J in F'Range loop
         N := (N + Character'Pos (F (J))) mod (Hash_Header'Last + 1);
      end loop;

      return N;
   end Hash;

   ----------
   -- List --
   ----------

   procedure List
     (Self     : access Object;
      How_Many : in CORBA.Unsigned_Long;
      BL       : out BindingList;
      BI       : out BindingIterator_Forward.Ref)
   is
      use BindingIterator.Impl;

      Len  : Natural := 0;
      Size : Natural := Natural (How_Many);
      Head : Bound_Object_Ptr;
      Iter : BindingIterator.Impl.Object_Ptr;
      Oid  : PortableServer.ObjectId;
      Ref  : CORBA.Object.Ref;

   begin
      Valid (Self.Self);

      Lock;

      --  How many bound objects in this naming context.

      Head := Self.Head;
      while Head /= null loop
         Len  := Len + 1;
         Head := Head.Next;
      end loop;
      Head := Self.Head;

      --  First, copy the first bound objects to fill BL.

      if Len < Size then
         Size := Len;
      end if;

      if Size > 0 then
         declare
            Table : Bindings.Element_Array (1 .. Size);
         begin
            for I in 1 .. Size loop
               Table (I) := (To_Name (Head.BN), Head.BT);
               Head := Head.Next;
            end loop;
            BL  := BindingList (Bindings.To_Sequence (Table));
            Len := Len - Size;
         end;
      end if;

      Iter       := BindingIterator.Impl.Create;
      Iter.Index := 1;
      Iter.Table := new Bindings.Element_Array (1 .. Len);

      --  Copy the remaining bound objects into the iterator.

      for I in Iter.Table'Range loop
         Iter.Table (I) := (To_Name (Head.BN), Head.BT);
         Head := Head.Next;
      end loop;

      Unlock;

      --  Activate object Iterator.

      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Iter), BI);
   end List;

   -----------------------
   -- Look_For_BO_In_NC --
   -----------------------

   function Look_For_BO_In_NC
     (NC  : Object_Ptr;
      Key : String)
     return Bound_Object_Ptr is
   begin
      Display_NC ("look for """ & Key & """", NC);
      return BOHT.Get (Key'Unrestricted_Access);
   end Look_For_BO_In_NC;

   -----------------
   -- New_Context --
   -----------------

   function New_Context
     (Self : access Object)
     return NamingContext.Ref
   is
      My_Ref : NamingContext.Ref;

   begin
      Broca.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Create), My_Ref);
      return My_Ref;
   end New_Context;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (Self : access Object;
      N    : in Name;
      Obj  : in CORBA.Object.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         Rebind (Ctx, To_Name (Last), Obj);

      else
         declare
            BON : String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            Lock;
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               Unlock;
               declare
                  Member : NotFound_Members;
               begin
                  Member.Why          := Missing_Node;
                  Member.Rest_Of_Name := Null_Name;
                  Broca.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            if BO.BT /= NObject then
               Unlock;
               declare
                  Member : NotFound_Members;
               begin
                  Member.Why          := Not_Object;
                  Member.Rest_Of_Name := Null_Name;
                  Broca.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            Append_BO_To_NC   (Self.Self, BON, Last, NObject, Obj);
            Unlock;
         end;
      end if;
   end Rebind;

   --------------------
   -- Rebind_Context --
   --------------------

   procedure Rebind_Context
     (Self : access Object;
      N    : in Name;
      NC   : in NamingContext.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         NamingContext.Rebind_Context (Ctx, To_Name (Last), NC);

      else
         declare
            BON : String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            Lock;
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               Unlock;
               declare
                  Member : NotFound_Members;
               begin
                  Member.Why          := Missing_Node;
                  Member.Rest_Of_Name := Null_Name;
                  Broca.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            if BO.BT /= NContext then
               Unlock;
               declare
                  Member : NotFound_Members;
               begin
                  Member.Why          := Not_Context;
                  Member.Rest_Of_Name := Null_Name;
                  Broca.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            Append_BO_To_NC
              (Self.Self, BON, Last, NContext, CORBA.Object.Ref (NC));
            Unlock;
         end;
      end if;
   end Rebind_Context;

   -----------------------
   -- Remove_BO_From_NC --
   -----------------------

   procedure Remove_BO_From_NC
     (NC : in     Object_Ptr;
      BO : in out Bound_Object_Ptr) is
   begin
      Valid (NC, True);

      if BO.Next /= null then
         BO.Next.Prev := BO.Prev;
      end if;
      if BO.Prev /= null then
         BO.Prev.Next := BO.Next;
      end if;
      if NC.Head = BO then
         NC.Head := BO.Next;
      end if;
      if NC.Tail = BO then
         NC.Tail := BO.Prev;
      end if;

      BO.Prev := null;
      BO.Next := null;

      declare
         BON : String := Encode (NC, BO.BN);
      begin
         BOHT.Set (BON'Unrestricted_Access, null);
      end;
      Free (BO);
      Display_NC ("remove object from naming context", NC);
   end Remove_BO_From_NC;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Self : access Object;
      N    : in Name)
     return CORBA.Object.Ref
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         return NamingContext.Resolve (Ctx, To_Name (Last));

      else
         declare
            BON : String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;
            Obj : CORBA.Object.Ref;

         begin
            Lock;
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               Unlock;
               declare
                  Member : NotFound_Members;

               begin
                  Member.Why          := Missing_Node;
                  Member.Rest_Of_Name := Null_Name;
                  Broca.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            Obj := BO.Obj;
            Unlock;
            return Obj;
         end;
      end if;
   end Resolve;

   -------------
   -- To_Name --
   -------------

   function To_Name (NC : NameComponent) return Name is
   begin
      return Name (Names.To_Sequence ((1 => NC)));
   end To_Name;

   ------------
   -- Unbind --
   ------------

   procedure Unbind
     (Self : access Object;
      N    : in Name)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         Unbind (Ctx, To_Name (Last));

      else
         declare
            BON : String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            Lock;
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               Unlock;
               declare
                  Member : NotFound_Members;

               begin
                  Member.Why          := Missing_Node;
                  Member.Rest_Of_Name := Null_Name;
                  Broca.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            Unlock;
         end;
      end if;
   end Unbind;

   -----------
   -- Valid --
   -----------

   procedure Valid
     (NC     : Object_Ptr;
      Locked : Boolean := False) is
   begin
      if NC = null then
         if Locked then
            Unlock;
         end if;
         raise CannotProceed;
      end if;
   end Valid;

end CosNaming.NamingContext.Impl;
