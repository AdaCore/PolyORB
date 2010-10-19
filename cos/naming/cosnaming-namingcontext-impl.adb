------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         C O S N A M I N G . N A M I N G C O N T E X T . I M P L          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Exceptions;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.Utils.Strings;

with CosNaming.BindingIterator.Impl;
with CosNaming.NamingContext.Helper;
with CosNaming.NamingContext.Skel;
pragma Warnings (Off, CosNaming.NamingContext.Skel);

with GNAT.HTable;

package body CosNaming.NamingContext.Impl is

   use CosNaming;

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("cosnaming.namingcontext");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package Names renames IDL_SEQUENCE_CosNaming_NameComponent;

   Null_Name : constant Name := Name (Names.Null_Sequence);

   --  Each naming context has its own internal id (Key). Bindings
   --  from local naming contexts are stored in the same hash table
   --  (BOHT). Each binding is encoded using its naming context
   --  internal id, its name component name and its name component
   --  type (Encode).

   subtype Hash_Header is Natural range 0 .. 30;

   function Hash  (F : PolyORB.Utils.Strings.String_Ptr) return Hash_Header;
   function Equal (F1, F2 : PolyORB.Utils.Strings.String_Ptr) return Boolean;

   package BOHT is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Header,
      Element    => Bound_Object_Ptr,
      No_Element => null,
      Key        => PolyORB.Utils.Strings.String_Ptr,
      Hash       => Hash,
      Equal      => Equal);

   function Encode
     (Ctx : Object_Ptr;
      N   : NameComponent)
     return String;
   --  Encode this name component using the naming context internal
   --  id, the name component name and name component type.

   procedure Append_BO_To_NC
     (NC  : Object_Ptr;
      Key : String;
      BN  : NameComponent;
      BT  : BindingType;
      Obj : CORBA.Object.Ref);
   --  Append a bound object to a naming context (NC). This bound
   --  object is composed of a binding (BN, BT) and an object Obj.
   --  Set a new entry in the hash table using its Key.

   procedure Display_NC
     (Text : String;
      NC   : Object_Ptr);
   --  Display the list of bound objects of naming context NC with a
   --  output title Text.

   procedure Get_Ctx_And_Last_NC
     (Self : access Object;
      N    : Name;
      Len  : out    Natural;
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
     (NC : Object_Ptr;
      BO : in out Bound_Object_Ptr);
   --  Remove a bound object from a naming context NC.

   function To_Name (NC : NameComponent) return Name;
   --  Basic function which returns a sequence of one name component.

   procedure Free is
      new Ada.Unchecked_Deallocation (Bound_Object, Bound_Object_Ptr);

   Seed : Key_Type := (others => 'A');

   --------------
   -- Allocate --
   --------------

   function Allocate return Key_Type;
   function Allocate return Key_Type is
      N : Natural  := Key_Size;
      K : constant Key_Type := Seed;

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
     (NC  : Object_Ptr;
      Key : String;
      BN  : NameComponent;
      BT  : BindingType;
      Obj : CORBA.Object.Ref)
   is
      BO : constant Bound_Object_Ptr := new Bound_Object;

   begin
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
      N    : Name;
      Obj  : CORBA.Object.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         NamingContext.bind (Ctx, To_Name (Last), Obj);

      else
         declare
            BON : constant String := Encode (Self.Self, Last);

         begin
            PTM.Enter (Self.Mutex);
            if Look_For_BO_In_NC (Self.Self, BON) /= null then
               PTM.Leave (Self.Mutex);
               raise AlreadyBound;
            end if;

            Append_BO_To_NC (Self.Self, BON, Last, nobject, Obj);
            PTM.Leave (Self.Mutex);
         end;
      end if;
   end Bind;

   ------------------
   -- Bind_Context --
   ------------------

   procedure Bind_Context
     (Self : access Object;
      N    : Name;
      NC   : NamingContext.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      pragma Debug (O ("Bind_Context: enter"));
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);
      pragma Debug (O ("Bind_Context: len is" & Len'Img));

      if Len /= 1 then
         pragma Debug
           (O ("Bind_Context: binding relative name " & To_String (Last.id)));
         NamingContext.bind_context (Ctx, To_Name (Last), NC);
      else
         declare
            BON : constant String := Encode (Self.Self, Last);

         begin
            PTM.Enter (Self.Mutex);
            if Look_For_BO_In_NC (Self.Self, BON) /= null then
               PTM.Leave (Self.Mutex);
               raise AlreadyBound;
            end if;

            Append_BO_To_NC
              (Self.Self, BON, Last, ncontext, CORBA.Object.Ref (NC));
            PTM.Leave (Self.Mutex);
         end;
      end if;
   end Bind_Context;

   ----------------------
   -- Bind_New_Context --
   ----------------------

   function Bind_New_Context
     (Self : access Object;
      N    : Name)
     return NamingContext.Ref'Class
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         return Ref (bind_new_context (Ctx, To_Name (Last)));

      else
         Ctx := NamingContext.Ref (New_Context (Self));
         Bind_Context (Self, N, Ctx);
         return Ctx;
      end if;
   end Bind_New_Context;

   ------------
   -- Create --
   ------------

   function Create return Object_Ptr is
      Obj : constant Object_Ptr := new Object;

   begin
      Initialize (Obj);

      return Obj;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : Object_Ptr) is
   begin
      Self.Self := Self;
      Self.Key  := Allocate;
      PTM.Create (Self.Mutex);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : access Object) is
   begin
      if Self.Head /= null then
         raise NotEmpty;
      end if;
   end Destroy;

   ----------------
   -- Display_NC --
   ----------------

   procedure Display_NC
     (Text : String;
      NC   : Object_Ptr)
   is
      BO : Bound_Object_Ptr;

   begin
      O (Text, Notice);

      BO := NC.Head;
      while BO /= null loop
         O (String (NC.Key)
            & " ... "
            & To_Standard_String (BO.BN.id)
            & ASCII.HT
            & To_Standard_String (BO.BN.kind)
            & ASCII.HT
            & BO.BT'Img, Notice);
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
      NI  : constant Natural := Length (N.id);
      NK  : constant Natural := Length (N.kind);

   begin
      Len := Key_Size + 1 + NI + 1 + NK + 1;

      declare
         BON : String (1 .. Len);

      begin
         BON (1 .. Key_Size) := String (Ctx.Key);

         Len := Key_Size + 1;
         BON (Len) := ASCII.HT;

         BON (Len + 1 .. Len + NI) := To_String (N.id);

         Len := Len + NI + 1;
         BON (Len) := ASCII.HT;

         BON (Len + 1 .. Len + NK) := To_String (N.kind);

         Len := Len + NK + 1;
         BON (Len) := ';';

         return BON;
      end;
   end Encode;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : PolyORB.Utils.Strings.String_Ptr) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   -------------------------
   -- Get_Ctx_And_Last_NC --
   -------------------------

   procedure Get_Ctx_And_Last_NC
     (Self : access Object;
      N    : Name;
      Len  : out    Natural;
      Ctx  : out    NamingContext.Ref;
      NC   : out    NameComponent)
   is
      use Names;

   begin
      pragma Debug (O ("Get_Ctx_And_Last_NC: enter"));
      PTM.Enter (Self.Mutex);
      declare
         NCA         : Element_Array := To_Element_Array (Sequence (N));
         Current_Obj : CORBA.Object.Ref;
         Current_Ctx : NamingContext.Ref;
         Current_Idx : Natural;

      begin
         PTM.Leave (Self.Mutex);

         Len := NCA'Length;
         if Len = 0 then
            raise InvalidName;
         end if;

         if Len > 1 then
            Current_Idx := NCA'First;
            pragma Debug
              (O ("Get_Ctx_And_Last_NC: resolve "
                  & To_String (NCA (Current_Idx).id)));
            Current_Obj := Resolve (Self, To_Name (NCA (Current_Idx)));
            Current_Ctx := NamingContext.Helper.To_Ref (Current_Obj);

            Current_Idx := Current_Idx + 1;
            while Current_Idx < NCA'Last loop
               pragma Debug
                 (O ("Get_Ctx_And_Last_NC: resolve "
                     & To_String (NCA (Current_Idx).id)));
               Current_Obj := resolve
                 (Current_Ctx, To_Name (NCA (Current_Idx)));
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

            Member.why          := not_context;
            Member.rest_of_name := To_Sequence
              (NCA (Current_Idx + 1 .. NCA'Last));
            PolyORB.Exceptions.User_Raise_Exception
              (NotFound'Identity, Member);
         end;
      end;
   end Get_Ctx_And_Last_NC;

   ----------
   -- Hash --
   ----------

   function Hash (F : PolyORB.Utils.Strings.String_Ptr) return Hash_Header is
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
      How_Many : CORBA.Unsigned_Long;
      BL       : out BindingList;
      BI       : out BindingIterator_Forward.Ref)
   is
      use BindingIterator.Impl;

      Len  : Natural := 0;
      Size : Natural := Natural (How_Many);
      Head : Bound_Object_Ptr;
      Iter : BindingIterator.Impl.Object_Ptr;

   begin
      PTM.Enter (Self.Mutex);

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

      PTM.Leave (Self.Mutex);

      --  Activate object Iterator.

      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Iter), BI);
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
     return NamingContext.Ref'Class
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      My_Ref : NamingContext.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Impl.Create), My_Ref);
      return My_Ref;
   end New_Context;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (Self : access Object;
      N    : Name;
      Obj  : CORBA.Object.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         rebind (Ctx, To_Name (Last), Obj);

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            PTM.Enter (Self.Mutex);
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               PTM.Leave (Self.Mutex);
               declare
                  Member : NotFound_Members;
               begin
                  Member.why          := missing_node;
                  Member.rest_of_name := Null_Name;
                  PolyORB.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            if BO.BT /= nobject then
               PTM.Leave (Self.Mutex);
               declare
                  Member : NotFound_Members;
               begin
                  Member.why          := not_object;
                  Member.rest_of_name := Null_Name;
                  PolyORB.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            Append_BO_To_NC   (Self.Self, BON, Last, nobject, Obj);
            PTM.Leave (Self.Mutex);
         end;
      end if;
   end Rebind;

   --------------------
   -- Rebind_Context --
   --------------------

   procedure Rebind_Context
     (Self : access Object;
      N    : Name;
      NC   : NamingContext.Ref)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         NamingContext.rebind_context (Ctx, To_Name (Last), NC);

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            PTM.Enter (Self.Mutex);
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               PTM.Leave (Self.Mutex);
               declare
                  Member : NotFound_Members;
               begin
                  Member.why          := missing_node;
                  Member.rest_of_name := Null_Name;
                  PolyORB.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            if BO.BT /= ncontext then
               PTM.Leave (Self.Mutex);
               declare
                  Member : NotFound_Members;
               begin
                  Member.why          := not_context;
                  Member.rest_of_name := Null_Name;
                  PolyORB.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            Append_BO_To_NC
              (Self.Self, BON, Last, ncontext, CORBA.Object.Ref (NC));
            PTM.Leave (Self.Mutex);
         end;
      end if;
   end Rebind_Context;

   -----------------------
   -- Remove_BO_From_NC --
   -----------------------

   procedure Remove_BO_From_NC
     (NC : Object_Ptr;
      BO : in out Bound_Object_Ptr) is
   begin
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
         BON : constant String := Encode (NC, BO.BN);
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
      N    : Name)
     return CORBA.Object.Ref
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         return NamingContext.resolve (Ctx, To_Name (Last));

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;
            Obj : CORBA.Object.Ref;

         begin
            PTM.Enter (Self.Mutex);
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               PTM.Leave (Self.Mutex);
               declare
                  Member : NotFound_Members;

               begin
                  Member.why          := missing_node;
                  Member.rest_of_name := Null_Name;
                  PolyORB.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            Obj := BO.Obj;
            PTM.Leave (Self.Mutex);
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
      N    : Name)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last);

      if Len /= 1 then
         unbind (Ctx, To_Name (Last));

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            PTM.Enter (Self.Mutex);
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               PTM.Leave (Self.Mutex);
               declare
                  Member : NotFound_Members;

               begin
                  Member.why          := missing_node;
                  Member.rest_of_name := Null_Name;
                  PolyORB.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            Remove_BO_From_NC (Self.Self, BO);
            PTM.Leave (Self.Mutex);
         end;
      end if;
   end Unbind;

end CosNaming.NamingContext.Impl;
