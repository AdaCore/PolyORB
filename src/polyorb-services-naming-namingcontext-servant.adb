------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.SERVICES.NAMING.NAMINGCONTEXT.SERVANT               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2021, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with GNAT.HTable;
--  XXX Use PolyORB's Hash table ...

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Any.ObjRef;
with PolyORB.Errors;
with PolyORB.Exceptions;
with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.References;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Strings;

with PolyORB.Minimal_Servant.Tools;

with PolyORB.Services.Naming.Helper;
with PolyORB.Services.Naming.NamingContext.Client;
with PolyORB.Services.Naming.NamingContext.Helper;

package body PolyORB.Services.Naming.NamingContext.Servant is

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Any.ObjRef;
   use PolyORB.Log;
   use PolyORB.Requests;
   use PolyORB.Types;

   use PolyORB.Services.Naming.Helper;
   use PolyORB.Services.Naming.NamingContext.Helper;

   package PSNH renames  PolyORB.Services.Naming.Helper;
   package PSNNC renames  PolyORB.Services.Naming.NamingContext.Client;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.services.naming.namingcontext.servant");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   package PTM renames PolyORB.Tasking.Mutexes;
   Critical_Section : PTM.Mutex_Access;

   procedure Bind
     (Self : access Object;
      N    : Name;
      Obj  : PolyORB.References.Ref;
      Exc  : access Any.Any);

   procedure Bind_Context
     (Self : access Object;
      N    : Name;
      NC   : NamingContext.Ref;
      Exc  : access PolyORB.Any.Any);

   function Bind_New_Context
     (Self : access Object;
      N    : Name;
      Exc  : access PolyORB.Any.Any) return NamingContext.Ref;

   procedure Destroy
     (Self : access Object);

   procedure Initialize;

   function Is_A (Logical_Type_Id : Standard.String)
                  return PolyORB.Types.Boolean;

--    procedure List
--       (Self     : access Object;
--        How_Many : PolyORB.Types.Unsigned_Long;
--        BL       : out    BindingList;
--        BI       : out    BindingIterator_Forward.Ref);

   function New_Context
     (Self : access Object)
     return NamingContext.Ref;

   procedure Rebind
     (Self : access Object;
      N    : Name;
      Obj  : PolyORB.References.Ref;
      Exc  : access Any.Any);

   procedure Rebind_Context
     (Self : access Object;
      N    : Name;
      NC   : NamingContext.Ref;
      Exc  : access Any.Any);

   function Resolve
     (Self : access Object;
      N    : Name;
      Exc  : access Any.Any) return PolyORB.References.Ref;

   procedure Unbind
     (Self : access Object;
      N    : Name;
      Exc  : access Any.Any);
   --  Actual functions implemented by the servant.

   ------------
   -- Invoke --
   ------------

   overriding procedure Invoke
     (Self    : access Object;
      Request : PolyORB.Requests.Request_Access)
   is
      Operation : Standard.String renames Request.all.Operation.all;

      Arg_List    : PolyORB.Any.NVList.Ref;
   begin
      pragma Debug (C, O ("The server is executing the request:"
                    & PolyORB.Requests.Image (Request.all)));

      Create (Arg_List);

      if Operation = "_is_a" then
         declare
            use PolyORB.Errors;

            Type_Id          : PolyORB.Types.String;
            Argument_Type_Id : constant PolyORB.Any.Any :=
              Get_Empty_Any (TypeCode.TC_String);

            Result           : PolyORB.Types.Boolean;
            Exception_Error  : Error_Container;
         begin
            --  Create argument list

            Add_Item (Arg_List,
                      To_PolyORB_String ("Type_Id"),
                      Argument_Type_Id,
                      PolyORB.Any.ARG_IN);

            Arguments (Request, Arg_List, Exception_Error);

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive
            end if;

            Type_Id := From_Any (Argument_Type_Id);

            --  Call implementation
            Result := Is_A (To_Standard_String (Type_Id));

            --  Set Result
            Set_Result (Request, To_Any (Result));

         end;

      elsif Operation = "bind" then

         declare
            use PolyORB.Errors;

            N            : Name;
            Argument_N   : constant PolyORB.Any.Any := Get_Empty_Any (TC_Name);

            Obj          : PolyORB.References.Ref;
            Argument_Obj : constant PolyORB.Any.Any :=
              Get_Empty_Any (PSNH.TC_Object);

            Exception_Error        : Error_Container;
         begin
            --  Create argument list

            Add_Item (Arg_List,
                      To_PolyORB_String ("n"),
                      Argument_N,
                      PolyORB.Any.ARG_IN);

            Add_Item (Arg_List,
                      To_PolyORB_String ("obj"),
                      Argument_Obj,
                      PolyORB.Any.ARG_IN);

            Arguments (Request, Arg_List, Exception_Error);

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive

            end if;

            --  Convert arguments from their Any
            N   := From_Any (Argument_N);
            Obj := From_Any (Argument_Obj);

            --  Call implementation
            Bind (Self, N, Obj, Request.Exception_Info'Access);

            return;
         end;

      elsif Operation = "rebind" then

         declare
            use PolyORB.Errors;

            N            : Name;
            Argument_N   : constant PolyORB.Any.Any := Get_Empty_Any (TC_Name);

            Obj          : PolyORB.References.Ref;
            Argument_Obj : constant PolyORB.Any.Any :=
              Get_Empty_Any (PSNH.TC_Object);

            Exception_Error        : Error_Container;
         begin
            --  Create argument list

            Add_Item (Arg_List,
                      To_PolyORB_String ("n"),
                      Argument_N,
                      PolyORB.Any.ARG_IN);
            Add_Item (Arg_List,
                      To_PolyORB_String ("obj"),
                      Argument_Obj,
                      PolyORB.Any.ARG_IN);

            Arguments (Request, Arg_List, Exception_Error);

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive

            end if;

            --  Convert arguments from their Any
            N   := From_Any (Argument_N);
            Obj := From_Any (Argument_Obj);

            --  Call implementation
            Rebind (Self, N, Obj, Request.Exception_Info'Access);
            return;
         end;

      elsif Operation = "bind_context" then

         declare
            use PolyORB.Errors;

            N           : Name;
            Argument_N  : constant PolyORB.Any.Any := Get_Empty_Any (TC_Name);

            Nc          : NamingContext.Ref;
            Argument_Nc : constant PolyORB.Any.Any :=
              Get_Empty_Any (TC_NamingContext);

            Exception_Error       : Error_Container;
         begin
            --  Create argument list

            Add_Item  (Arg_List,
                       To_PolyORB_String ("n"),
                       Argument_N,
                       PolyORB.Any.ARG_IN);
            Add_Item (Arg_List,
                      To_PolyORB_String ("nc"),
                      Argument_Nc,
                      PolyORB.Any.ARG_IN);

            Arguments (Request, Arg_List, Exception_Error);

            --  Convert arguments from their Any

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive

            end if;

            N := From_Any (Argument_N);
            Nc := From_Any (Argument_Nc);

            --  Call implementation
            Bind_Context (Self, N, Nc, Request.Exception_Info'Access);

            return;
         end;

      elsif Operation = "rebind_context" then

         declare
            use PolyORB.Errors;

            N           : Name;
            Argument_N  : constant PolyORB.Any.Any := Get_Empty_Any (TC_Name);

            Nc          : NamingContext.Ref;
            Argument_Nc : constant PolyORB.Any.Any :=
              Get_Empty_Any (TC_NamingContext);

            Exception_Error       : Error_Container;
         begin
            --  Create argument list

            Add_Item (Arg_List,
                      To_PolyORB_String ("n"),
                      Argument_N,
                      PolyORB.Any.ARG_IN);
            Add_Item (Arg_List,
                      To_PolyORB_String ("nc"),
                      Argument_Nc,
                      PolyORB.Any.ARG_IN);

            Arguments (Request, Arg_List, Exception_Error);

            --  Convert arguments from their Any

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive

            end if;

            N  := From_Any (Argument_N);
            Nc := From_Any (Argument_Nc);

            --  Call implementation
            Rebind_Context (Self, N, Nc, Request.Exception_Info'Access);

            return;
         end;

      elsif Operation = "resolve" then

         declare
            use PolyORB.Errors;

            N               : Name;
            Argument_N      : constant PolyORB.Any.Any :=
              Get_Empty_Any (TC_Name);

            Result          : PolyORB.References.Ref;
            Exception_Error : Error_Container;
         begin
            --  Create argument list

            Add_Item (Arg_List,
                      To_PolyORB_String ("n"),
                      Argument_N,
                      PolyORB.Any.ARG_IN);

            Arguments (Request, Arg_List, Exception_Error);

            --  Convert arguments from their Any

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive

            end if;

            N := From_Any (Argument_N);

            --  Call implementation
            pragma Debug (C, O ("Invoke: call Resolve stub"));
            Result := Resolve (Self, N, Request.Exception_Info'Access);

            --  Set Result

            if Is_Empty (Request.Exception_Info) then
               Set_Result (Request,
                 PolyORB.Services.Naming.Helper.To_Any (Result));
            end if;

            return;
         end;

      elsif Operation = "unbind" then

         declare
            use PolyORB.Errors;

            N           : Name;
            Argument_N  : constant PolyORB.Any.Any := Get_Empty_Any (TC_Name);
            Exception_Error       : Error_Container;
         begin
            --  Create argument list
            Add_Item (Arg_List,
                      To_PolyORB_String ("n"),
                      Argument_N,
                      PolyORB.Any.ARG_IN);

            Arguments (Request, Arg_List, Exception_Error);

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive

            end if;

            --  Convert arguments from their Any
            N := From_Any (Argument_N);

            --  Call implementation
            Unbind (Self, N, Request.Exception_Info'Access);
            return;
         end;

      elsif Operation = "new_context" then

         declare
            use PolyORB.Errors;

            Result          : NamingContext.Ref;
            Exception_Error : Error_Container;
         begin
            --  Create argument list
            Arguments (Request, Arg_List, Exception_Error);

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive

            end if;

            --  Call implementation
            Result := New_Context (Self);

            --  Set Result
            Set_Result (Request, To_Any (Result));

            return;
         end;

      elsif Operation = "bind_new_context" then

         declare
            use PolyORB.Errors;

            N          : Name;
            Argument_N : constant PolyORB.Any.Any := Get_Empty_Any (TC_Name);

            Result          : NamingContext.Ref;
            Exception_Error : Error_Container;
         begin
            --  Create argument list
            Add_Item (Arg_List,
                      To_PolyORB_String ("n"),
                      Argument_N,
                      PolyORB.Any.ARG_IN);

            Arguments (Request, Arg_List, Exception_Error);

            if Found (Exception_Error) then
               raise Program_Error;
               --  XXX We should do something more constructive

            end if;

            --  Convert arguments from their Any
            N := From_Any (Argument_N);

            --  Call implementation
            Result := Bind_New_Context
                        (Self, N, Request.Exception_Info'Access);

            if Is_Empty (Request.Exception_Info) then
               --  Set Result
               Set_Result (Request, To_Any (Result));
            end if;

            return;
         end;

      elsif Operation = "destroy" then

         --  Call implementation
         Destroy  (Self);

         return;

--       elsif Operation = "list" then ...

      else
         --         PolyORB.Exceptions.Raise_Bad_Operation;
         raise Program_Error;
      end if;
   end Invoke;

   ---------------------------
   -- Get_Parameter_Profile --
   ---------------------------

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref;

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref
   is
      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      pragma Debug (C, O ("Parameter profile for " & Method & " requested."));

      if Method = "_is_a" then
         Add_Item (Result,
                   (Name      => To_PolyORB_String ("Type_Id"),
                    Argument  => Get_Empty_Any (TypeCode.TC_String),
                    Arg_Modes => ARG_IN));

      elsif Method = "bind" then
         Add_Item (Result,
                   (Name      => To_PolyORB_String ("n"),
                    Argument  => Get_Empty_Any (TC_Name),
                    Arg_Modes => ARG_IN));

         Add_Item (Result,
                   (Name      => To_PolyORB_String ("obj"),
                    Argument  => Get_Empty_Any (PSNH.TC_Object),
                    Arg_Modes => ARG_IN));

      elsif Method = "bind_context" then
         Add_Item (Result,
                   (Name      => To_PolyORB_String ("n"),
                    Argument  => Get_Empty_Any (TC_Name),
                    Arg_Modes => ARG_IN));

         Add_Item (Result,
                   (Name      => To_PolyORB_String ("nc"),
                    Argument  => Get_Empty_Any (TC_NamingContext),
                    Arg_Modes => ARG_IN));

      elsif Method = "bind_new_context" then
         Add_Item (Result,
                   (Name      => To_PolyORB_String ("n"),
                    Argument  => Get_Empty_Any (TC_Name),
                    Arg_Modes => ARG_IN));

      elsif Method = "destroy" then
         null;
         --  XXX should add an item ?

      elsif Method = "new_context" then
         null;
         --  XXX should add an item ?

      elsif Method = "rebind" then
         Add_Item (Result,
                   (Name      => To_PolyORB_String ("n"),
                    Argument  => Get_Empty_Any (TC_Name),
                    Arg_Modes => ARG_IN));

         Add_Item (Result,
                   (Name      => To_PolyORB_String ("obj"),
                    Argument  => Get_Empty_Any (PSNH.TC_Object),
                    Arg_Modes => ARG_IN));

      elsif Method = "rebind_context" then
         Add_Item (Result,
                   (Name      => To_PolyORB_String ("n"),
                    Argument  => Get_Empty_Any (TC_Name),
                    Arg_Modes => ARG_IN));

         Add_Item (Result,
                   (Name      => To_PolyORB_String ("nc"),
                    Argument  => Get_Empty_Any (TC_NamingContext),
                    Arg_Modes => ARG_IN));

      elsif Method = "resolve" then
         Add_Item (Result,
                   (Name      => To_PolyORB_String ("n"),
                    Argument  => Get_Empty_Any (TC_Name),
                    Arg_Modes => ARG_IN));

      elsif Method = "unbind" then
         Add_Item (Result,
                   (Name      => To_PolyORB_String ("n"),
                    Argument  => Get_Empty_Any (TC_Name),
                    Arg_Modes => ARG_IN));

      else
         raise Program_Error;
      end if;

      return Result;
   end Get_Parameter_Profile;

   ------------------------
   -- Get_Result_Profile --
   ------------------------

   function Get_Result_Profile (Method : String) return PolyORB.Any.Any;

   function Get_Result_Profile (Method : String) return PolyORB.Any.Any is
   begin
      pragma Debug (C, O ("Result profile for " & Method & " requested."));

      if Method = "_is_a" then
         return Get_Empty_Any (TypeCode.TC_Boolean);

      elsif Method = "bind" then
         return Get_Empty_Any (TypeCode.TC_Void);

      elsif Method = "bind_context" then
         return Get_Empty_Any (TypeCode.TC_Void);

      elsif Method = "bind_new_context" then
         return Get_Empty_Any (TC_NamingContext);

      elsif Method = "destroy" then
         return Get_Empty_Any (TypeCode.TC_Void);

      elsif Method = "new_context" then
         return Get_Empty_Any (TypeCode.TC_Void);

      elsif Method = "rebind" then
         return Get_Empty_Any (TypeCode.TC_Void);

      elsif Method = "rebind_context" then
         return Get_Empty_Any (TypeCode.TC_Void);

      elsif Method = "resolve" then
         return Get_Empty_Any (PSNH.TC_Object);

      elsif Method = "unbind" then
         return Get_Empty_Any (TypeCode.TC_Void);

      else
         raise Program_Error;
      end if;
   end Get_Result_Profile;

   -------------
   -- If_Desc --
   -------------

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description is
   begin
      return
        (PP_Desc => Get_Parameter_Profile'Access,
         RP_Desc => Get_Result_Profile'Access);
   end If_Desc;

   ------------------------------
   -- Servant actual functions --
   ------------------------------

   type String_Access is access String;

   package Names renames SEQUENCE_NameComponent;

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
     (NC  : Object_Ptr;
      Key : String;
      BN  : NameComponent;
      BT  : BindingType;
      Obj : PolyORB.References.Ref);
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
      NC   : out    NameComponent;
      Exc  : access Any.Any);
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

   procedure Valid
     (NC     : Object_Ptr;
      Locked : Boolean := False);
   --  Check whether NC is null. If null, raise an exception and
   --  unlock global lock if locked.

   procedure Free is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Bound_Object,


      Name   => Bound_Object_Ptr);

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
      Obj : PolyORB.References.Ref)
   is
      BO : constant Bound_Object_Ptr := new Bound_Object;

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
      N    : Name;
      Obj  : PolyORB.References.Ref;
      Exc  : access Any.Any)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last, Exc);
      if not Is_Empty (Exc.all) then
         return;
      end if;

      if Len /= 1 then
         PSNNC.Bind (Ctx, To_Name (Last), Obj);

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            M : AlreadyBound_Members;
         begin
            PTM.Enter (Critical_Section);
            if Look_For_BO_In_NC (Self.Self, BON) /= null then
               PTM.Leave (Critical_Section);
               Exc.all := To_Any (M);
               return;
            end if;

            Append_BO_To_NC (Self.Self, BON, Last, Nobject, Obj);
            PTM.Leave (Critical_Section);
         end;
      end if;
   end Bind;

   ------------------
   -- Bind_Context --
   ------------------

   procedure Bind_Context
     (Self : access Object;
      N    : Name;
      NC   : NamingContext.Ref;
      Exc  : access Any.Any)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      pragma Debug (C, O ("Bind_Context: enter"));
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last, Exc);
      if not Is_Empty (Exc.all) then
         return;
      end if;
      pragma Debug (C, O ("Bind_Context: len is" & Len'Img));

      if Len /= 1 then
         pragma Debug (C, O ("Bind_Context: binding relative name "
           & To_String (Last.id)));
         PSNNC.Bind_Context (Ctx, To_Name (Last), NC);
      else
         declare
            BON : constant String := Encode (Self.Self, Last);

         begin
            PTM.Enter (Critical_Section);
            if Look_For_BO_In_NC (Self.Self, BON) /= null then
               PTM.Leave (Critical_Section);
               raise AlreadyBound;
            end if;

            Append_BO_To_NC (Self.Self, BON, Last, Ncontext,
                             PolyORB.References.Ref (NC));
            PTM.Leave (Critical_Section);
         end;
      end if;
   end Bind_Context;

   ----------------------
   -- Bind_New_Context --
   ----------------------

   function Bind_New_Context
     (Self : access Object;
      N    : Name;
      Exc  : access Any.Any) return NamingContext.Ref
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last, Exc);
      if not Is_Empty (Exc.all) then
         return NamingContext.Nil_Ref;
      end if;

      if Len /= 1 then
         return PSNNC.Bind_New_Context (Ctx, To_Name (Last));

      else
         Ctx := New_Context (Self);
         Bind_Context (Self, N, Ctx, Exc);
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
     (Text : String;
      NC   : Object_Ptr)
   is
      BO : Bound_Object_Ptr;

   begin
      pragma Debug (C, O (Text));

      BO := NC.Head;
      while BO /= null loop
         pragma Debug
           (C, O (String (NC.Key)
                    & " ... "
                    & To_Standard_String (BO.BN.id)
                    & ASCII.HT
                    & To_Standard_String (BO.BN.kind)
                    & ASCII.HT
                    & BO.BT'Img));
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

   function Equal (F1, F2 : String_Access) return Boolean is
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
      NC   : out    NameComponent;
      Exc  : access Any.Any)
   is
      use Names;

   begin
      pragma Debug (C, O ("Get_Ctx_And_Last_NC: enter"));
      Valid (Self.Self);

      PTM.Enter (Critical_Section);
      declare
         NCA         : constant Element_Array :=
           To_Element_Array (Sequence (N));
         Current_Obj : PolyORB.References.Ref;
         Current_Ctx : NamingContext.Ref;
         Current_Idx : Natural;

      begin
         PTM.Leave (Critical_Section);

         Len := NCA'Length;
         if Len = 0 then
            raise InvalidName;
         end if;

         if Len > 1 then
            Current_Idx := NCA'First;
            pragma Debug
              (C, O ("Get_Ctx_And_Last_NC: resolve "
                  & To_String (NCA (Current_Idx).id)));
            Current_Obj := Resolve (Self, To_Name (NCA (Current_Idx)), Exc);
            if not Is_Empty (Exc.all) then
               return;
            end if;
            Current_Ctx := NamingContext.Helper.To_Ref (Current_Obj);

            Current_Idx := Current_Idx + 1;
            while Current_Idx < NCA'Last loop
               pragma Debug
                 (C, O ("Get_Ctx_And_Last_NC: resolve "
                     & To_String (NCA (Current_Idx).id)));
               Current_Obj := PSNNC.Resolve
                 (Current_Ctx, To_Name (NCA (Current_Idx)));
               Current_Ctx := NamingContext.Helper.To_Ref (Current_Obj);
               Current_Idx := Current_Idx + 1;
            end loop;

            Ctx := Current_Ctx;
         end if;

         NC  := NCA (NCA'Last);

      exception
         --      when PolyORB.Exceptions.Bad_Param =>
         when others =>
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

   function Hash (F : String_Access) return Hash_Header is
      N : Natural := 0;

   begin
      --  Add up characters of name, mod our table size

      for J in F'Range loop
         N := (N + Character'Pos (F (J))) mod (Hash_Header'Last + 1);
      end loop;

      return N;
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PTM.Create (Critical_Section);
   end Initialize;

   ----------
   -- Is_A --
   ----------

   function Is_A (Logical_Type_Id : Standard.String)
                  return PolyORB.Types.Boolean
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Logical_Type_Id);
      pragma Warnings (On);
      return True;
   end Is_A;

   ----------
   -- List --
   ----------

--    procedure List
--      (Self     : access Object;
--       How_Many : PolyORB.Types.Unsigned_Long;
--       BL       : out    BindingList;
--       BI       : out    BindingIterator_Forward.Ref)
--    is
--       use PolyORB.Services.Naming.BindingIterator.Servant;

--       Len  : Natural := 0;
--       Size : Natural := Natural (How_Many);
--       Head : Bound_Object_Ptr;
--       Iter : BindingIterator.Servant.Object_Ptr;

--    begin
--       Valid (Self.Self);

--       PTM.Enter (Critical_Section);

--       --  How many bound objects in this naming context.

--       Head := Self.Head;
--       while Head /= null loop
--          Len  := Len + 1;
--          Head := Head.Next;
--       end loop;
--       Head := Self.Head;

--       --  First, copy the first bound objects to fill BL.

--       if Len < Size then
--          Size := Len;
--       end if;

--       if Size > 0 then
--          declare
--             Table : Bindings.Element_Array (1 .. Size);
--          begin
--             for I in 1 .. Size loop
--                Table (I) := (To_Name (Head.BN), Head.BT);
--                Head := Head.Next;
--             end loop;
--             BL  := BindingList (Bindings.To_Sequence (Table));
--             Len := Len - Size;
--          end;
--       end if;

--       Iter       := BindingIterator.Servant.Create;
--       Iter.Index := 1;
--       Iter.Table := new Bindings.Element_Array (1 .. Len);

--       --  Copy the remaining bound objects into the iterator.

--       for I in Iter.Table'Range loop
--          Iter.Table (I) := (To_Name (Head.BN), Head.BT);
--          Head := Head.Next;
--       end loop;

--       PTM.Leave (Critical_Section);

--       --  Activate object Iterator.

--       --  PolyORB.CORBA_P.Server_Tools.Initiate_Servant
--       --    (PortableServer.Servant (Iter), BI);
--    end List;

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
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.Errors;

      Exception_Error : Error_Container;

      My_Ref : NamingContext.Ref;

   begin
      PolyORB.Minimal_Servant.Tools.Initiate_Servant
         (Create,
          To_PolyORB_String ("NAMING"),
          PolyORB.References.Ref (My_Ref),
          Exception_Error);

      if Found (Exception_Error) then
         raise Program_Error;
      end if;

      return My_Ref;
   end New_Context;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (Self : access Object;
      N    : Name;
      Obj  : PolyORB.References.Ref;
      Exc  : access Any.Any)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last, Exc);
      if not Is_Empty (Exc.all) then
         return;
      end if;

      if Len /= 1 then
         PSNNC.Rebind (Ctx, To_Name (Last), Obj);

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            PTM.Enter (Critical_Section);
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               PTM.Leave (Critical_Section);
               declare
                  Member : NotFound_Members;
               begin
                  Member.why          := missing_node;
                  Member.rest_of_name := Null_Name;
                  PolyORB.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            if BO.BT /= Nobject then
               PTM.Leave (Critical_Section);
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
            Append_BO_To_NC   (Self.Self, BON, Last, Nobject, Obj);
            PTM.Leave (Critical_Section);
         end;
      end if;
   end Rebind;

   --------------------
   -- Rebind_Context --
   --------------------

   procedure Rebind_Context
     (Self : access Object;
      N    : Name;
      NC   : NamingContext.Ref;
      Exc  : access Any.Any)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last, Exc);
      if not Is_Empty (Exc.all) then
         return;
      end if;

      if Len /= 1 then
         PSNNC.Rebind_Context (Ctx, To_Name (Last), NC);

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            PTM.Enter (Critical_Section);
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               PTM.Leave (Critical_Section);
               declare
                  Member : NotFound_Members;
               begin
                  Member.why          := missing_node;
                  Member.rest_of_name := Null_Name;
                  PolyORB.Exceptions.User_Raise_Exception
                    (NotFound'Identity, Member);
               end;
            end if;

            if BO.BT /= Ncontext then
               PTM.Leave (Critical_Section);
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
            Append_BO_To_NC (Self.Self, BON, Last, Ncontext,
                             PolyORB.References.Ref (NC));
            PTM.Leave (Critical_Section);
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
      N    : Name;
      Exc  : access Any.Any) return PolyORB.References.Ref
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last, Exc);
      if not Is_Empty (Exc.all) then
         return References.Nil_Ref;
      end if;

      if Len /= 1 then
         return PSNNC.Resolve (Ctx, To_Name (Last));

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;
            Obj : PolyORB.References.Ref;

         begin
            PTM.Enter (Critical_Section);
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               PTM.Leave (Critical_Section);
               declare
                  Member : NotFound_Members;

               begin
                  Member.why          := missing_node;
                  Member.rest_of_name := Null_Name;
                  Exc.all := To_Any (Member);
                  return References.Nil_Ref;
               end;
            end if;

            Obj := BO.Obj;
            PTM.Leave (Critical_Section);
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
      N    : Name;
      Exc  : access Any.Any)
   is
      Len  : Natural;
      Ctx  : NamingContext.Ref;
      Last : NameComponent;

   begin
      Get_Ctx_And_Last_NC (Self, N, Len, Ctx, Last, Exc);
      if not Is_Empty (Exc.all) then
         return;
      end if;

      if Len /= 1 then
         PSNNC.Unbind (Ctx, To_Name (Last));

      else
         declare
            BON : constant String := Encode (Self.Self, Last);
            BO  : Bound_Object_Ptr;

         begin
            PTM.Enter (Critical_Section);
            BO := Look_For_BO_In_NC (Self.Self, BON);

            if BO = null then
               PTM.Leave (Critical_Section);
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
            PTM.Leave (Critical_Section);
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
            PTM.Leave (Critical_Section);
         end if;
         raise CannotProceed;
      end if;
   end Valid;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"naming.NamingContext.servant",
       Conflicts => Empty,
       Depends   => +"tasking.mutexes",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Services.Naming.NamingContext.Servant;
