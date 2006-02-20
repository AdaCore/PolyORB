------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . O B J E C T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

with PolyORB.CORBA_P.Local;
with PolyORB.CORBA_P.Names;
with PolyORB.Initialization;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.HFunctions.Mul;
with PolyORB.Utils.Strings;

with CORBA.Object.Helper;
with CORBA.ORB;

package body CORBA.Object is

   use PolyORB.Smart_Pointers;

   type Internal_Object is new PolyORB.Smart_Pointers.Entity with record
      The_Object : PolyORB.Objects.Object_Id_Access;
   end record;
   type Internal_Object_Access is access all Internal_Object;

   --  Client stub for remote calls implementing predefined CORBA::Object
   --  operations

   RPC_Result_Name   : constant CORBA.Identifier := To_CORBA_String ("Result");

   RPC_Is_A_Op_Name  : constant CORBA.Identifier
                         := CORBA.To_CORBA_String ("_is_a");
   RPC_Is_A_Arg_Name : constant CORBA.Identifier
                         := To_CORBA_String ("Type_Id");

   function RPC_Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String) return CORBA.Boolean;
   --  Client stub for remote call for class membership determination.
   --  Note: The body of RPC_Is_A is essneitally a copy of generated code.

   RPC_Non_Existent_Op_Name : constant CORBA.Identifier
                                := To_CORBA_String ("_non_existent");

   function RPC_Non_Existent (Self : Ref) return CORBA.Boolean;
   --  Client stub for remote call for object (non)existence test.
   --  Note: The body of RPC_Non_Existent is essneitally a copy of generated
   --  code, with a specific added exception handler for the OBJEXT_NOT_EXIST
   --  case (where True is returned, and no exception is raised).

   RPC_Interface_Op_Name : constant CORBA.Identifier
                             := To_CORBA_String ("_interface");

   ----------
   -- Hash --
   ----------

   function Hash
     (Self    : Ref;
      Maximum : CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
      use PolyORB.Utils.HFunctions.Mul;

   begin
      return CORBA.Unsigned_Long
        (Hash (To_Standard_String (CORBA.ORB.Object_To_String (Self)),
               Default_Hash_Parameters,
               Natural (Maximum)));
   end Hash;

   -------------------
   -- Get_Interface --
   -------------------

   function Get_Interface
     (Self : Ref)
     return CORBA.Object.Ref'Class
   is
      Operation_Name   : CORBA.Identifier renames RPC_Interface_Op_Name;

      Request          : CORBA.Request.Object;
      Ctx              : constant CORBA.Context.Ref
        := CORBA.Context.Nil_Ref;
      Arg_List         : CORBA.NVList.Ref;
      Result           : CORBA.NamedValue;

   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         Raise_No_Implement (No_Implement_Members'(Minor     => 3,
                                                   Completed => Completed_No));
      end if;

      CORBA.ORB.Create_List (0, Arg_List);

      --  No arguments

      Result := (Name      => RPC_Result_Name,
                 Argument  => CORBA.Internals.Get_Empty_Any (TC_Object),
                 Arg_Modes => 0);

      CORBA.Object.Create_Request
        (Self, Ctx, Operation_Name, Arg_List, Result, Request, 0);

      CORBA.Request.Invoke (Request, 0);

      return CORBA.Object.Helper.From_Any (Result.Argument);
   end Get_Interface;

   --------------
   -- RPC_Is_A --
   --------------

   function RPC_Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String) return CORBA.Boolean
   is
      Operation_Name   : CORBA.Identifier renames RPC_Is_A_Op_Name;
      Arg_Name_Type_Id : CORBA.Identifier renames RPC_Is_A_Arg_Name;

      Request          : CORBA.Request.Object;
      Ctx              : constant CORBA.Context.Ref := CORBA.Context.Nil_Ref;
      Argument_Type_Id : CORBA.Any := CORBA.To_Any
        (To_CORBA_String (Logical_Type_Id));
      Arg_List         : CORBA.NVList.Ref;
      Result           : CORBA.NamedValue;

   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item
        (Arg_List,
         Arg_Name_Type_Id,
         Argument_Type_Id,
         CORBA.ARG_IN);

      Result :=
        (Name      => RPC_Result_Name,
         Argument  => CORBA.Internals.Get_Empty_Any (CORBA.TC_Boolean),
         Arg_Modes => 0);

      CORBA.Object.Create_Request
        (Self, Ctx, Operation_Name, Arg_List, Result, Request, 0);

      CORBA.Request.Invoke (Request, 0);

      return CORBA.From_Any (Result.Argument);
   end RPC_Is_A;

   ----------------------
   -- RPC_Non_Existent --
   ----------------------

   function RPC_Non_Existent (Self : Ref) return CORBA.Boolean is
      Operation_Name   : CORBA.Identifier renames RPC_Non_Existent_Op_Name;

      Request          : CORBA.Request.Object;
      Ctx              : constant CORBA.Context.Ref := CORBA.Context.Nil_Ref;
      Arg_List         : CORBA.NVList.Ref;
      Result           : CORBA.NamedValue;

   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      CORBA.ORB.Create_List (0, Arg_List);

      --  No arguments

      Result :=
        (Name      => RPC_Result_Name,
         Argument  => CORBA.Internals.Get_Empty_Any (CORBA.TC_Boolean),
         Arg_Modes => 0);

      CORBA.Object.Create_Request
        (Self, Ctx, Operation_Name, Arg_List, Result, Request, 0);

      --  Special case: for a non-existent object, return True instead of
      --  raising OBJECT_NOT_EXIST.

      begin
         CORBA.Request.Invoke (Request, 0);
         return CORBA.From_Any (Result.Argument);
      exception
         when CORBA.Object_Not_Exist =>
            return True;
      end;
   end RPC_Non_Existent;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String) return CORBA.Boolean is
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Any object is a CORBA::Object

      if Is_Equivalent
        (Logical_Type_Id,
         PolyORB.CORBA_P.Names.OMG_RepositoryId ("CORBA/Object"))
      then
         return True;
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then

         --  For true CORBA local objects, call corresponding local subprogram

         if PolyORB.CORBA_P.Local.Is_CORBA_Local (Self) then
            return PolyORB.CORBA_P.Local.Is_A
              (PolyORB.CORBA_P.Local.Local_Object_Base_Ref
                 (Entity_Of (Self)),
               Logical_Type_Id);

         --  Neutral core object

         else
            Raise_No_Implement (No_Implement_Members'
                                (Minor     => 3,
                                 Completed => Completed_No));

         end if;
      end if;

      --  Any object is of the class of its actual (i. e. most derived) type.

      if Is_Equivalent (Logical_Type_Id,
                        PolyORB.References.Type_Id_Of
                          (Internals.To_PolyORB_Ref (Self)))
      then
         return True;
      end if;

      --  If class membership cannot be determined locally, perform a remote
      --  call on the object. An exception may be raised (and propagated to the
      --  caller) if communication cannot be established to the remote ORB.

      return RPC_Is_A (Self, Logical_Type_Id);
   end Is_A;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent
     (Self         : Ref;
      Other_Object : Ref'Class)
      return Boolean
   is
      use PolyORB.References;

      S_Is_Local : constant Boolean := PolyORB.CORBA_P.Local.Is_Local (Self);
      O_Is_Local : constant Boolean
        := PolyORB.CORBA_P.Local.Is_Local (Other_Object);

   begin
      if Is_Nil (Self) or else Is_Nil (Other_Object) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      if S_Is_Local or else O_Is_Local then
         return Entity_Of (Self) = Entity_Of (Other_Object);
      end if;

      declare
         Left, Right : PolyORB.References.Ref;
      begin
         Set (Left, Entity_Of (Self));
         Set (Right, Entity_Of (Other_Object));
         return Is_Same_Object (Left, Right);
      end;
   end Is_Equivalent;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Self : Ref) return CORBA.Boolean is
   begin
      return Is_Nil (PolyORB.Smart_Pointers.Ref (Self));
   end Is_Nil;

   ------------------
   -- Non_Existent --
   ------------------

   function Non_Existent (Self : Ref) return CORBA.Boolean is
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         --  We do not authoritatively know that the designated object is
         --  non-existent, because we have a non-nil local reference.
         --  ??? we could perform an OA lookup here to return a more precise
         --  result.
         return False;

      else
         return RPC_Non_Existent (Self);
      end if;
   end Non_Existent;

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request
     (Self      : Ref;
      Ctx       : CORBA.Context.Ref;
      Operation : Identifier;
      Arg_List  : CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : Flags) is
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         Raise_No_Implement (No_Implement_Members'(Minor     => 4,
                                                   Completed => Completed_No));
      end if;

      CORBA.Request.Create_Request
        (CORBA.AbstractBase.Ref (Self),
         Ctx,
         Operation,
         Arg_List,
         Result,
         Request,
         Req_Flags);
   end Create_Request;

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request
     (Self      : Ref;
      Ctx       : CORBA.Context.Ref;
      Operation : Identifier;
      Arg_List  : CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : ExceptionList.Ref;
      Ctxt_List : ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : Flags) is
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         Raise_No_Implement (No_Implement_Members'(Minor     => 4,
                                                   Completed => Completed_No));
      end if;

      CORBA.Request.Create_Request
        (CORBA.AbstractBase.Ref (Self),
         Ctx,
         Operation,
         Arg_List,
         Result,
         Exc_List,
         Ctxt_List,
         Request,
         Req_Flags);
   end Create_Request;

   ---------------
   -- Duplicate --
   ---------------

   procedure Duplicate (Self : in out Ref) is
   begin
      Duplicate (PolyORB.Smart_Pointers.Ref (Self));
   end Duplicate;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Ref) is
   begin
      Release (PolyORB.Smart_Pointers.Ref (Self));
   end Release;

   ----------------------
   -- Object_To_String --
   ----------------------

   function  Object_To_String
     (Obj : CORBA.Object.Ref'Class)
     return CORBA.String is
   begin
      --  Object locality checked inside CORBA.ORB.

      return CORBA.ORB.Object_To_String (Obj);
   end Object_To_String;

   package body Internals is

      -----------------------
      -- To_PolyORB_Object --
      -----------------------

      function To_PolyORB_Object
        (R : Ref)
        return PolyORB.Objects.Object_Id
      is
      begin
         return Internal_Object_Access (Entity_Of (R)).The_Object.all;
      end To_PolyORB_Object;

      --------------------
      -- To_PolyORB_Ref --
      --------------------

      function To_PolyORB_Ref (R : Ref)
                              return PolyORB.References.Ref
      is
         E : constant PolyORB.Smart_Pointers.Entity_Ptr
           := Entity_Of (R);
         Result : PolyORB.References.Ref;
      begin
         PolyORB.References.Set (Result, E);
         return Result;
      end To_PolyORB_Ref;

      --------------------------
      -- Convert_To_CORBA_Ref --
      --------------------------

      procedure Convert_To_CORBA_Ref
        (Neutral_Ref : PolyORB.References.Ref;
         CORBA_Ref   : in out CORBA.Object.Ref'Class)
      is
         E : constant PolyORB.Smart_Pointers.Entity_Ptr
           := PolyORB.References.Entity_Of (Neutral_Ref);
      begin
         Set (CORBA_Ref, E);
      end Convert_To_CORBA_Ref;

   end Internals;

   ---------------
   -- TC_Object --
   ---------------

   TC_Object_Cache : CORBA.TypeCode.Object;

   function TC_Object return CORBA.TypeCode.Object is
   begin
      return TC_Object_Cache;
   end TC_Object;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      TC_Object_Cache := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Object);
      CORBA.TypeCode.Internals.Add_Parameter
        (TC_Object_Cache, To_Any (To_CORBA_String ("Object")));
      CORBA.TypeCode.Internals.Add_Parameter
        (TC_Object_Cache, To_Any (To_CORBA_String ("IDL:CORBA/Object:1.0")));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"corba.object",
       Conflicts => Empty,
       Depends   => +"corba" & "any",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end CORBA.Object;
