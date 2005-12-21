------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . O B J E C T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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

--  The following subprograms still have to be implemented :
--     Non_Existent

with PolyORB.CORBA_P.Local;
with PolyORB.CORBA_P.Names;
with PolyORB.Initialization;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.HFunctions.Mul;
with PolyORB.Utils.Strings;

with CORBA.AbstractBase;
with CORBA.Object.Helper;
with CORBA.ORB;

package body CORBA.Object is

   use PolyORB.Smart_Pointers;

   type Internal_Object is new PolyORB.Smart_Pointers.Entity with record
      The_Object : PolyORB.Objects.Object_Id_Access;
   end record;
   type Internal_Object_Access is access all Internal_Object;

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
     (Self : in Ref)
     return CORBA.Object.Ref'Class
   is
      Operation_Name   : constant CORBA.Identifier
        := CORBA.To_CORBA_String ("_interface");

      Request          : CORBA.Request.Object;
      Ctx              : constant CORBA.Context.Ref
        := CORBA.Context.Nil_Ref;
      Arg_List         : CORBA.NVList.Ref;
      Result           : CORBA.NamedValue;
      Result_Name      : CORBA.String := To_CORBA_String ("Result");
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         Raise_No_Implement (No_Implement_Members'(Minor     => 3,
                                                   Completed => Completed_No));
      end if;

      --  Create argument list (empty)

      CORBA.ORB.Create_List (0, Arg_List);

      --  Set result type (maybe void)

      Result := (Name      => CORBA.Identifier (Result_Name),
                 Argument  => CORBA.Internals.Get_Empty_Any (TC_Object),
                 Arg_Modes => 0);

      CORBA.Object.Create_Request
        (Self, Ctx, Operation_Name, Arg_List, Result, Request, 0);

      CORBA.Request.Invoke (Request, 0);

      --  Request has been synchronously invoked.

      --  Retrieve return value.

      return CORBA.Object.Helper.From_Any (Result.Argument);
   end Get_Interface;

   --------------
   -- RPC_Is_A --
   --------------

   function RPC_Is_A
     (Self            : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean;
   --  Perform a remote call on Self (a reference that designates
   --  a CORBA object) for class membership determination.
   --  Note: the body of RPC_Is_A is a copy of generated code.

   function RPC_Is_A
     (Self            : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean
   is
      Operation_Name   : constant CORBA.Identifier
        := CORBA.To_CORBA_String ("_is_a");

      Arg_Name_Type_Id : CORBA.Identifier
        := To_CORBA_String ("Type_Id");
      Request          : CORBA.Request.Object;
      Ctx              : constant CORBA.Context.Ref
        := CORBA.Context.Nil_Ref;
      Argument_Type_Id : CORBA.Any := CORBA.To_Any
        (To_CORBA_String (Logical_Type_Id));
      Arg_List         : CORBA.NVList.Ref;
      Result           : CORBA.NamedValue;
      Result_Name      : CORBA.String := To_CORBA_String ("Result");
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list

      CORBA.ORB.Create_List (0, Arg_List);
      CORBA.NVList.Add_Item
        (Arg_List,
         Arg_Name_Type_Id,
         Argument_Type_Id,
         CORBA.ARG_IN);

      --  Set result type (maybe void)

      Result
        := (Name      => CORBA.Identifier (Result_Name),
            Argument  => CORBA.Internals.Get_Empty_Any (CORBA.TC_Boolean),
            Arg_Modes => 0);

      CORBA.Object.Create_Request
        (Self, Ctx, Operation_Name, Arg_List, Result, Request, 0);

      CORBA.Request.Invoke (Request, 0);

      --  Request has been synchronously invoked.

      --  Retrieve return value.

      return CORBA.From_Any (Result.Argument);
   end RPC_Is_A;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean
   is
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      if Is_Equivalent
        (Logical_Type_Id,
         PolyORB.CORBA_P.Names.OMG_RepositoryId ("CORBA/Object"))
      --  Any object Is_A CORBA::Object.
      then
         return True;
      end if;

      if PolyORB.CORBA_P.Local.Is_Local (Self) then
         if PolyORB.CORBA_P.Local.Is_CORBA_Local (Self) then
            --  For true CORBA local objects call corresponding subprogram.
            return PolyORB.CORBA_P.Local.Is_A
                    (PolyORB.CORBA_P.Local.Local_Object_Base_Ref
                      (Entity_Of (Self)),
                     Logical_Type_Id);
         else
            --  Neutral core object.
            Raise_No_Implement (No_Implement_Members'
                                (Minor     => 3,
                                 Completed => Completed_No));

         end if;
      end if;

      if Is_Equivalent
        (Logical_Type_Id,
         PolyORB.References.Type_Id_Of
         (Internals.To_PolyORB_Ref (Self)))
      --  Any object is of the class of its actual (i. e. most derived) type.

      then
         return True;
      end if;

      --  If class membership cannot be determined locally,
      --  perform a remote call on the object.

      return RPC_Is_A (Self, Logical_Type_Id);
   exception
      when others =>
         return False;
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

   function Is_Nil (Self : in Ref) return CORBA.Boolean is
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
         return False;
      end if;

      raise Program_Error;
   end Non_Existent;

   --------------------
   -- Create_Request --
   --------------------

   procedure Create_Request
     (Self      : in     Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags) is
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
     (Self      : in     Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : in     ExceptionList.Ref;
      Ctxt_List : in     ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags) is
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
     (Obj : in CORBA.Object.Ref'Class)
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
        (R : in Ref)
        return PolyORB.Objects.Object_Id
      is
      begin
         return Internal_Object_Access (Entity_Of (R)).The_Object.all;
      end To_PolyORB_Object;

      --------------------
      -- To_PolyORB_Ref --
      --------------------

      function To_PolyORB_Ref (R : in Ref)
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
        (Neutral_Ref : in     PolyORB.References.Ref;
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
