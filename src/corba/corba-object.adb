------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . O B J E C T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  The following subprograms still have to be implemented :
--     Get_Policy
--     Non_Existent

--  $Id$

with GNAT.HTable;

with PolyORB.CORBA_P.Names;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Smart_Pointers;

with CORBA.AbstractBase;
with CORBA.Object.Helper;
with CORBA.ORB;

package body CORBA.Object is

   use PolyORB.Log;
   use PolyORB.Smart_Pointers;

   package L is new PolyORB.Log.Facility_Log ("corba.object");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   pragma Warnings (Off);
   pragma Unreferenced (O);
   pragma Warnings (On);

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
      type My_Range is new Long range 0 .. Long (Maximum);
      function My_Hash is new GNAT.HTable.Hash (My_Range);
   begin
      return CORBA.Unsigned_Long
        (My_Hash (To_Standard_String (CORBA.ORB.Object_To_String (Self))));
   end Hash;

   -------------------
   -- Get_Interface --
   -------------------

   function Get_Interface
     (Self : in Ref)
     return CORBA.Object.Ref'Class
   is
      Operation_Name   : constant CORBA.Identifier
        := CORBA.To_CORBA_String ("get_interface");

      Request          : CORBA.Request.Object;
      Ctx              : CORBA.Context.Ref := CORBA.Context.Nil_Ref;
      Arg_List         : CORBA.NVList.Ref;
      Result           : CORBA.NamedValue;
      Result_Name      : CORBA.String := To_CORBA_String ("Result");
   begin
      if Is_Nil (Self) then
         raise Constraint_Error;
      end if;

      --  Create argument list (empty)
      CORBA.ORB.Create_List (0, Arg_List);

      --  Set result type (maybe void)
      Result := (Name => CORBA.Identifier (Result_Name),
                 Argument => Get_Empty_Any (TC_Object),
                 Arg_Modes => 0);

      CORBA.Object.Create_Request
        (Self, Ctx, Operation_Name, Arg_List, Result, Request, 0);

      CORBA.Request.Invoke (Request, 0);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      return CORBA.Object.Helper.From_Any (Result.Argument);
   end Get_Interface;

   ----------
   -- Is_A --
   ----------

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
      Ctx              : CORBA.Context.Ref := CORBA.Context.Nil_Ref;
      Argument_Type_Id : CORBA.Any := CORBA.To_Any
        (To_CORBA_String (Logical_Type_Id));
      Arg_List         : CORBA.NVList.Ref;
      Result           : CORBA.NamedValue;
      Result_Name      : CORBA.String := To_CORBA_String ("Result");
   begin
      if Is_Nil (Self) then
         raise Constraint_Error;
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
        := (Name => CORBA.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (CORBA.TC_Boolean),
         Arg_Modes => 0);

      CORBA.Object.Create_Request
        (Self, Ctx, Operation_Name, Arg_List, Result, Request, 0);

      CORBA.Request.Invoke (Request, 0);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      return CORBA.From_Any (Result.Argument);
   end RPC_Is_A;

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean
   is
   begin
      if Is_Equivalent
        (Logical_Type_Id,
         PolyORB.CORBA_P.Names.OMG_RepositoryId ("CORBA/Object"))
      --  Any object Is_A CORBA::Object.

        or else Is_Equivalent
        (Logical_Type_Id,
         PolyORB.References.Type_Id_Of
         (To_PolyORB_Ref (Self)))
      --  Any object is of the class of its
      --  actual (i. e. most derived) type.

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
      use PolyORB.Smart_Pointers;
   begin
      return (Entity_Of (Self) = Entity_Of (Other_Object));
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
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Non_Existent (Self);
      --  "Possible infinite recursion"
      pragma Warnings (On);
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

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Ref) is
   begin
      Release (PolyORB.Smart_Pointers.Ref (Self));
   end Release;

   function  Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
     return CORBA.String is
   begin
      return CORBA.ORB.Object_To_String (Obj);
   end Object_To_String;

--    --------------------------
--    -- Set_Policy_Overrides --
--    --------------------------

--    procedure Set_Policy_Overrides
--      (Self     : in Ref;
--       Policies :    CORBA.Policy.PolicyList;
--       Set_Add  :    SetOverrideType)
--    is
--    begin
--       null;
--    end Set_Policy_Overrides;

   --  XXX remove
--    ---------------------
--    -- To_CORBA_Object --
--    ---------------------

--    function To_CORBA_Object
--      (O : in PolyORB.Objects.Object_Id)
--      return Ref
--    is
--       Result : Ref;
--       Internal : Internal_Object_Access;
--    begin
--       Internal := new Internal_Object;
--       Internal.The_Object := new PolyORB.Objects.Object_Id'(O);

--       PolyORB.Smart_Pointers.Set
--         (PolyORB.Smart_Pointers.Ref (Result),
--          Entity_Ptr (Internal));
--       return Result;
--    end To_CORBA_Object;

   ----------------------
   -- To_PolyORB_Object --
   ----------------------

   function To_PolyORB_Object
     (R : in Ref)
     return PolyORB.Objects.Object_Id
   is
   begin
      return Internal_Object_Access (Entity_Of (R)).The_Object.all;
   end To_PolyORB_Object;

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

   procedure Convert_To_CORBA_Ref
     (Neutral_Ref : in     PolyORB.References.Ref;
      CORBA_Ref   : in out CORBA.Object.Ref'Class)
   is
      E : constant PolyORB.Smart_Pointers.Entity_Ptr
        := PolyORB.References.Entity_Of (Neutral_Ref);
   begin
      Set (CORBA_Ref, E);
   end Convert_To_CORBA_Ref;

   function TC_Object return CORBA.TypeCode.Object is
      use PolyORB.Any.TypeCode;

      T : CORBA.TypeCode.Object := PolyORB.Any.TypeCode.TC_Object;
   begin
      Add_Parameter (T, To_Any (To_CORBA_String ("Object")));
      Add_Parameter (T, To_Any (To_CORBA_String ("IDL:CORBA/Object:1.0")));
      return T;
   end TC_Object;

end CORBA.Object;
