------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . O B J E C T                          --
--                                                                          --
--                                 S p e c                                  --
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

--  $Id$

with PolyORB.Objects;
with PolyORB.References;

with CORBA.AbstractBase;
with CORBA.Context;
with CORBA.ContextList;
with CORBA.ExceptionList;
with CORBA.NVList;
with CORBA.Request;

package CORBA.Object is

   --  pragma Elaborate_Body;

   type Ref is new CORBA.AbstractBase.Ref with private;

   --  Requires CORBA.InterfaceDef to be implemented.

   function Get_Interface
     (Self : in Ref)
     return CORBA.Object.Ref'Class;
   --  Return a reference to the InterfaceDef that describes
   --  the designated object.

   function Is_Nil  (Self : in Ref) return CORBA.Boolean;
   function Is_Null (Self : in Ref) return CORBA.Boolean
     renames Is_Nil;

   procedure Release (Self : in out Ref);

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean;

   function Non_Existent (Self : Ref) return CORBA.Boolean;

   function Is_Equivalent
     (Self         : Ref;
      Other_Object : Ref'Class) return Boolean;

   procedure Create_Request
     (Self      : in     Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags);

   procedure Create_Request
     (Self      : in     Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : in     ExceptionList.Ref;
      Ctxt_List : in     ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags);

   function Hash
     (Self    : Ref;
      Maximum : CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

--    --  ??? The following subprogram is declared a function in
--    --  the Ada Language Mapping specification.

--    type SetOverrideType is (SET_OVERRIDE, ADD_OVERRIDE);
--    procedure Set_Policy_Overrides
--      (Self : in Ref;
--       Policies : CORBA.Policy.PolicyList;
--       Set_Add : SetOverrideType);

   --  Requires CORBA.DomainManager to be implemented.

   --     function Get_Domain_Managers
   --       (Self : Ref)
   --       return CORBA.DomainManager.DomainManagerList;

   function TC_Object return CORBA.TypeCode.Object;

   function  Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
     return CORBA.String;

   function To_PolyORB_Object
     (R : in Ref)
     return PolyORB.Objects.Object_Id;
   --  XXX What is this supposed to do?
   --   It is not possible in general to associate a PolyORB Object_Id
   --   with a CORBA.Object.Ref. This can be done only when R designates
   --   an object located on this middleware instance.

   function To_PolyORB_Ref (R : in Ref)
     return PolyORB.References.Ref;
   procedure Convert_To_CORBA_Ref
     (Neutral_Ref : in     PolyORB.References.Ref;
      CORBA_Ref   : in out CORBA.Object.Ref'Class);
   --  Conversion functions between CORBA and neutral references.

private

   type Ref is new CORBA.AbstractBase.Ref with null record;

   pragma Inline (Object_To_String);

end CORBA.Object;
