------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         C O R B A . O B J E C T                          --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA.Context;
with CORBA.NVList;
with CORBA.ContextList;
with CORBA.ExceptionList;
with CORBA.Request;

with CORBA.AbstractBase;

package CORBA.Object is

   ------------------------------
   -- CORBA 2.2 28 February 99 --
   ------------------------------

   --  A reference to an object.
   --  type REF can be derived (see client mapping § 21.8); however, the base
   --  type REF must be able to reference an internal object (see
   --  resolve_initial_references).
   --
   --  To allow such a behavior, a reference is in fact an access to tagged
   --  type Entity, defined in broca.object.

   --  CORBA 2.3
   type Ref is new CORBA.AbstractBase.Ref with private;
   type Ref_Ptr is access all Ref;

   function Object_To_String (Obj : Ref) return CORBA.String;
   --  Returns the IOR corresponding to this object it is called by
   --  CORBA.ORB.Object_To_String see CORBA specification for details

   function Is_A
     (Self            : in Ref;
      Logical_Type_Id : in Standard.String)
      return CORBA.Boolean;
   --  Returns True if this object is of this Logical_Type_Id
   --  or one of its descendants.

   --    not yet implemented
   --
   --    function To_Ref (From : in Any) return Ref;
   --
   --    function Get_Implementation (Self : in Ref)
   --      return CORBA.ImplementationDef.Ref;
   --
   --    function Get_Interface (Self : in Ref)
   --      return CORBA.InterfaceDef.Ref;

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

   Nil_Ref : constant Ref;

   function TC_Object return CORBA.TypeCode.Object;

private

   type Ref is new CORBA.AbstractBase.Ref with null record;
   procedure Deallocate is new Ada.Unchecked_Deallocation (Ref, Ref_Ptr);

   -----------
   --  Any  --
   -----------

   type Content_ObjRef is new Content with
      record
         Value : Ref_Ptr;
      end record;
   type Content_ObjRef_Ptr is access all Content_ObjRef;
   procedure Deallocate (Object : access Content_ObjRef);
   function Duplicate (Object : access Content_ObjRef)
     return Any_Content_Ptr;

   Nil_Ref : constant Ref
     := (CORBA.AbstractBase.Nil_Ref with null record);

end CORBA.Object;
