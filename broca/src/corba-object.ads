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

--  with Ada.Unchecked_Deallocation;

--  with Broca.Refs;

--  with CORBA.ImplementationDef;
--  with CORBA.InterfaceDef;
with CORBA.Context;
with CORBA.NVList;
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
   --  To allow such a behavior, a reference is in fact an access to a tagged
   --  type, ref_type, defined in broca.object.
   --  type Ref is new Broca.Refs.Ref with private;

   --  CORBA 2.3
   type Ref is new CORBA.AbstractBase.Ref with private;

   function Object_To_String (Obj : Ref) return CORBA.String;
   --  Returns the IOR corresponding to this object it is called by
   --  CORBA.ORB.Object_To_String see CORBA specification for details

   --  function Is_Nil  (Self : in Ref) return CORBA.Boolean;
   --  function Is_Null (Self : in Ref) return CORBA.Boolean renames Is_Nil;

--    procedure Release (Self : in out Ref);

--    function Is_A
--      (Self            : in Ref;
--       Logical_Type_Id : in CORBA.String)
--       return CORBA.Boolean;
--    --  Returns True if this object is of this Logical_Type_Id (here
--    --  Logical_Type_Id is a Repository_Id) or one of its descendants

--    function Non_Existent
--      (Self : in Ref)
--       return CORBA.Boolean;
--    --  Returns True if the ORB knows that the implementation referenced by
--    --  this proxy object does not exist

--    function Is_Equivalent
--      (Self  : in Ref;
--       Other : in Ref)
--       return CORBA.Boolean;
--    --  Returns True if both objects point to the same distant
--    --  implementation

--    function Hash
--      (Self    : in Ref;
--       Maximum : in CORBA.Unsigned_Long)
--       return CORBA.Unsigned_Long;
--    --  Return a hash value for object not implemented yet, it returns 0



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

private

   type Ref is new CORBA.AbstractBase.Ref with null record;


   -----------
   --  Any  --
   -----------

   type Content_ObjRef is new Content with
      record
         Value : Ref;
      end record;
   type Content_ObjRef_Ptr is access all Content_ObjRef;

end CORBA.Object;
