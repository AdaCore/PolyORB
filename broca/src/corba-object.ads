--  with Ada.Unchecked_Deallocation;

with Broca.Refs;

--  with CORBA.ImplementationDef;
--  with CORBA.InterfaceDef;
--  with CORBA.Context;
--  with CORBA.NVList;
--  with CORBA.Request;

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
   type Ref is new Broca.Refs.Ref with private;

   function Object_To_String (Obj : Ref) return CORBA.String;
   --  Returns the IOR corresponding to this object it is called by
   --  CORBA.ORB.Object_To_String see CORBA specification for details

   function Is_Nil  (Self : in Ref) return CORBA.Boolean;
   function Is_Null (Self : in Ref) return CORBA.Boolean renames Is_Nil;

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
   --    function To_Any (From : in Ref) return Any;
   --    function To_Ref (From : in Any) return Ref;
   --
   --    function Get_Implementation (Self : in Ref)
   --      return CORBA.ImplementationDef.Ref;
   --
   --    function Get_Interface (Self : in Ref)
   --      return CORBA.InterfaceDef.Ref;
   --
   --    procedure Create_Request
   --      (Self      : in     Ref;
   --       Ctx       : in     CORBA.Context.Object;
   --       Operation : in     Identifier;
   --       Arg_List  : in     CORBA.NVList.Object;
   --       Result    : in out NamedValue;
   --       Request   :    out CORBA.Request.Object;
   --       Req_Flags : in     Flags;
   --       Returns   : out    Status);

private

   type Ref is new Broca.Refs.Ref with null record;

end CORBA.Object;
