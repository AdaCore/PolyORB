with Droopi.Objects;
with Droopi.Smart_Pointers;
with Droopi.References.IOR;

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

   --  function Get_Interface
   --    (Self : in Ref) return CORBA.InterfaceDef.Ref;

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

   function  Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
     return CORBA.String;

   function To_Droopi_Object
     (R : in Ref)
     return Droopi.Objects.Object_Id;
   --  XXX What is this supposed to do?
   --   It is not possible in general to associate a Droopi Object_Id
   --   with a CORBA.Object.Ref. This can be done only when R designates
   --   an object located on this middleware instance.

   function To_Droopi_Ref
     (R : in Ref)
     return Droopi.References.Ref;

   type Reference_Info is new Droopi.Smart_Pointers.Entity with record
      IOR : Droopi.References.IOR.IOR_Type;
   end record;
   --  XXX Actually, type info from IOR_Type should be moved into
   --  Droopi.References.Reference_Info, and then IOR_Type can be
   --  suppressed completely, and Droopi.References.Ref/Reference_Info
   --  can be used instead.

private

   type Ref is new CORBA.AbstractBase.Ref with null record;

   pragma Inline (Object_To_String);

end CORBA.Object;
