with CORBA;
with CORBA.Request;
with CORBA.Context;
with CORBA.NVList;
with CORBA.Policy;
with Droopi.Smart_Pointers;

package CORBA.Object is

   type Ref is new Droopi.Smart_Pointers.Ref with private;

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

   function Hash
     (Self    : Ref;
      Maximum : CORBA.Unsigned_Long) return CORBA.Unsigned_Long;

   procedure Create_Request
     (Self      : in     Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : access NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags);

   --  ??? The following subprogram is declared a function in
   --  the Ada Language Mapping specification.

   type SetOverrideType is (SET_OVERRIDE, ADD_OVERRIDE);
   procedure Set_Policy_Overrides
     (Self : in Ref;
      Policies : CORBA.Policy.PolicyList;
      Set_Add : SetOverrideType);

   --  Requires CORBA.DomainManager to be implemented.

   --     function Get_Domain_Managers
   --       (Self : Ref)
   --       return CORBA.DomainManager.DomainManagerList;

private

   type Ref is new Droopi.Smart_Pointers.Ref with null record;

end CORBA.Object;

