with CORBA;
with CORBA.Policy;
with Droopi.Objects;
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

   function To_CORBA_Object
     (O : in Droopi.Objects.Object_Id)
     return Ref;

   function To_Droopi_Object
     (R : in Ref)
     return Droopi.Objects.Object_Id;

private

   type Ref is new Droopi.Smart_Pointers.Ref with null record;

end CORBA.Object;

