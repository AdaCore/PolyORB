with CORBA;
with CORBA.Policy;

with Droopi.Smart_Pointers;

package Minimum_CORBA.Object is

   type Ref is new Droopi.Smart_Pointers.Ref with private;

   function Is_Nil  (Self : in Ref) return CORBA.Boolean;
   function Is_Null (Self : in Ref) return CORBA.Boolean
     renames Is_Nil;

   procedure Release (Self : in out Ref);

   function Is_Equivalent
     (Self         : Ref;
      Other_Object : Ref'Class) return Boolean;

   function Hash
     (Self    : Ref;
      Maximum : CORBA.Unsigned_Long) return CORBA.Unsigned_Long;

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

end Minimum_CORBA.Object;


