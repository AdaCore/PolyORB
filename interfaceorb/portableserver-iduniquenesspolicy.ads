
package PortableServer.IdUniquenessPolicy is

   type Ref is new CORBA.Policy.Ref with null record;

   function Get_Value (Self : Ref) return IdUniquenessPolicyValue;

end PortableServer.IdUniquenessPolicy;
