
package PortableServer.IdAssignmentPolicy is

   type Ref is new CORBA.Policy.Ref with null record;

   function Get_Value (Self : Ref) return IdAssignmentPolicyValue;

end PortableServer.IdAssignmentPolicy;
