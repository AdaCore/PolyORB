
package PortableServer.LifespanPolicy is

   type Ref is new CORBA.Policy.Ref with null record;

   function Get_Value (Self : Ref) return LifespanPolicyValue;

end PortableServer.LifespanPolicy;
