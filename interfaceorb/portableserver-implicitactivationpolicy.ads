
package PortableServer.ImplicitActivationPolicy is

   type Ref is new CORBA.Policy.Ref with null record;

   function Get_Value (Self : Ref)
     return ImplicitActivationPolicyValue;

end PortableServer.ImplicitActivationPolicy;
