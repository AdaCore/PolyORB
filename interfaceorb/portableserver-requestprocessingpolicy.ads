
package PortableServer.RequestProcessingPolicy is

   type Ref is new CORBA.Policy.Ref with null record;

   function Get_Value (Self : Ref)
     return RequestProcessingPolicyValue;

end PortableServer.RequestProcessingPolicy;
