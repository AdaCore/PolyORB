with CORBA.Policy;

package PortableServer.ServantRetentionPolicy is

   type Ref is new CORBA.Policy.Ref with null record;

   function Get_Value (Self : Ref)
     return ServantRetentionPolicyValue;

end PortableServer.ServantRetentionPolicy;
