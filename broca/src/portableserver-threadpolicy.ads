with PortableServer;
with CORBA;
with CORBA.Policy;

package PortableServer.ThreadPolicy is
   type Ref is new CORBA.Policy.Ref with null record;
   -- function Copy (Self: Ref) return Ref;

   function Get_Value (Self : Ref) return Portableserver.ThreadPolicyValue;
end PortableServer.ThreadPolicy;
