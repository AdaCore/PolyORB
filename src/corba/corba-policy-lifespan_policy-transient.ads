package CORBA.Policy.Lifespan_Policy.Transient is

   type Transient_Policy is new LifespanPolicy with null record;
   type Transient_Policy_Access is access all Transient_Policy;

   function Create return Transient_Policy_Access;

end CORBA.Policy.Lifespan_Policy.Transient;
