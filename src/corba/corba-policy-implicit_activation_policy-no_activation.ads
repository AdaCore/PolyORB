package CORBA.Policy.Implicit_Activation_Policy.No_Activation is

   type No_Activation_Policy is new ImplicitActivationPolicy with null record;
   type No_Activation_Policy_Access is access all No_Activation_Policy;

   function Create return No_Activation_Policy_Access;

end CORBA.Policy.Implicit_Activation_Policy.No_Activation;
