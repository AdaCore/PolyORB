with CORBA.Policy_Values; use CORBA.Policy_Values;
with CORBA.POA_Types;     use CORBA.POA_Types;

package CORBA.Policy.Implicit_Activation_Policy is

   type ImplicitActivationPolicy is abstract new Policy with
     record
         Value : ImplicitActivationPolicyValue;
     end record;
   subtype Implicit_Activation_Policy is ImplicitActivationPolicy;
   type ImplicitActivationPolicy_Access is
     access all ImplicitActivationPolicy'Class;
   subtype Implicit_Activation_Policy_Access is
     ImplicitActivationPolicy_Access;

   function Create (Value : ImplicitActivationPolicyValue)
                   return ImplicitActivationPolicy_Access;
   function Create (P : ImplicitActivationPolicy)
                   return ImplicitActivationPolicy_Access;
   --  The factory to create the different policies according to
   --  the value of Value

   function Create return ImplicitActivationPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   procedure Free (P   : in     ImplicitActivationPolicy;
                   Ptr : in out Policy_Access)
      is abstract;

--    function Activate_Servant
--      (Self             : access ImplicitActivationPolicy;
--       OA               : access CORBA.POA_Types.Obj_Adapter;
--       P_Servant        : Servant)
--      return Object_Id is abstract;
--    --  Case IMPLICIT_ACTIVATION:
--    --  Activate the servant and returns its Id
--    --  Case NO_IMPLICIT_ACTIVATION:
--    --  Returns Nil

end CORBA.Policy.Implicit_Activation_Policy;
