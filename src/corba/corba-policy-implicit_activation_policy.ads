with CORBA.Policy_Values; use CORBA.Policy_Values;
with CORBA.POA_Types;     use CORBA.POA_Types;

package CORBA.Policy.Implicit_Activation_Policy is

   type ImplicitActivationPolicy is abstract new Policy with private;
   type ImplicitActivationPolicy_Access is
     access all ImplicitActivationPolicy;

   function Activate_Servant
     (Self             : access ImplicitActivationPolicy;
      OA               : access CORBA.POA_Types.Obj_Adapter;
      P_Servant        : Servant)
     return Object_Id is abstract;
   --  Case IMPLICIT_ACTIVATION:
   --  Activate the servant and returns its Id
   --  Case NO_IMPLICIT_ACTIVATION:
   --  Returns Nil

private
   type ImplicitActivationPolicy is abstract new Policy with
      record
         Value : ImplicitActivationPolicyValue;
      end record;

end CORBA.Policy.Implicit_Activation_Policy;
