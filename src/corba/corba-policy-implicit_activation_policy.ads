with CORBA.POA_Types;     use CORBA.POA_Types;

package CORBA.Policy.Implicit_Activation_Policy is

   type ImplicitActivationPolicy is abstract new Policy with null record;
   subtype Implicit_Activation_Policy is ImplicitActivationPolicy;
   type ImplicitActivationPolicy_Access is
     access all ImplicitActivationPolicy'Class;
   subtype Implicit_Activation_Policy_Access is
     ImplicitActivationPolicy_Access;

   function Create return ImplicitActivationPolicy_Access is abstract;
   --  The real creation function that has to be implemented for each
   --  possible Policy

   function Activate_Servant (Self      : ImplicitActivationPolicy;
                              OA        : CORBA.POA_Types.Obj_Adapter_Access;
                              P_Servant : Servant_Access)
                             return Object_Id_Access
      is abstract;
   --  Case NO_ACTIVATION:
   --    Returns null
   --  Case ACTIVATION:
   --    Activates the servant in the Active Object Map.

   procedure Free (P   : in     ImplicitActivationPolicy;
                   Ptr : in out Policy_Access)
      is abstract;

end CORBA.Policy.Implicit_Activation_Policy;
