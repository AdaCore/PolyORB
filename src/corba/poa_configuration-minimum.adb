with Droopi.POA_Policies.Id_Assignment_Policy.System;
with Droopi.POA_Policies.Id_Uniqueness_Policy.Unique;
with Droopi.POA_Policies.Implicit_Activation_Policy.No_Activation;
with Droopi.POA_Policies.Lifespan_Policy.Transient;
with Droopi.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
with Droopi.POA_Policies.Servant_Retention_Policy.Retain;
with Droopi.POA_Policies.Thread_Policy.Orb_Ctrl;
with CORBA.Policy_Values;

package body  POA_Configuration.Minimum is

   use Droopi.POA_Policies;
   use Droopi.POA_Policies.Policies_Factory_Pkg;
   use CORBA.Policy_Values;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : Minimum_Configuration;
                         F : Droopi.POA_Policies.Policies_Factory)
   is
   begin
      Register
        (F.all,
         SYSTEM_ID,
         Policy_Access (Id_Assignment_Policy.System.Create));
      Register
        (F.all,
         UNIQUE_ID,
         Policy_Access (Id_Uniqueness_Policy.Unique.Create));
      Register
        (F.all,
         NO_IMPLICIT_ACTIVATION,
         Policy_Access (Implicit_Activation_Policy.No_Activation.Create));
      Register
        (F.all,
         TRANSIENT,
         Policy_Access (Lifespan_Policy.Transient.Create));
      Register
        (F.all,
         USE_ACTIVE_OBJECT_MAP_ONLY,
         Policy_Access (Request_Processing_Policy.
                        Active_Object_Map_Only.Create));
      Register
        (F.all,
         RETAIN,
         Policy_Access (Servant_Retention_Policy.Retain.Create));
      Register
        (F.all,
         ORB_CTRL_MODEL,
         Policy_Access (Thread_Policy.Orb_Ctrl.Create));
   end Initialize;

end POA_Configuration.Minimum;
