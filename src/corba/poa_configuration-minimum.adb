with Droopi.POA_Policies.Id_Assignment_Policy.System;
with Droopi.POA_Policies.Id_Uniqueness_Policy.Unique;
with Droopi.POA_Policies.Implicit_Activation_Policy.No_Activation;
with Droopi.POA_Policies.Lifespan_Policy.Transient;
with Droopi.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
with Droopi.POA_Policies.Servant_Retention_Policy.Retain;
with Droopi.POA_Policies.Thread_Policy.ORB_Ctrl;

package body  POA_Configuration.Minimum is

   use Droopi.POA_Policies;
   use Droopi.POA_Policies.Policy_Repository_Pkg;

   ----------------
   -- Initialize --
   ----------------

   My_Default_Policies : aliased PolicyList;

   procedure Initialize
     (C : Minimum_Configuration;
      F : Droopi.POA_Policies.Policy_Repository)
   is
      use Droopi.POA_Policies.Policy_Sequences;
      P : constant Element_Array
        := (Policy_Access (Id_Assignment_Policy.System.Create),
            Policy_Access (Id_Uniqueness_Policy.Unique.Create),
            Policy_Access (Implicit_Activation_Policy.No_Activation.Create),
            Policy_Access (Lifespan_Policy.Transient.Create),
            Policy_Access
              (Request_Processing_Policy.Active_Object_Map_Only.Create),
            Policy_Access (Servant_Retention_Policy.Retain.Create),
            Policy_Access (Thread_Policy.ORB_Ctrl.Create));
   begin
      for I in P'Range loop
         Register (F.all, Policy_Id (P (I).all), P (I));
      end loop;
      My_Default_Policies := To_Sequence (P);
--       Register
--         (F.all,
--          SYSTEM_ID,
--          Policy_Access (Id_Assignment_Policy.System.Create));
--       Register
--         (F.all,
--          UNIQUE_ID,
--          Policy_Access (Id_Uniqueness_Policy.Unique.Create));
--       Register
--         (F.all,
--          NO_IMPLICIT_ACTIVATION,
--          Policy_Access (Implicit_Activation_Policy.No_Activation.Create));
--       Register
--         (F.all,
--          TRANSIENT,
--          Policy_Access (Lifespan_Policy.Transient.Create));
--       Register
--         (F.all,
--          USE_ACTIVE_OBJECT_MAP_ONLY,
--          Policy_Access (Request_Processing_Policy.
--                         Active_Object_Map_Only.Create));
--       Register
--         (F.all,
--          RETAIN,
--          Policy_Access (Servant_Retention_Policy.Retain.Create));
--       Register
--         (F.all,
--          ORB_CTRL_MODEL,
--          Policy_Access (Thread_Policy.ORB_Ctrl.Create));
   end Initialize;

   function Default_Policies
     (C : Minimum_Configuration)
     return Droopi.POA_Policies.PolicyList_Access is
   begin
      return My_Default_Policies'Access;
   end Default_Policies;

end POA_Configuration.Minimum;
