--  A POA configuration corresponding to minimumCORBA policies.

--  $Id$

with PolyORB.POA_Policies;
with PolyORB.POA_Policies.Id_Assignment_Policy.System;
with PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique;
with PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation;
with PolyORB.POA_Policies.Lifespan_Policy.Transient;
with PolyORB.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
with PolyORB.POA_Policies.Servant_Retention_Policy.Retain;
with PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;

package body PolyORB.POA_Config.Minimum is

   use PolyORB.POA_Policies;

   ----------------
   -- Initialize --
   ----------------

   My_Default_Policies : aliased PolicyList;

   procedure Initialize
     (C : Minimum_Configuration)
   is
      use PolyORB.POA_Policies.Policy_Sequences;
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
         PolyORB.POA_Policies.Policy_Repository.Register
           (Policy_Id (P (I).all), P (I));
      end loop;
      My_Default_Policies := To_Sequence (P);
   end Initialize;

   function Default_Policies
     (C : Minimum_Configuration)
     return PolyORB.POA_Policies.PolicyList_Access is
   begin
      return My_Default_Policies'Access;
   end Default_Policies;

end PolyORB.POA_Config.Minimum;
