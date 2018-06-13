------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R T _ P O A . B A S I C _ R T _ P O A           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Log;
with PolyORB.POA_Policies.Implicit_Activation_Policy;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.RT_POA.Basic_RT_POA is

   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.POA;

   package L is new Log.Facility_Log ("polyorb.rt_poa.basic_rt_poa");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   type Oid_Information is record
      U_Oid                 : PolyORB.POA_Types.Unmarshalled_Oid;
      Model                 : Priority_Model;
      Oid_ORB_Priority      : ORB_Priority;
      Oid_External_Priority : External_Priority;
   end record;

   package Oid_Lists is
     new PolyORB.Utils.Chained_Lists (Oid_Information);
   use Oid_Lists;
   subtype Oid_List is Oid_Lists.List;

   Shadow_Oids : Oid_List;
   --  This list keeps track of information that have to be stored
   --  along with the Oid.

   procedure Set_Policies
     (OA       : access Basic_RT_Obj_Adapter;
      Policies :        POA_Policies.PolicyList);
   --  Set OA policies from the values in Policies.

   ------------------
   -- Set_Policies --
   ------------------

   procedure Set_Policies
     (OA       : access Basic_RT_Obj_Adapter;
      Policies :        POA_Policies.PolicyList)
   is
      use PolyORB.POA_Policies;
      use Policy_Lists;

      It : Policy_Lists.Iterator := First (Policies);

      A_Policy : Policy_Access;

   begin
      while not Last (It) loop
         A_Policy := Value (It).all;

         if A_Policy.all in PriorityModelPolicy'Class then
            if OA.Priority_Model_Policy /= null then
               pragma Debug
                 (C, O ("Duplicate in PriorityModelPolicy: using last one"));
               null;
            end if;
            OA.Priority_Model_Policy := PriorityModelPolicy_Access (A_Policy);
            pragma Debug (C, O ("Setting up PriorityModelPolicy"));
         end if;

         if A_Policy.all in ThreadPoolPolicy'Class then
            if OA.Thread_Pool_Policy /= null then
               pragma Debug
                 (C, O ("Duplicate in ThreadPoolPolicy: using last one"));
               null;
            end if;
            OA.Thread_Pool_Policy := ThreadPoolPolicy_Access (A_Policy);
            pragma Debug (C, O ("Setting up ThreadPoolPolicy"));
         end if;

         Next (It);
      end loop;
   end Set_Policies;

   ---------------------------------------------
   -- CORBA-like POA interface implementation --
   ---------------------------------------------

   ----------------
   -- Create_POA --
   ----------------

   overriding procedure Create_POA
     (Self         : access Basic_RT_Obj_Adapter;
      Adapter_Name :        Standard.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out PolyORB.POA.Obj_Adapter_Access;
      Error        : in out PolyORB.Errors.Error_Container)
   is
   begin
      POA := new Basic_RT_Obj_Adapter;

      Initialize_POA
        (PolyORB.POA.Obj_Adapter (Self.all)'Access,
         Adapter_Name,
         A_POAManager,
         Policies,
         PolyORB.POA.Obj_Adapter_Access (POA),
         Error);

      if Found (Error) then
         return;
      end if;

      Set_Policies (Basic_RT_Obj_Adapter (POA.all)'Access, Policies);
   end Create_POA;

   ------------
   -- Export --
   ------------

   overriding procedure Export
     (OA    : access Basic_RT_Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
   begin
      pragma Debug (C, O ("Export: enter"));

      --  Export servant

      Export (PolyORB.POA.Obj_Adapter (OA.all)'Access, Obj,
              Key, Oid, Error);

      if Found (Error) then
         return;
      end if;

      --  XXX Caching must be propagated to all procedures that store
      --  a servant in the POA, to be investigated !!

      if OA.Priority_Model_Policy /= null then
         --  Cache information on Priority_Model_Policy in servant

         Set_Servant_Priority_Information (OA.Priority_Model_Policy.all, Obj);

      end if;

      if OA.Thread_Pool_Policy /= null then
         --  Cache information on Thread_Pool_Policy in servant

         Set_Servant_Lane (OA.Thread_Pool_Policy.all, Obj);

      end if;

      pragma Debug (C, O ("Export: leave"));
   end Export;

   -------------------------------
   -- Get_Scheduling_Parameters --
   -------------------------------

   overriding procedure Get_Scheduling_Parameters
     (Self                     : access Basic_RT_Obj_Adapter;
      Id                       : Object_Id_Access;
      Model                    :    out Priority_Model;
      Server_ORB_Priority      :    out ORB_Priority;
      Server_External_Priority :    out External_Priority;
      Error                    : in out PolyORB.Errors.Error_Container)
   is
      Servant : Servants.Servant_Access;

   begin
      Find_Servant (Self, Id, Servant, Error);

      if Found (Error) then
         declare
            U_Oid  : Unmarshalled_Oid;
            It     : Iterator := First (Shadow_Oids);
            Error2 : PolyORB.Errors.Error_Container;
         begin
            Oid_To_U_Oid (Id.all, U_Oid, Error2);
            if Found (Error2) then
               Catch (Error);
               Error := Error2;
               pragma Warnings (Off);
               --  Model, Server_External_Priority, and Server_ORB_Priority not
               --  set before return
               return;
               pragma Warnings (On);
            end if;
            while not Last (It) loop
               if U_Oid = Value (It).all.U_Oid then
                  Model := Value (It).all.Model;
                  Server_ORB_Priority := Value (It).all.Oid_ORB_Priority;
                  Server_External_Priority
                    := Value (It).all.Oid_External_Priority;

                  Catch (Error);
                  return;
               end if;
               Next (It);
            end loop;

            return;
         end;

      end if;

      Get_Servant_Priority_Information
        (Servant,
         Model,
         Server_ORB_Priority,
         Server_External_Priority,
         Error);
   end Get_Scheduling_Parameters;

   ------------------------------------------------
   -- CORBA-like RT POA interface implementation --
   ------------------------------------------------

   ------------------------------------------------
   -- Create_Object_Identification_With_Priority --
   ------------------------------------------------

   overriding procedure Create_Object_Identification_With_Priority
     (Self                     : access Basic_RT_Obj_Adapter;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.POA_Policies.Implicit_Activation_Policy;

   begin
      --  Check Self's policies are correct

      Ensure_No_Implicit_Activation
        (Self.Implicit_Activation_Policy.all,
         Error);

      if Found (Error) then
         return;
      end if;

      if Self.Priority_Model_Policy = null
        or else Self.Priority_Model_Policy.Model /= SERVER_DECLARED
      then
         Throw (Error, WrongPolicy_E, Null_Members'(Null_Member));
         return;
      end if;

      --  Check Server_External_Priority is correct

      if Self.Thread_Pool_Policy = null
        or else not Is_Valid_Priority (Self.Thread_Pool_Policy.all,
                                       Server_External_Priority)
      then
         Throw (Error,
                Bad_Param_E,
                System_Exception_Members'(Minor     => 0,
                                          Completed => Completed_No));
         return;
      end if;

      Create_Object_Identification (Self, Hint, U_Oid, Error);

      Append
        (Shadow_Oids,
         Oid_Information'(U_Oid,
                          SERVER_DECLARED,
                          Server_ORB_Priority,
                          Server_External_Priority));
   end Create_Object_Identification_With_Priority;

   ------------------------------------------
   -- Activate_Object_With_Id_And_Priority --
   ------------------------------------------

   overriding procedure Activate_Object_With_Id_And_Priority
     (Self                     : access Basic_RT_Obj_Adapter;
      P_Servant                :        Servants.Servant_Access;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.POA_Policies.Implicit_Activation_Policy;

      It : Iterator := First (Shadow_Oids);

   begin
      --  Check Self's policies are correct

      Ensure_No_Implicit_Activation
        (Self.Implicit_Activation_Policy.all,
         Error);

      if Found (Error) then
         return;
      end if;

      if Self.Priority_Model_Policy = null
        or else Self.Priority_Model_Policy.Model /= SERVER_DECLARED
      then
         Throw (Error, WrongPolicy_E, Null_Members'(Null_Member));
         return;
      end if;

      --  Check Server_External_Priority is correct

      if Self.Thread_Pool_Policy = null
        or else not Is_Valid_Priority (Self.Thread_Pool_Policy.all,
                                       Server_External_Priority)
      then
         Throw (Error,
                Bad_Param_E,
                System_Exception_Members'(Minor     => 0,
                                          Completed => Completed_No));
         return;
      end if;

      --  Activate object

      Activate_Object (Self, P_Servant, Hint, U_Oid, Error);

      if Found (Error) then
         return;
      end if;

      --  Check the object has not been previously set up with a
      --  different priority.

      while not Last (It) loop
         if U_Oid = Value (It).all.U_Oid
           and then Value (It).all.Oid_External_Priority
           /= Server_External_Priority
         then
            Throw (Error,
                   Bad_Inv_Order_E,
                   System_Exception_Members'(Minor     => 18,
                                             Completed => Completed_No));
            return;
         end if;
         Next (It);
      end loop;

      --  Cache information on Priority_Model_Policy

      Set_Servant_Priority_Information
        (Self.Priority_Model_Policy.all,
         P_Servant,
         Server_ORB_Priority,
         Server_External_Priority,
         Error);

      if Self.Thread_Pool_Policy /= null then
         --  Cache information on Thread_Pool_Policy in servant

         Set_Servant_Lane (Self.Thread_Pool_Policy.all, P_Servant);

      end if;

   end Activate_Object_With_Id_And_Priority;

end PolyORB.RT_POA.Basic_RT_POA;
