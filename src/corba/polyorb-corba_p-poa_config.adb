------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . P O A _ C O N F I G            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PortableServer.IdAssignmentPolicy;
with PortableServer.IdUniquenessPolicy;
with PortableServer.ImplicitActivationPolicy;
with PortableServer.LifespanPolicy;
with PortableServer.RequestProcessingPolicy;
with PortableServer.ServantRetentionPolicy;
with PortableServer.ThreadPolicy;

with PolyORB.POA_Policies.Id_Assignment_Policy.System;
with PolyORB.POA_Policies.Id_Assignment_Policy.User;

with PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
with PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique;

with PolyORB.POA_Policies.Implicit_Activation_Policy.Activation;
with PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation;

with PolyORB.POA_Policies.Lifespan_Policy.Persistent;
with PolyORB.POA_Policies.Lifespan_Policy.Transient;

with PolyORB.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
with PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
with PolyORB.POA_Policies.Request_Processing_Policy.Use_Servant_Manager;

with PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
with PolyORB.POA_Policies.Servant_Retention_Policy.Retain;

with PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
with PolyORB.POA_Policies.Thread_Policy.Single_Thread;
with PolyORB.POA_Policies.Thread_Policy.Main_Thread;

with PolyORB.Utils.Chained_Lists;

package body PolyORB.CORBA_P.POA_Config is

   use CORBA;

   use PortableServer;
   use PortableServer.IdAssignmentPolicy;
   use PortableServer.IdUniquenessPolicy;
   use PortableServer.ImplicitActivationPolicy;
   use PortableServer.LifespanPolicy;
   use PortableServer.RequestProcessingPolicy;
   use PortableServer.ServantRetentionPolicy;
   use PortableServer.ThreadPolicy;

   use PolyORB.POA_Policies;

   use PolyORB.POA_Policies.Id_Assignment_Policy.System;
   use PolyORB.POA_Policies.Id_Assignment_Policy.User;

   use PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
   use PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique;

   use PolyORB.POA_Policies.Implicit_Activation_Policy.Activation;
   use PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation;

   use PolyORB.POA_Policies.Lifespan_Policy.Persistent;
   use PolyORB.POA_Policies.Lifespan_Policy.Transient;

   use PolyORB.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
   use PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
   use PolyORB.POA_Policies.Request_Processing_Policy.Use_Servant_Manager;
   package RPP renames PolyORB.POA_Policies.Request_Processing_Policy;

   use PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
   use PolyORB.POA_Policies.Servant_Retention_Policy.Retain;

   use PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
   use PolyORB.POA_Policies.Thread_Policy.Single_Thread;
   use PolyORB.POA_Policies.Thread_Policy.Main_Thread;

   type Allocator_Record is record
      Policy : CORBA.PolicyType;
      Allocator : Policy_Type_Allocator;
   end record;

   package Allocator_List is
      new PolyORB.Utils.Chained_Lists (Allocator_Record);
   use Allocator_List;

   Callbacks : Allocator_List.List;

   ------------------------
   -- Convert_PolicyList --
   ------------------------

   function Convert_PolicyList
     (List : CORBA.Policy.PolicyList)
     return PolyORB.POA_Policies.PolicyList
   is
      package PL renames PolyORB.POA_Policies.Policy_Lists;

      package ISP renames CORBA.Policy.IDL_SEQUENCE_Policy;

      CORBA_Policy_Array : constant
        ISP.Element_Array := ISP.To_Element_Array (ISP.Sequence (List));

      Result : PolicyList;
      Policy : CORBA.PolicyType;

   begin
      for J in CORBA_Policy_Array'Range loop
         Policy := CORBA.Policy.Get_Policy_Type (CORBA_Policy_Array (J));

         case Policy is

            when THREAD_POLICY_ID =>
               declare
                  PolicyValue : constant ThreadPolicyValue
                    := Get_Value
                    (PortableServer.ThreadPolicy.To_Ref
                     (CORBA_Policy_Array (J)));
               begin
                  case PolicyValue is
                     when ORB_CTRL_MODEL =>
                        PL.Append
                          (Result,
                           Policy_Access (Thread_Policy.ORB_Ctrl.Create));

                     when SINGLE_THREAD_MODEL =>
                        PL.Append
                          (Result,
                           Policy_Access (Thread_Policy.Single_Thread.Create));

                     when MAIN_THREAD_MODEL =>
                        PL.Append
                          (Result,
                           Policy_Access (Thread_Policy.Main_Thread.Create));
                  end case;
               end;

            when LIFESPAN_POLICY_ID =>
               declare
                  PolicyValue : constant LifespanPolicyValue
                    := Get_Value
                    (PortableServer.LifespanPolicy.To_Ref
                     (CORBA_Policy_Array (J)));
               begin
                  case PolicyValue is
                     when PortableServer.TRANSIENT =>
                        PL.Append
                          (Result,
                           Policy_Access (Lifespan_Policy.Transient.Create));

                     when PERSISTENT =>
                        PL.Append
                          (Result,
                           Policy_Access (Lifespan_Policy.Persistent.Create));
                  end case;
               end;

            when ID_UNIQUENESS_POLICY_ID =>
               declare
                  PolicyValue : constant IdUniquenessPolicyValue
                    := Get_Value
                    (PortableServer.IdUniquenessPolicy.To_Ref
                     (CORBA_Policy_Array (J)));
               begin
                  case PolicyValue is
                     when UNIQUE_ID =>
                        PL.Append
                          (Result,
                           Policy_Access (Id_Uniqueness_Policy.Unique.Create));

                     when MULTIPLE_ID =>
                        PL.Append
                          (Result,
                           Policy_Access
                           (Id_Uniqueness_Policy.Multiple.Create));
                  end case;
               end;

            when ID_ASSIGNMENT_POLICY_ID =>
               declare
                  PolicyValue : constant IdAssignmentPolicyValue
                    := Get_Value
                    (PortableServer.IdAssignmentPolicy.To_Ref
                     (CORBA_Policy_Array (J)));
               begin
                  case PolicyValue is
                     when USER_ID =>
                        PL.Append
                          (Result,
                           Policy_Access (Id_Assignment_Policy.User.Create));

                     when SYSTEM_ID =>
                        PL.Append
                          (Result,
                           Policy_Access (Id_Assignment_Policy.System.Create));
                  end case;
               end;

            when IMPLICIT_ACTIVATION_POLICY_ID =>
               declare
                  PolicyValue : constant ImplicitActivationPolicyValue
                    := Get_Value
                    (PortableServer.ImplicitActivationPolicy.To_Ref
                     (CORBA_Policy_Array (J)));
               begin
                  case PolicyValue is
                     when IMPLICIT_ACTIVATION =>
                        PL.Append
                          (Result,
                           Policy_Access
                           (Implicit_Activation_Policy.Activation.Create));

                     when NO_IMPLICIT_ACTIVATION =>
                        PL.Append
                          (Result,
                           Policy_Access
                           (Implicit_Activation_Policy.No_Activation.Create));
                  end case;
               end;

            when SERVANT_RETENTION_POLICY_ID =>
               declare
                  PolicyValue : constant ServantRetentionPolicyValue
                    := Get_Value
                    (PortableServer.ServantRetentionPolicy.To_Ref
                     (CORBA_Policy_Array (J)));
               begin
                  case PolicyValue is
                     when RETAIN =>
                        PL.Append
                          (Result,
                           Policy_Access
                           (Servant_Retention_Policy.Retain.Create));

                     when NON_RETAIN =>
                        PL.Append
                          (Result,
                           Policy_Access
                           (Servant_Retention_Policy.Non_Retain.Create));
                  end case;
               end;

            when REQUEST_PROCESSING_POLICY_ID =>
               declare
                  PolicyValue : constant RequestProcessingPolicyValue
                    := Get_Value
                    (PortableServer.RequestProcessingPolicy.To_Ref
                     (CORBA_Policy_Array (J)));
               begin
                  case PolicyValue is

                     when USE_ACTIVE_OBJECT_MAP_ONLY =>
                        PL.Append
                          (Result,
                           Policy_Access (RPP.Active_Object_Map_Only.Create));

                     when USE_DEFAULT_SERVANT =>
                        PL.Append
                          (Result,
                           Policy_Access (RPP.Use_Default_Servant.Create));

                     when USE_SERVANT_MANAGER =>
                        PL.Append
                          (Result,
                           Policy_Access (RPP.Use_Servant_Manager.Create));
                  end case;
               end;

            when others =>
               null;
         end case;

         --  Iterate through allocators' list

         declare
            Iter : Iterator := First (Callbacks);
         begin
            while not Last (Iter) loop
               declare
                  Info : constant Allocator_Record := Value (Iter).all;

               begin
                  if Policy = Info.Policy then
                     PL.Append (Result,
                                Info.Allocator.all (CORBA_Policy_Array (J)));
                     exit;
                  end if;
               end;
               Next (Iter);
            end loop;
         end;
      end loop;

      return Result;
   end Convert_PolicyList;

   --------------
   -- Register --
   --------------

   procedure Register
     (Policy    : CORBA.PolicyType;
      Allocator : Policy_Type_Allocator)
   is
      Elt : constant Allocator_Record := (Policy, Allocator);

   begin
      Append (Callbacks, Elt);
   end Register;

end PolyORB.CORBA_P.POA_Config;
