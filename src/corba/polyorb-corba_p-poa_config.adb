------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . C O R B A _ P . P O A _ C O N F I G            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with CORBA.Policy;
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

with PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
with PolyORB.POA_Policies.Servant_Retention_Policy.Retain;

with PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
with PolyORB.POA_Policies.Thread_Policy.Single_Thread;
with PolyORB.POA_Policies.Thread_Policy.Main_Thread;

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
   package RPP renames PolyORB.POA_Policies.Request_Processing_Policy;

   use PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
   use PolyORB.POA_Policies.Servant_Retention_Policy.Retain;

   use PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
   use PolyORB.POA_Policies.Thread_Policy.Single_Thread;
   use PolyORB.POA_Policies.Thread_Policy.Main_Thread;

   ------------------------
   -- Convert_PolicyList --
   ------------------------

   function Convert_PolicyList
     (List : CORBA.Policy.PolicyList)
     return PolyORB.POA_Policies.PolicyList
   is

      package PS renames PolyORB.POA_Policies.Policy_Sequences;
      package ISP renames CORBA.Policy.IDL_Sequence_Policy;

      CORBA_Policy_Array : constant
        ISP.Element_Array := ISP.To_Element_Array (ISP.Sequence (List));

      Result : PolicyList;
      Policy : CORBA.PolicyType;

   begin
      for J in CORBA_Policy_Array'Range loop
         Policy := PortableServer.ThreadPolicy.Get_Policy_Type
           (PortableServer.ThreadPolicy.Ref (CORBA_Policy_Array (J).all));

         case Policy is

            when THREAD_POLICY_ID =>
               declare
                  PolicyValue : constant ThreadPolicyValue
                    := Get_Value (PortableServer.ThreadPolicy.Ref
                                  (CORBA_Policy_Array (J).all));
               begin
                  case PolicyValue is
                     when ORB_CTRL_MODEL =>
                        PS.Append
                          (Result,
                           Policy_Access (Thread_Policy.ORB_Ctrl.Create));

                     when SINGLE_THREAD_MODEL =>
                        PS.Append
                          (Result,
                           Policy_Access (Thread_Policy.Single_Thread.Create));

                     when MAIN_THREAD_MODEL =>
                        PS.Append
                          (Result,
                           Policy_Access (Thread_Policy.Main_Thread.Create));
                  end case;
               end;

            when LIFESPAN_POLICY_ID =>
               declare
                  PolicyValue : constant LifespanPolicyValue
                    := Get_Value (PortableServer.LifespanPolicy.Ref
                                  (CORBA_Policy_Array (J).all));
               begin
                  case PolicyValue is
                     when PortableServer.TRANSIENT =>
                        PS.Append
                          (Result,
                           Policy_Access (Lifespan_Policy.Transient.Create));


                     when PERSISTENT =>
                        PS.Append
                          (Result,
                           Policy_Access (Lifespan_Policy.Persistent.Create));
                  end case;
               end;

            when ID_UNIQUENESS_POLICY_ID =>
               declare
                  PolicyValue : constant IdUniquenessPolicyValue
                    := Get_Value (PortableServer.IdUniquenessPolicy.Ref
                                  (CORBA_Policy_Array (J).all));
               begin
                  case PolicyValue is
                     when UNIQUE_ID =>
                        PS.Append
                          (Result,
                           Policy_Access (Id_Uniqueness_Policy.Unique.Create));

                     when MULTIPLE_ID =>
                        PS.Append
                          (Result,
                           Policy_Access
                           (Id_Uniqueness_Policy.Multiple.Create));
                  end case;
               end;

            when ID_ASSIGNMENT_POLICY_ID =>
               declare
                  PolicyValue : constant IdAssignmentPolicyValue
                    := Get_Value (PortableServer.IdAssignmentPolicy.Ref
                                  (CORBA_Policy_Array (J).all));
               begin
                  case PolicyValue is
                     when USER_ID =>
                        PS.Append
                          (Result,
                           Policy_Access (Id_Assignment_Policy.User.Create));

                     when SYSTEM_ID =>
                        PS.Append
                          (Result,
                           Policy_Access (Id_Assignment_Policy.System.Create));
                  end case;
               end;

            when IMPLICIT_ACTIVATION_POLICY_ID =>
               declare
                  PolicyValue : constant ImplicitActivationPolicyValue
                    := Get_Value (PortableServer.ImplicitActivationPolicy.Ref
                                  (CORBA_Policy_Array (J).all));
               begin
                  case PolicyValue is
                     when IMPLICIT_ACTIVATION =>
                        PS.Append
                          (Result,
                           Policy_Access
                           (Implicit_Activation_Policy.Activation.Create));

                     when NO_IMPLICIT_ACTIVATION =>
                        PS.Append
                          (Result,
                           Policy_Access
                           (Implicit_Activation_Policy.No_Activation.Create));
                  end case;
               end;

            when SERVANT_RETENTION_POLICY_ID =>
               declare
                  PolicyValue : constant ServantRetentionPolicyValue
                    := Get_Value (PortableServer.ServantRetentionPolicy.Ref
                                  (CORBA_Policy_Array (J).all));
               begin
                  case PolicyValue is
                     when RETAIN =>
                        PS.Append
                          (Result,
                           Policy_Access
                           (Servant_Retention_Policy.Retain.Create));

                     when NON_RETAIN =>
                        PS.Append
                          (Result,
                           Policy_Access
                           (Servant_Retention_Policy.Non_Retain.Create));
                  end case;
               end;

            when REQUEST_PROCESSING_POLICY_ID =>
               declare
                  PolicyValue : constant RequestProcessingPolicyValue
                    := Get_Value (PortableServer.RequestProcessingPolicy.Ref
                                  (CORBA_Policy_Array (J).all));
               begin
                  case PolicyValue is

                     when USE_ACTIVE_OBJECT_MAP_ONLY =>
                        PS.Append
                          (Result,
                           Policy_Access (RPP.Active_Object_Map_Only.Create));

                     when USE_DEFAULT_SERVANT =>
                        PS.Append
                          (Result,
                           Policy_Access (RPP.Use_Default_Servant.Create));


                     when USE_SERVANT_MANAGER =>
                        raise Not_Implemented;
                  end case;
               end;

            when others =>
               raise Program_Error;
         end case;

      end loop;

      return Result;
   end Convert_PolicyList;

end PolyORB.CORBA_P.POA_Config;
