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

with PolyORB.POA_Policies.Id_Assignment_Policy.System;
with PolyORB.POA_Policies.Id_Assignment_Policy.User;

with PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
with PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique;

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

   use PolyORB.POA_Policies.Id_Assignment_Policy.System;
   use PolyORB.POA_Policies.Id_Assignment_Policy.User;

   use PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
   use PolyORB.POA_Policies.Id_Uniqueness_Policy.Unique;

   use PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation;

   use PolyORB.POA_Policies.Lifespan_Policy.Persistent;
   use PolyORB.POA_Policies.Lifespan_Policy.Transient;

   use PolyORB.POA_Policies.Request_Processing_Policy.Active_Object_Map_Only;
   use PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;

   use PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
   use PolyORB.POA_Policies.Servant_Retention_Policy.Retain;

   use PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
   use PolyORB.POA_Policies.Thread_Policy.Single_Thread;
   use PolyORB.POA_Policies.Thread_Policy.Main_Thread;

   use PortableServer;
   use PolyORB.POA_Policies;

   -----------------------
   -- Create_PolicyList --
   -----------------------

   function Create_PolicyList
     (Tp           : PortableServer.ThreadPolicyValue;
      Lp           : PortableServer.LifespanPolicyValue;
      Up           : PortableServer.IdUniquenessPolicyValue;
      Ip           : PortableServer.IdAssignmentPolicyValue;
      Ap           : PortableServer.ImplicitActivationPolicyValue;
      Sp           : PortableServer.ServantRetentionPolicyValue;
      Rp           : PortableServer.RequestProcessingPolicyValue)
     return PolyORB.POA_Policies.PolicyList
   is
      use PolyORB.POA_Policies.Policy_Sequences;

      Result : PolicyList;
   begin

      --  Thread policy.

      case Tp is
         when ORB_CTRL_MODEL =>
            Append (Result, Policy_Access (Thread_Policy.ORB_Ctrl.Create));

         when SINGLE_THREAD_MODEL =>
            Append (Result,
                    Policy_Access (Thread_Policy.Single_Thread.Create));

         when MAIN_THREAD_MODEL =>
            Append (Result,
                    Policy_Access (Thread_Policy.Main_Thread.Create));
      end case;

      --  Lifespan policy.

      case Lp is
         when TRANSIENT =>
            Append (Result, Policy_Access (Lifespan_Policy.Transient.Create));

         when PERSISTENT =>
            Append (Result, Policy_Access (Lifespan_Policy.Persistent.Create));

      end case;

      --  IdUniqueness policy.

      case Up is
         when UNIQUE_ID =>
            Append (Result,
                    Policy_Access (Id_Uniqueness_Policy.Unique.Create));

         when MULTIPLE_ID =>
            Append (Result,
                    Policy_Access (Id_Uniqueness_Policy.Multiple.Create));

      end case;

      --  IdAssignment policy.

      case Ip is
         when USER_ID =>
            Append (Result, Policy_Access (Id_Assignment_Policy.User.Create));

         when SYSTEM_ID =>
            Append (Result,
                    Policy_Access (Id_Assignment_Policy.System.Create));
      end case;

      --  ImplicitActivation policy.

      case Ap is
         when IMPLICIT_ACTIVATION =>
            raise Not_Implemented;

         when NO_IMPLICIT_ACTIVATION =>
            Append (Result,
                    Policy_Access
                    (Implicit_Activation_Policy.No_Activation.Create));
      end case;

      --  ServantRetention policy.

      case Sp is
         when RETAIN =>
            Append (Result,
                    Policy_Access (Servant_Retention_Policy.Retain.Create));

         when NON_RETAIN =>
            Append (Result,
                    Policy_Access
                    (Servant_Retention_Policy.Non_Retain.Create));

      end case;

      --  RequestProcessing policy.

      case Rp is
         when USE_ACTIVE_OBJECT_MAP_ONLY =>
            Append (Result,
                    Policy_Access
                    (Request_Processing_Policy.Active_Object_Map_Only.Create));

         when USE_DEFAULT_SERVANT =>
            Append (Result,
                    Policy_Access
                    (Request_Processing_Policy.Use_Default_Servant.Create));

         when USE_SERVANT_MANAGER =>
            raise Not_Implemented;
      end case;

      return Result;
   end Create_PolicyList;

end PolyORB.CORBA_P.POA_Config;
