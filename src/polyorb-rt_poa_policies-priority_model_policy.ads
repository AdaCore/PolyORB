------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.RT_POA_POLICIES.PRIORITY_MODEL_POLICY               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Errors;
with PolyORB.POA_Policies;
with PolyORB.Servants;
with PolyORB.Tasking.Priorities;

package PolyORB.RT_POA_Policies.Priority_Model_Policy is

   use PolyORB.POA_Policies;
   use PolyORB.Tasking.Priorities;

   type Priority_Model is
     (CLIENT_PROPAGATED,
      SERVER_DECLARED);

   type PriorityModelPolicy (Model : Priority_Model) is
     new PolyORB.POA_Policies.Policy with private;

   type PriorityModelPolicy_Access is access all PriorityModelPolicy'Class;

   function Create
     (Model                    : Priority_Model;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority)
     return Policy_Access;

   function Policy_Id (Self : PriorityModelPolicy) return String;

   procedure Check_Compatibility
     (Self           :        PriorityModelPolicy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   procedure Get_Servant_Priority_Information
     (Servant                  :        Servants.Servant_Access;
      Model                    :    out Priority_Model;
      Server_ORB_Priority      :    out ORB_Priority;
      Server_External_Priority :    out External_Priority;
      Error                    : in out PolyORB.Errors.Error_Container);
   --  Retrieve information on ThreadPoolPolicy stored in Servant

   procedure Set_Servant_Priority_Information
     (Self    : PriorityModelPolicy;
      Servant : PolyORB.Servants.Servant_Access);
   --  Cache Self information into Servant, use Self data

   procedure Set_Servant_Priority_Information
     (Self                     :        PriorityModelPolicy;
      Servant                  :        Servants.Servant_Access;
      Server_ORB_Priority      :        ORB_Priority;
      Server_External_Priority :        External_Priority;
      Error                    : in out PolyORB.Errors.Error_Container);
   --  Cache Self information into Servant. Force values to
   --  Server_ORB_Priority and Server_External_Priority.

private

   type PriorityModelPolicy (Model : Priority_Model) is
     new PolyORB.POA_Policies.Policy
     with record
        Server_ORB_Priority      : ORB_Priority;
        Server_External_Priority : External_Priority := Invalid_Priority;
     end record;

end PolyORB.RT_POA_Policies.Priority_Model_Policy;
