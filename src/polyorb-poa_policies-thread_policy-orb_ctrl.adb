------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.POA_POLICIES.THREAD_POLICY.ORB_CTRL                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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

--  Implementation of the 'ORB Control' POA Policy.

--  Under this policy, the ORB is responsible for the creation, management,
--  and destruction of threads used with one or more POAs.

with PolyORB.Servants;

package body PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl is

   ------------
   -- Create --
   ------------

   function Create return ORB_Ctrl_Policy_Access is
      Result : constant ORB_Ctrl_Policy_Access := new ORB_Ctrl_Policy;

   begin
      ThreadPolicy (Result.all).Executor := new ORB_Ctrl_Executor;

      return Result;
   end Create;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : ORB_Ctrl_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return "THREAD_POLICY.ORB_CTRL";
   end Policy_Id;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        ORB_Ctrl_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Unreferenced (Error);
      pragma Warnings (On);

   begin
      null;
      --  No rule to test

   end Check_Compatibility;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   function Handle_Request_Execution
     (Self      : access ORB_Ctrl_Executor;
      Msg       : PolyORB.Components.Message'Class;
      Requestor : PolyORB.Components.Component_Access)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Servants;

      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      --  At this stage, PolyORB.ORB.Run has already affected a thread
      --  to handle the request execution, in which this current call
      --  is executed. Thus we just need to call the Execute_Servant
      --  procedure to go on with the request execution.

      return Execute_Servant (Servant_Access (Requestor), Msg);
   end Handle_Request_Execution;

end PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
