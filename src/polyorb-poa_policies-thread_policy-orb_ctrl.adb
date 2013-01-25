------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.POA_POLICIES.THREAD_POLICY.ORB_CTRL                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

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

   overriding function Policy_Id
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

   overriding procedure Check_Compatibility
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
      --  No rule to test

      null;
   end Check_Compatibility;

end PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
