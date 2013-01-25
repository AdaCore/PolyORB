------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . P O A _ C O N F I G . M I N I M U M            --
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

--  A POA configuration corresponding to minimumCORBA policies

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

   My_Default_Policies : PolicyList;
   Initialized         : Boolean := False;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (C : Minimum_Configuration) is
      pragma Warnings (Off);
      pragma Unreferenced (C);
      pragma Warnings (On);

      use PolyORB.POA_Policies.Policy_Lists;

   begin
      if Initialized then
         return;
      end if;

      Append (My_Default_Policies,
              Policy_Access (Id_Assignment_Policy.System.Create));

      Append (My_Default_Policies,
              Policy_Access (Id_Uniqueness_Policy.Unique.Create));

      Append (My_Default_Policies,
              Policy_Access
              (Implicit_Activation_Policy.No_Activation.Create));

      Append (My_Default_Policies,
              Policy_Access (Lifespan_Policy.Transient.Create));

      Append (My_Default_Policies,
              Policy_Access
              (Request_Processing_Policy.Active_Object_Map_Only.Create));

      Append (My_Default_Policies,
              Policy_Access (Servant_Retention_Policy.Retain.Create));

      Append (My_Default_Policies,
              Policy_Access (Thread_Policy.ORB_Ctrl.Create));

      Initialized := True;
   end Initialize;

   ----------------------
   -- Default_Policies --
   ----------------------

   overriding function Default_Policies
     (C : Minimum_Configuration) return PolyORB.POA_Policies.PolicyList is
   begin
      if not Initialized then
         Initialize (C);
      end if;

      return My_Default_Policies;
   end Default_Policies;

end PolyORB.POA_Config.Minimum;
