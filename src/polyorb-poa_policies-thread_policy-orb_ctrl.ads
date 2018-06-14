------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.POA_POLICIES.THREAD_POLICY.ORB_CTRL                --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2012;

--  Implementation of the 'ORB Control' POA Policy.

package PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl is

   type ORB_Ctrl_Policy is new ThreadPolicy with private;
   type ORB_Ctrl_Policy_Access is access all ORB_Ctrl_Policy;

   function Create return ORB_Ctrl_Policy_Access;

   overriding procedure Check_Compatibility
     (Self           : ORB_Ctrl_Policy;
      Other_Policies : AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   overriding function Policy_Id (Self : ORB_Ctrl_Policy) return String;

private

   type ORB_Ctrl_Policy is new ThreadPolicy with null record;
   subtype ORB_Ctrl_Executor is Servants.Executor;

end PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
