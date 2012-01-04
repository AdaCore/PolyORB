------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.THREAD_POLICY.SINGLE_THREAD              --
--                                                                          --
--                                 S p e c                                  --
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

pragma Ada_2005;

--  Implementation of the 'Single thread' POA Policy.

with PolyORB.Components;
with PolyORB.Requests;

package PolyORB.POA_Policies.Thread_Policy.Single_Thread is

   type Single_Thread_Policy is new ThreadPolicy with private;

   type Single_Thread_Policy_Access is access all Single_Thread_Policy;

   function Create return Single_Thread_Policy_Access;

   procedure Check_Compatibility
     (Self           :        Single_Thread_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   function Policy_Id
     (Self : Single_Thread_Policy)
     return String;

private

   type Single_Thread_Policy is new ThreadPolicy with null record;

   type Single_Thread_Executor is new Servants.Executor with null record;

   overriding function Execute_In_Context
     (Self      : access Single_Thread_Executor;
      Req       : Requests.Request_Access;
      Requestor : Components.Component_Access) return Boolean;

end PolyORB.POA_Policies.Thread_Policy.Single_Thread;
