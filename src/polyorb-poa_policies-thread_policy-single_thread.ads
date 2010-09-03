------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.THREAD_POLICY.SINGLE_THREAD              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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
