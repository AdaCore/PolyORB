------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.POA_POLICIES.THREAD_POLICY.ORB_CTRL                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

--  Implementation of the 'ORB Control' POA Policy.

package PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl is

   type ORB_Ctrl_Policy is new ThreadPolicy with private;
   type ORB_Ctrl_Policy_Access is access all ORB_Ctrl_Policy;

   function Create return ORB_Ctrl_Policy_Access;

   procedure Check_Compatibility
     (Self           : ORB_Ctrl_Policy;
      Other_Policies : AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   function Policy_Id (Self : ORB_Ctrl_Policy) return String;

private

   type ORB_Ctrl_Policy is new ThreadPolicy with null record;
   subtype ORB_Ctrl_Executor is Servants.Executor;

end PolyORB.POA_Policies.Thread_Policy.ORB_Ctrl;
