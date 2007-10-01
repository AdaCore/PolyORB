------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.RT_POA_POLICIES.THREAD_POOL_POLICY                 --
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
with PolyORB.Lanes;
with PolyORB.POA_Policies;
with PolyORB.Servants;
with PolyORB.Tasking.Priorities;

package PolyORB.RT_POA_Policies.Thread_Pool_Policy is

   use PolyORB.Lanes;
   use PolyORB.POA_Policies;
   use PolyORB.Tasking.Priorities;

   type ThreadPoolPolicy is new PolyORB.POA_Policies.Policy with private;

   type ThreadPoolPolicy_Access is access all ThreadPoolPolicy'Class;

   function Create (Lanes : Lane_Root_Access) return Policy_Access;

   function Policy_Id (Self : ThreadPoolPolicy) return String;

   procedure Check_Compatibility
     (Self           :        ThreadPoolPolicy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container);

   function Get_Servant_Lane
     (Servant : PolyORB.Servants.Servant_Access)
     return Lane_Root_Access;
   --  Retrieve information on ThreadPoolPolicy stored in Servant,
   --  return null if unset.

   procedure Set_Servant_Lane
     (Self    : ThreadPoolPolicy;
      Servant : PolyORB.Servants.Servant_Access);
   --  Cache Self information into Servant

   function Is_Valid_Priority
     (Self     : ThreadPoolPolicy;
      Priority : External_Priority)
     return Boolean;

private

   type ThreadPoolPolicy is new PolyORB.POA_Policies.Policy with record
      Lanes : Lane_Root_Access;
   end record;

end PolyORB.RT_POA_Policies.Thread_Pool_Policy;
