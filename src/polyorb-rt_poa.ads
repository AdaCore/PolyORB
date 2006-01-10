------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B . R T _ P O A                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

--  Abstract interface for the RT POA Object Adapter.

--  This package provides an extension to PolyORB's POA, and allow the
--  user to affect priorities to servants. It is notionnally
--  equivalent to RTCORBA specification of the RT-POA.

with PolyORB.Errors;
with PolyORB.POA;
with PolyORB.POA_Types;
with PolyORB.Servants;

with PolyORB.RT_POA_Policies.Priority_Model_Policy;
with PolyORB.RT_POA_Policies.Thread_Pool_Policy;

with PolyORB.Tasking.Priorities;

package PolyORB.RT_POA is

   use PolyORB.POA_Types;
   use PolyORB.RT_POA_Policies.Priority_Model_Policy;
   use PolyORB.RT_POA_Policies.Thread_Pool_Policy;
   use PolyORB.Tasking.Priorities;

   type RT_Obj_Adapter is abstract new PolyORB.POA.Obj_Adapter with record
      --  Note: RT_POA may be used as a basic POA. Thus, RT-POA
      --  specifications do not require these policies to be set.

      Priority_Model_Policy : PriorityModelPolicy_Access;
      Thread_Pool_Policy    : ThreadPoolPolicy_Access;
   end record;

   type RT_Obj_Adapter_Access is access all RT_Obj_Adapter'Class;

   procedure Create_Object_Identification_With_Priority
     (Self                     : access RT_Obj_Adapter;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Reserve a complete object identifier, possibly using
   --  the given Hint (if not null) for the construction of
   --  the object identifier included in the Object_Id.

   procedure Activate_Object_With_Id_And_Priority
     (Self                     : access RT_Obj_Adapter;
      P_Servant                :        Servants.Servant_Access;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Activate an object, i.e. associate it with a local
   --  identification, possibly using the given Hint (if not null) for
   --  the construction of the object identifier included in the

   procedure Get_Scheduling_Parameters
     (Self                     : access RT_Obj_Adapter;
      Id                       : Object_Id_Access;
      Model                    :    out Priority_Model;
      Server_ORB_Priority      :    out ORB_Priority;
      Server_External_Priority :    out External_Priority;
      Error                    : in out PolyORB.Errors.Error_Container)
      is abstract;
   --  Return scheduling parameters associated to servant P_Servant
   --  stored in Self.

end PolyORB.RT_POA;
