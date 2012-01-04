------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B . R T _ P O A                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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
