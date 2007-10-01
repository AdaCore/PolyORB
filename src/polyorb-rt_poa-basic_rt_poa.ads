------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R T _ P O A . B A S I C _ R T _ P O A           --
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

with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.POA;
with PolyORB.POA_Manager;
with PolyORB.POA_Policies;
with PolyORB.Servants;

package PolyORB.RT_POA.Basic_RT_POA is

   type Basic_RT_Obj_Adapter is new PolyORB.RT_POA.RT_Obj_Adapter
     with private;

   type Basic_RT_Obj_Adapter_Access is access all Basic_RT_Obj_Adapter;

   ---------------------------------------------
   -- CORBA-like POA interface implementation --
   ---------------------------------------------

   procedure Create_POA
     (Self         : access Basic_RT_Obj_Adapter;
      Adapter_Name :        Standard.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out PolyORB.POA.Obj_Adapter_Access;
      Error        : in out PolyORB.Errors.Error_Container);

   procedure Export
     (OA    : access Basic_RT_Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);

   ------------------------------------------------
   -- CORBA-like RT POA interface implementation --
   ------------------------------------------------

   procedure Create_Object_Identification_With_Priority
     (Self                     : access Basic_RT_Obj_Adapter;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Errors.Error_Container);

   procedure Activate_Object_With_Id_And_Priority
     (Self                     : access Basic_RT_Obj_Adapter;
      P_Servant                :        Servants.Servant_Access;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Errors.Error_Container);

   procedure Get_Scheduling_Parameters
     (Self                     : access Basic_RT_Obj_Adapter;
      Id                       : Object_Id_Access;
      Model                    :    out Priority_Model;
      Server_ORB_Priority      :    out ORB_Priority;
      Server_External_Priority :    out External_Priority;
      Error                    : in out PolyORB.Errors.Error_Container);

private

   type Basic_RT_Obj_Adapter is new PolyORB.RT_POA.RT_Obj_Adapter
     with null record;

end PolyORB.RT_POA.Basic_RT_POA;
