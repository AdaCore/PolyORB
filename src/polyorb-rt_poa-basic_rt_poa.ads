------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R T _ P O A . B A S I C _ R T _ P O A           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Any.NVList;
with PolyORB.Exceptions;
with PolyORB.Objects;
with PolyORB.POA;
with PolyORB.POA_Manager;
with PolyORB.POA_Policies;
with PolyORB.References;
with PolyORB.Servants;
with PolyORB.Types;

package PolyORB.RT_POA.Basic_RT_POA is

   type Basic_RT_Obj_Adapter is new PolyORB.RT_POA.RT_Obj_Adapter
     with private;

   type Basic_RT_Obj_Adapter_Access is access all Basic_RT_Obj_Adapter;

   ---------------------------------------------
   -- CORBA-like POA interface implementation --
   ---------------------------------------------

   procedure Create_POA
     (Self         : access Basic_RT_Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out PolyORB.POA.Obj_Adapter_Access;
      Error        : in out PolyORB.Exceptions.Error_Container);

   procedure Find_POA
     (Self        : access Basic_RT_Obj_Adapter;
      Name        :        String;
      Activate_It :        Boolean;
      POA         :    out PolyORB.POA.Obj_Adapter_Access;
      Error       : in out PolyORB.Exceptions.Error_Container);

   procedure Destroy
     (Self                : access Basic_RT_Obj_Adapter;
      Etherealize_Objects : in     Types.Boolean;
      Wait_For_Completion : in     Types.Boolean);

   procedure Create_Object_Identification
     (Self  : access Basic_RT_Obj_Adapter;
      Hint  :        Object_Id_Access;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container);

   procedure Activate_Object
     (Self      : access Basic_RT_Obj_Adapter;
      P_Servant : in     Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      U_Oid     :    out Unmarshalled_Oid;
      Error     : in out PolyORB.Exceptions.Error_Container);

   procedure Deactivate_Object
     (Self      : access Basic_RT_Obj_Adapter;
      Oid       : in     Object_Id;
      Error     : in out PolyORB.Exceptions.Error_Container);

   procedure Servant_To_Id
     (Self      : access Basic_RT_Obj_Adapter;
      P_Servant : in     Servants.Servant_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Exceptions.Error_Container);

   procedure Id_To_Servant
     (Self    : access Basic_RT_Obj_Adapter;
      Oid     :        Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container);

   procedure Get_Servant
     (Self    : access Basic_RT_Obj_Adapter;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container);

   procedure Set_Servant
     (Self    : access Basic_RT_Obj_Adapter;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container);

   procedure Get_Servant_Manager
     (Self    : access Basic_RT_Obj_Adapter;
      Manager :    out ServantManager_Access;
      Error   : in out PolyORB.Exceptions.Error_Container);

   procedure Set_Servant_Manager
     (Self    : access Basic_RT_Obj_Adapter;
      Manager :        ServantManager_Access;
      Error   : in out PolyORB.Exceptions.Error_Container);

   procedure Get_The_Children
     (Self     : access Basic_RT_Obj_Adapter;
      Children :    out POAList);

   ------------------------------------------------
   -- CORBA-like RT POA interface implementation --
   ------------------------------------------------

   procedure Create_Object_Identification_With_Priority
     (Self                     : access Basic_RT_Obj_Adapter;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : in     ORB_Priority;
      Server_External_Priority : in     External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Exceptions.Error_Container);

   procedure Activate_Object_With_Id_And_Priority
     (Self                     : access Basic_RT_Obj_Adapter;
      P_Servant                :        Servants.Servant_Access;
      Hint                     :        Object_Id_Access;
      Server_ORB_Priority      : in     ORB_Priority;
      Server_External_Priority : in     External_Priority;
      U_Oid                    :    out Unmarshalled_Oid;
      Error                    : in out PolyORB.Exceptions.Error_Container);

   procedure Get_Scheduling_Parameters
     (Self                     : access Basic_RT_Obj_Adapter;
      Id                       : in     Object_Id_Access;
      Model                    :    out Priority_Model;
      Server_ORB_Priority      :    out ORB_Priority;
      Server_External_Priority :    out External_Priority;
      Error                    : in out PolyORB.Exceptions.Error_Container);

   --------------------------------------------------
   -- PolyORB Obj_Adapter interface implementation --
   --------------------------------------------------

   procedure Create (OA : access Basic_RT_Obj_Adapter);

   procedure Destroy (OA : access Basic_RT_Obj_Adapter);

   procedure Export
     (OA    : access Basic_RT_Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container);

   procedure Unexport
     (OA    : access Basic_RT_Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container);

   procedure Object_Key
     (OA      : access Basic_RT_Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Exceptions.Error_Container);

   function Get_Empty_Arg_List
     (OA     : access Basic_RT_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.NVList.Ref;

   function Get_Empty_Result
     (OA     : access Basic_RT_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.Any;

   procedure Find_Servant
     (OA      : access Basic_RT_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container);

   procedure Release_Servant
     (OA      : access Basic_RT_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access);

   ----------------------
   -- Utility function --
   ----------------------

   procedure Copy_Obj_Adapter
     (From : in     Basic_RT_Obj_Adapter;
      To   : access Basic_RT_Obj_Adapter);

   procedure Remove_POA_By_Name
     (Self       : access Basic_RT_Obj_Adapter;
      Child_Name :        Types.String);

   --------------------------------
   -- Proxy namespace management --
   --------------------------------

   function Is_Proxy_Oid
     (OA  : access Basic_RT_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean;

   procedure To_Proxy_Oid
     (OA    : access Basic_RT_Obj_Adapter;
      R     :        References.Ref;
      Oid   :    out Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container);

   function Proxy_To_Ref
     (OA  : access Basic_RT_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return References.Ref;

private

   --  This implementation of a RT POA relies on the implementation of
   --  a POA for primitives inherited from POA.

   type Basic_RT_Obj_Adapter is new PolyORB.RT_POA.RT_Obj_Adapter
     with record
        Non_RT_POA : PolyORB.POA_Types.Obj_Adapter_Ref;

     end record;

end PolyORB.RT_POA.Basic_RT_POA;
