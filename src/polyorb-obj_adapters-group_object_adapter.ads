------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.OBJ_ADAPTERS.GROUP_OBJECT_ADAPTER                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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

--  Special Object Adapter to manage group servants

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.HTables.Perfect;
with PolyORB.Utils.HFunctions.Hyper;
with PolyORB.References;

package PolyORB.Obj_Adapters.Group_Object_Adapter is

   --------------------------
   -- Group_Object_Adapter --
   --------------------------

   type Group_Object_Adapter is new Obj_Adapter with private;
   type Group_Object_Adapter_Access is access all Group_Object_Adapter'Class;

   procedure Create (GOA : access Group_Object_Adapter);

   procedure Destroy (GOA : access Group_Object_Adapter);

   --------------------------------------
   -- Interface to application objects --
   --------------------------------------

   procedure Export
     (GOA   : access Group_Object_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Unexport
     (GOA   : access Group_Object_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Object_Key
     (GOA     : access Group_Object_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Get_QoS
     (OA    : access Group_Object_Adapter;
      Id    :        Objects.Object_Id;
      QoS   :    out PolyORB.QoS.QoS_Parameters;
      Error : in out PolyORB.Errors.Error_Container);

   ----------------------------------------------------
   -- Interface to ORB (acting on behalf of clients) --
   ----------------------------------------------------

   function Get_Empty_Arg_List
     (GOA    : access Group_Object_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
      return Any.NVList.Ref;

   function Get_Empty_Result
     (GOA    : access Group_Object_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
      return Any.Any;

   procedure Find_Servant
     (GOA     : access Group_Object_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Release_Servant
     (GOA     : access Group_Object_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access);

   ------------------------------
   -- Group Servant Management --
   ------------------------------

   function Get_Group
     (The_Ref              : PolyORB.References.Ref;
      Allow_Group_Creation : Boolean := False)
     return PolyORB.Servants.Servant_Access;
   --  Search for a group. If Allow_Group_Creation is true and the
   --  group is not found, create and register the group.

private

   package Perfect_Htable is
      new PolyORB.Utils.HTables.Perfect
     (PolyORB.Servants.Servant_Access,
      PolyORB.Utils.HFunctions.Hyper.Hash_Hyper_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Hash,
      PolyORB.Utils.HFunctions.Hyper.Next_Hash_Parameters);
   use Perfect_Htable;

   type Simple_Executor is new Servants.Executor with null record;

   function Handle_Request_Execution
     (Self      : access Simple_Executor;
      Msg       : PolyORB.Components.Message'Class;
      Requestor : PolyORB.Components.Component_Access)
     return PolyORB.Components.Message'Class;

   type Group_Object_Adapter is new Obj_Adapter with record
      Lock : PolyORB.Tasking.Mutexes.Mutex_Access;
      --  Mutex

      Registered_Groups : Table_Instance;
      --  List of registered groups

      S_Exec : aliased Simple_Executor;

   end record;

end PolyORB.Obj_Adapters.Group_Object_Adapter;
