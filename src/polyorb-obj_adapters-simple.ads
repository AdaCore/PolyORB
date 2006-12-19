------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . O B J _ A D A P T E R S . S I M P L E           --
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

--  Simple implementation of a PolyORB's Object Adapter.

with PolyORB.Components;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Dynamic_Tables;

package PolyORB.Obj_Adapters.Simple is

   pragma Elaborate_Body;

   type Simple_Obj_Adapter is new Obj_Adapter with private;

   procedure Create (OA : access Simple_Obj_Adapter);

   procedure Destroy (OA : access Simple_Obj_Adapter);

   procedure Export
     (OA    : access Simple_Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Unexport
     (OA    : access Simple_Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);

   procedure Object_Key
     (OA      : access Simple_Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Get_QoS
     (OA    : access Simple_Obj_Adapter;
      Id    :        Objects.Object_Id;
      QoS   :    out PolyORB.QoS.QoS_Parameters;
      Error : in out PolyORB.Errors.Error_Container);

   --  In the Simple Object Adapter, the methods of an object
   --  are described using two factory functions (provided by
   --  the application layer) that construct an argument list
   --  and a result Any for a given method.

   type Parameter_Profile_Description is
     access function (Method : String)
     return Any.NVList.Ref;

   type Result_Profile_Description is
     access function (Method : String)
     return Any.Any;

   type Interface_Description is record
      PP_Desc : Parameter_Profile_Description;
      RP_Desc : Result_Profile_Description;
   end record;

   procedure Set_Interface_Description
     (OA      : in out Simple_Obj_Adapter;
      Id      : access Objects.Object_Id;
      If_Desc :        Interface_Description);

   function Get_Empty_Arg_List
     (OA     : access Simple_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.NVList.Ref;

   function Get_Empty_Result
     (OA     : access Simple_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.Any;

   procedure Find_Servant
     (OA      : access Simple_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container);

   procedure Release_Servant
     (OA      : access Simple_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access);

private

   type Object_Map_Entry is record
      Servant : Servants.Servant_Access;
      --  May be null (for empty entries).

      If_Desc : Interface_Description;
   end record;

   package Object_Map_Entry_Arrays is new PolyORB.Utils.Dynamic_Tables
     (Object_Map_Entry, Natural, 1, 10, 1);
   subtype Object_Map_Entry_Array is Object_Map_Entry_Arrays.Instance;

   type Simple_Executor is new Servants.Executor with null record;

   function Handle_Request_Execution
     (Self      : access Simple_Executor;
      Msg       : PolyORB.Components.Message'Class;
      Requestor : PolyORB.Components.Component_Access)
     return PolyORB.Components.Message'Class;

   type Simple_Obj_Adapter is new Obj_Adapter with record
      Object_Map : Object_Map_Entry_Array;
      --  Object_Ids are simply the indices of the objects
      --  within the object map.

      S_Exec : aliased Simple_Executor;

      Lock : PolyORB.Tasking.Mutexes.Mutex_Access;
   end record;

end PolyORB.Obj_Adapters.Simple;
