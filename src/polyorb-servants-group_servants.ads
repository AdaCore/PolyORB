------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E R V A N T S . G R O U P _ S E R V A N T S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2014, Free Software Foundation, Inc.          --
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

--  A servant that manages a group of servants, and acts as a proxy for them

with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.References;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Tasking.Mutexes;

package PolyORB.Servants.Group_Servants is

   use PolyORB.Objects;

   --  This package use one exception in polyorb-exceptions :
   --  NotAGroupObject_E : used by some functions when a parameter is not a
   --                      group object or when a group is not found

   ------------------------------
   -- Group servants interface --
   ------------------------------

   function Create_Group_Servant
     (Oid : Object_Id_Access) return PolyORB.Servants.Servant_Access;
   --  Create a new group servant

   procedure Destroy_Group_Servant
     (Group : in out PolyORB.Servants.Servant_Access);
   --  Destroy group servant

   procedure Get_Group_Object_Id
     (Group : PolyORB.Servants.Servant_Access;
      Oid   : out Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container);
   --  Return group object id

   procedure Get_Group_Length
     (Group : PolyORB.Servants.Servant_Access;
      L     : out Natural;
      Error : in out PolyORB.Errors.Error_Container);
   --  Return group length

   --------------------------
   -- Group servants tools --
   --------------------------

   procedure Associate
     (Group : PolyORB.Servants.Servant_Access;
      Ref   : PolyORB.References.Ref;
      Error : in out PolyORB.Errors.Error_Container);
   --  Associate a servant ref with a group

   procedure Disassociate
     (Group :        PolyORB.Servants.Servant_Access;
      Ref   :        PolyORB.References.Ref;
      Error : in out PolyORB.Errors.Error_Container);
   --  Disassociate a servant ref with a group

   --  Iterator on a group servant

   type Iterator is private;

   procedure First
     (Group :        PolyORB.Servants.Servant_Access;
      It    :    out Iterator;
      Error : in out PolyORB.Errors.Error_Container);
   --  Create Iterator and set it on the first element

   function Value (It : Iterator) return PolyORB.References.Ref;
   --  Return current iterator reference

   procedure Next (It : in out Iterator);
   --  Increment iterator

   function Last (It : Iterator) return Boolean;
   --  Return True if iterator is in group range

private

   -------------------
   -- Group Servant --
   -------------------

   --  State of argument proxy

   type Proxy_State is (Not_Ready, Wait_First, Wait_Other);

   --  List of servants registered in group
   package Target_List_Package
   is new PolyORB.Utils.Chained_Lists
     (PolyORB.References.Ref,
      PolyORB.References."=");
   --  XXX questionnable. works with CORBA GOA, but need to
   --  be replaced by Is_Same_Object function

   type Group_Servant is new PolyORB.Servants.Servant with record
      --  Object_Id
      Oid         : Object_Id_Access;
      --  List of target objects
      Target_List : Target_List_Package.List;
      --  Request response counter
      Counter     : Natural;

      --------------------
      -- For args proxy --
      --------------------

      Args_Src    : PolyORB.Components.Component_Access;
      --  Current Args list
      Args        : PolyORB.Any.NVList.Ref;
      Error       : PolyORB.Errors.Error_Container;
      --  Proxy state
      State       : Proxy_State := Not_Ready;
      --  Mutex to avoid concurrent proxy access
      Mutex       : Tasking.Mutexes.Mutex_Access;
      Group_Lock  : Tasking.Mutexes.Mutex_Access;
   end record;

   type Group_Servant_Access is access all Group_Servant;

   overriding function Handle_Message
     (Self : not null access Group_Servant;
      Msg  : Components.Message'Class) return Components.Message'Class;
   --  Function used to intercept Unmarshall_Arguments message

   overriding function Execute_Servant
     (Self : not null access Group_Servant;
      Req  : Requests.Request_Access) return Boolean;
   --  Dispatch request to targets

   procedure Register
     (Self : access Group_Servant;
      Ref  : PolyORB.References.Ref);
   --  Add a target ref to a group

   procedure Unregister
     (Self : access Group_Servant;
      Ref  : PolyORB.References.Ref);
   --  Remove a target ref from a group

   type Iterator is record
      It : Target_List_Package.Iterator;
   end record;

end PolyORB.Servants.Group_Servants;
