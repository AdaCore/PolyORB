------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . P O A _ M A N A G E R . B A S I C _ M A N A G E R     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  Base POA Manager concrete implementation.

with PolyORB.Requests;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Chained_Lists;

package PolyORB.POA_Manager.Basic_Manager is

   pragma Elaborate_Body;

   use PolyORB.Requests;

   type Basic_POA_Manager is new POAManager with private;
   type Basic_POA_Manager_Access is access all Basic_POA_Manager;

   --------------------------------------------------------------------
   -- Procedures and functions to implement the POAManager interface --
   --------------------------------------------------------------------

   overriding procedure Activate
     (Self  : access Basic_POA_Manager;
      Error : in out PolyORB.Errors.Error_Container);

   overriding procedure Hold_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Errors.Error_Container);

   overriding procedure Discard_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Errors.Error_Container);
   overriding procedure Deactivate
     (Self                : access Basic_POA_Manager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean);

   overriding function Get_State (Self : Basic_POA_Manager) return State;

   -------------------------------------------------------------
   -- Procedures and functions specific to the implementation --
   -------------------------------------------------------------

   overriding procedure Create (M : access Basic_POA_Manager);

   overriding procedure Register_POA
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access);

   overriding procedure Remove_POA
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access);

   overriding function Get_Hold_Servant
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access)
     return PolyORB.Servants.Servant_Access;

   ----------------------------------
   -- Holding state implementation --
   ----------------------------------

   --  When the POAManager is in the HOLDING state:
   --    A new entry to the queue is created, along with a Hold_Servant
   --    that has access to this entry. The servant is returned to the POA,
   --    which returns it as the requested servant.
   --    When the Handle_Message method of the servant is called, the
   --    Hold_Servant queues the request in the POAManager queue.
   --  When the POAManager changes again to the ACTIVE state:
   --    The requests in the queue are re-sent to the POA, that will send them
   --    to the ORB to be executed again.
   --    Note that the requests are queued in the ORB queue, using ORB
   --    queueing policy.

   type Hold_Servant is new PolyORB.Servants.Servant with private;
   type Hold_Servant_Access is access all Hold_Servant;

   overriding function Execute_Servant
     (Obj : not null access Hold_Servant;
      Req : Requests.Request_Access) return Boolean;
   --  Implementation of the Hold_Servant servant

private

   package POA_Lists is
      new PolyORB.Utils.Chained_Lists (Obj_Adapter_Access, "=", True);
   subtype POAList is POA_Lists.List;

   package Requests_Queues is
      new PolyORB.Utils.Chained_Lists (Request_Access);
   subtype Requests_Queue is Requests_Queues.List;

   type Basic_POA_Manager is new POAManager with record
      Current_State : State;

      Managed_POAs  : POAList;

      Lock : PolyORB.Tasking.Mutexes.Mutex_Access;
      --  Lock the POA Manager

      PM_Hold_Servant : Hold_Servant_Access := null;
      --  Reference to the holding servant

      Held_Requests : Requests_Queue;
      --  List of requests held by the POAManager
   end record;

   overriding procedure Finalize (Self : in out Basic_POA_Manager);

   type Hold_Servant is new PolyORB.Servants.Servant with record
      PM : Basic_POA_Manager_Access := null;
   end record;

end PolyORB.POA_Manager.Basic_Manager;
