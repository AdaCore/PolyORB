------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . P O A _ M A N A G E R . B A S I C _ M A N A G E R     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

--  Base POA Manager concrete implementation.

with PolyORB.Components;
with PolyORB.Exceptions;
with PolyORB.POA_Types;
with PolyORB.Servants.Interface;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Chained_Lists;

package PolyORB.POA_Manager.Basic_Manager is

   pragma Elaborate_Body;

   use PolyORB.POA_Types;
   use PolyORB.Servants.Interface;

   type Basic_POA_Manager is new POAManager with private;
   type Basic_POA_Manager_Access is access all Basic_POA_Manager;

   --------------------------------------------------------------------
   -- Procedures and functions to implement the POAManager interface --
   --------------------------------------------------------------------

   procedure Activate
     (Self  : access Basic_POA_Manager;
      Error : in out PolyORB.Exceptions.Error_Container);

   procedure Hold_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Exceptions.Error_Container);

   procedure Discard_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Exceptions.Error_Container);
   procedure Deactivate
     (Self                : access Basic_POA_Manager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean);

   function Get_State (Self : Basic_POA_Manager) return State;

   -------------------------------------------------------------
   -- Procedures and functions specific to the implementation --
   -------------------------------------------------------------

   procedure Create (M : access Basic_POA_Manager);

   procedure Register_POA
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access);

   procedure Remove_POA
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access);

   function Get_Hold_Servant
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

   function Execute_Servant
     (Obj : access Hold_Servant;
      Msg :        PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;
   --  Implementation of the Hold_Servant servant

private

   package POA_Lists is
      new PolyORB.Utils.Chained_Lists (Obj_Adapter_Access, "=", True);
   subtype POAList is POA_Lists.List;

   package Requests_Queue_P is
      new PolyORB.Utils.Chained_Lists (Execute_Request);
   subtype Requests_Queue is Requests_Queue_P.List;

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

   procedure Finalize (Self : in out Basic_POA_Manager);

   type Hold_Servant is new PolyORB.Servants.Servant with record
      PM : Basic_POA_Manager_Access := null;
   end record;

end PolyORB.POA_Manager.Basic_Manager;
