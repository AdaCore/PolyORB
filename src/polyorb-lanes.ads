------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O L Y O R B . L A N E S                         --
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

with PolyORB.Jobs;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Priorities;
with PolyORB.Tasking.Threads;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

--  XXX Should this be in the PolyORB.Tasking.* hierarchy ?

package PolyORB.Lanes is

   use PolyORB.Jobs;
   use PolyORB.Tasking.Priorities;
   use PolyORB.Tasking.Threads;

   --  A lane is an advanced queue made of several job queues and a
   --  set of threads (Ti)i=1..n at priority P (Ti). The lane
   --  schedules all queued jobs and dispatch threads to process
   --  them, depending on lane's policy.

   ---------------
   -- Lane_Root --
   ---------------

   --  Lane_Root is the root type for all lanes.

   type Lane_Root is abstract tagged limited private;
   type Lane_Root_Access is access all Lane_Root'Class;

   procedure Queue_Job (L : access Lane_Root; J : Job_Access) is abstract;

   procedure Destroy (L : access Lane_Root) is abstract;

   ----------
   -- Lane --
   ----------

   --  A Lane is an advanced queue made of one job queue and several
   --  attached threads, all threads are at the same priority tuple
   --  (ORB_Component_Priority, External_Priority).

   type Lane
     (ORB_Priority              : ORB_Component_Priority;
      Ext_Priority              : External_Priority;
      Base_Number_Of_Threads    : Natural;
      Dynamic_Number_Of_Threads : Natural;
      Stack_Size                : Natural;
      Buffer_Request            : Boolean;
      Max_Buffered_Requests     : PolyORB.Types.Unsigned_Long;
      Max_Buffer_Size           : PolyORB.Types.Unsigned_Long)
   is new Lane_Root with private;
   --  XXX missing: dynamic thread allocation, stack size

   type Lane_Access is access all Lane'Class;

   function Create
     (ORB_Priority              : ORB_Component_Priority;
      Ext_Priority              : External_Priority;
      Base_Number_Of_Threads    : Natural;
      Dynamic_Number_Of_Threads : Natural;
      Stack_Size                : Natural;
      Buffer_Request            : Boolean;
      Max_Buffered_Requests     : PolyORB.Types.Unsigned_Long;
      Max_Buffer_Size           : PolyORB.Types.Unsigned_Long)
     return Lane_Access;

   procedure Queue_Job (L : access Lane; J : Job_Access);

   procedure Destroy (L : access Lane);

   ---------------------
   -- Extensible_Lane --
   ---------------------

   --  An Extensible_Lane is a lane to which no thread are attached at
   --  startup. Thread may be attached to this lane, they will be used
   --  to process queued jobs.

   type Extensible_Lane is new Lane with private;

   procedure Attach_Thread (EL : in out Extensible_Lane; T : Thread_Access);

   --------------
   -- Lane_Set --
   --------------

   --  A Lane_Set is a set of Lanes.

   type Lanes_Set (Length : Positive) is new Lane_Root with private;

   procedure Add_Lane
     (Set   : in out Lanes_Set;
      L     :        Lane_Access;
      Index :        Positive);
   --  Add lane L at position Index in Set

   procedure Queue_Job (L : access Lanes_Set; J : Job_Access);

   procedure Destroy (L : access Lanes_Set);

private

   type Lane_Runnable is new PolyORB.Tasking.Threads.Runnable with record
      L : Lane_Access;
      J : Job_Access;
   end record;

   type Lane_Runnable_Access is access all Lane_Runnable;

   procedure Run (R : access Lane_Runnable);

   ------------------------------
   -- Management of idle tasks --
   ------------------------------

   package PTM  renames PolyORB.Tasking.Mutexes;
   package PTCV renames PolyORB.Tasking.Condition_Variables;

   type Idle_Task is record
      CV : PTCV.Condition_Access;
      R  : Lane_Runnable_Access;
   end record;

   package Idle_Task_Lists is new PolyORB.Utils.Chained_Lists (Idle_Task);

   package CV_Lists is
      new PolyORB.Utils.Chained_Lists (PTCV.Condition_Access, PTCV."=");

   --  Lane_Root

   type Lane_Root is abstract tagged limited null record;

   --  Lane

   type Thread_Array is array (Positive range <>) of Thread_Access;

   type Lane
     (ORB_Priority              : ORB_Component_Priority;
      Ext_Priority              : External_Priority;
      Base_Number_Of_Threads    : Natural;
      Dynamic_Number_Of_Threads : Natural;
      Stack_Size                : Natural;
      Buffer_Request            : Boolean;
      Max_Buffered_Requests     : PolyORB.Types.Unsigned_Long;
      Max_Buffer_Size           : PolyORB.Types.Unsigned_Long)
   is new Lane_Root with record
      Lock      : PTM.Mutex_Access;
      Threads   : Thread_Array (1 .. Base_Number_Of_Threads);
      Job_Queue : Job_Queue_Access;

      Idle_Task_List : Idle_Task_Lists.List;
      --  List of idle tasks

      Free_CV : CV_Lists.List;

      Clean_Up_In_Progress : Boolean := False;
   end record;

   --  Extensible_Lane

   package Thread_Lists is new PolyORB.Utils.Chained_Lists (Thread_Access);

   type Extensible_Lane is new Lane with record
      Additional_Threads : Thread_Lists.List;
   end record;

   --  Lane_Set

   type Lane_Array is array (Positive range <>) of Lane_Access;

   type Lanes_Set (Length : Positive) is new Lane_Root with record
      Set : Lane_Array (1 .. Length);
   end record;

end PolyORB.Lanes;
