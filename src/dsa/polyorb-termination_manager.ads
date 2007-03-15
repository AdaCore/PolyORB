------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T E R M I N A T I O N _ M A N A G E R           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

--  The termination algorithm implemented in this package works by sending
--  "waves". Each wave reach the entire set of connex partitions participating
--  in the distributed program. Each wave possess a chronological stamp. These
--  stamps, allow the construction of an implicit covering tree of all the
--  partitions. When we speak of child and father, we are refering to childs
--  and fathers in the sense of this implicit tree. Note that for each wave the
--  tree may be different.

--  XXX  * Make the algorithm fault tolerant: if the initiator is not
--         responding, spawn a new one.
--       * Implement the waves as in GARLIC, one detection wave and one
--         termination wave. Not really needed but may be more efficient.

package PolyORB.Termination_Manager is
   pragma Remote_Types;

   type Term_Manager is tagged limited private;
   type Term_Manager_Access is access all Term_Manager'Class;

   type Stamp_Type is private;
   --  A new stamp value is assigned to each termination detection wave

   function Is_Terminated (TM : access Term_Manager; Stamp : Stamp_Type)
     return Boolean;
   --  Return True iff the partition controlled by TM and all of its childs are
   --  terminated.

   function Terminate_Now (TM : access Term_Manager; Stamp : Stamp_Type)
     return Boolean;
   --  Terminate all the child partitions of the partition controlled by TM,
   --  then terminates the partition itself.

   type Termination_Type is (Global_Termination,
                             Local_Termination,
                             Deferred_Termination);
   --  The termination policies

   procedure Start (TM                 : access Term_Manager;
                    T                  : Termination_Type;
                    Initiator          : Boolean;
                    Time_Between_Waves : Duration;
                    Time_Before_Start  : Duration);
   --  Start the Termination Manager with the chosen policy

private
   type Stamp_Type is mod 2 ** 8;

   type Request_Status is (Outdated, Not_From_Father, Valid);

   function ">" (S1, S2 : Stamp_Type) return Boolean;
   --  Compare two stamps. S1 > S2 means that S1 is very likely to have been
   --  issued prior to S2. (Borrowed from GLADE s-garter).

   type Action is access
     function (TM : Term_Manager_Access; Stamp : Stamp_Type) return Boolean;

   function Call_On_Neighbours
     (A : Action; TM : access Term_Manager; Stamp : Stamp_Type) return Boolean;
   --  Call action A on all the neighbours of the partition controlled by TM,
   --  returns the global AND of every neighbour return value to A.

   function Is_Locally_Terminated
     (Expected_Running_Tasks : Natural) return Boolean;
   --  Wrapper for the Is_Locally_Terminated function defined in ORB_Controller

   type Term_Manager is tagged limited record
      Terminated : Boolean := False;
      --  The termination status for this partition. If this becomes true, the
      --  partition will shutdown itself.

      Termination_Policy : Termination_Type := Global_Termination;
      --  The termination policy of the local partition, by default it is
      --  Global_Termination.

      Time_Between_Waves : Duration := 1.0;
      --  The time we wait between two consecutive waves.

      Time_Before_Start : Duration := 5.0;
      --  The time we wait before the initiator starts sending waves, this time
      --  allows all the partitions to boot and start their own TM.

      Is_Initiator : Boolean := False;
      --  Is this Term_Manager the initiator

      Current_Stamp : Stamp_Type := 0;
      --  The stamp of the last received wave

      Non_Terminating_Tasks : Natural := 0;
      --  The number of expected non terminated tasks when we perform a local
      --  termination computation.
   end record;
end PolyORB.Termination_Manager;
