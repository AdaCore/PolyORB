------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . P A R T I T I O N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2009, Free Software Foundation, Inc.          --
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

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with System.Partition_Interface;
with System.RPC;

package body PolyORB.DSA_P.Partitions is

   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log ("polyorb.dsa_p.partitions");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Partitions_Mutex : Mutex_Access;

   Next_Partition_ID : Integer := 1;
   --  ID to be assigned to the next requesting partition. Note that we start
   --  at 1 because the value 0 is reserved to denote the unset (initial) state
   --  of System.Standard_Library.Local_Parition_ID.

   function Elaborate return Boolean;
   --  Initialize the Partitions_Mutex and set the local partition ID.
   --  See comment at end of this unit for explanation of why a function
   --  is required.

   ---------------------------
   -- Allocate_Partition_ID --
   ---------------------------

   function Allocate_Partition_ID (Name : String) return Integer is
      Current_Partition_ID : Integer;
   begin
      Enter (Partitions_Mutex);
      Current_Partition_ID := Next_Partition_ID;
      Next_Partition_ID := Next_Partition_ID + 1;
      Leave (Partitions_Mutex);

      pragma Debug
        (C, O ("Assigned partition id"
              & Integer'Image (Current_Partition_ID)
              & " to " & Name));
      return Current_Partition_ID;
   end Allocate_Partition_ID;

   ---------------
   -- Elaborate --
   ---------------

   function Elaborate return Boolean is
      use System.Partition_Interface;
   begin
      Create (Partitions_Mutex);

      --  We set the partition Id of the main partition here to avoid a
      --  possible race condition.

      Set_Local_Partition_ID
        (System.RPC.Partition_ID
         (Allocate_Partition_ID (Get_Local_Partition_Name
                                 & " (main partition)")));
      return True;
   end Elaborate;

   --------------------------------------------
   -- Initialization of the Partitions_Mutex --
   --------------------------------------------

   --  We need the mutex to be initialized, and the local partition ID to be
   --  set, before this package is registered. Otherwise, if we perform these
   --  steps in the elaboration statements of this packge, then there is
   --  a tiny, but non-zero, vulnerability window where we can service RPCs
   --  without having initialized the mutex (because the registration is
   --  performed before the elaboration statements are executed).

   Dummy : constant Boolean := Elaborate;
   pragma Unreferenced (Dummy);
   --  Dummy value declared only for the sake of evaluating the side effects
   --  of Elaborate.

end PolyORB.DSA_P.Partitions;
