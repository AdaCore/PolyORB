------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . P A R T I T I O N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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
   pragma Unreferenced (C); --  For conditional pragma Debug

   Partitions_Mutex : Mutex_Access;
   Next_Partition_ID : Integer := 0;

   function Allocate_Partition_ID
     (Name : String)
      return Integer
   is
      Current_Partition_ID : Integer;
   begin
      Enter (Partitions_Mutex);
      Current_Partition_ID := Next_Partition_ID;
      Next_Partition_ID := Next_Partition_ID + 1;
      Leave (Partitions_Mutex);
      pragma Debug
        (O ("Assigned partition id"
              & Integer'Image (Current_Partition_ID)
              & " to " & Name));
      return Current_Partition_ID;
   end Allocate_Partition_ID;

   use System.Partition_Interface;
begin
   Create (Partitions_Mutex);

   --  We set the partition Id of the main partition here to avoid a possible
   --  race condition.

   Set_Local_Partition_ID
     (System.RPC.Partition_ID (Allocate_Partition_ID ("main partition")));
end PolyORB.DSA_P.Partitions;
