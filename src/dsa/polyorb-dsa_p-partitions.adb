------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . P A R T I T I O N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;

package body PolyORB.DSA_P.Partitions is

   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log ("polyorb.dsa_p.partitions");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Partitions_Mutex : Mutex_Access;
   Next_Partition_ID : Integer := 0;

   procedure Initialize;

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

   procedure Initialize is
   begin
      Create (Partitions_Mutex);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name => +"dsa.partitions",
       Conflicts => Empty,
       Depends => +"dsa"
         & "tasking.mutexes",
       Provides => Empty,
       Init => Initialize'Access));
end PolyORB.DSA_P.Partitions;
