------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . P A R T I T I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Group;             use System.Garlic.Group;
with System.Garlic.Heart;             use System.Garlic.Heart;
with System.Garlic.Options;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Soft_Links;        use System.Garlic.Soft_Links;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;

package body System.Garlic.Partitions is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARPAR", "(s-garpar): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Allocator_Mutex : Mutex_Access := Create;
   --  Critical section of PID allocator.

   Allocator_Ready : Barrier_Access := Create;
   --  Barrier to block until the confirmation comes back.

   Allocator_Value : Partition_ID;

   function Allocate (From : Partition_ID) return Partition_ID;
   --  Internal allocation. From indicates the partition that initiated
   --  the allocation process.

   --------------
   -- Allocate --
   --------------

   function Allocate (From : Partition_ID) return Partition_ID
   is
      Partition : Partition_ID;
   begin
      Enter_Critical_Section;
      for PID in Partitions.Table'Range loop
         if not Partitions.Table (PID).Allocated then
            Partitions.Table (PID).Allocated   := True;
            Partitions.Table (PID).Boot_Server := From;
            Partition := PID;
            exit;
         end if;
      end loop;
      Leave_Critical_Section;

      return Partition;
   end Allocate;

   ------------------
   -- Allocate_PID --
   ------------------

   function Allocate_PID return Partition_ID
   is
      PID   : Partition_ID;
      Query : aliased Params_Stream_Type (0);
   begin
      Enter (Allocator_Mutex);
      Partitions.Enter;
      Allocator_Value := Allocate (Self_PID);
      Partitions.Leave;
      PID := Allocator_Value;

      pragma Debug (D (D_Warning, "Propose new partition" & PID'Img));

      Request_Type'Output (Query'Access, (Map_Partition_Info, PID));
      Broadcast (Partition_Operation, Query'Access);

      Wait (Allocator_Ready);
      PID := Allocator_Value;

      pragma Debug
        (D (D_Warning, "Validate new partition" & Allocator_Value'Img));

      Leave (Allocator_Mutex);

      return PID;
   end Allocate_PID;

   -------------------------
   -- Dump_Partition_Info --
   -------------------------

   procedure Dump_Partition_Info
     (PID  : in Partition_ID;
      Info : in Partition_Info) is
   begin
      D (D_Dump, "Information on partition" & PID'Img);
      if Info.Logical_Name /= null then
         D (D_Dump, "  Name:         " & Info.Logical_Name.all);
      else
         D (D_Dump, "  Name:         <no name>");
      end if;
      D (D_Dump, "  Allocated:    " & Info.Allocated'Img);
      D (D_Dump, "  Location:     " & To_String (Info.Location));
      D (D_Dump, "  Termination:  " & Info.Termination'Img);
      D (D_Dump, "  Reconnection: " & Info.Reconnection'Img);
      D (D_Dump, "  Boot_Ability: " & Info.Boot_Ability'Img);
      D (D_Dump, "  Boot_Server: "  & Info.Boot_Server'Img);
      D (D_Dump, "  Status:       " & Status_Type'Image (Info.Status));
   end Dump_Partition_Info;

   ---------------------
   -- Get_Boot_Server --
   ---------------------

   function Get_Boot_Server return String is
   begin
      return To_String (Partitions.Get_Component (Boot_PID).Location);
   end Get_Boot_Server;

   ------------------------
   -- Get_Partition_Info --
   ------------------------

   function Get_Partition_Info (Partition : Partition_ID)
      return Partition_Info
   is
      Info    : Partition_Info;
      Version : Version_Id;

      --  Get a consistent content of PID slot. If info is not available,
      --  then send a request to boot partition and wait until partition
      --  table is updated.

   begin
      loop
         Info := Partitions.Get_Component (Partition);

         exit when Info.Status = Done;

         pragma Debug
           (D (D_Debug,
               "Looking for information on partition" & Partition'Img));

         Partitions.Enter;
         Info := Partitions.Get_Component (Partition);

         --  Note that Partitions.Table (PID) can be updated between
         --  the two Get_Component occurences. For this reason, there
         --  is another loop exit at the end of this block.

         if Info.Status = None
           and then not Options.Boot_Partition
         then
            Info.Status := Busy;
            Partitions.Set_Component (Partition, Info);

            if Boot_PID /= Self_PID then
               declare
                  Query : aliased Params_Stream_Type (0);
               begin
                  Request_Type'Output
                    (Query'Access,
                     Request_Type'(Get_Partition_Info, Partition));
                  Send (Boot_PID, Partition_Operation, Query'Access);
               end;
            end if;
         end if;

         Partitions.Leave (Version);
         if Info.Status = Done then
            Dump_Partition_Info (Partition, Info);
            exit;
         end if;
         Partitions.Differ (Version);
      end loop;

      return Info;
   end Get_Partition_Info;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol (Partition : Partition_ID) return Protocol_Access is
   begin
      return Get_Partition_Info (Partition).Protocol;
   end Get_Protocol;

   -----------------------
   -- Get_Self_Location --
   -----------------------

   function Get_Self_Location return Location_Type is
      Boot_Protocol : constant Protocol_Access :=
        Partitions.Get_Component (Boot_PID).Protocol;
   begin
      return To_Location (Boot_Protocol, Get_Info (Boot_Protocol));
   end Get_Self_Location;

   ------------------
   -- Validate_PID --
   ------------------

   procedure Validate_PID
     (PID  : in out Types.Partition_ID;
      From : in Types.Partition_ID)
   is
      Info : Partition_Info;
   begin
      --  We assume that this piece of code is wrapped into a critical
      --  section on the Partitions table.

      Info := Partitions.Get_Component (PID);
      if Info.Allocated then
         --  The partition with the biggest PID is always right.

         if Info.Boot_Server <= From then
            Info.Boot_Server := From;

         --  Otherwise, find another slot.

         else
            PID := Allocate (From);
         end if;

      else
         Info.Allocated   := True;
         Info.Boot_Server := PID;
      end if;

      pragma Debug
        (D (D_Warning,
            "Approve new PID" & PID'Img &
            " proposed by PID" & From'Img));

      --  We are back on the partition that initiated the allocation process.

      if From = Self_PID then
         Allocator_Value := PID;
         Signal (Allocator_Ready);
      end if;
   end Validate_PID;

end System.Garlic.Partitions;
