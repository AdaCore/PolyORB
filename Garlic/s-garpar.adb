------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . P A R T I T I O N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
with System.Garlic.Name_Table;        use System.Garlic.Name_Table;
with System.Garlic.Options;           use System.Garlic.Options;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Soft_Links;        use System.Garlic.Soft_Links;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Table;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;

package body System.Garlic.Partitions is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARPAR", "(s-garpar): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   type Partition_Info is record
      Location       : Physical_Location.Location_Type;
      Protocol       : Protocols.Protocol_Access;
      Logical_Name   : Utils.String_Access;
      Termination    : Types.Termination_Type;
      Reconnection   : Types.Reconnection_Type;
      Has_Light_PCS  : Boolean;
      Is_Boot_Mirror : Boolean;
      Boot_Partition : Types.Partition_ID;
      Online         : Boolean;
      Status         : Types.Status_Type;
      Allocated      : Boolean;
   end record;

   --  Allocated      : true when this slot is not empty
   --  Location       : partition physical location
   --  Protocol       : cache for location protocol
   --  Logical_Name   : name of the partition (may be duplicated)
   --  Termination    : termination policy to adopt for this partition
   --  Reconnection   : reconnection policy to adopt for this partition
   --  Has_Light_PCS  : true for a partition which does not receive request
   --  Is_Boot_Mirror : true for a partition which has a copy of PID table
   --  Boot_Partition : pid of the partition used to boot a partition
   --  Online         : a communication link has been established
   --  Status         : partition info status

   --  A partition can be allocated. This means that this partition id
   --  is used. But no info can be available on it. So, its status can be:
   --  * None: this partition is in use but no info is available.
   --  * Busy: we already asked for info on it.
   --  * Done: this partition is fully defined and alive.
   --  * Dead: this partition is fully defined but dead.

   Null_Partition : constant Partition_Info :=
     (Allocated      => False,
      Location       => Physical_Location.Null_Location,
      Protocol       => null,
      Logical_Name   => null,
      Termination    => Types.Global_Termination,
      Reconnection   => Types.Rejected_On_Restart,
      Has_Light_PCS  => False,
      Is_Boot_Mirror => False,
      Boot_Partition => Types.Null_PID,
      Online         => False,
      Status         => Types.None);

   type Request_Kind is
      (Compute_Partition_ID,
       Copy_Partition_Table,
       Define_New_Partition,
       Pull_Partition_Table,
       Push_Partition_Table);

   type Request_Type (Kind : Request_Kind := Pull_Partition_Table) is
      record
         case Kind is
            when Pull_Partition_Table |
                 Copy_Partition_Table =>
               null;
            when Push_Partition_Table |
                 Compute_Partition_ID =>
               Partition : Types.Partition_ID;
            when Define_New_Partition =>
               null;
               --  Note that a partition info always follows such a
               --  request.
         end case;
      end record;

   package Partitions is new System.Garlic.Table.Complex
     (Index_Type     => Types.Partition_ID,
      Null_Index     => Types.Null_PID,
      First_Index    => Types.First_PID,
      Initial_Size   => Natural (Types.Partition_ID_Increment),
      Increment_Size => Natural (Types.Partition_ID_Increment),
      Component_Type => Partition_Info,
      Null_Component => Null_Partition);

   Boot_Mirrors : Natural := 0;
   --  Number of boot mirrors

   Allocator_Mutex : Mutex_Access;
   --  Critical section for PID allocator.

   Allocator_Ready : Barrier_Access;
   --  Allocating a partition id can generate a group
   --  communication. We block until we have a reply from the other
   --  boot mirrors.

   Allocator_Value : Partition_ID;

   function Allocate (From : Partition_ID) return Partition_ID;
   --  Internal allocation. From indicates the partition that initiated
   --  the allocation process.

   procedure Dump_Partition_Info
     (PID  : in Types.Partition_ID;
      Info : in Partition_Info);
   --  Dump a summary of all the information we have on a partition

   procedure Get_Partition_Info
     (Partition : in     Partition_ID;
      Info      :    out Partition_Info;
      Error     : in out Error_Type);
   --  If cached, then return local partition info. Otherwise, on a non
   --  boot mirror, send a request. Wait for info to be available.

   function Has_Global_Termination (Info : Partition_Info) return Boolean;
   --  Return True when the partition is done and has a termination
   --  policy sets to global termination.

   function Has_Local_Termination (Info : Partition_Info) return Boolean;
   --  Return True when the partition is done and has a termination
   --  policy sets to local termination.

   function Is_Known (Info : Partition_Info) return Boolean;
   --  Return True when the partition is known (done or dead).

   function Is_Online (Info : Partition_Info) return Boolean;
   --  Return True when the partition is done and online.

   type Matching_Function is
     access function (Info : Partition_Info) return Boolean;

   function Matching_Partitions
     (Match : Matching_Function)
     return Partition_List;
   --  Return a list of partitions ids that match the criteria Match.

   procedure Read_Partition
     (Stream : access Streams.Params_Stream_Type;
      PID    : in Partition_ID;
      Info   : in out Partition_Info);
   --  Unmarshal partition info and update partition table if needed.

   procedure Read_Partitions
     (Stream : access Streams.Params_Stream_Type);
   --  Unmarshal partition info table.

   procedure Validate_PID
     (PID  : in out Types.Partition_ID;
      From : in Types.Partition_ID);
   --  Validation occurs when all the boot mirrors agree on a given
   --  PID.

   procedure Write_Partition
     (Stream : access Streams.Params_Stream_Type;
      PID    : in Partition_ID;
      Info   : in Partition_Info);
   --  Marshal partition info if needed.

   procedure Write_Partitions
     (Stream : access Streams.Params_Stream_Type);
   --  Marshal partition info table.

   Copy_Table : constant Request_Type := (Kind => Copy_Partition_Table);
   Pull_Table : constant Request_Type := (Kind => Pull_Partition_Table);

   --------------
   -- Allocate --
   --------------

   function Allocate (From : Partition_ID) return Partition_ID
   is
      Info : Partition_Info;
      PID  : Partition_ID := Null_PID;
   begin
      Partitions.Enter;
      for P in First_PID .. Last_PID loop
         Info := Partitions.Get_Component (P);
         if not Info.Allocated then
            pragma Debug (D ("Allocate a new partition"));
            PID := P;
            exit;

         --  We can reallocate a partition when this partition acts
         --  as a client (no RCI, no RACW). In this case, the partition
         --  is not cached except in this table.

         elsif Info.Status = Dead
           and then Info.Has_Light_PCS
         then
            pragma Debug (D ("Recycle a dead partition"));
            PID := P;
            exit;
         end if;
      end loop;

      if PID /= Null_PID then
         Info.Allocated      := True;
         Info.Status         := None;
         Info.Boot_Partition := From;
         Partitions.Set_Component (PID, Info);
      end if;
      Partitions.Leave;

      return PID;
   end Allocate;

   ------------------
   -- Allocate_PID --
   ------------------

   procedure Allocate_PID
     (Partition : out Partition_ID;
      Error     : in out Error_Type)
   is
      Query : aliased Params_Stream_Type (0);
      PID   : Partition_ID;
   begin
      Enter (Allocator_Mutex);
      Allocator_Value := Allocate (Self_PID);

      loop
         PID := Allocator_Value;

         if Boot_Mirrors > 1 then
            pragma Debug (D ("Propose a new partition" & PID'Img));

            Request_Type'Output
              (Query'Access, (Compute_Partition_ID, PID));
            Broadcast (Partition_Operation, Query'Access);

            Wait (Allocator_Ready);
         end if;

         --  If the allocated value has changed, there was a
         --  conflict. So, make another pass to check that all boot
         --  mirrors agree on this new value.

         exit when PID = Allocator_Value;

      end loop;

      pragma Debug (D ("Validate new partition" & PID'Img));
      Leave (Allocator_Mutex);

      Partition := PID;
   end Allocate_PID;

   -------------------------
   -- Dump_Partition_Info --
   -------------------------

   procedure Dump_Partition_Info
     (PID  : in Partition_ID;
      Info : in Partition_Info) is
   begin
      D ("* Partition" & PID'Img);
      if Info.Logical_Name /= null then
         D ("  Name           " & Info.Logical_Name.all);
      elsif  Info.Status = Dead then
         D ("  Name           <not available>");
      else
         D ("  Name           <newly allocated>");
         return;
      end if;
      D ("  Allocated      " & Info.Allocated'Img);
      if Info.Status = Dead then
         D ("  Location       <not available>");
      else
         D ("  Location       " & To_String (Info.Location));
      end if;
      D ("  Termination    " & Info.Termination'Img);
      D ("  Reconnection   " & Info.Reconnection'Img);
      D ("  Is_Boot_Mirror " & Info.Is_Boot_Mirror'Img);
      D ("  Boot_Partition"  & Info.Boot_Partition'Img);
      D ("  Online         "  & Info.Online'Img);
      D ("  Status:        " & Status_Type'Image (Info.Status));
   end Dump_Partition_Info;

   --------------------------
   -- Dump_Partition_Table --
   --------------------------

   procedure Dump_Partition_Table
   is
      PIDs : Partition_List := Known_Partitions;
   begin
      D ("Partition Info Table");
      D ("--------------------");
      for I in PIDs'Range loop
         Dump_Partition_Info (PIDs (I), Partitions.Get_Component (PIDs (I)));
      end loop;
   end Dump_Partition_Table;

   ------------------------
   -- Get_Boot_Partition --
   ------------------------

   procedure Get_Boot_Partition
     (Partition      : in Types.Partition_ID;
      Boot_Partition : out Types.Partition_ID;
      Error          : in out Utils.Error_Type)
   is
      Info : Partition_Info;
   begin
      Get_Partition_Info (Partition, Info, Error);
      Boot_Partition := Info.Boot_Partition;
   end Get_Boot_Partition;

   ---------------------
   -- Get_Boot_Server --
   ---------------------

   function Get_Boot_Server return String is
   begin
      return To_String (Partitions.Get_Component (Boot_PID).Location);
   end Get_Boot_Server;

   ------------------
   -- Get_Location --
   ------------------

   procedure Get_Location
     (Partition : in Partition_ID;
      Location  : out Location_Type;
      Error     : in out Error_Type)
   is
      Info : Partition_Info;
   begin
      Get_Partition_Info (Partition, Info, Error);
      Location := Info.Location;
   end Get_Location;

   --------------
   -- Get_Name --
   --------------

   procedure Get_Name
     (Partition : in Partition_ID;
      Name      : out Name_Id;
      Error     : in out Error_Type)
   is
      N : String_Access;
   begin
      Get_Name (Partition, N, Error);
      if not Found (Error) then
         Name := Get (N.all);
      end if;
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   procedure Get_Name
     (Partition : in Partition_ID;
      Name      : out String_Access;
      Error     : in out Error_Type)
   is
      Info : Partition_Info;
   begin
      Get_Partition_Info (Partition, Info, Error);
      Name := Info.Logical_Name;
   end Get_Name;

   ------------------------
   -- Get_Partition_Info --
   ------------------------

   procedure Get_Partition_Info
     (Partition : in     Partition_ID;
      Info      :    out Partition_Info;
      Error     : in out Error_Type)
   is
      Current : Partition_Info;
      Version : Version_Id;
      Query   : aliased Params_Stream_Type (0);

      --  Get a consistent content of PID slot. If info is not available,
      --  then send a request to boot partition and wait until partition
      --  table is updated.

   begin
      loop
         Current := Partitions.Get_Component (Partition);

         exit when Current.Status in Done .. Dead;

         pragma Debug
           (D ("Looking for information on partition" & Partition'Img));

         Partitions.Enter;
         Current := Partitions.Get_Component (Partition);

         --  Note that Current can be updated between the two Get_Component
         --  calls. For this reason, there is another loop exit at the end
         --  of this block.

         if Current.Status = None
           and then not Options.Is_Boot_Server
         then
            Current.Status := Busy;
            Partitions.Set_Component (Partition, Current);

            if Boot_PID /= Self_PID then
               Request_Type'Output (Query'Access, Pull_Table);
            end if;
         end if;

         Partitions.Leave (Version);

         if not Empty (Query'Access) then
            Send_Boot_Server (Partition_Operation, Query'Access, Error);
         end if;

         exit when Found (Error) or else Current.Status = Done;

         Partitions.Differ (Version);
      end loop;

      Info := Current;
   end Get_Partition_Info;

   ------------------
   -- Get_Protocol --
   ------------------

   procedure Get_Protocol
     (Partition : in Partition_ID;
      Protocol  : out Protocol_Access;
      Error     : in out Error_Type)
   is
      Info : Partition_Info;
   begin
      Get_Partition_Info (Partition, Info, Error);
      Protocol := Info.Protocol;
   end Get_Protocol;

   -----------------------------
   -- Get_Reconnection_Policy --
   -----------------------------

   procedure Get_Reconnection_Policy
     (Partition    : in Partition_ID;
      Reconnection : out  Reconnection_Type;
      Error        : in out Error_Type)
   is
      Info : Partition_Info;
   begin
      Get_Partition_Info (Partition, Info, Error);
      Reconnection := Info.Reconnection;
   end Get_Reconnection_Policy;

   -----------------------
   -- Get_Self_Location --
   -----------------------

   function Get_Self_Location return Location_Type is
      Boot_Protocol : constant Protocol_Access :=
        Partitions.Get_Component (Boot_PID).Protocol;
   begin
      return To_Location (Boot_Protocol, Get_Info (Boot_Protocol));
   end Get_Self_Location;

   -----------------------------------
   -- Global_Termination_Partitions --
   -----------------------------------

   function Global_Termination_Partitions return Partition_List is
   begin
      Partitions.Enter;
      declare
         Result : constant Partition_List :=
           Matching_Partitions (Has_Global_Termination'Access);
      begin
         Partitions.Leave;
         return Result;
      end;
   end Global_Termination_Partitions;

   ------------------------------
   -- Handle_Partition_Request --
   ------------------------------

   procedure Handle_Partition_Request
     (Partition : in Partition_ID;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
      Request : Request_Type;
      Booted  : Boolean := False;
      Info    : Partition_Info;
      To_All  : aliased Params_Stream_Type (0);
   begin
      Request := Request_Type'Input (Query);

      pragma Debug
        (D ("Receive from partition" & Partition'Img &
            " request " & Request.Kind'Img));

      --  Suspend any request different from a push table request until
      --  this partition has its partition id.

      if Self_PID = Null_PID
        and then Request.Kind /= Push_Partition_Table
      then
         pragma Debug (D ("Postpone request until pid is known"));

         Wait_For_My_Partition_ID;
      end if;

      Partitions.Enter;
      case Request.Kind is

         when Copy_Partition_Table =>
            pragma Debug (D ("Copy partition table from" & Partition'Img));

            --  Merge the local table with the one we received.

            Read_Partitions  (Query);

            --  Broadcast to any partition in the group. This is step
            --  8. Except if the current partition has initiated the
            --  broadcast.

            if Partition /= Self_PID then
               pragma Debug (D ("Send partition table to group"));

               Request_Type'Output (Query, Copy_Table);
               Write_Partitions    (Query);
            end if;

         when Pull_Partition_Table =>
            pragma Debug (D ("Push partition table to" & Partition'Img));

            --  Send a copy of the local table.

            Request_Type'Output (Reply, (Push_Partition_Table, Null_PID));
            Write_Partitions    (Reply);

         when Compute_Partition_ID =>

            --  Fix conflicts.

            Validate_PID (Request.Partition, Partition);

            if Partition /= Self_PID then
               Request_Type'Output (Query, Request);
            end if;

         when Define_New_Partition =>
            pragma Debug (D ("Define new partition" & Partition'Img));

            --  Merge this new partition in the local partition table.

            Info := Partitions.Get_Component (Partition);
            Read_Partition (Query, Partition, Info);

            if Options.Is_Boot_Mirror then

               --  This is step 2 for boot partition.

               Info.Boot_Partition := Self_PID;
               Partitions.Set_Component (Partition, Info);

               --  There is no need to broadcast the partition table
               --  for a boot mirror because it will broadcast anyway
               --  a copy of its table to update it.

               if not Info.Is_Boot_Mirror
                 and then Boot_Mirrors > 1
               then
                  pragma Debug (D ("Send partition table to group"));

                  Request_Type'Output (To_All'Access, Copy_Table);
                  Write_Partitions    (To_All'Access);
               end if;
            end if;

            --  Reply to the new partition with a copy of the
            --  partition table. This is step 3 for boot partition.

            pragma Debug (D ("Send partition table back to" & Partition'Img));

            Request_Type'Output (Reply, (Push_Partition_Table, Partition));
            Write_Partitions    (Reply);

         when Push_Partition_Table =>
            pragma Debug (D ("Push partition table"));

            --  Merge the local table with the table we received.

            Read_Partitions (Query);

            if Self_PID = Null_PID then

               --  If the current partition has no its PID yet, then
               --  this operation of Push_Partition_Table is a reply
               --  from its boot partition.

               --  If the boot partition is not the default boot
               --  partition, move the old partition info into the new
               --  partition info.

               if Boot_PID /= Partition then
                  Info := Partitions.Get_Component (Boot_PID);
                  if Info.Logical_Name /= null then
                     Free (Info.Logical_Name);
                  end if;
                  Free (Info.Location);
                  Info.Allocated := False;
                  Info.Status    := None;
                  Partitions.Set_Component (Boot_PID, Info);
                  Boot_PID := Partition;
               end if;

               if not Options.Mirror_Expected
                 or else Boot_Mirrors > 1
               then
                  --  This is step 4. We compute the current partition
                  --  id.

                  Self_PID := Request.Partition;
                  Booted   := True;

                  Info := Partitions.Get_Component (Self_PID);
                  Info.Boot_Partition  := Partition;
                  Partitions.Set_Component (Self_PID, Info);

                  --  If this partition wants to join the boot mirrors
                  --  group, send a copy of its partition table to
                  --  update it. This is step 7.

                  if Options.Is_Boot_Mirror then
                     pragma Debug (D ("Send partition table to group"));

                     Request_Type'Output (To_All'Access, Copy_Table);
                     Write_Partitions    (To_All'Access);
                     Boot_Mirrors := Boot_Mirrors + 1;
                  end if;

               else
                  --  Keep the current partition id to null and ask
                  --  once more for the partition table. The code
                  --  above will be executed and we hope that in the
                  --  meantime new boot mirrors will be launched.

                  pragma Debug (D ("Waiting for boot mirrors"));
                  delay 2.0;

                  Request_Type'Output (Reply, Pull_Table);
               end if;

            elsif Request.Partition /= Null_PID then

               --  When Self_PID is different from Null_PID and when
               --  Request.Partition is different from Null_PID, this
               --  Push_Partition_Table operation comes from an
               --  invalidation operation. This should be broadcast to
               --  the other boot mirrors.

               if Boot_Mirrors > 1 then
                  Request_Type'Output (To_All'Access, Copy_Table);
                  Write_Partitions    (To_All'Access);
               end if;
            end if;
      end case;

      Partitions.Leave;

      pragma Debug (Dump_Partition_Table);

      --  We have to leave the critical section to send messages to
      --  other partitions. This prevents potential deadlocks
      --  especially when the target becomes the current partition
      --  itself.

      if not Empty (To_All'Access) then
         Broadcast (Partition_Operation, To_All'Access);
      end if;

      --  This is step 5. Release startup from step 6.

      if Booted then
         Set_My_Partition_ID (Error);
      end if;
   end Handle_Partition_Request;

   ----------------------------
   -- Has_Global_Termination --
   ----------------------------

   function Has_Global_Termination (Info : Partition_Info) return Boolean is
   begin
      return Info.Allocated
        and then Info.Status = Done
        and then Info.Termination = Global_Termination;
   end Has_Global_Termination;

   ---------------------------
   -- Has_Local_Termination --
   ---------------------------

   function Has_Local_Termination (Partition : Partition_ID) return Boolean is
      Info  : Partition_Info;
      Error : Error_Type;
   begin
      Get_Partition_Info (Partition, Info, Error);
      return Has_Local_Termination (Info);
   end Has_Local_Termination;

   ---------------------------
   -- Has_Local_Termination --
   ---------------------------

   function Has_Local_Termination (Info : Partition_Info) return Boolean is
   begin
      return Info.Allocated
        and then Info.Status = Done
        and then Info.Termination = Local_Termination;
   end Has_Local_Termination;

   --------------------------
   -- Invalidate_Partition --
   --------------------------

   procedure Invalidate_Partition
     (Partition : in Partition_ID)
   is
      Query  : aliased Params_Stream_Type (0);
      To_All : aliased Params_Stream_Type (0);
      Info   : Partition_Info;
      Error  : Error_Type;
   begin
      pragma Debug (D ("Invalidate partition" & Partition'Img));

      Partitions.Enter;

      Info := Partitions.Get_Component (Partition);
      Info.Status := Dead;
      Free (Info.Logical_Name);
      if Info.Is_Boot_Mirror then
         Boot_Mirrors := Boot_Mirrors - 1;
      end if;
      Partitions.Set_Component (Partition, Info);

      --  If this partition was the current partition boot mirror,
      --  then choose another boot mirror (the smallest one).

      if Partition = Boot_PID then
         for PID in First_PID .. Partitions.Last loop
            Info := Partitions.Get_Component (PID);
            if Info.Allocated
              and then Info.Status = Done
              and then Info.Is_Boot_Mirror
            then
               Boot_PID := PID;
               exit;
            end if;
         end loop;

         --  If this partition is its own boot mirror, then it is the
         --  boot partition.

         if Boot_PID = Self_PID then
            Set_Slave (False);
         end if;

         pragma Debug (D ("New boot PID is" & Boot_PID'Img));
      end if;

      if Options.Is_Boot_Mirror then
         if Boot_Mirrors > 1 then
            Request_Type'Output (To_All'Access, Copy_Table);
            Write_Partitions    (To_All'Access);
         end if;

      elsif Partition /= Boot_PID then
         --  This can happen when we did not succeed to find a new
         --  boot mirror. In this case, the current partition is going
         --  to terminate. We do not need to broadcast the invalidation.

         Request_Type'Output (Query'Access, (Push_Partition_Table, Partition));
         Write_Partitions    (Query'Access);
      end if;

      Partitions.Leave;

      --  We have to leave the critical section to send messages to
      --  other partitions. This prevents potential deadlocks
      --  especially when the target becomes the current partition
      --  itself.

      if not Empty (To_All'Access) then
         Broadcast (Partition_Operation, To_All'Access);

      elsif not Empty (Query'Access) then
         Send_Boot_Server (Partition_Operation, Query'Access, Error);
      end if;

      pragma Debug (Dump_Partition_Table);
   end Invalidate_Partition;

   --------------
   -- Is_Known --
   --------------

   function Is_Known (Info : Partition_Info) return Boolean is
   begin
      return Info.Allocated
        and then Info.Status in Done .. Dead;
   end Is_Known;

   ---------------
   -- Is_Online --
   ---------------

   function Is_Online (Info : Partition_Info) return Boolean is
   begin
      return Info.Status = Done
        and then Info.Online;
   end Is_Online;

   ----------------------
   -- Known_Partitions --
   ----------------------

   function Known_Partitions return Partition_List is
   begin
      Partitions.Enter;
      declare
         Result : constant Partition_List :=
           Matching_Partitions (Is_Known'Access);
      begin
         Partitions.Leave;
         return Result;
      end;
   end Known_Partitions;

   ----------------------------------
   -- Local_Termination_Partitions --
   ----------------------------------

   function Local_Termination_Partitions return Partition_List is
   begin
      Partitions.Enter;
      declare
         Result : constant Partition_List :=
           Matching_Partitions (Has_Local_Termination'Access);
      begin
         Partitions.Leave;
         return Result;
      end;
   end Local_Termination_Partitions;

   -------------------------
   -- Matching_Partitions --
   -------------------------

   function Matching_Partitions
     (Match : Matching_Function)
     return Partition_List
   is
      Result : Partition_List (Natural (First_PID) ..
                               Natural (Partitions.Last));
      Index  : Natural := Natural (First_PID);
      Info   : Partition_Info;
   begin
      for PID in First_PID .. Partitions.Last loop
         Info := Partitions.Get_Component (PID);
         if Match (Info) then
            Result (Index) := PID;
            Index := Index + 1;
         end if;
      end loop;
      return Result (Natural (First_PID) .. Index - 1);
   end Matching_Partitions;

   --------------------
   -- N_Boot_Mirrors --
   --------------------

   function N_Boot_Mirrors return Natural
   is
   begin
      return Boot_Mirrors;
   end N_Boot_Mirrors;

   ----------------------
   -- Next_Boot_Mirror --
   ----------------------

   function Next_Boot_Mirror return Partition_ID is
      Info : Partition_Info;
   begin
      Partitions.Enter;
      for P in Self_PID + 1 .. Partitions.Last loop
         Info := Partitions.Get_Component (P);
         if Info.Allocated
           and then Info.Status = Done
           and then Info.Is_Boot_Mirror
         then
            Partitions.Leave;
            return P;
         end if;
      end loop;

      for P in First_PID .. Self_PID loop
         Info := Partitions.Get_Component (P);
         if Info.Allocated
           and then Info.Status = Done
           and then Info.Is_Boot_Mirror
         then
            Partitions.Leave;
            return P;
         end if;
      end loop;

      pragma Assert (False);
      return Null_PID;
   end Next_Boot_Mirror;

   -----------------------
   -- Online_Partitions --
   -----------------------

   function Online_Partitions return Partition_List is
   begin
      Partitions.Enter;
      declare
         Result : constant Partition_List :=
           Matching_Partitions (Is_Online'Access);
      begin
         Partitions.Leave;
         return Result;
      end;
   end Online_Partitions;

   --------------------
   -- Read_Partition --
   --------------------

   procedure Read_Partition
     (Stream : access Params_Stream_Type;
      PID    : in Partition_ID;
      Info   : in out Partition_Info)
   is
      Status         : Status_Type       := Status_Type'Input (Stream);
      Location       : String            := String'Input (Stream);
      Logical_Name   : String            := String'Input (Stream);
      Termination    : Termination_Type  := Termination_Type'Input (Stream);
      Reconnection   : Reconnection_Type := Reconnection_Type'Input (Stream);
      Has_Light_PCS  : Boolean           := Boolean'Input (Stream);
      Is_Boot_Mirror : Boolean           := Boolean'Input (Stream);
      Boot_Partition : Partition_ID      := Partition_ID'Input (Stream);
   begin
      if not Info.Allocated
        or else Info.Status /= Dead
      then
         if Status = Dead then
            Free (Info.Logical_Name);
            Free (Info.Location);
         else
            if Info.Location = Null_Location then
               if Options.Execution_Mode = Replay_Mode then
                  Info.Location := To_Location ("replay://");
               else
                  Info.Location := String_To_Location (Location);

                  pragma Debug (D ("Partition" & PID'Img &
                                   " has location " & Location));
               end if;
               Info.Protocol := Get_Protocol (Info.Location);
            end if;
            if Info.Logical_Name = null then
               Info.Logical_Name := new String'(Logical_Name);
            end if;
         end if;
         Info.Allocated      := True;
         Info.Termination    := Termination;
         Info.Reconnection   := Reconnection;
         Info.Has_Light_PCS  := Has_Light_PCS;
         Info.Is_Boot_Mirror := Is_Boot_Mirror;
         Info.Boot_Partition := Boot_Partition;
         Info.Status         := Status;
         Partitions.Set_Component (PID, Info);
      end if;
   end Read_Partition;

   ---------------------
   -- Read_Partitions --
   ---------------------

   procedure Read_Partitions (Stream : access Params_Stream_Type)
   is
      PID     : Partition_ID;
      Info    : Partition_Info;
      Mirrors : Natural := 0;
   begin
      Boot_Mirrors := 0;
      while Boolean'Input (Stream) loop
         Partition_ID'Read (Stream, PID);
         Info := Partitions.Get_Component (PID);
         Read_Partition (Stream, PID, Info);
         if Info.Is_Boot_Mirror then
            Mirrors := Mirrors + 1;
         end if;
      end loop;
      Boot_Mirrors := Mirrors;
   end Read_Partitions;

   -----------------------
   -- Send_Boot_Request --
   -----------------------

   procedure Send_Boot_Request
     (Location : in Location_Type;
      Error    : in out Error_Type)
   is
      Request : Request_Type (Define_New_Partition);
      Info    : Partition_Info;
      Query   : aliased Params_Stream_Type (0);
   begin
      --  We will send a Define_New_Partition request to the boot
      --  partition. This is step 1. This will cause a dialog to be
      --  established and a new Partition_ID to be allocated. The
      --  partition location will be registered into the boot
      --  partition's repository. This is step 2. The boot partition
      --  sends the partition table back to the partition. This is
      --  step 3. At this point, Self_PID and Boot_PID is known but
      --  startup can be kept blocking. The partition will continue to
      --  ask for the table until there are two boot mirrors if the
      --  option Mirror_Expected is set to true. This is step
      --  4. Otherwise, startup can complete and Self_PID_Barrier is
      --  open. This is step 5. When a partition is a boot mirror then
      --  it also sends a copy of its partition table to the boot
      --  mirrors group. This is step 7. Then, the copy is fully
      --  updated during the broadcast when it returns. This is step
      --  8.

      Info :=
        (Allocated      => True,
         Location       => Location,
         Protocol       => null,
         Logical_Name   => Options.Partition_Name,
         Termination    => Options.Termination,
         Reconnection   => Options.Reconnection,
         Has_Light_PCS  => Can_Have_A_Light_Runtime,
         Is_Boot_Mirror => Options.Is_Boot_Mirror,
         Boot_Partition => Null_PID,
         Online         => False,
         Status         => Done);

      --  This is step 1.

      Request_Type'Output (Query'Access, Request);
      Write_Partition     (Query'Access, Null_PID, Info);
      Send_Boot_Server    (Partition_Operation, Query'Access, Error);
   end Send_Boot_Request;

   -----------------------
   -- Set_Boot_Location --
   -----------------------

   procedure Set_Boot_Location
     (Location : in Location_Type)
   is
      Info : Partition_Info :=
        (Allocated      => True,
         Location       => Location,
         Protocol       => Get_Protocol (Location),
         Logical_Name   => null,
         Reconnection   => Rejected_On_Restart,
         Termination    => Global_Termination,
         Has_Light_PCS  => False,
         Is_Boot_Mirror => True,
         Boot_Partition => Null_PID,
         Online         => False,
         Status         => Done);
   begin
      if Options.Is_Boot_Server then
         Info.Boot_Partition := Boot_PID;
         Info.Logical_Name   := Options.Partition_Name;
      end if;

      pragma Debug (D ("Set boot location to " & To_String (Info.Location)));

      --  Use Last_PID to store boot partition info

      Partitions.Set_Component (Boot_PID, Info);
      Boot_Mirrors := Boot_Mirrors + 1;
   end Set_Boot_Location;

   ----------------
   -- Set_Online --
   ----------------

   procedure Set_Online
     (Partition : in Partition_ID;
      Online    : in Boolean) is
      Info : Partition_Info;
   begin
      Info := Partitions.Get_Component (Partition);
      Info.Online := Online;
      Partitions.Set_Component (Partition, Info);
   end Set_Online;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      --  Resume tasks waiting for an update of partition info table.

      Partitions.Update;
   end Shutdown;

   ------------------
   -- Validate_PID --
   ------------------

   procedure Validate_PID
     (PID  : in out Types.Partition_ID;
      From : in Types.Partition_ID)
   is
      Info : Partition_Info;
   begin
      Info := Partitions.Get_Component (PID);
      if Info.Allocated then
         --  The partition with the smallest PID is always right.

         if From <= Info.Boot_Partition then
            Info.Boot_Partition := From;

         --  Otherwise, find another slot.

         else
            PID := Allocate (From);
         end if;

      else
         Info.Allocated      := True;
         Info.Boot_Partition := PID;
      end if;
      Partitions.Set_Component (PID, Info);

      pragma Debug
        (D ("Approve new PID" & PID'Img & " proposed by PID" & From'Img));

      --  We are back on the partition that initiated the allocation
      --  process.

      if From = Self_PID then
         Allocator_Value := PID;
         Signal (Allocator_Ready);
      end if;
   end Validate_PID;

   ---------------------
   -- Write_Partition --
   ---------------------

   procedure Write_Partition
     (Stream : access Params_Stream_Type;
      PID    : in Partition_ID;
      Info   : in Partition_Info)
   is
   begin
      Status_Type'Write       (Stream, Info.Status);
      if Info.Status = Dead then
         String'Output (Stream, "");
         String'Output (Stream, "");
      else
         String'Output (Stream, To_String (Info.Location));
         String'Output (Stream, Info.Logical_Name.all);
      end if;
      Termination_Type'Write  (Stream, Info.Termination);
      Reconnection_Type'Write (Stream, Info.Reconnection);
      Boolean'Write           (Stream, Info.Has_Light_PCS);
      Boolean'Write           (Stream, Info.Is_Boot_Mirror);
      Partition_ID'Write      (Stream, Info.Boot_Partition);
   end Write_Partition;

   ----------------------
   -- Write_Partitions --
   ----------------------

   procedure Write_Partitions (Stream : access Params_Stream_Type)
   is
      Info : Partition_Info;
   begin
      for PID in First_PID .. Partitions.Last loop
         Info := Partitions.Get_Component (PID);
         if Info.Allocated
           and then Info.Status in Done .. Dead
         then
            Boolean'Write (Stream, True);
            Partition_ID'Write (Stream, PID);
            Write_Partition (Stream, PID, Info);
         end if;
      end loop;
      Boolean'Write (Stream, False);
   end Write_Partitions;

begin
   Create (Allocator_Mutex);
   Create (Allocator_Ready);
end System.Garlic.Partitions;
