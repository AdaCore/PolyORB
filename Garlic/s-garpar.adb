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
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with GNAT.Strings;                    use GNAT.Strings;

with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Exceptions;        use System.Garlic.Exceptions;
with System.Garlic.Group;             use System.Garlic.Group;
with System.Garlic.Heart;             use System.Garlic.Heart;
with System.Garlic.Options;           use System.Garlic.Options;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Soft_Links;
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
      Allocated       : Boolean;
      Partition_Name  : String_Access;
      Is_Active_Part  : Boolean;
      Net_Loc_In_Use  : Location_Type;
      Net_Locations   : String_Access;
      Mem_Locations   : String_Access;
      Termination     : Types.Termination_Type;
      Reconnection    : Types.Reconnection_Type;
      Is_Pure_Client  : Boolean;
      Is_Boot_Mirror  : Boolean;
      Boot_Partition  : Types.Partition_ID;
      Online          : Boolean;
      Status          : Types.Status_Type;
   end record;

   --  Allocated      : true when this slot is not empty
   --  Partition_Name : name of the partition (may be duplicated)
   --  Is_Active_Part : true when partition is active
   --  Net_Loc_In_Use : location to use locally to communicate
   --  Net_Locations  : network locations for active partition
   --  Mem_Locations  : memory locations for passive partition
   --  Termination    : termination policy to adopt for this partition
   --  Reconnection   : reconnection policy to adopt for this partition
   --  Is_Pure_Client : true for a partition which does not receive request
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
      Partition_Name => null,
      Is_Active_Part => True,
      Net_Loc_In_Use => Null_Location,
      Net_Locations  => null,
      Mem_Locations  => null,
      Termination    => Types.Global_Termination,
      Reconnection   => Types.Reject_On_Restart,
      Is_Pure_Client => False,
      Is_Boot_Mirror => False,
      Boot_Partition => Types.Null_PID,
      Online         => False,
      Status         => None);

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

         end case;
      end record;

   package Partitions is new System.Garlic.Table.Medium
     (Index_Type     => Types.Partition_ID,
      Null_Index     => Types.Null_PID,
      First_Index    => Types.First_PID,
      Initial_Size   => Natural (Types.Partition_ID_Increment),
      Increment_Size => Natural (Types.Partition_ID_Increment),
      Component_Type => Partition_Info,
      Null_Component => Null_Partition);

   Boot_Mirrors : Natural := 0;
   --  Number of boot mirrors

   Allocator_Mutex : Soft_Links.Mutex_Access;
   --  Critical section for PID allocator.

   Allocator_Watcher : Soft_Links.Watcher_Access;
   --  Allocating a partition id can generate a group
   --  communication. We block until we have a reply from the other
   --  boot mirrors.

   Allocator_Value : Partition_ID;

   function Allocate
     (From : Partition_ID;
      Name : String := "")
     return Partition_ID;
   --  Internal allocation. From indicates the partition that
   --  initiated the allocation process. Name is useful to identify
   --  passive partitions because they are unique.

   procedure Dump_Partition_Info
     (PID  : in Types.Partition_ID;
      Info : in Partition_Info;
      Key  : in Debug_Key);
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
      Active : in     Boolean;
      Name   : in     String;
      Info   : in out Partition_Info;
      Error  : in out Error_Type);
   --  Unmarshal partition info and update partition table if needed.

   procedure Read_Partitions
     (Stream : access Streams.Params_Stream_Type;
      Error  : in out Error_Type);
   --  Unmarshal partition info table.

   procedure Validate_PID
     (PID  : in out Types.Partition_ID;
      From : in Types.Partition_ID;
      Name : in String);
   --  Validate when all the boot mirrors agree on a given PID.

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

   function Allocate
     (From : Partition_ID;
      Name : String := "")
     return Partition_ID
   is
      Info    : Partition_Info;
      PID     : Partition_ID := Null_PID;
      Passive : constant Boolean := (Name'Length /= 0);

   begin
      Partitions.Enter;

      --  Special case for passive partitions. We use the partition
      --  name as a key to resolve pid allocation conflicts. Note that
      --  a null name denotes an active partition.

      if Passive then
         for P in First_PID .. Partitions.Last loop
            Info := Partitions.Get_Component (P);
            if Info.Allocated
              and then Info.Partition_Name /= null
              and then Info.Partition_Name.all = Name
            then
               PID  := P;
               exit;
            end if;
         end loop;
      end if;

      --  If we do not have already a partition id.

      if PID = Null_PID then
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
              and then Info.Is_Pure_Client
            then
               pragma Debug (D ("Recycle a dead partition"));
               PID := P;
               exit;
            end if;
         end loop;

         Info.Boot_Partition := From;
         Info.Allocated      := True;
      end if;

      if PID /= Null_PID then
         Info.Status         := None;

         if Passive
           and then Info.Partition_Name = null
         then
            Info.Partition_Name := new String'(Name);
         end if;

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
      Name      : in String := "";
      Error     : in out Error_Type)
   is
      Query   : aliased Params_Stream_Type (0);
      PID     : Partition_ID;
      Version : Version_Id;

   begin
      loop
         Soft_Links.Enter (Allocator_Mutex);
         PID := Allocate (Self_PID, Name);

         exit when PID = Null_PID
           or else Boot_Mirrors <= 1;

         Allocator_Value := Null_PID;

         pragma Debug (D ("Propose a new partition" & PID'Img));

         Request_Type'Output (Query'Access, (Compute_Partition_ID, PID));
         String'Output (Query'Access, Name);

         Soft_Links.Lookup (Allocator_Watcher, Version);
         Broadcast (Partition_Operation, Query'Access);

         Soft_Links.Differ (Allocator_Watcher, Version);

         --  If the allocated value has changed, there was a
         --  conflict. So, make another pass to check that all boot
         --  mirrors agree on this new value.

         exit when PID = Allocator_Value;
      end loop;

      pragma Debug (D ("Validate new partition" & PID'Img));
      Soft_Links.Leave (Allocator_Mutex);

      if PID = Null_PID then
         Throw (Error, "partition id table full");
      end if;

      Partition := PID;
   end Allocate_PID;

   -------------------------
   -- Dump_Partition_Info --
   -------------------------

   procedure Dump_Partition_Info
     (PID  : in Partition_ID;
      Info : in Partition_Info;
      Key  : in Debug_Key)
   is
      Any : String_List_Access;

   begin
      D ("* Partition" & PID'Img, Key);
      if Info.Partition_Name /= null then
         D ("  Partition_Name " & Info.Partition_Name.all, Key);

      elsif  Info.Status = Dead then
         D ("  Partition_Name <not available>", Key);

      elsif Info.Status = None then
         D ("  Partition_Name <newly allocated>", Key);
         return;
      end if;

      D ("  Allocated      " & Info.Allocated'Img, Key);
      D ("  Is_Active_Part " & Info.Is_Active_Part'Img, Key);

      if Info.Net_Locations /= null then
         Any := Split_String (Info.Net_Locations.all);
         if Any'Length = 1 then
            D ("  Net_Locations  " & Info.Net_Locations.all, Key);

         else
            D ("  Net_Locations  " & Any (1).all, Key);
            for I in 2 .. Any'Last loop
               D ("                 " & Any (I).all, Key);
            end loop;
         end if;
      end if;

      if Info.Net_Loc_In_Use = Null_Location then
         D ("  Net_Loc_In_Use <not available>", Key);

      else
         D ("  Net_Loc_In_Use " & To_String (Info.Net_Loc_In_Use), Key);
      end if;

      if Info.Mem_Locations /= null then
         D ("  Mem_Locations  " & Info.Mem_Locations.all, Key);
      end if;

      D ("  Termination    " & Info.Termination'Img, Key);
      D ("  Reconnection   " & Info.Reconnection'Img, Key);
      D ("  Is_Boot_Mirror " & Info.Is_Boot_Mirror'Img, Key);
      D ("  Boot_Partition"  & Info.Boot_Partition'Img, Key);
      D ("  Online         " & Info.Online'Img, Key);
      D ("  Status         " & Status_Type'Image (Info.Status), Key);
   end Dump_Partition_Info;

   --------------------------
   -- Dump_Partition_Table --
   --------------------------

   procedure Dump_Partition_Table
     (Key : in Debug_Key := Debug.Always) is
   begin
      D ("Partition Info Table", Key);
      D ("--------------------", Key);
      for P in First_PID .. Partitions.Last loop
         Dump_Partition_Info (P, Partitions.Get_Component (P), Key);
      end loop;
   end Dump_Partition_Table;

   ------------------------
   -- Get_Boot_Locations --
   ------------------------

   function Get_Boot_Locations return String is
   begin
      return Partitions.Get_Component (Boot_PID).Net_Locations.all;
   end Get_Boot_Locations;

   ------------------------
   -- Get_Boot_Partition --
   ------------------------

   procedure Get_Boot_Partition
     (Partition      : in Types.Partition_ID;
      Boot_Partition : out Types.Partition_ID;
      Error          : in out Error_Type)
   is
      Info : Partition_Info;

   begin
      Get_Partition_Info (Partition, Info, Error);
      Boot_Partition := Info.Boot_Partition;
   end Get_Boot_Partition;

   -----------------------------
   -- Get_Is_Active_Partition --
   -----------------------------

   procedure Get_Is_Active_Partition
     (Partition : in Partition_ID;
      Active    : out Boolean;
      Error     : in out Error_Type)
   is
      Info : Partition_Info;

   begin
      Get_Partition_Info (Partition, Info, Error);
      Active := Info.Is_Active_Part;
   end Get_Is_Active_Partition;

   ----------------------
   -- Get_Mem_Location --
   ----------------------

   procedure Get_Mem_Location
     (Partition : in Partition_ID;
      Location  : out String_Access;
      Error     : in out Error_Type)
   is
      Info : Partition_Info;

   begin
      Get_Partition_Info (Partition, Info, Error);
      if Info.Mem_Locations = null then
         Throw (Error, "no data location available");
      else
         Location := Info.Mem_Locations;
      end if;
   end Get_Mem_Location;

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
      Name := Info.Partition_Name;
   end Get_Name;

   ----------------------
   -- Get_Net_Location --
   ----------------------

   procedure Get_Net_Location
     (Partition : in Partition_ID;
      Location  : out Location_Type;
      Error     : in out Error_Type)
   is
      Info : Partition_Info;

   begin
      Get_Partition_Info (Partition, Info, Error);
      if Info.Net_Loc_In_Use = Null_Location then
         Throw (Error, "no network location");

      else
         Location := Info.Net_Loc_In_Use;
      end if;
   end Get_Net_Location;

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

         --  Note that Current can be updated between the two Get_Component
         --  calls. For this reason, there is another loop exit at the end
         --  of this block.

         Partitions.Enter;
         Current := Partitions.Get_Component (Partition);
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
      Info   : Partition_Info;
      Result : Protocol_Access;

   begin
      Get_Partition_Info (Partition, Info, Error);
      Result := Get_Protocol (Info.Net_Loc_In_Use);
      if Result = null then
         Throw (Error, "no protocol available");
      else
         Protocol := Result;
      end if;
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

   -----------------------------------
   -- Global_Termination_Partitions --
   -----------------------------------

   function Global_Termination_Partitions return Partition_List is
   begin
      return Matching_Partitions (Has_Global_Termination'Access);
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
      PID     : Partition_ID;

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

      --  This code is not inside the critical section because we need
      --  to call allocate_pid for a passive partition. Allocate_PID
      --  is called during the first connection for an active
      --  partition but we need another mechanism for passive
      --  partition for obvious reasons.

      if Request.Kind = Define_New_Partition then
         declare
            Active : constant Boolean := Boolean'Input (Query);
            Name   : constant String  := String'Input (Query);

         begin
            if Active then
               PID := Partition;

            else
               Allocate_PID (PID, Name, Error);
               if Found (Error) then
                  return;
               end if;
            end if;

            --  Get partition info because the net location in use may
            --  have been assigned. The partition info is not always
            --  completly unknown.

            Info := Partitions.Get_Component (PID);
            Read_Partition (Query, Active, Name, Info, Error);
            if Found (Error) then
               return;
            end if;
         end;
      end if;

      Partitions.Enter;
      case Request.Kind is

         when Copy_Partition_Table =>
            pragma Debug (D ("Copy partition table from" & Partition'Img));

            --  Merge the local table with the one we received.

            Read_Partitions  (Query, Error);
            if Found (Error) then
               Partitions.Leave;
               return;
            end if;

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
            pragma Debug (D ("Compute new partition id"));

            declare
               Partition_Name : constant String := String'Input (Query);
            begin
               --  Fix conflicts.

               Validate_PID (Request.Partition, Partition, Partition_Name);

               --  This request comes from a broadcast request. This
               --  explains why the original sender is Partition when
               --  the sender of the message could be different.

               if Partition /= Self_PID
                 and then Request.Partition /= Null_PID
               then
                  Request_Type'Output (Query, Request);
                  String'Output (Query, Partition_Name);
               end if;
            end;

         when Define_New_Partition =>
            pragma Debug (D ("Define new partition" & PID'Img));

            Partitions.Set_Component (PID, Info);

            if Options.Is_Boot_Mirror then

               --  This is step 2 for boot partition.
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

               --  Reply to the new partition with a copy of the
               --  partition table. This is step 3 for boot partition.

               pragma Debug (D ("Push partition table to" & Partition'Img));

               Request_Type'Output (Reply, (Push_Partition_Table, Partition));
               Write_Partitions    (Reply);
            end if;

         when Push_Partition_Table =>
            pragma Debug (D ("Push partition table"));

            --  Merge the local table with the table we received.

            Read_Partitions (Query, Error);
            if Found (Error) then
               Partitions.Leave;
               return;
            end if;

            if Self_PID = Null_PID then

               --  If the current partition has no its PID yet, then
               --  this operation of Push_Partition_Table is a reply
               --  from its boot partition.

               --  If the boot partition is not the default boot
               --  partition, move the old partition info into the new
               --  partition info.

               if Boot_PID /= Partition then
                  Info := Partitions.Get_Component (Boot_PID);
                  Destroy (Info.Partition_Name);
                  Destroy (Info.Net_Loc_In_Use);
                  Destroy (Info.Net_Locations);
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

      pragma Debug (Dump_Partition_Table (Private_Debug_Key));

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

   exception when others =>
      Throw (Error, "Data error in Partitions.Handle_Partition_Request");
   end Handle_Partition_Request;

   ----------------------------
   -- Has_Global_Termination --
   ----------------------------

   function Has_Global_Termination (Info : Partition_Info) return Boolean is
   begin
      return Info.Allocated
        and then Info.Status = Done
        and then Info.Online
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
        and then Info.Online
        and then Info.Termination = Local_Termination;
   end Has_Local_Termination;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Partitions.Initialize;
      Soft_Links.Create (Allocator_Mutex);
      Soft_Links.Create (Allocator_Watcher);
   end Initialize;

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
      Destroy (Info.Partition_Name);
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
         --  FIXME: The test should be Partition = Boot_PID.
         --  This can happen when we did not succeed to find a new
         --  boot mirror. In this case, the current partition is going
         --  to terminate. We do not need to broadcast the
         --  invalidation.

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

      pragma Debug (Dump_Partition_Table (Private_Debug_Key));
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
      return Matching_Partitions (Is_Known'Access);
   end Known_Partitions;

   ----------------------------------
   -- Local_Termination_Partitions --
   ----------------------------------

   function Local_Termination_Partitions return Partition_List is
   begin
      return Matching_Partitions (Has_Local_Termination'Access);
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
      Partitions.Enter;
      for PID in First_PID .. Partitions.Last loop
         Info := Partitions.Get_Component (PID);
         if Match (Info) then
            Result (Index) := PID;
            Index := Index + 1;
         end if;
      end loop;
      Partitions.Leave;
      return Result (Natural (First_PID) .. Index - 1);
   end Matching_Partitions;

   --------------------
   -- N_Boot_Mirrors --
   --------------------

   function N_Boot_Mirrors return Natural is
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
         --  FIXME: Done => Allocated
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
      return Matching_Partitions (Is_Online'Access);
   end Online_Partitions;

   --------------------
   -- Read_Partition --
   --------------------

   procedure Read_Partition
     (Stream : access Params_Stream_Type;
      Active : in Boolean;
      Name   : in String;
      Info   : in out Partition_Info;
      Error  : in out Error_Type)
   is
      Net_Locations  : String            := String'Input (Stream);
      Mem_Locations  : String            := String'Input (Stream);
      Termination    : Termination_Type  := Termination_Type'Input (Stream);
      Reconnection   : Reconnection_Type := Reconnection_Type'Input (Stream);
      Is_Pure_Client : Boolean           := Boolean'Input (Stream);
      Is_Boot_Mirror : Boolean           := Boolean'Input (Stream);
      Boot_Partition : Partition_ID      := Partition_ID'Input (Stream);
      Status         : Status_Type       := Status_Type'Input (Stream);

   begin
      if Info.Allocated
        and then Info.Status = Dead
      then
         return;
      end if;

      if Status = Dead then
         Destroy (Info.Partition_Name);
         Destroy (Info.Net_Locations);
         Destroy (Info.Mem_Locations);
         Destroy (Info.Net_Loc_In_Use);

      else
         if Info.Mem_Locations = null then
            Info.Mem_Locations := new String'(Mem_Locations);
         end if;

         if Info.Net_Locations = null then

            --  It is the first time we hear about this partition

            if Options.Execution_Mode = Replay_Mode then
               Info.Net_Locations := new String'("replay://");
               Info.Net_Loc_In_Use := To_Location (Info.Net_Locations.all);

               --  We already know about this partition but we have no
               --  info on it.

            elsif Net_Locations /= "" then
               Info.Net_Locations := new String'(Net_Locations);
               declare
                  SA : String_List_Access
                    := Split_String (Info.Net_Locations.all);
                  UL : Location_Type;
               begin
                  --  Try to find a usable location. If we cannot
                  --  agree on a location, then keep using the
                  --  current protocol initialized by
                  --  Analyze_Stream - the protocol used during the
                  --  first connection with us. If this partition
                  --  has no connection with us, then do nothing
                  --  and hope that we will not try to connect it.

                  for N in SA'Range loop
                     UL := To_Location (SA (N).all);
                     if Get_Protocol (UL) /= null
                       and then Get_Data (UL)'Length /= 0
                     then
                        Info.Net_Loc_In_Use := UL;
                        exit;
                     end if;
                  end loop;
               end;
            end if;
         end if;
      end if;
      if Info.Partition_Name = null then
         Info.Partition_Name := new String'(Name);
      end if;
      Info.Allocated      := True;
      Info.Is_Active_Part := Active;
      Info.Termination    := Termination;
      Info.Reconnection   := Reconnection;
      Info.Is_Pure_Client := Is_Pure_Client;
      Info.Is_Boot_Mirror := Is_Boot_Mirror;
      Info.Status         := Status;
      if Boot_Partition /= Null_PID then
         Info.Boot_Partition := Boot_Partition;
      end if;
   exception when others =>
      Throw (Error, "Data error in Partitions.Read_Partition");
   end Read_Partition;

   ---------------------
   -- Read_Partitions --
   ---------------------

   procedure Read_Partitions
     (Stream : access Params_Stream_Type;
      Error  : in out Error_Type)
   is
      PID     : Partition_ID;
      Info    : Partition_Info;
      Mirrors : Natural := 0;

   begin
      Boot_Mirrors := 0;
      while Boolean'Input (Stream) loop
         Partition_ID'Read (Stream, PID);
         declare
            Active : Boolean := Boolean'Input (Stream);
            Name   : String  := String'Input (Stream);

         begin
            Info := Partitions.Get_Component (PID);
            Read_Partition (Stream, Active, Name, Info, Error);
            if Found (Error) then
               return;
            end if;
            Partitions.Set_Component (PID, Info);

--              --  If this is a passive partition, then register the
--              --  partition name in the hash table.

--              if not Active then
--                 Partitions.Set_Name (PID, Name);
--              end if;

            if Info.Is_Boot_Mirror then
               Mirrors := Mirrors + 1;
            end if;
         end;
      end loop;
      Boot_Mirrors := Mirrors;
      exception when others =>
      Throw (Error, "Data error in Partitions.Read_Partitions");
   end Read_Partitions;

   --------------------------------
   -- Register_Passive_Partition --
   --------------------------------

   procedure Register_Passive_Partition
     (Partition      : out Partition_ID;
      Partition_Name : in String;
      Mem_Locations  : in String;
      Error          : in out Error_Type)
   is
      PID     : Partition_ID;
      Info    : Partition_Info;
      Name    : String_Access;
      Locs    : String_Access;
      Query   : aliased Params_Stream_Type (0);
      Version : Version_Id;

   begin
      if Options.Is_Boot_Mirror then

         --  Use Partition_Name to compute a unique partition id for a
         --  passive partition.

         Allocate_PID (PID, Partition_Name, Error);
         if Found (Error) then
            return;
         end if;

         Info := Partitions.Get_Component (PID);
         if Info.Status = None then

            --  Passive partitions do not partipate to the global
            --  termination and their termination mode is
            --  local_termination. The reconnection mode is useless
            --  and we set it at Reject_On_Restart because they are
            --  not supposed to be invalidated (no communication error).

            Info.Is_Active_Part := False;
            Info.Net_Locations  := null;
            Info.Mem_Locations  := new String'(Mem_Locations);
            Info.Termination    := Local_Termination;
            Info.Reconnection   := Reject_On_Restart;
            Info.Is_Pure_Client := True;
            Info.Is_Boot_Mirror := False;
            Info.Status         := Done;
            Partitions.Set_Component (PID, Info);
         end if;

         --  Broadcast the partition id of this partition.

         if Boot_Mirrors > 1 then
            Partitions.Enter;
            Request_Type'Output (Query'Access, Copy_Table);
            Write_Partitions    (Query'Access);
            Partitions.Leave;
            Broadcast (Partition_Operation, Query'Access);
         end if;

         pragma Debug (Dump_Partition_Table (Private_Debug_Key));

      else
         Name := new String'(Partition_Name);
         Locs := new String'(Mem_Locations);

         --  Delegate the partition id computation of a passive
         --  partition to a boot mirror.

         Send_Partition_Definition
           (Partition      => Null_PID,
            Partition_Name => Name,
            Is_Active_Part => False,
            Net_Locations  => null,
            Mem_Locations  => Locs,
            Termination    => Local_Termination,
            Reconnection   => Reject_On_Restart,
            Is_Pure_Client => True,
            Is_Boot_Mirror => False,
            Error          => Error);

         if Found (Error) then
            Destroy (Name);
            Destroy (Locs);
            return;
         end if;

         loop
            --  Wait for this passive partition to be registered in
            --  the local partition table. In this case, the passive
            --  partition is elaborated.

            PID := Null_PID;
            Partitions.Enter;
            for P in First_PID .. Partitions.Last loop
               declare
                  Info : Partition_Info := Partitions.Get_Component (P);

               begin
                  if Info.Status = Done
                    and then not Info.Is_Active_Part
                    and then Info.Partition_Name.all = Partition_Name
                  then
                     PID := P;
                     exit;
                  end if;
               end;
            end loop;
            Partitions.Leave (Version);

            exit when PID /= Null_PID;

            Partitions.Differ (Version);
         end loop;
      end if;

      Partition := PID;
   end Register_Passive_Partition;

   -------------------------------
   -- Send_Partition_Definition --
   -------------------------------

   procedure Send_Partition_Definition
     (Partition      : in Partition_ID;
      Partition_Name : in String_Access;
      Is_Active_Part : in Boolean;
      Net_Locations  : in String_Access;
      Mem_Locations  : in String_Access;
      Termination    : in Termination_Type;
      Reconnection   : in Reconnection_Type;
      Is_Pure_Client : in Boolean;
      Is_Boot_Mirror : in Boolean;
      Error          : in out Error_Type)
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
      --  mirrors group. This is step 7. Then, the copy is updated
      --  during the broadcast when it returns. This is step 8.

      Info :=
        (Allocated      => True,
         Partition_Name => Partition_Name,
         Is_Active_Part => Is_Active_Part,
         Net_Loc_In_Use => Null_Location,
         Net_Locations  => Net_Locations,
         Mem_Locations  => Mem_Locations,
         Termination    => Termination,
         Reconnection   => Reconnection,
         Is_Pure_Client => Is_Pure_Client,
         Is_Boot_Mirror => Is_Boot_Mirror,
         Boot_Partition => Null_PID,
         Online         => False,
         Status         => Done);

      Request_Type'Output (Query'Access, Request);
      Write_Partition     (Query'Access, Partition, Info);
      Send_Boot_Server    (Partition_Operation, Query'Access, Error);
   end Send_Partition_Definition;

   -----------------------
   -- Set_Boot_Location --
   -----------------------

   procedure Set_Boot_Location
     (Location  : in Location_Type)
   is
      Info : Partition_Info :=
        (Allocated      => True,
         Partition_Name => null,
         Is_Active_Part => True,
         Net_Loc_In_Use => Location,
         Net_Locations  => new String'(Merge_String (Options.Boot_Location)),
         Mem_Locations  => null,
         Termination    => Global_Termination,
         Reconnection   => Reject_On_Restart,
         Is_Pure_Client => False,
         Is_Boot_Mirror => True,
         Boot_Partition => Null_PID,
         Online         => False,
         Status         => Done);

   begin
      if Options.Is_Boot_Server then
         Info.Boot_Partition := Boot_PID;
         Info.Partition_Name := Options.Partition_Name;
         Info.Mem_Locations
           := new String'(Merge_String (Options.Data_Location));
      end if;

      pragma Debug (D ("Use boot location to " &
                       To_String (Info.Net_Loc_In_Use)));

      --  Use Last_PID to store boot partition info

      Partitions.Set_Component (Boot_PID, Info);
      Boot_Mirrors := Boot_Mirrors + 1;
   end Set_Boot_Location;

   ----------------
   -- Set_Online --
   ----------------

   procedure Set_Online
     (Partition : in Partition_ID;
      Online    : in Boolean)
   is
      Info : Partition_Info;

   begin
      Info := Partitions.Get_Component (Partition);
      Info.Online := Online;
      Partitions.Set_Component (Partition, Info);
   end Set_Online;

   -----------------------
   -- Set_Used_Protocol --
   -----------------------

   procedure Set_Used_Protocol
     (Partition : in Partition_ID;
      Protocol  : in Protocol_Access)
   is
      Info : Partition_Info := Partitions.Get_Component (Partition);

   begin
      if Info.Net_Loc_In_Use = Null_Location then
         pragma Debug (D ("Net Loc In Use seems to be " &
                          Get_Name (Protocol)));

         Info.Net_Loc_In_Use := To_Location (Protocol, "");
         Partitions.Set_Component (Partition, Info);
      end if;
   end Set_Used_Protocol;

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
      From : in Types.Partition_ID;
      Name : in String)
   is
      Info : Partition_Info;

   begin
      if PID /= Null_PID then
         Info := Partitions.Get_Component (PID);
         if Info.Allocated
           and then Info.Boot_Partition < From
         then
            PID := Null_PID;

         else
            Info.Allocated      := True;
            Info.Boot_Partition := From;
            if Name'Length /= 0 then
               Info.Partition_Name := new String'(Name);
            end if;
            Partitions.Set_Component (PID, Info);

            pragma Debug
              (D ("Approve new PID" & PID'Img &
                  " proposed by PID" & From'Img));
         end if;
      end if;

      --  Back on the partition that initiated the allocation process.

      if From = Self_PID then
         Allocator_Value := PID;
         Soft_Links.Update (Allocator_Watcher);
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
      pragma Unreferenced (PID);
   begin
      Boolean'Write           (Stream, Info.Is_Active_Part);
      if Info.Status = Dead then
         String'Output (Stream, "");
         String'Output (Stream, "");
         String'Output (Stream, "");

      else
         String'Output (Stream, Info.Partition_Name.all);

         if Info.Net_Locations = null then
            String'Output (Stream, "");

         else
            String'Output (Stream, Info.Net_Locations.all);
         end if;

         if Info.Mem_Locations = null then
            String'Output (Stream, "");

         else
            String'Output (Stream, Info.Mem_Locations.all);
         end if;
      end if;

      Termination_Type'Write  (Stream, Info.Termination);
      Reconnection_Type'Write (Stream, Info.Reconnection);
      Boolean'Write           (Stream, Info.Is_Pure_Client);
      Boolean'Write           (Stream, Info.Is_Boot_Mirror);
      Partition_ID'Write      (Stream, Info.Boot_Partition);
      Status_Type'Write       (Stream, Info.Status);
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

end System.Garlic.Partitions;
