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
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;

package body System.Garlic.Partitions is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARPAR", "(s-garpar): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Allocator_Mutex : Mutex_Type;
   --  Critical section of PID allocator

   Allocator_Ready : Barrier_Type;
   --  Barrier to block until the confirmation comes back

   Allocator_Value : Partition_ID;

   function Allocate (From : Partition_ID) return Partition_ID;
   --  Internal allocation. From indicates the partition that initiated
   --  the allocation process.

   procedure Dump_Partition_Info
     (PID  : in Types.Partition_ID;
      Info : in Partition_Info);
   --  Dump a summary of all the information we have on a partition

   procedure Read_Partition
     (Stream : access Streams.Params_Stream_Type;
      PID    : in Partition_ID;
      Info   : in out Partition_Info);
   --  Unmarshal partition info and update partition table if needed.

   procedure Read_Partitions
     (Stream : access Streams.Params_Stream_Type);
   --  Unmarshal partition info table.

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
      PID  : Partition_ID := First_PID;
   begin
      Enter_Critical_Section;
      loop
         Info := Partitions.Get_Component (PID);
         exit when not Info.Allocated or else PID = Last_Partition_ID;
         PID := PID + 1;
      end loop;
      if not Info.Allocated then
         Info.Allocated      := True;
         Info.Boot_Partition := From;
         Partitions.Set_Component (PID, Info);
      else
         PID := Null_PID;
      end if;
      Leave_Critical_Section;
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
   begin
      Enter (Allocator_Mutex);
      Partitions.Enter;
      Allocator_Value := Allocate (Self_PID);
      Partitions.Leave;

      if N_Boot_Mirrors > 1 then
         pragma Debug (D ("Propose new partition" & Allocator_Value'Img));

         Request_Type'Output
           (Query'Access, (Compute_Partition_ID, Allocator_Value));
         Broadcast (Partition_Operation, Query'Access);

         Wait (Allocator_Ready);
      end if;

      Partition := Allocator_Value;

      pragma Debug (D ("Validate new partition" & Allocator_Value'Img));
      Leave (Allocator_Mutex);
   end Allocate_PID;

   --------------------
   -- Boot_Partition --
   --------------------

   function Boot_Partition (Partition : Partition_ID) return Partition_ID
   is
      Error : Error_Type;
      Info  : Partition_Info;
   begin
      Get_Partition_Info (Partition, Info, Error);
      Catch (Error);
      return Info.Boot_Partition;
   end Boot_Partition;

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
      D ("  Remote_Units  "  & Info.Remote_Units'Img);
      D ("  Status:        " & Status_Type'Image (Info.Status));
   end Dump_Partition_Info;

   --------------------------
   -- Dump_Partition_Table --
   --------------------------

   procedure Dump_Partition_Table is
   begin
      D ("Partition Info Table");
      D ("--------------------");
      for P in Partitions.Table'Range loop
         if Partitions.Table (P).Allocated then
            Dump_Partition_Info (P, Partitions.Get_Component (P));
         end if;
      end loop;
   end Dump_Partition_Table;

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
      if not Found (Error) then
         Name := Info.Logical_Name;
      end if;
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
               declare
                  Query : aliased Params_Stream_Type (0);
               begin
                  Request_Type'Output (Query'Access, Pull_Table);
                  Send_Boot_Server (Partition_Operation, Query'Access, Error);
               end;
            end if;
         end if;

         Partitions.Leave (Version);
         if Found (Error) or else Current.Status = Done then
            exit;
         end if;
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

   ----------------------------
   -- Get_Termination_Policy --
   ----------------------------

   procedure Get_Termination_Policy
     (Partition   : in Partition_ID;
      Termination : out Termination_Type;
      Error       : in out Error_Type)
   is
      Info : Partition_Info;
   begin
      Get_Partition_Info (Partition, Info, Error);
      Termination := Info.Termination;
   end Get_Termination_Policy;

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

            --  Broadcast to any partition in the group. This is step 8.

            Read_Partitions  (Query);
            if Partition /= Self_PID then
               pragma Debug (D ("Send partition table to group"));

               Request_Type'Output (Query, Copy_Table);
               Write_Partitions    (Query);
            end if;

         when Pull_Partition_Table =>
            pragma Debug (D ("Push partition table to" & Partition'Img));

            Request_Type'Output (Reply, (Push_Partition_Table, Null_PID));
            Write_Partitions    (Reply);

         when Compute_Partition_ID =>
            Validate_PID (Request.Partition, Partition);

            if Partition /= Self_PID then
               Request_Type'Output (Query, Request);
            end if;

         when Define_New_Partition =>
            pragma Debug (D ("Define new partition" & Partition'Img));

            Info := Partitions.Get_Component (Partition);
            Read_Partition (Query, Partition, Info);

            if Options.Is_Boot_Mirror then

               --  This is step 2 for boot partition.

               Info.Boot_Partition := Self_PID;
               Partitions.Set_Component (Partition, Info);

               if N_Boot_Mirrors > 1 then
                  pragma Debug (D ("Send partition table to group"));

                  Request_Type'Output (To_All'Access, Copy_Table);
                  Write_Partitions    (To_All'Access);
               end if;
            end if;

            --  Reply to a partition declaration with a set partition
            --  info request. This is step 3 for boot partition.

            pragma Debug (D ("Send partition table back to" & Partition'Img));

            Request_Type'Output (Reply, (Push_Partition_Table, Partition));
            Write_Partitions    (Reply);

         when Push_Partition_Table =>
            pragma Debug (D ("Push partition table"));

            Read_Partitions (Query);

            --  This is a set partition info request issued from a new
            --  partition info request. This way we get the partition id of
            --  the current partition. This is step 3 for booting
            --  partition.

            if Self_PID = Null_PID then

               --  We have the reply from the boot partition. Move
               --  the old partition info into the new partition
               --  info.

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

            elsif Request.Partition /= Null_PID then

               if N_Boot_Mirrors > 1 then

                  --  Why do we send a copy to the group ???

                  Request_Type'Output (To_All'Access, Copy_Table);
                  Write_Partitions    (To_All'Access);
               end if;
            end if;

            --  This is step 4.

            if Self_PID = Null_PID then
               if not Options.Mirror_Expected
                 or else N_Boot_Mirrors > 1
               then
                  Self_PID := Request.Partition;
                  Booted   := True;

                  Info := Partitions.Get_Component (Self_PID);
                  Info.Is_Boot_Mirror  := Options.Is_Boot_Mirror;
                  Info.Boot_Partition  := Partition;
                  Partitions.Set_Component (Self_PID, Info);

                  --  If this partition wants to join the boot server group,
                  --  send an add partition info request. This is step 7.

                  if Info.Is_Boot_Mirror
                    and then N_Boot_Mirrors > 1
                  then
                     pragma Debug (D ("Send partition table to group"));

                     Request_Type'Output (To_All'Access, Copy_Table);
                     Write_Partitions    (To_All'Access);
                  end if;

               else
                  pragma Debug (D ("Waiting for boot mirrors"));
                  delay 2.0;

                  Request_Type'Output (Reply, Pull_Table);
               end if;
            end if;

      end case;

      pragma Debug (Dump_Partition_Table);

      Partitions.Leave;

      if not Empty (To_All'Access) then
         Broadcast (Partition_Operation, To_All'Access);
      end if;

      --  This is step 5. Release startup from step 6.

      if Booted then
         Set_My_Partition_ID (Error);
      end if;
   end Handle_Partition_Request;

   --------------------------
   -- Invalidate_Partition --
   --------------------------

   procedure Invalidate_Partition
     (Partition : in Partition_ID)
   is
      Mirror : Partition_ID := First_PID;
      Query  : aliased Params_Stream_Type (0);
      Info   : Partition_Info;
      Error  : Error_Type;
   begin
      Partitions.Enter;
      Info := Partitions.Get_Component (Partition);
      Info.Status := Dead;
      Free (Info.Logical_Name);
      Partitions.Set_Component (Partition, Info);

      --  If this partition was the boot server, then choose as boot server
      --  the first boot mirror.

      if Partition = Boot_PID then
         while Mirror <= Partitions.Last
           and then
           (Partitions.Table (Mirror).Status /= Done
            or else not Partitions.Table (Mirror).Is_Boot_Mirror
            or else not Partitions.Table (Mirror).Allocated)
         loop
            Mirror := Mirror + 1;
         end loop;

         if Mirror <= Partitions.Last then
            pragma Debug (D ("New boot PID is" & Mirror'Img));

            Boot_PID := Mirror;
            if Boot_PID = Self_PID then
               Set_Slave (False);
            end if;
         end if;
      end if;
      Partitions.Leave;

      if Options.Is_Boot_Mirror
        and then N_Boot_Mirrors > 1
      then
         Request_Type'Output (Query'Access, Copy_Table);
         Write_Partitions    (Query'Access);
         Broadcast (Partition_Operation, Query'Access);

      elsif Partition /= Boot_PID then
         Request_Type'Output (Query'Access, (Push_Partition_Table, Partition));
         Write_Partitions    (Query'Access);
         Send_Boot_Server (Partition_Operation, Query'Access, Error);
      end if;

      pragma Debug (Dump_Partition_Table);
   end Invalidate_Partition;

   -------------
   -- Is_Dead --
   -------------

   function Is_Dead (Partition : Partition_ID) return Boolean is
   begin
      return Partitions.Get_Component (Partition) .Status = Dead;
   end Is_Dead;

   ----------------------
   -- Next_Boot_Mirror --
   ----------------------

   function Next_Boot_Mirror (Partition : Partition_ID)
     return Partition_ID is
   begin
      pragma Assert (Partitions.Table (Partition) .Is_Boot_Mirror);
      Partitions.Enter;
      for P in Partition + 1 .. Partitions.Table'Last loop
         if Partitions.Table (P) .Allocated
           and then Partitions.Table (P) .Is_Boot_Mirror
           and then Partitions.Table (P) .Status = Done
         then
            Partitions.Leave;
            return P;
         end if;
      end loop;

      for P in Null_PID + 1 .. Partition loop
         if Partitions.Table (P) .Allocated
           and then Partitions.Table (P) .Is_Boot_Mirror
           and then Partitions.Table (P) .Status = Done
         then
            Partitions.Leave;
            return P;
         end if;
      end loop;

      raise Program_Error;
   end Next_Boot_Mirror;

   --------------------
   -- Next_Partition --
   --------------------

   procedure Next_Partition
     (Partition : in out Types.Partition_ID)
   is
      Next : Partition_ID := Partition;
   begin
      Partitions.Enter;
      loop
         if Next = Partitions.Table'Last then
            Next := Null_PID;
         else
            Next := Next + 1;
         end if;
         exit when Next = Null_PID
           or else Partitions.Table (Next).Allocated;
      end loop;
      Partitions.Leave;

      pragma Debug
        (D ("Partition next to" & Partition'Img & " is" & Next'Img));

      Partition := Next;
   end Next_Partition;

   --------------------
   -- N_Boot_Mirrors --
   --------------------

   function N_Boot_Mirrors return Natural
   is
      Mirrors : Natural := 0;
   begin
      Enter_Critical_Section;

      for P in Partitions.Table'Range loop
         if Partitions.Table (P).Allocated
           and then Partitions.Table (P).Is_Boot_Mirror
         then
            Mirrors := Mirrors + 1;
         end if;
      end loop;

      Leave_Critical_Section;

      return Mirrors;
   end N_Boot_Mirrors;

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
                                   " has location " & Location & " or " &
                                   To_String (Info.Location)));
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
      PID  : Partition_ID;
      Info : Partition_Info;
   begin
      Enter_Critical_Section;

      while Boolean'Input (Stream) loop
         Partition_ID'Read (Stream, PID);
         Info := Partitions.Get_Component (PID);
         Read_Partition (Stream, PID, Info);
      end loop;

      Leave_Critical_Section;
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
      --  established and a new Partition_ID to be allocated. The partition
      --  location will be registered into the boot partition's
      --  repository. This is step 2. The boot partition sends the
      --  partition table back to the partition. This is step 3. At this
      --  point, Self_PID and Boot_PID is known but startup can be kept
      --  blocking. The partition will continue to ask for the table until
      --  there are two boot mirrors if the option Mirror_Expected is set
      --  to true. This is step 4. Otherwise, startup can complete and
      --  Self_PID_Barrier is open. This is step 5. When a partition is a
      --  potential boot server then it also sends an add partition info
      --  request to the boot partition. This is step 7. Then an all
      --  partition info request will be broadcast. This is step 8.

      Info :=
        (Allocated      => True,
         Location       => Location,
         Protocol       => null,
         Logical_Name   => Options.Partition_Name,
         Termination    => Options.Termination,
         Reconnection   => Options.Reconnection,
         Has_Light_PCS  => Can_Have_A_Light_Runtime,
         Is_Boot_Mirror => False,
         Boot_Partition => Null_PID,
         Remote_Units   => Null_Unit_Id,
         Status         => Done);

      --  This is step 1.

      Request_Type'Output (Query'Access, Request);
      Write_Partition     (Query'Access, Null_PID, Info);
      Send_Boot_Server    (Partition_Operation, Query'Access, Error);
   end Send_Boot_Request;

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

         if Info.Boot_Partition <= From then
            Info.Boot_Partition := From;

         --  Otherwise, find another slot.

         else
            PID := Allocate (From);
         end if;

      else
         Info.Allocated      := True;
         Info.Boot_Partition := PID;
      end if;

      pragma Debug
        (D ("Approve new PID" & PID'Img & " proposed by PID" & From'Img));

      --  We are back on the partition that initiated the allocation process.

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
      Enter_Critical_Section;

      for PID in Partitions.Table'Range loop
         Info := Partitions.Get_Component (PID);
         if Info.Allocated
           and then Info.Status /= None
           and then Info.Status /= Busy
         then
            Boolean'Write (Stream, True);
            Partition_ID'Write (Stream, PID);
            Write_Partition (Stream, PID, Info);
         end if;
      end loop;
      Boolean'Write (Stream, False);

      Leave_Critical_Section;

      pragma Debug (Dump_Partition_Table);
   end Write_Partitions;

begin
   Create (Allocator_Mutex);
   Create (Allocator_Ready);
end System.Garlic.Partitions;
