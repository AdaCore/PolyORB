------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . H E A R T                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
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

with Ada.Exceptions;                  use Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions);
with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Filters;           use System.Garlic.Filters;
with System.Garlic.Name_Table;        use System.Garlic.Name_Table;
with System.Garlic.Options;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Soft_Links;        use System.Garlic.Soft_Links;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Trace;             use System.Garlic.Trace;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;
with System.Standard_Library;

with System.Garlic.Linker_Options;
pragma Warnings (Off, System.Garlic.Linker_Options);

package body System.Garlic.Heart is

   use Ada.Streams;
   use System.Garlic.Types, System.Garlic.Utils;
   use System.Garlic.Partitions;

   package Partitions renames System.Garlic.Partitions.Partitions;
   use Partitions;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARHEA", "(s-garhea): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Shutdown_Policy     : Shutdown_Type     := Shutdown_On_Boot_Partition_Error;
   --  These parameters control how Garlic will act in face of errors.
   --  They don't need extra protection because they should not be modified
   --  by more than one task (in fact, they should not be modified after
   --  the elaboration is terminated).

   Elaboration_Barrier : Barrier_Type;
   --  This barrier will be no longer blocking when the elaboration is
   --  terminated.

   Self_PID_Barrier : Barrier_Access := new Barrier_Type;
   --  Block any task until Self_PID is different from Null_PID

   Handlers : array (External_Opcode) of Request_Handler;
   --  Handler callbacks table

   Partition_Error_Notification : RPC_Error_Notifier_Type;
   --  Call this procedure when a partition dies

   Request_Get : constant Request_Type := (Kind => Get_Partition_Info);

   function Allocate_PID return Types.Partition_ID;
   --  Allocate a new partition ID

   procedure Dump_Partition_Info
     (PID  : in Partition_ID;
      Info : in Partition_Info);
   --  Dump a summary of all the information we have on a partition

   function Get_Partition_Info
     (PID : Partition_ID)
      return Partition_Info;
   --  If cached, then return local partition info. Otherwise, on a non
   --  boot partition send a request. Wait for info to be available.

   function Get_Protocol
     (Partition : Partition_ID)
      return Protocol_Access;
   pragma Inline (Get_Protocol);
   --  Same as above. But for boot partition, then get protocol from
   --  boot server option.

   procedure Handle_External
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type);
   --  Public operations

   procedure Handle_Internal
     (Partition : in Partition_ID;
      Opcode    : in Internal_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type);
   --  Internal operations

   function Opcode_Read (Opcode : Stream_Element) return Any_Opcode;
   pragma Inline (Opcode_Read);
   function Opcode_Write (Opcode : Any_Opcode) return Stream_Element;
   pragma Inline (Opcode_Write);
   --  Read and write opcode on one byte

   procedure Partition_Request_Handler
     (Partition : in Partition_ID;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type);
   --  Handle Partition_Service operations

   procedure Partition_RPC_Receiver
     (Params : access Streams.Params_Stream_Type;
      Result : access Streams.Params_Stream_Type);
   --  Global RPC receiver

   function PID_Read (Partition : Stream_Element) return Partition_ID;
   pragma Inline (PID_Read);
   function PID_Write (Partition : Partition_ID) return Stream_Element;
   pragma Inline (PID_Write);
   --  Read and write partition id on one byte

   procedure Shutdown;
   --  Generates a local shutdown

   ------------------
   -- Allocate_PID --
   ------------------

   function Allocate_PID return Partition_ID
   is
      Partition : Partition_ID := Null_PID;
   begin
      Partitions.Enter;
      Enter_Critical_Section;
      for PID in Partitions.Table'Range loop
         if not Partitions.Table (PID).Allocated then
            Partitions.Table (PID).Allocated := True;
            Partition := PID;
            exit;
         end if;
      end loop;
      Leave_Critical_Section;
      Partitions.Leave;

      pragma Debug (D (D_Warning, "Allocate partition" & Partition'Img));

      return Partition;
   end Allocate_PID;

   --------------------
   -- Analyze_Stream --
   --------------------

   procedure Analyze_Stream
     (Partition  : out Partition_ID;
      Opcode     : out Any_Opcode;
      Unfiltered : out Stream_Element_Access;
      Filtered   : in  Stream_Element_Access;
      Offset     : in  Ada.Streams.Stream_Element_Count := 0)
   is
      PID   : Partition_ID;
      Code  : Any_Opcode;
      First : constant Stream_Element_Count := Filtered'First + Offset;
      Last  : constant Stream_Element_Count := Filtered'Last;
      Data  : Stream_Element_Array renames Filtered (First + 2 .. Last);
   begin
      --  Do not analyze any request until boot partition completes
      --  its initialization (ie initializes its partition id). Especially
      --  do not allocate any partition id until boot pid is defined.

      if Options.Boot_Partition and then Self_PID = Null_PID then
         Self_PID_Barrier.Wait;
      end if;

      --  Dump the stream for debugging purpose

      pragma Debug (D (D_Dump, "Dumping stream to analyze"));
      pragma Debug (Dump (D_Dump, Filtered, Private_Debug_Key));

      --  Record the current packet content in the trace file if needed

      if Options.Execution_Mode = Trace_Mode then
         Trace_Data (Partition, Filtered);
      end if;

      --  Read the partition id from the stream and check that it is valid

      PID := PID_Read (Filtered (First));
      if not PID'Valid then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Received incorrect partition id");
      end if;

      --  Read the opcode from the stream and check that it is valid

      Code := Opcode_Read (Filtered (First + 1));
      if not Code'Valid then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Received incorrect opcode");
      elsif Code = No_Operation then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Received unexpected No_Operation opcode");
      end if;

      --  When the partition id is unknown, allocate a new one

      if PID = Null_PID then
         PID := Allocate_PID;
      end if;

      pragma Debug
        (D (D_Debug,
            "Received request " & Code'Img &
            " from partition" & PID'Img));

      --  Unfilter the data and put it in a stream

      Unfiltered := Filter_Incoming (PID, Code, Data);
      Partition  := PID;
      Opcode     := Code;

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug
           (D (D_Exception, "Analyze_Stream: "& Exception_Information (E)));
         raise;
   end Analyze_Stream;

   ------------------------------
   -- Can_Have_A_Light_Runtime --
   ------------------------------

   function Can_Have_A_Light_Runtime return Boolean is
   begin
      --  If the termination is not Local_Termination, fail

      if Options.Termination /= Local_Termination then
         return False;
      end if;

      --  If there is any RCI or RACW package, fail

      if Options.Has_RCI_Pkg_Or_RACW_Var then
         return False;
      end if;

      --  If this is the main partition, fail

      if Options.Boot_Partition then
         return False;
      end if;

      --  There is no reason not to have a light runtime

      return True;
   end Can_Have_A_Light_Runtime;

   --------------------------
   -- Complete_Elaboration --
   --------------------------

   procedure Complete_Elaboration is
   begin
      pragma Debug
        (D (D_Debug, "Complete termination"));

      Elaboration_Barrier.Signal_All (Permanent => True);
   end Complete_Elaboration;

   -------------------------
   -- Dump_Partition_Info --
   -------------------------

   procedure Dump_Partition_Info
     (PID  : in Partition_ID;
      Info : in Partition_Info) is
   begin
      D (D_Dump, "Information on partition" & Partition_ID'Image (PID));
      if Info.Logical_Name /= null then
         D (D_Dump, "  Name:         " & Info.Logical_Name.all);
      else
         D (D_Dump, "  Name:         <no name>");
      end if;
      D (D_Dump, "  Allocated:    " & Info.Allocated'Img);
      D (D_Dump, "  Location:     " & To_String (Info.Location));
      D (D_Dump, "  Termination:  " & Info.Termination'Img);
      D (D_Dump, "  Reconnection: " & Info.Reconnection'Img);
      D (D_Dump, "  Status:       " & Status_Type'Image (Info.Status));
   end Dump_Partition_Info;

   ---------------------
   -- Get_Boot_Server --
   ---------------------

   function Get_Boot_Server return String is
   begin
      return To_String (Partitions.Get_Component (Boot_PID).Location);
   end Get_Boot_Server;

   -------------------------
   -- Get_My_Partition_ID --
   -------------------------

   function Get_My_Partition_ID return Partition_ID
   is
      Boot_Partition : Partition_Info;
      Boot_Request   : Request_Type (Set_Partition_Info);
   begin
      if Self_PID = Null_PID then

         pragma Debug (D (D_Debug, "Looking for my partition id"));

         if Options.Boot_Partition then

            --  Get previous boot partition info. Compute a new boot pid
            --  and then reset boot partition info in its slot. Deallocate
            --  the previous slot.

            Partitions.Enter;

            Boot_Partition := Partitions.Get_Component (Boot_PID);
            Boot_Partition.Allocated := False;
            Boot_Partition.Status    := None;
            Partitions.Set_Component (Boot_PID, Boot_Partition);

            Boot_PID := Null_PID + 1;
            Boot_Partition.Allocated := True;
            Boot_Partition.Status    := Done;
            Partitions.Set_Component (Boot_PID, Boot_Partition);

            Dump_Partition_Info (Boot_PID, Boot_Partition);
            Partitions.Leave;

            Set_My_Partition_ID (Boot_PID);

         else
            --  We will send a Set_Partition_Info request to the boot
            --  partition. This is step 1. This will cause a dialog to be
            --  established and a new Partition_ID to be allocated, and our
            --  location will be registered into the boot partition's
            --  repository. This is step 2. The boot partition sends this
            --  partition info back to this partition. This is step 3. At
            --  this point, Self_PID is known but startup is kept
            --  blocking. The boot partition sends partition info on itself
            --  to this partition. This is step 4. At this point, Boot_PID
            --  is known. Therefore, startup can complete and
            --  Self_PID_Barrier is open. This is step 5.

            Boot_Partition := Partitions.Get_Component (Boot_PID);

            Boot_Request.Logical_Name := Options.Partition_Name;
            Boot_Request.Reconnection := Options.Reconnection;
            Boot_Request.Termination  := Options.Termination;
            Boot_Request.Location     :=
              To_Location (Boot_Partition.Protocol,
                           Get_Info (Boot_Partition.Protocol));

            --  This is step 1.

            declare
               Query : aliased Params_Stream_Type (0);
            begin
               Partition_ID'Write  (Query'Access, Null_PID);
               Request_Type'Output (Query'Access, Boot_Request);
               Send (Boot_PID, Partition_Operation, Query'Access);
            end;

            --  This is step 5.

            Self_PID_Barrier.Wait;

         end if;

         Dump_Partition_Info (Boot_PID, Partitions.Get_Component (Boot_PID));
         Dump_Partition_Info (Self_PID, Partitions.Get_Component (Self_PID));
      end if;

      return Self_PID;
   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D (D_Exception, Exception_Information (E)));
         raise;
   end Get_My_Partition_ID;

   ------------------------
   -- Get_Partition_Info --
   ------------------------

   function Get_Partition_Info (PID : Partition_ID)
      return Partition_Info
   is
      Info    : Partition_Info;
      Version : Version_Id;

      --  Get a consistent content of PID slot. If info is not available,
      --  then send a request to boot partition and wait until partition
      --  table is updated.

   begin
      loop
         Info := Partitions.Get_Component (PID);

         exit when Info.Status = Done;

         pragma Debug
           (D (D_Debug,
               "Looking for information on partition" & PID'Img));

         Partitions.Enter;
         Info := Partitions.Get_Component (PID);

         --  Note that Partitions.Table (PID) can be updated between
         --  the two Get_Component occurences. For this reason, there
         --  is another loop exit at the end of this block.

         if Info.Status = None
           and then not Options.Boot_Partition
         then
            Info.Status := Busy;
            Partitions.Set_Component (PID, Info);

            declare
               Query : aliased Params_Stream_Type (0);
            begin
               Partition_ID'Write  (Query'Access, PID);
               Request_Type'Output (Query'Access, Request_Get);
               Send (Boot_PID, Partition_Operation, Query'Access);
            end;
         end if;

         Partitions.Leave (Version);
         if Info.Status = Done then
            Dump_Partition_Info (PID, Info);
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

   ---------------------
   -- Handle_External --
   ---------------------

   procedure Handle_External
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type)
   is
      Handle : Request_Handler;
   begin
      pragma Assert (Self_PID /= Null_PID);

      if Opcode /= Shutdown_Service then
         Soft_Links.Activity_Detected;
      end if;

      Handle := Handlers (Opcode);
      pragma Assert (Handle /= null);

      Handle (Partition, Opcode, Query, Reply);
   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug
           (D (D_Exception, "Handle_External: " & Exception_Information (E)));
         raise Communication_Error;
   end Handle_External;

   ---------------------
   -- Handle_Internal --
   ---------------------

   procedure Handle_Internal
     (Partition : in Partition_ID;
      Opcode    : in Internal_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type) is
   begin
      Soft_Links.Activity_Detected;

      case Opcode is
         when No_Operation =>
            null;

         when Partition_Operation =>
            Partition_Request_Handler (Partition, Query, Reply);

         when Shutdown_Operation =>
            Heart.Shutdown;

      end case;

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug
           (D (D_Exception,
               "Handle_Internal: " & Exception_Information (E)));
         raise Communication_Error;
   end Handle_Internal;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Self_PID := Null_PID;
      Boot_PID := Last_PID;
   end Initialize;

   --------------
   -- Location --
   --------------

   function Location (Partition : Partition_ID) return Location_Type is
   begin
      return Get_Partition_Info (Partition) .Location;
   end Location;

   ----------
   -- Name --
   ----------

   function Name (Partition : Partition_ID) return Name_Id is
   begin
      return Get (Name (Partition));
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Partition : Partition_ID) return String is
   begin
      return Get_Partition_Info (Partition).Logical_Name.all;
   end Name;

   --------------------
   -- Next_Partition --
   --------------------

   procedure Next_Partition
     (Partition : in out Types.Partition_ID;
      Allocated : in Boolean := True)
   is
      From : Partition_ID := Partitions.Table'First;
      Last : constant Partition_ID := Partitions.Table'Last;
      Next : Partition_ID;
   begin
      if Partition /= Null_PID then
         if Partition < Partitions.Table'Last then
            From := Partition + 1;
         else
            pragma Debug (D (D_Debug,
                             "Partition" & Partition'Img &
                             " ->" & Null_PID'Img));
            Partition := Null_PID;
            return;
         end if;
      end if;

      Next := Null_PID;
      Partitions.Enter;
      for PID in From .. Last loop
         if Partitions.Table (PID).Allocated = Allocated then
            Next := PID;
            exit;
         end if;
      end loop;
      Partitions.Leave;
      pragma Debug (D (D_Debug,
                       "Partition" & Partition'Img &
                       " -> Partition" & Next'Img));
      Partition := Next;
   end Next_Partition;

   -----------------
   -- Opcode_Read --
   -----------------

   function Opcode_Read (Opcode : Stream_Element) return Any_Opcode is
   begin
      return Any_Opcode'Val (Opcode);
   end Opcode_Read;

   ------------------
   -- Opcode_Write --
   ------------------

   function Opcode_Write (Opcode : Any_Opcode) return Stream_Element is
   begin
      return Any_Opcode'Pos (Opcode);
   end Opcode_Write;

   -------------------------------
   -- Partition_Request_Handler --
   -------------------------------

   procedure Partition_Request_Handler
     (Partition : in Partition_ID;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type)
   is
      Request : Request_Type;
      PID     : Partition_ID;
      Booted  : Boolean := False;
      Info    : Partition_Info;
   begin
      Partition_ID'Read (Query, PID);
      Request := Request_Type'Input (Query);

      pragma Debug
        (D (D_Warning,
            "Receive from partition" & Partition'Img &
            " request " & Request.Kind'Img &
            " on partition " & PID'Img));

      if Options.Execution_Mode = Replay_Mode
        and then Request.Kind = Set_Partition_Info
      then
         Request.Location := To_Location ("replay://");
      end if;

      --  If we receive a Set_Partition_Info request although the partition
      --  id of the current partition is unknown, the request concerns the
      --  current partition. It provides its new partition id. We also ask
      --  for the boot partition info as we know its pid. Note that we have
      --  to send this request because the current boot partition info is
      --  still incomplete.  We don't Set_My_Partition_ID to prevent this
      --  partition from completing elaboration. But we preserve the
      --  partition id in Self_PID.

      if Request.Kind = Set_Partition_Info then
         if Self_PID = Null_PID then
            pragma Debug
              (D (D_Warning, "Boot partition id is" & Partition'Img));

            --  This is request from step 3. We receive partition info for
            --  this partition. This info comes from the boot partition.
            --  This is a way to get the boot partition id. Deallocate the
            --  previous slot and allocate the new one.

            if Boot_PID /= Partition then
               Partitions.Enter;
               Info := Partitions.Get_Component (Boot_PID);
               Partitions.Set_Component (Partition, Info);
               Info.Allocated := False;
               Info.Status    := None;
               Partitions.Set_Component (Boot_PID, Info);
               Boot_PID := Partition;
               Partitions.Leave;
            end if;

            Partition_ID'Write (Reply, Partition);
            Request_Type'Output (Reply, Request_Get);

            pragma Debug (D (D_Warning, "Self partition id is" & PID'Img));

            --  This is request from step 4.

            Self_PID := PID;
         end if;

         --  When a partition sends a set_partition_info on itself, then it
         --  is the boot partition. That means that the current partition
         --  has completed its boot.

         Booted := (Partition = PID);

      end if;

      --  A request with a null pid is a request sent by a partition
      --  during its elaboration. The partition has sent its info to the
      --  boot partition and the boot partition is supposed to send it
      --  back to it.

      if PID = Null_PID then
         PID := Partition;

         pragma Debug
           (D (D_Debug, "Reply to partition" & PID'Img &" with its info"));

         Partition_ID'Write  (Reply, PID);
         Request_Type'Output (Reply, Request);
      end if;

      Partitions.Enter;
      Info := Partitions.Get_Component (PID);

      if Request.Kind  = Get_Partition_Info then
         if Info.Status = Done then
            pragma Debug
              (D (D_Debug, "Reply to partition" & Partition'Img &
                  " on partition " & PID'Img));

            Partition_ID'Write (Reply, PID);
            Request_Type'Output
              (Reply,
               (Set_Partition_Info,
                Info.Logical_Name,
                Info.Location,
                Info.Termination,
                Info.Reconnection));
         end if;

      else
         Info.Allocated    := True;
         Info.Logical_Name := Request.Logical_Name;
         Info.Location     := Request.Location;
         Info.Termination  := Request.Termination;
         Info.Reconnection := Request.Reconnection;
         Info.Protocol     := Get_Protocol (Info.Location);
         Info.Status       := Done;

         pragma Debug
           (D (D_Debug, "Set info on partition" & PID'Img));

         Dump_Partition_Info (PID, Info);
         Partitions.Set_Component (PID, Info);

      end if;

      Partitions.Leave;

      --  Release startup from step 5. We cannot resume elaboration
      --  before this point because Boot_PID and Self_PID will not
      --  be set if the main task tries to get info on them.

      if Booted then
         Set_My_Partition_ID (Self_PID);
      end if;

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D (D_Exception, "Partition_Request_Handler: " &
                          Exception_Information (E)));
         raise;
   end Partition_Request_Handler;

   ----------------------------
   -- Partition_RPC_Receiver --
   ----------------------------

   procedure Partition_RPC_Receiver
     (Params : access Streams.Params_Stream_Type;
      Result : access Streams.Params_Stream_Type)
   is
      Receiver : RPC_Receiver;
   begin
      RPC_Receiver'Read (Params, Receiver);
      Receiver (Params, Result);
   end Partition_RPC_Receiver;

   --------------
   -- PID_Read --
   --------------

   function PID_Read (Partition : Stream_Element) return Partition_ID is
   begin
      return Partition_ID (Partition);
   end PID_Read;

   ---------------
   -- PID_Write --
   ---------------

   function PID_Write (Partition : Partition_ID) return Stream_Element is
   begin
      return Stream_Element (Partition);
   end PID_Write;

   --------------------
   -- Process_Stream --
   --------------------

   procedure Process_Stream
     (Partition  : in Partition_ID;
      Opcode     : in Any_Opcode;
      Unfiltered : in Stream_Element_Access)
   is
      Query : aliased Params_Stream_Type (Unfiltered.all'Length);
      Reply : aliased Params_Stream_Type (0);
   begin
      --  Dump the stream for debugging purpose

      pragma Debug (D (D_Dump, "Dumping stream to process"));
      pragma Debug (Dump (D_Dump, Unfiltered, Private_Debug_Key));

      To_Params_Stream_Type (Unfiltered.all, Query'Access);

      --  Depending on the opcode, dispatch to the public or internal routines.

      case Opcode is
         when Internal_Opcode =>
            Handle_Internal (Partition, Opcode, Query'Access, Reply'Access);
         when External_Opcode   =>
            Handle_External (Partition, Opcode, Query'Access, Reply'Access);
         when Invalid_Operation =>
            raise Program_Error;
      end case;

      if not Empty (Reply'Access) then
         pragma Debug (D (D_Debug, "Send reply to" & Partition'Img));
         Send (Partition, Opcode, Reply'Access);
      end if;

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D (D_Exception, "Process_Stream: " &
                          Exception_Information (E)));
         raise;
   end Process_Stream;

   -------------------------
   -- Reconnection_Policy --
   -------------------------

   function Reconnection_Policy
     (Partition : Partition_ID)
      return Reconnection_Type is
   begin
      return Get_Partition_Info (Partition) .Reconnection;
   end Reconnection_Policy;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Opcode  : in Any_Opcode;
      Handler : in Request_Handler) is
   begin
      pragma Debug
        (D (D_Debug,
            "Register request handler for opcode " & Opcode'Img));

      Handlers (Opcode) := Handler;
   end Register_Handler;

   -------------------------------------------
   -- Register_Partition_Error_Notification --
   -------------------------------------------

   procedure Register_Partition_Error_Notification
     (Callback : in RPC_Error_Notifier_Type) is
   begin
      Partition_Error_Notification := Callback;
   end Register_Partition_Error_Notification;

   ----------------------------
   -- Remote_Partition_Error --
   ----------------------------

   procedure Remote_Partition_Error
     (Partition : in Partition_ID)
   is
      Info : Partition_Info;
   begin
      pragma Debug
        (D (D_Warning,
            "Partition" & Partition'Img & " is dead"));

      if Shutdown_Policy = Shutdown_On_Any_Partition_Error then
         pragma Debug
            (D (D_Debug, "Due to the policy, I will shutdown"));
         Soft_Shutdown;
      end if;
      if Partition = Boot_PID and then
        Shutdown_Policy = Shutdown_On_Boot_Partition_Error then
         pragma Debug
           (D (D_Debug, "I cannot live without a boot partition"));
         Soft_Shutdown;
      end if;
      if Partition_Error_Notification /= null then
         Partition_Error_Notification (Partition);
      end if;

      Partitions.Enter;
      Info := Partitions.Get_Component (Partition);
      Info.Status := None;
      Partitions.Set_Component (Partition, Info);
      Partitions.Leave;
   end Remote_Partition_Error;

   ----------
   -- Send --
   ----------

   procedure Send
     (Partition : in Partition_ID;
      Opcode    : in Any_Opcode;
      Params    : access Params_Stream_Type)
   is
      Filtered : Stream_Element_Access;
      Length   : Stream_Element_Offset;
      Stream   : Stream_Element_Access;
   begin
      pragma Debug
        (D (D_Warning,
            "Send " & Opcode'Img &
            " request to partition" & Partition'Img));

      --  Filter the data according to the remote partition and the opcode

      Filtered := Filter_Outgoing (Partition, Opcode, Params);

      --  Workaround: XXXXX (Bad code generation on Solaris)
      if Filtered = null then
         raise Program_Error;
      end if;

      --  Compute the length of the packet: this is the length of the
      --  unused space that will be used by the protocol to stick its own
      --  data at the beginning + 1 for the opcode + 1 for partition id +
      --  the length of the unfiltered data. Allocate a packet of the right
      --  length.

      Length := Unused_Space + 2 + Filtered'Length;
      Stream := new Stream_Element_Array (1 .. Length);

      --  Put the opcode and the partition id at the beginning of
      --  the reserved section, then the filtered data, which can then
      --  be deallocated.

      Stream (Unused_Space + 1) := PID_Write (Self_PID);
      Stream (Unused_Space + 2) := Opcode_Write (Opcode);
      Stream (Unused_Space + 3 .. Stream'Last) := Filtered.all;
      Free (Filtered);

      --  If the data is for a remote partition, send it using the right
      --  protocol. Otherwise, make local calls (this can happen for a call
      --  on which pragma All_Calls_Remote applies) without extra space.

      if Partition = Self_PID then
         declare
            PID        : Partition_ID;
            Code       : Any_Opcode;
            Unfiltered : Stream_Element_Access;
         begin
            Analyze_Stream (PID, Code, Unfiltered, Stream, Unused_Space);
            Process_Stream (PID, Code, Unfiltered);
            Free (Unfiltered);
         exception when others =>
            Free (Unfiltered);
         end;
      else
         Send (Get_Protocol (Partition), Partition, Stream);
      end if;

      --  Free the data, even if an exception occurs

      Free (Stream);

   exception when others =>
      Free (Stream);
      Free (Filtered);
      raise;
   end Send;

   -----------------------
   -- Set_Boot_Location --
   -----------------------

   procedure Set_Boot_Location
     (Location : in Location_Type)
   is
      Info : Partition_Info :=
        (Allocated    => True,
         Location     => Location,
         Protocol     => Get_Protocol (Location),
         Logical_Name => null,
         Reconnection => Rejected_On_Restart,
         Termination  => Global_Termination,
         Status       => Done);
   begin
      if Options.Boot_Partition then
         Info.Logical_Name := Options.Partition_Name;
      end if;

      pragma Debug
        (D (D_Warning,
            "Set boot location to " & To_String (Location)));

      --  Use Last_PID to store boot partition info

      Partitions.Set_Component (Boot_PID, Info);
   end Set_Boot_Location;

   -------------------------
   -- Set_My_Partition_ID --
   -------------------------

   procedure Set_My_Partition_ID (Partition : in Partition_ID) is
   begin
      --  Set this so that exception informations contain the Partition_ID

      System.Standard_Library.Local_Partition_ID := Natural (Partition);

      --  Save partition id and signal update of this variable

      Self_PID := Partition;

      --  Boot_PID and Self_PID have been computed. Boot process is
      --  over. Initialize boot protocol back to a normal mode.

      Initialize (Get_Protocol (Boot_PID));

      Self_PID_Barrier.Signal_All (Permanent => True);
   end Set_My_Partition_ID;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
     (Shutdown : Shutdown_Type := Shutdown_On_Boot_Partition_Error)
   is
   begin
      Shutdown_Policy := Shutdown;
   end Set_Policy;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Shutdown_In_Progress := True;
      Trace.Shutdown;
      Soft_Links.Termination_Shutdown;
      Physical_Location.Shutdown;
      RPC_Shutdown;
      Free (Self_PID_Barrier);
      Delete_Termination_Sanity_File;
   end Shutdown;

   -------------------
   -- Soft_Shutdown --
   -------------------

   procedure Soft_Shutdown
   is
      PID  : Partition_ID := Null_PID;
      Info : Partition_Info;
   begin
      Shutdown_In_Progress := True;
      if Options.Boot_Partition then
         loop
            Next_Partition (PID);
            exit when PID = Null_PID;
            Info := Partitions.Get_Component (PID);

            if PID /= Self_PID
              and then Info.Status = Done
              and then Info.Termination /= Local_Termination
            then
               declare
                  Empty : aliased Params_Stream_Type (0);
               begin
                  Send (PID, Shutdown_Operation, Empty'Access);
               exception
                  when Communication_Error => null;
               end;
            end if;
         end loop;
      end if;
      Heart.Shutdown;
   end Soft_Shutdown;

   ------------------------
   -- Termination_Policy --
   ------------------------

   function Termination_Policy (Partition : Partition_ID)
     return Termination_Type is
   begin
      return Get_Partition_Info (Partition).Termination;
   end Termination_Policy;

   -------------------------------------
   -- Wait_For_Elaboration_Completion --
   -------------------------------------

   procedure Wait_For_Elaboration_Completion is
   begin
      Elaboration_Barrier.Wait;
   end Wait_For_Elaboration_Completion;

end System.Garlic.Heart;
