------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . H E A R T                   --
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

with Ada.Exceptions;                  use Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions);
with Ada.Unchecked_Conversion;
with Interfaces;
with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Filters;           use System.Garlic.Filters;
with System.Garlic.Group;             use System.Garlic.Group;
with System.Garlic.Name_Table;        use System.Garlic.Name_Table;
with System.Garlic.Options;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Soft_Links;        use System.Garlic.Soft_Links;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Trace;             use System.Garlic.Trace;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;
with System.Standard_Library;

with System.Garlic.Linker_Options;
pragma Warnings (Off, System.Garlic.Linker_Options);

package body System.Garlic.Heart is

   use Ada.Streams;

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

   procedure Handle_Partition_Request
     (Partition : in Partition_ID;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type);
   --  Handle Partition_Service operations

   function Opcode_Read (Opcode : Stream_Element) return Any_Opcode;
   pragma Inline (Opcode_Read);
   function Opcode_Write (Opcode : Any_Opcode) return Stream_Element;
   pragma Inline (Opcode_Write);
   --  Read and write opcode on one byte

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

   function Convert is
      new Ada.Unchecked_Conversion (System.Address, RPC_Receiver);

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
   -- Get_My_Partition_ID --
   -------------------------

   function Get_My_Partition_ID return Partition_ID
   is
      Boot_Partition : Partition_Info;
      Boot_Request   : Request_Type (New_Partition_Info);
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
            --  We will send a New_Partition_Info request to the boot
            --  partition. This is step 1. This will cause a dialog to be
            --  established and a new Partition_ID to be allocated. The
            --  partition location will be registered into the boot
            --  partition's repository. This is step 2. The boot partition
            --  sends this partition info back to the partition. This is
            --  step 3. At this point, Self_PID is known but startup is
            --  kept blocking. The partition sends a request to get
            --  partition info on the boot partition. This is step 4. The
            --  boot partition replies on itself to this partition. This is
            --  step 5. At this point, Boot_PID is known. Therefore,
            --  startup can complete and Self_PID_Barrier is open. This is
            --  step 6. When a partition is a potential boot server then it
            --  also sends an add partition info request to the boot
            --  partition. This is step 7. Then an all partition info
            --  request will be broadcast. This is step 8.

            Boot_Request.Partition := Null_PID;
            Boot_Request.Info :=
              (Allocated     => True,
               Location      => Get_Self_Location,
               Protocol      => null,
               Logical_Name  => Options.Partition_Name,
               Termination   => Options.Termination,
               Reconnection  => Options.Reconnection,
               Light_RTS     => Can_Have_A_Light_Runtime,
               Boot_Server   => False,
               Status        => Done);

            --  This is step 1.

            declare
               Query : aliased Params_Stream_Type (0);
            begin
               Request_Type'Output (Query'Access, Boot_Request);
               Send (Boot_PID, Partition_Operation, Query'Access);
            end;

            --  This is step 6.

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
   -- Handle_Any_Request --
   ------------------------

   procedure Handle_Any_Request
     (Partition : in Partition_ID;
      Opcode    : in Any_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type) is
   begin
      --  Depending on the opcode, dispatch to the public or internal routines.

      case Opcode is
         when Internal_Opcode =>
            Handle_Internal (Partition, Opcode, Query, Reply);
         when External_Opcode   =>
            Handle_External (Partition, Opcode, Query, Reply);
         when Invalid_Operation =>
            raise Program_Error;
      end case;
   end Handle_Any_Request;

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
            Handle_Partition_Request (Partition, Query, Reply);

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

   ------------------------------
   -- Handle_Partition_Request --
   ------------------------------

   procedure Handle_Partition_Request
     (Partition : in Partition_ID;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type)
   is
      Request : Request_Type;
      PID     : Partition_ID;
      Booted  : Boolean := False;
      Info    : Partition_Info;
      To_All  : aliased Params_Stream_Type (0);
   begin
      Request := Request_Type'Input (Query);

      pragma Debug
        (D (D_Warning,
            "Receive from partition" & Partition'Img &
            " request " & Request.Kind'Img));

      Partitions.Enter;
      case Request.Kind is

         when Add_Partition_Info =>
            pragma Debug (D (D_Debug, "List all partitions"));

            --  Add this partition in the list of potential boot servers.

            Info := Partitions.Get_Component (Partition);
            Info.Boot_Server := True;
            Partitions.Set_Component (Partition, Info);

            --  Broadcast to any partition in the group. This is step 8.

            Request_Type'Output
              (To_All'Access, Request_Type'(Kind => All_Partition_Info));
            for P in Partitions.Table'Range loop
               if Partitions.Table (P).Allocated then
                  Info := Partitions.Get_Component (P);
                  Boolean'Write (To_All'Access, True);
                  Partition_ID'Write (To_All'Access, P);
                  Partition_Info'Write (To_All'Access, Info);
               end if;
            end loop;
            Boolean'Write (To_All'Access, False);

         when All_Partition_Info =>
            pragma Debug (D (D_Debug, "List all partitions"));

            while Boolean'Input (Query) loop
               Partition_ID'Read (Query, PID);
               Partition_Info'Read (Query, Info);

               if Options.Execution_Mode = Replay_Mode then
                  Info.Location := To_Location ("replay://");
               end if;
               Info.Protocol := Get_Protocol (Info.Location);

               Partitions.Set_Component (PID, Info);
               Dump_Partition_Info (PID, Info);
            end loop;

         when Get_Partition_Info =>
            pragma Debug
              (D (D_Debug, "Reply to partition" & Partition'Img &
                  " on partition " & Request.Partition'Img));

            Request_Type'Output
              (Reply,
               (Set_Partition_Info,
                Request.Partition,
                Partitions.Get_Component (Request.Partition)));

         when New_Partition_Info =>
            pragma Debug
              (D (D_Debug, "Set info on partition" & Partition'Img));

            --  This is step 2 for boot partition.

            if Options.Execution_Mode = Replay_Mode then
               Request.Info.Location := To_Location ("replay://");
            end if;
            Request.Info.Protocol := Get_Protocol (Request.Info.Location);

            Dump_Partition_Info (Partition, Request.Info);
            Partitions.Set_Component (Partition, Request.Info);

            --  Reply to a partition declaration with a set partition info
            --  request. This is step 3 for boot partition.

            Request_Type'Output
              (Reply, (Set_Partition_Info, Partition, Request.Info));

         when Set_Partition_Info =>
            pragma Debug
              (D (D_Debug, "Set info on partition" & Request.Partition'Img));

            if Options.Execution_Mode = Replay_Mode then
               Request.Info.Location := To_Location ("replay://");
            end if;
            Request.Info.Protocol := Get_Protocol (Request.Info.Location);

            Dump_Partition_Info (Request.Partition, Request.Info);
            Partitions.Set_Component (Request.Partition, Request.Info);

            --  This is a set partition info request issued from a new
            --  partition info request. This way we get the partition id of
            --  the current partition. This is step 3 for booting
            --  partition.

            if Self_PID = Null_PID then
               Self_PID := Request.Partition;

               if Boot_PID /= Partition then
                  Info := Partitions.Get_Component (Boot_PID);
                  Partitions.Set_Component (Partition, Info);
                  Info.Allocated := False;
                  Info.Status    := None;
                  Partitions.Set_Component (Boot_PID, Info);
                  Boot_PID := Partition;
               end if;

               --  This is step 4 for booting partition.

               Request_Type'Output (Reply, (Get_Partition_Info, Partition));
            end if;

            --  When a partition sends info on itself, this partition
            --  is the boot partition. This is the last step before
            --  elaboration completion. This is step 5.

            if Request.Partition = Partition then
               Booted := True;

               Info := Partitions.Get_Component (Self_PID);
               Info.Boot_Server := not Info.Light_RTS;
               Partitions.Set_Component (Self_PID, Info);

               --  If this partition wants to join the boot server group,
               --  send an add partition info request. This is step 7.

               if Info.Boot_Server then
                  Request_Type'Output
                    (Reply, Request_Type'(Kind => Add_Partition_Info));
               end if;
            end if;

      end case;

      Partitions.Leave;

      if not Empty (To_All'Access) then
         Broadcast (Partition_Operation, To_All'Access);
      end if;

      --  Release startup from step 6. We cannot resume elaboration
      --  before this point because Boot_PID and Self_PID will not
      --  be set if the main task tries to get info on them.

      if Booted then
         Set_My_Partition_ID (Self_PID);
      end if;

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D (D_Exception, "Handle_Partition_Request: " &
                          Exception_Information (E)));
         raise;
   end Handle_Partition_Request;

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
      Increment : in Boolean := True;
      Allocated : in Boolean := True)
   is
      Next : Partition_ID;
      From : Partition_ID := Partition;
   begin
      if Increment then
         if Partition = Null_PID then
            From := Partitions.Table'Last;
         end if;
      else
         if Partition = Null_PID then
            From := Partitions.Table'First;
         end if;
      end if;
      Next := From;

      Partitions.Enter;
      loop
         if Increment then
            if Next = Partitions.Table'Last then
               Next := Partitions.Table'First;
            else
               Next := Next + 1;
            end if;
         else
            if Next = Partitions.Table'First then
               Next := Partitions.Table'Last;
            else
               Next := Next - 1;
            end if;
         end if;
         if Next = From
           or else Partitions.Table (Next).Allocated = Allocated then
            Partition := Next;
            exit;
         end if;
      end loop;
      Partitions.Leave;

      if Increment then
         pragma Debug (D (D_Debug,
                          "Next partition of " & Partition'Img &
                          " is partition" & Next'Img));
         null;
      else
         pragma Debug (D (D_Debug,
                          "Prev partition of " & Partition'Img &
                          " is partition" & Next'Img));
         null;
      end if;
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

   ----------------------------
   -- Partition_RPC_Receiver --
   ----------------------------

   procedure Partition_RPC_Receiver
     (Params : access Streams.Params_Stream_Type;
      Result : access Streams.Params_Stream_Type)
   is
      Receiver : constant RPC_Receiver :=
        Convert (System.Address (Interfaces.Unsigned_64'Input (Params)));
   begin
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

      Handle_Any_Request (Partition, Opcode, Query'Access, Reply'Access);

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
         Light_RTS    => False,
         Boot_Server  => True,
         Status       => Done);
   begin
      if Options.Boot_Partition then
         Info.Logical_Name := Options.Partition_Name;
      end if;

      pragma Debug
        (D (D_Warning,
            "Set boot location to " & To_String (Info.Location)));

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
      PID  : Partition_ID;
      Next : Partition_ID;
      Info : Partition_Info;
   begin
      Shutdown_In_Progress := True;
      if Options.Boot_Partition then
         Next := Null_PID;
         loop
            PID := Next;
            Next_Partition (Next);
            exit when Next <= PID;
            Info := Partitions.Get_Component (Next);

            if Next /= Self_PID
              and then Info.Status = Done
              and then Info.Termination /= Local_Termination
            then
               declare
                  Empty : aliased Params_Stream_Type (0);
               begin
                  Send (Next, Shutdown_Operation, Empty'Access);
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
