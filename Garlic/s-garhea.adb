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

with Ada.Exceptions;                  use Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions);
with System.Garlic.Debug;             use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Exceptions;        use System.Garlic.Exceptions;
with System.Garlic.Filters;           use System.Garlic.Filters;
with System.Garlic.Options;           use System.Garlic.Options;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Protocols;         use System.Garlic.Protocols;
with System.Garlic.Soft_Links;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Trace;             use System.Garlic.Trace;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;
with System.Standard_Library;

with System.Garlic.Linker_Options;
pragma Warnings (Off, System.Garlic.Linker_Options);

package body System.Garlic.Heart is

   use Ada.Streams;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARHEA", "(s-garhea): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Shutdown_Policy     : Shutdown_Type     := Shutdown_On_Boot_Partition_Error;
   --  These parameters control how Garlic will act in face of errors.
   --  They don't need extra protection because they should not be modified
   --  by more than one task (in fact, they should not be modified after
   --  the elaboration is terminated).

   Elaboration_Watcher : Soft_Links.Watcher_Access;
   --  This barrier will be no longer blocking when the elaboration is
   --  terminated.

   Self_PID_Watcher : Soft_Links.Watcher_Access;
   --  Block any task until Self_PID is different from Null_PID

   Handlers : array (External_Opcode) of Request_Handler;
   --  Handler callbacks table

   Handlers_Watcher : Soft_Links.Watcher_Access;

   Notify_Partition_RPC_Error : RPC_Error_Notifier_Type;
   --  Call this procedure when a partition dies.

   procedure Handle_External
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);
   --  Public operations

   procedure Handle_Internal
     (Partition : in Partition_ID;
      Opcode    : in Internal_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);
   --  Internal operations

   function Opcode_Read (Opcode : Stream_Element) return Any_Opcode;
   pragma Inline (Opcode_Read);
   function Opcode_Write (Opcode : Any_Opcode) return Stream_Element;
   pragma Inline (Opcode_Write);
   --  Read and write opcode on one byte

   Shutdown_Activation : Status_Type := None;

   -----------------------
   -- Activate_Shutdown --
   -----------------------

   procedure Activate_Shutdown is
   begin
      pragma Debug (D ("Activate partition shutdown"));

      --  Only the environment task can activate the shutdown process.

      if not Soft_Links.Is_Environment_Task then
         Soft_Links.Enter_Critical_Section;

         --  If the partition is not yet elaborated, resume main task
         --  which will execute the termination procedure. When the
         --  partition is not elaborated, it is blocked waiting for
         --  its partition id.

         if Self_PID = Null_PID then
            Soft_Links.Update (Self_PID_Watcher);

         elsif Shutdown_Activation = None then
            Shutdown_Activation := Busy;
         end if;
         Soft_Links.Leave_Critical_Section;
         pragma Debug (D ("Non env. task cannot run shutdown process"));
         return;
      end if;

      --  The environment task can activate shutdown several times but
      --  do it for real only once.

      Soft_Links.Enter_Critical_Section;
      if Shutdown_Activation = Done then
         Soft_Links.Leave_Critical_Section;
         pragma Debug (D ("Env. task is already running shutdown process"));
         return;
      end if;
      Shutdown_Activation := Done;
      Soft_Links.Leave_Critical_Section;

      pragma Debug (D ("Env. task is running shutdown process"));

      if Options.Is_Boot_Server then

         --  Global shutdown has been detected. Send shutdown operation to
         --  any online partition interested by a global termination. Ignore
         --  any communication error as this is a shutdown.

         declare
            PIDs  : Partition_List := Global_Termination_Partitions;
            Error : Error_Type;
         begin
            for I in PIDs'Range loop
               declare
                  Empty : aliased Params_Stream_Type (0);
               begin
                  if PIDs (I) /= Self_PID then
                     Send (PIDs (I), Shutdown_Operation, Empty'Access, Error);
                     Catch (Error);
                  end if;
               end;
            end loop;
         end;
      end if;

      --  Set connection hits back to normal.

      Set_Connection_Hits (Options.Def_Connection_Hits);
      pragma Debug (D ("Activate protocols shutdown"));
      Protocols.Shutdown;
      pragma Debug (D ("Activate trace shutdown"));
      Trace.Shutdown;
      pragma Debug (D ("Activate RPC shutdown"));
      Soft_Links.RPC_Shutdown;
      Soft_Links.Destroy (Self_PID_Watcher);

      pragma Debug (D ("Partition shutdown completed"));
   end Activate_Shutdown;

   --------------------
   -- Analyze_Stream --
   --------------------

   procedure Analyze_Stream
     (Partition  : in out Partition_ID;
      Protocol   : in Protocol_Access;
      Opcode     : out Any_Opcode;
      Unfiltered : out Stream_Element_Access;
      Filtered   : in  Stream_Element_Access;
      Offset     : in  Ada.Streams.Stream_Element_Offset;
      Error      : in out Error_Type)
   is
      PID   : Partition_ID;
      Code  : Any_Opcode;
      First : constant Stream_Element_Offset := Filtered'First + Offset;

   begin
      --  Dump the stream for debugging purpose

      pragma Debug (D ("Dumping stream to analyze"));
      pragma Debug (Dump (Filtered, Private_Debug_Key));

      --  Read the partition id from the stream and check that it is valid

      PID := Read (Filtered (First .. First + Partition_ID_Byte - 1));
      if not PID'Valid then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Received incorrect partition id");
      end if;

      --  Do not analyze any request until current partition completes its
      --  initialization (ie initializes its partition id). Especially do
      --  not allocate any partition id until current pid is defined.

      if Self_PID = Null_PID
        and then PID = Null_PID
      then
         Wait_For_My_Partition_ID;
      end if;

      --  Read the opcode from the stream and check that it is valid

      Code := Opcode_Read (Filtered (First + Partition_ID_Byte));
      if not Code'Valid then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Received incorrect opcode");
      elsif Code = No_Operation then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Received unexpected No_Operation opcode");
      end if;

      --  Record the current packet content in the trace file if needed
      --  except for All_Call_Remote (Protocol = null).

      if Options.Execution_Mode = Trace_Mode
        and then Protocol /= null
      then
         Trace_Received_Data (PID, Filtered, Offset);
      end if;

      --  When the partition id is unknown, allocate a new one

      if PID = Null_PID
        and then Options.Is_Boot_Mirror
      then
         Allocate_PID (PID, Null_String, Error);
         if Found (Error) then
            return;
         end if;
      end if;

      --  This partition was unknown. We save how to communicate with
      --  this partition as it may have no self location itself.

      if Partition = Null_PID then
         Set_Used_Protocol (PID, Protocol);
      end if;

      if PID /= Null_PID then
         pragma Debug
           (D ("Received request " & Code'Img & " from partition" & PID'Img));

         --  Unfilter the data and put it in a stream

         Filter_Incoming
           (PID, Code, Filtered,
            Offset + Partition_ID_Byte + 1,
            Unfiltered, Error);
      end if;

      Partition  := PID;
      Opcode     := Code;
   end Analyze_Stream;

   --------------------------
   -- Complete_Elaboration --
   --------------------------

   procedure Complete_Elaboration is
   begin
      pragma Debug (D ("Complete termination"));

      Soft_Links.Update (Elaboration_Watcher);
   end Complete_Elaboration;

   -------------------------
   -- Get_My_Partition_ID --
   -------------------------

   procedure Get_My_Partition_ID
     (PID   : out Partition_ID;
      Error : in out Error_Type) is
   begin
      PID := Self_PID;

      --  The current partition has a fake partition id and partition info.

      if Self_PID = Null_PID then

         pragma Debug (D ("Looking for my partition id"));

         if Options.Is_Boot_Server then
            Self_PID := Boot_PID;
            Set_My_Partition_ID (Error);
            if Found (Error) then
               return;
            end if;

         else
            Send_Partition_Definition
              (Partition      => Null_PID,
               Partition_Name => Options.Partition_Name,
               Is_Active_Part => True,
               Net_Locations  => new String'(Merge_String
                                             (Options.Self_Location)),
               Mem_Locations  => new String'(Merge_String
                                             (Options.Data_Location)),
               Termination    => Options.Termination,
               Reconnection   => Options.Reconnection,
               Is_Pure_Client => Options.Is_Pure_Client,
               Is_Boot_Mirror => Options.Is_Boot_Mirror,
               Error          => Error);
            if Found (Error) then
               return;
            end if;

            Wait_For_My_Partition_ID;
            if Self_PID = Null_PID then
               Throw (Error, "boot partition failed to return partition id");
            end if;
         end if;
      end if;

      PID := Self_PID;
   end Get_My_Partition_ID;

   ------------------------
   -- Handle_Any_Request --
   ------------------------

   procedure Handle_Any_Request
     (Partition : in Partition_ID;
      Opcode    : in Any_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
      procedure Reset_Stamp;
      --  Set stamp to no stamp when current stamp differs from no
      --  stamp.

      procedure Reset_Stamp is
         Stamp : Stamp_Type := Soft_Links.Get_Stamp;
      begin
         if Stamp /= No_Stamp then
            Soft_Links.Set_Stamp (No_Stamp);
         end if;
      end Reset_Stamp;

   begin

      --  Depending on the opcode, dispatch to the public or internal
      --  routines.

      case Opcode is
         when Internal_Opcode =>
            Handle_Internal (Partition, Opcode, Query, Reply, Error);
         when External_Opcode   =>
            Handle_External (Partition, Opcode, Query, Reply, Error);
         when Invalid_Operation =>
            Throw (Error, "Handle_Any_Request: invalid operation");
      end case;

      pragma Debug (Reset_Stamp);
   end Handle_Any_Request;

   ---------------------
   -- Handle_External --
   ---------------------

   procedure Handle_External
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
      Version : Version_Id;

   begin
      pragma Assert (Self_PID /= Null_PID);

      if Handlers (Opcode) = null then
         loop
            Soft_Links.Lookup (Handlers_Watcher, Version);
            exit when Handlers (Opcode) /= null;
            Soft_Links.Differ (Handlers_Watcher, Version);
         end loop;
      end if;

      Handlers (Opcode) (Partition, Opcode, Query, Reply, Error);

   exception when E : others =>
      Throw (Error, "Handle_External: " & Exception_Information (E));
      --  XXXXX: Is this really needed?
   end Handle_External;

   ---------------------
   -- Handle_Internal --
   ---------------------

   procedure Handle_Internal
     (Partition : in Partition_ID;
      Opcode    : in Internal_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
   begin
      case Opcode is
         when No_Operation =>
            null;

         when Partition_Operation =>
            Handle_Partition_Request (Partition, Query, Reply, Error);

         when Shutdown_Operation =>
            Activate_Shutdown;

      end case;
   end Handle_Internal;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Soft_Links.Create (Elaboration_Watcher, No_Version);
      Soft_Links.Create (Self_PID_Watcher, No_Version);
      Soft_Links.Create (Handlers_Watcher, No_Version);
   end Initialize;

   ----------------------------
   -- Notify_Partition_Error --
   ----------------------------

   procedure Notify_Partition_Error
     (Partition : in Partition_ID)
   is
   begin
      if Shutdown_Activated then
         return;
      end if;

      pragma Debug (D ("Partition" & Partition'Img & " is dead"));

      Invalidate_Partition (Partition);

      if Shutdown_Policy = Shutdown_On_Any_Partition_Error then
         pragma Debug (D ("Due to the policy, I will shutdown"));
         Activate_Shutdown;
      end if;

      if Partition = Boot_PID
        and then Shutdown_Policy = Shutdown_On_Boot_Partition_Error
      then
         pragma Debug (D ("I cannot live without a boot partition"));
         Activate_Shutdown;
      end if;

      --  First, shutdown Garlic then shutdown upper layers.

      if Notify_Partition_RPC_Error /= null then
         Notify_Partition_RPC_Error (Partition);
      end if;
   end Notify_Partition_Error;

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

   --------------------
   -- Process_Stream --
   --------------------

   procedure Process_Stream
     (Partition  : in Partition_ID;
      Opcode     : in Any_Opcode;
      Unfiltered : in Stream_Element_Access;
      Error      : in out Error_Type)
   is
      Query : aliased Params_Stream_Type (Unfiltered.all'Length);
      Reply : aliased Params_Stream_Type (0);
   begin

      --  Dump the stream for debugging purpose

      pragma Debug (D ("Dumping stream to process"));
      pragma Debug (Dump (Unfiltered, Private_Debug_Key));

      To_Params_Stream_Type (Unfiltered.all, Query'Access);

      Handle_Any_Request
        (Partition, Opcode, Query'Access, Reply'Access, Error);
      Deallocate (Query);

      if not Found (Error)
        and then not Empty (Reply'Access)
      then
         pragma Debug (D ("Send reply to" & Partition'Img));
         Send (Partition, Opcode, Reply'Access, Error);
      end if;

   exception when E : others =>
      Throw (Error, "Process_Stream: " & Exception_Information (E));
   end Process_Stream;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Opcode  : in Any_Opcode;
      Handler : in Request_Handler) is
   begin
      pragma Debug (D ("Register request handler for opcode " & Opcode'Img));

      Handlers (Opcode) := Handler;
      Soft_Links.Update (Handlers_Watcher);
   end Register_Handler;

   ---------------------------------
   -- Register_RPC_Error_Notifier --
   ---------------------------------

   procedure Register_RPC_Error_Notifier
     (Callback : in RPC_Error_Notifier_Type) is
   begin
      Notify_Partition_RPC_Error := Callback;
   end Register_RPC_Error_Notifier;

   ----------
   -- Send --
   ----------

   procedure Send
     (Partition : in Partition_ID;
      Opcode    : in Any_Opcode;
      Params    : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
      Filtered : Stream_Element_Access;
      Length   : Stream_Element_Offset;
      Stream   : Stream_Element_Access;
      Protocol : Protocol_Access;
      Index    : Stream_Element_Offset;

   begin

      pragma Debug
        (D ("Send " & Opcode'Img & " request to partition" & Partition'Img));

      --  Filter the data according to the remote partition and the opcode

      Filter_Outgoing (Partition, Opcode, Params, Filtered, Error);

      if Found (Error) then
         Notify_Partition_Error (Partition);
         return;
      end if;

      --  Workaround: XXXXX (Bad code generation on Solaris)
      if Filtered = null then
         raise Program_Error;
      end if;

      --  Compute the length of the packet: this is the length of the
      --  unused space that will be used by the protocol to stick its own
      --  data at the beginning + 1 for the opcode + 1 for partition id +
      --  the length of the filtered data. Allocate a packet of the right
      --  length.

      Length := Unused_Space + Partition_ID_Byte + 1 + Filtered'Length;
      Stream := new Stream_Element_Array (1 .. Length);

      --  Put the opcode and the partition id at the beginning of
      --  the reserved section, then the filtered data, which can then
      --  be deallocated.

      Index := Unused_Space + 1;
      Stream (Index .. Index + Partition_ID_Byte - 1) := Write (Self_PID);
      Index := Index + Partition_ID_Byte;
      Stream (Index) := Opcode_Write (Opcode);
      Index := Index + 1;
      Stream (Index .. Length) := Filtered.all;
      Free (Filtered);

      --  If the data is for a remote partition, send it using the right
      --  protocol. Otherwise, make local calls (this can happen for a call
      --  on which pragma All_Calls_Remote applies) without extra space.

      if Partition = Self_PID then
         declare
            PID        : Partition_ID;
            Code       : Any_Opcode;
            Unfiltered : Stream_Element_Access;
            Unused     : constant Stream_Element_Count := Unused_Space;
         begin
            Analyze_Stream
              (PID, null, Code, Unfiltered, Stream, Unused, Error);
            if not Found (Error) then
               Process_Stream (PID, Code, Unfiltered, Error);
            end if;
            Free (Unfiltered);
         end;
      else
         Get_Protocol (Partition, Protocol, Error);
         if not Found (Error) then
            Send (Protocol, Partition, Stream, Error);
            if Found (Error) then
               Notify_Partition_Error (Partition);
            end if;
         end if;
      end if;

      --  If this packet has anything to do with a remote call, record the
      --  fact that we sent a message.

      if Opcode = Remote_Call then
         Soft_Links.Activity_Detected;
      end if;

      Free (Stream);
   end Send;

   ----------------------
   -- Send_Boot_Server --
   ----------------------

   procedure Send_Boot_Server
     (Opcode : in Any_Opcode;
      Params : access Streams.Params_Stream_Type;
      Error  : out Error_Type)
   is
      Params_Copy : Params_Stream_Type (Params.Initial_Size);
      Boot_Server : Partition_ID := Boot_PID;
   begin
      --  Preserve Params because we may have to send it several times

      Copy (Params.all, Params_Copy);

      --  While shutdown is not in progress, we have a boot partition to
      --  talk to.

      while not Shutdown_Activated loop
         Send (Boot_PID, Opcode, Params, Error);

         --  Exit when the message has been sent correctly. When an
         --  error has occurred, exit when the boot server has not
         --  changed after its invalidation.

         exit when not Found (Error)
           or else Boot_PID = Boot_Server;

         --  Since there was an error, copy back Params_Copy into Params,
         --  after removing the junk that may still be in Params.

         Deallocate (Params.all);
         Copy (Params_Copy, Params.all);
      end loop;

      --  Either the shutdown is in progress, or we have sent the message
      --  succesfully. In any case, we can get rid of the copy now.

      Deallocate (Params_Copy);
   end Send_Boot_Server;

   -------------------------
   -- Set_My_Partition_ID --
   -------------------------

   procedure Set_My_Partition_ID (Error : in out Error_Type)
   is
      Protocol : Protocol_Access;
   begin

      --  Set this so that exception informations contain the Partition_ID

      System.Standard_Library.Local_Partition_ID := Natural (Self_PID);

      --  Boot_PID and Self_PID have been computed. Boot process is
      --  over. Initialize boot protocol back to a normal mode.

      Get_Protocol (Boot_PID, Protocol, Error);
      if Found (Error) then
         return;
      end if;

      Soft_Links.Update (Self_PID_Watcher);

      pragma Debug (Dump_Partition_Table (Private_Debug_Key));
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

   ------------------------
   -- Shutdown_Activated --
   ------------------------

   function Shutdown_Activated return Boolean is
   begin
      return Shutdown_Activation /= None;
   end Shutdown_Activated;

   -------------------------------------
   -- Wait_For_Elaboration_Completion --
   -------------------------------------

   procedure Wait_For_Elaboration_Completion is
   begin
      Soft_Links.Differ (Elaboration_Watcher, No_Version);
   end Wait_For_Elaboration_Completion;

   ------------------------------
   -- Wait_For_My_Partition_ID --
   ------------------------------

   procedure Wait_For_My_Partition_ID is
   begin
      Soft_Links.Differ (Self_PID_Watcher, No_Version);
      pragma Debug (Dump_Partition_Table (Private_Debug_Key));
      if Self_PID = Null_PID then
         Activate_Shutdown;
      end if;
   end Wait_For_My_Partition_ID;

end System.Garlic.Heart;
