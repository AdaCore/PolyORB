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

with Ada.Exceptions;                  use Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions);
with Ada.Unchecked_Conversion;
with Interfaces;
with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Filters;           use System.Garlic.Filters;
with System.Garlic.Options;           use System.Garlic.Options;
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

   Elaboration_Barrier : Barrier_Type;
   --  This barrier will be no longer blocking when the elaboration is
   --  terminated.

   Self_PID_Barrier : Barrier_Type;
   --  Block any task until Self_PID is different from Null_PID

   Handlers : array (External_Opcode) of Request_Handler;
   --  Handler callbacks table

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

      PID := PID_Read (Filtered (First));
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

      --  Record the current packet content in the trace file if needed

      if Options.Execution_Mode = Trace_Mode then
         Trace_Data (PID, Filtered, Offset);
      end if;

      --  When the partition id is unknown, allocate a new one

      if PID = Null_PID
        and then Options.Is_Boot_Mirror
      then
         Allocate_PID (PID, Error);
         if Found (Error) then
            return;
         end if;
      end if;

      if PID /= Null_PID then
         pragma Debug
           (D ("Received request " & Code'Img & " from partition" & PID'Img));

         --  Unfilter the data and put it in a stream

         Filter_Incoming (PID, Code, Filtered, Offset + 2, Unfiltered, Error);
      end if;

      Partition  := PID;
      Opcode     := Code;
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

      if Options.Is_Boot_Server then
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
      pragma Debug (D ("Complete termination"));

      Signal_All (Elaboration_Barrier);
   end Complete_Elaboration;

   -------------------------
   -- Get_My_Partition_ID --
   -------------------------

   procedure Get_My_Partition_ID
     (PID   : out Partition_ID;
      Error : in out Error_Type)
   is
   begin

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
            Send_Boot_Request (Get_Self_Location, Error);
            if Found (Error) then
               return;
            end if;
            Wait_For_My_Partition_ID;
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
      Handle : Request_Handler;
   begin
      pragma Assert (Self_PID /= Null_PID);

      if Opcode /= Shutdown_Service then
         Soft_Links.Activity_Detected;
      end if;

      Handle := Handlers (Opcode);
      pragma Assert (Handle /= null);

      Handle (Partition, Opcode, Query, Reply, Error);

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
      Soft_Links.Activity_Detected;

      case Opcode is
         when No_Operation =>
            null;

         when Partition_Operation =>
            Handle_Partition_Request (Partition, Query, Reply, Error);

         when Shutdown_Operation =>
            Heart.Shutdown;

      end case;
   end Handle_Internal;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Elaboration_Barrier);
      Create (Self_PID_Barrier);
   end Initialize;

   ----------------------------
   -- Notify_Partition_Error --
   ----------------------------

   procedure Notify_Partition_Error
     (Partition : in Partition_ID)
   is
   begin
      if Shutdown_In_Progress then
         return;
      end if;

      pragma Debug (D ("Partition" & Partition'Img & " is dead"));

      Invalidate_Partition (Partition);

      if Shutdown_Policy = Shutdown_On_Any_Partition_Error then
         pragma Debug (D ("Due to the policy, I will shutdown"));
         Soft_Shutdown;
      end if;

      if Partition = Boot_PID and then
        Shutdown_Policy = Shutdown_On_Boot_Partition_Error then
         pragma Debug (D ("I cannot live without a boot partition"));
         Soft_Shutdown;
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

      if not Empty (Reply'Access) then
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
            Unused     : constant Stream_Element_Count := Unused_Space;
         begin
            Analyze_Stream (PID, Code, Unfiltered, Stream, Unused, Error);
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

      while not Shutdown_In_Progress loop
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

      --  Set boot protocol back to normal mode

      Initialize (Protocol, null, null, False, Error);

      --  Resume any task blocked on Wait_For_My_Partition_ID

      if not Found (Error) then
         Signal_All (Self_PID_Barrier);
      end if;
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

      Set_Connection_Hits (0);
      Trace.Shutdown;
      Soft_Links.Termination_Shutdown;
      Physical_Location.Shutdown;
      RPC_Shutdown;

      Destroy (Self_PID_Barrier);
   end Shutdown;

   -------------------
   -- Soft_Shutdown --
   -------------------

   procedure Soft_Shutdown
   is
   begin
      Shutdown_In_Progress := True;
      if Options.Is_Boot_Server then

         --  Global shutdown has been detected. Send shutdown operation to
         --  any alive partition interested by a global termination. Ignore
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
      Heart.Shutdown;
   end Soft_Shutdown;

   -------------------------------------
   -- Wait_For_Elaboration_Completion --
   -------------------------------------

   procedure Wait_For_Elaboration_Completion is
   begin
      Wait (Elaboration_Barrier);
   end Wait_For_Elaboration_Completion;

   ------------------------------
   -- Wait_For_My_Partition_ID --
   ------------------------------

   procedure Wait_For_My_Partition_ID is
   begin
      Wait (Self_PID_Barrier);
      if Self_PID = Null_PID then
         Soft_Shutdown;
      end if;
   end Wait_For_My_Partition_ID;

end System.Garlic.Heart;
