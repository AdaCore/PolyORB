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
with Ada.Unchecked_Deallocation;
with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Filters;           use System.Garlic.Filters;
with System.Garlic.Name_Server;       use System.Garlic.Name_Server;
with System.Garlic.Name_Table;        use System.Garlic.Name_Table;
with System.Garlic.Options;
with System.Garlic.PID_Server;        use System.Garlic.PID_Server;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;
with System.Garlic.Soft_Links;        use System.Garlic.Soft_Links;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Trace;             use System.Garlic.Trace;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;
with System.Standard_Library;

with System.Garlic.Linker_Options;
pragma Warnings (Off, System.Garlic.Linker_Options);

package body System.Garlic.Heart is

   use Ada.Streams, System.Garlic.Types, System.Garlic.Utils;

   --  The protocol used is:
   --
   --   - <QUERY_PUBLIC_DATA> <PARTITION_ID>
   --   - <QUERY_PUBLIC_DATA_ANSWER> <PARTITION_ID> <PUBLIC_DATA>
   --   - <SET_PUBLIC_DATA> <PUBLIC_DATA>
   --   - <SHUTDOWN>

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

   Shutdown_In_Progress : Boolean := False;
   pragma Atomic (Shutdown_In_Progress);

   function Opcode_Read (Operation : Stream_Element) return Opcode;
   pragma Inline (Opcode_Read);
   function Opcode_Write (Operation : Opcode) return Stream_Element;
   pragma Inline (Opcode_Write);
   --  Read and write opcode on one byte

   protected type Local_Partition_ID_Type is
      entry Get (Partition : out Partition_ID);
      procedure Set (Partition : in Partition_ID);
      function Get_Immediately return Partition_ID;
   private
      In_Progress     : Boolean := False;
   end Local_Partition_ID_Type;
   --  Local partition ID. Needs comments ???

   type Local_Partition_ID_Access is access Local_Partition_ID_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Local_Partition_ID_Type,
                                      Local_Partition_ID_Access);

   Local_Partition_ID : Local_Partition_ID_Access :=
     new Local_Partition_ID_Type;
   --  Kludge to raise Program_Error at deallocation time. Should be cleaned
   --  up in the future ???

   Local_Partition : Partition_ID := Null_Partition_ID;
   --  Fast version for direct access

   Is_Boot : Boolean;
   --  Set to True if we are on the boot partition

   My_Public_Data : Public_Data;
   --  Public data of the local partition

   type Allocated_Map is array (Partition_ID range <>) of Boolean;
   --  Type of allocated partitions

   procedure Handle_Internal
     (Partition : in Partition_ID;
      Operation : in Internal_Opcode;
      Params    : access Params_Stream_Type);
   --  Internal operation

   procedure Handle_Public
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type);
   --  Public operation

   type Receiver_Array is array (Public_Opcode) of Public_Receiver;

   protected type Receiver_Map_Type is
      procedure Set (Operation : in Opcode; Receiver : in Public_Receiver);
      entry Get (Opcode) (Receiver : out Public_Receiver);
   private
      Receiver_Data : Receiver_Array;
   end Receiver_Map_Type;
   --  List of receivers for every opcode

   type Receiver_Map_Access is access Receiver_Map_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Receiver_Map_Type,
                                      Receiver_Map_Access);
   Receiver_Map : Receiver_Map_Access := new Receiver_Map_Type;
   --  Same kludge as above ???

   procedure Shutdown;
   --  Generates a local shutdown

   Partition_Error_Notification : RPC_Error_Notifier_Type;
   --  Call this procedure when a partition dies

   procedure Partition_RPC_Receiver
     (Params : access Streams.Params_Stream_Type;
      Result : access Streams.Params_Stream_Type);
   --  Global RPC receiver

   procedure Dump_Partition_Information (Partition : in Partition_ID;
                                         Public    : in Public_Data);
   --  Dump a summary of all the information we have on a partition

   --------------------------
   -- Add_New_Partition_ID --
   --------------------------

   procedure Add_New_Partition_ID (Partition : in Partition_ID) is
      Empty : aliased Params_Stream_Type (0);
   begin
      --  Send a NOP to establish the connection

      pragma Debug (D (D_Debug, "Sending a No_Operation"));
      Send (Partition, No_Operation, Empty'Access);
   end Add_New_Partition_ID;

   ------------------------
   -- Blocking_Partition --
   ------------------------

   function Blocking_Partition (Partition : Partition_ID) return Boolean is
      Public : constant Public_Data := Get_Public_Data (Partition);
   begin
      return Public.Termination = Local_Termination
        and then Public.Alive;
   end Blocking_Partition;

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

      if Is_Boot_Partition then
         return False;
      end if;

      --  There is no reason not to have a light runtime

      return True;
   end Can_Have_A_Light_Runtime;

   --------------------------------
   -- Dump_Partition_Information --
   --------------------------------

   procedure Dump_Partition_Information
     (Partition : in Partition_ID;
      Public    : in Public_Data)
   is
   begin
      D (D_Debug, "Information on partition" & Partition_ID'Image (Partition));
      D (D_Debug, "  Name:        " & Get (Public.Name));
      D (D_Debug, "  Location:    " & To_String (Public.Location));
      D (D_Debug, "  Termination: " & Public.Termination'Img);
      D (D_Debug, "  Reconnection: " & Public.Reconnection'Img);
      D (D_Debug, "  Alive:       " & Boolean'Image (Public.Alive));
   end Dump_Partition_Information;

   --------------------------
   -- Complete_Elaboration --
   --------------------------

   procedure Complete_Elaboration is
   begin
      pragma Debug
        (D (D_Elaborate, "Signaling that elaboration is terminated"));
      Elaboration_Barrier.Signal_All (Permanent => True);
   end Complete_Elaboration;

   ---------------------
   -- Get_My_Location --
   ---------------------

   function Get_My_Location return Location_Type is
   begin
      return My_Public_Data.Location;
   end Get_My_Location;

   -------------------------
   -- Get_My_Partition_ID --
   -------------------------

   function Get_My_Partition_ID return Partition_ID is
      Partition : Partition_ID;

   begin
      Local_Partition_ID.Get (Partition);

      if Partition = Null_Partition_ID then
         declare
            Params : aliased Params_Stream_Type (0);
         begin
            --  We will send a Set_Public_Data to the server. This will cause
            --  a dialog to be established and a new Partition_ID to be
            --  allocated, and our location will be registered into
            --  the server's base.

            pragma Assert (My_Public_Data.Alive);
            Public_Data'Write (Params'Access, My_Public_Data);
            Send (Server_Partition_ID, Set_Public_Data, Params'Access);
            Local_Partition_ID.Get (Partition);
         end;
      end if;

      return Partition;
   exception
      when E : others =>
         pragma Debug (D (D_Debug, Exception_Information (E)));
         raise;
   end Get_My_Partition_ID;

   -------------------------------------
   -- Get_My_Partition_ID_Immediately --
   -------------------------------------

   function Get_My_Partition_ID_Immediately return Partition_ID is
   begin
      if Local_Partition = Null_Partition_ID then
         return Local_Partition_ID.Get_Immediately;
      else
         return Local_Partition;
      end if;
   end Get_My_Partition_ID_Immediately;

   ---------------------
   -- Handle_Internal --
   ---------------------

   procedure Handle_Internal
     (Partition : in Partition_ID;
      Operation : in Internal_Opcode;
      Params    : access Params_Stream_Type) is
      Data  : Partition_Data;
      Asked : Partition_ID;

   begin

      Soft_Links.Activity_Detected;

      case Operation is

         when No_Operation => null;

         when Set_Public_Data =>

            pragma Debug
              (D (D_Server,
                  "Receive information on partition" & Partition'Img));

            Public_Data'Read (Params, Data.Public);
            if Options.Execution_Mode = Replay_Mode then
               Data.Public.Location := To_Location ("replay://");
            end if;

            pragma Debug (Dump_Partition_Information (Partition, Data.Public));
            Data.Known       := True;
            Data.Queried     := False;

            pragma Debug
              (D (D_Server,
                  "Receive that partition" & Partition'Img &
                  " is named " & Get (Data.Public.Name) &
                  " and is located at " & To_String (Data.Public.Location)));

            Partition_Map.Set_Data (Partition, Data);

         when Query_Public_Data =>
            declare
               Answer : aliased Params_Stream_Type (0);

            begin
               Partition_ID'Read (Params, Asked);
               if not Asked'Valid then
                  pragma Debug (D (D_Debug, "Invalid partition ID"));
                  raise Constraint_Error;
               end if;

               pragma Debug
                 (D (D_Server,
                     "Partition" & Partition'Img &
                     " is looking for information on partition" & Asked'Img));

               Data := Get_Partition_Data (Asked);

               pragma Debug
                 (D (D_Server,
                     "Return information on partition" & Asked'Img));
               pragma Debug (Dump_Partition_Information (Asked, Data.Public));

               Partition_ID'Write (Answer'Access, Asked);
               Public_Data'Write (Answer'Access, Data.Public);
               Send (Partition, Query_Public_Data_Answer, Answer'Access);
            end;

         when Query_Public_Data_Answer =>

            Partition_ID'Read (Params, Asked);
            if not Asked'Valid then
               pragma Debug (D (D_Debug, "Invalid partition ID"));
               raise Constraint_Error;
            end if;

            pragma Debug
              (D (D_Garlic,
                  "Receive query for information on partition" & Asked'Img));

            Public_Data'Read (Params, Data.Public);
            if Options.Execution_Mode = Replay_Mode then
               Data.Public.Location := To_Location ("replay://");
            end if;
            Data.Known       := True;
            Data.Queried     := False;
            Partition_Map.Set_Data (Asked, Data);

            pragma Debug (Dump_Partition_Information (Asked, Data.Public));

         when Shutdown =>

            pragma Debug
              (D (D_Garlic,
                  "Receive shutdown request from partition" & Partition'Img));

            Heart.Shutdown;

      end case;

      exception
         when E : others =>
            pragma Debug (D (D_Garlic, "Handle_Internal: fatal exception"));
            pragma Debug (D (D_Debug, Exception_Information (E)));
            raise Communication_Error;
   end Handle_Internal;

   -------------------
   -- Handle_Public --
   -------------------

   procedure Handle_Public
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type)
   is
      Receiver : Public_Receiver;
   begin
      if Operation /= Shutdown_Synchronization then
         Soft_Links.Activity_Detected;
      end if;
      Receiver_Map.Get (Operation) (Receiver);
      Receiver (Partition, Operation, Params);
   end Handle_Public;

   -----------------
   -- Has_Arrived --
   -----------------

   procedure Has_Arrived
     (Partition     : in Partition_ID;
      Filtered_Data : access Stream_Element_Array;
      Offset        : in Ada.Streams.Stream_Element_Count := 0)
   is
      Operation         : Opcode;
      First             : constant Stream_Element_Count :=
        Filtered_Data'First + Offset;
      Real_Data         : Stream_Element_Array
        renames Filtered_Data (First + 1 .. Filtered_Data'Last);
      Unfiltered_Data   : Stream_Element_Access;
      Unfiltered_Stream : aliased Params_Stream_Type (Real_Data'Length);
   begin
      --  Dump the stream for debugging purpose

      pragma Debug (D (D_Dump, "Dumping incoming stream"));
      pragma Debug (Dump (D_Dump, Filtered_Data, Private_Debug_Key));

      --  Record the current packet content in the trace file if needed

      if Options.Execution_Mode = Trace_Mode then
         Trace_Data (Partition, Filtered_Data);
      end if;

      --  Read the opcode from the stream and check that it is valid

      Operation := Opcode_Read (Filtered_Data (First));
      if not Operation'Valid then
         pragma Debug
           (D (D_Debug, "Received unknown opcode"));
         raise Constraint_Error;
      elsif Operation = No_Operation then
         pragma Debug (D (D_Debug, "Received No_Operation opcode"));
         raise Constraint_Error;
      end if;
      pragma Debug
        (D (D_Debug,
            "Received request with opcode " & Operation'Img &
            " from partition" & Partition_ID'Image (Partition)));

      --  Unfilter the data and put it in a stream. Note that the size of
      --  the stream has been set to Real_Data'Length, which is usually a
      --  good hint as most filteres will not change the size of the data.

      Unfiltered_Data :=
        Filter_Incoming (Partition, Operation, Real_Data);
      To_Params_Stream_Type (Unfiltered_Data.all, Unfiltered_Stream'Access);
      Free (Unfiltered_Data);

      --  Depending on the opcode, send the stream to the public or internal
      --  routines.

      case Operation is
         when Internal_Opcode =>
            Handle_Internal (Partition, Operation, Unfiltered_Stream'Access);
         when Public_Opcode   =>
            Handle_Public (Partition, Operation, Unfiltered_Stream'Access);
         when Invalid_Operation =>
            raise Program_Error;       -- We cannot land here, see test above
      end case;

   exception when others =>
      pragma Debug (D (D_Debug, "Exception in block Has_Arrived"));
      raise;
   end Has_Arrived;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      My_Public_Data.Name         := Get (Options.Partition_Name.all);
      My_Public_Data.Termination  := Options.Termination;
      My_Public_Data.Reconnection := Options.Reconnection;
      My_Public_Data.Alive        := True;

      pragma Debug
        (D (D_Debug, "My partition name is " & Get (My_Public_Data.Name)));
      pragma Debug
        (D (D_Debug,
            "My termination policy is " & Options.Termination'Img));
      pragma Debug
        (D (D_Debug,
            "My reconnection policy is " & Options.Reconnection'Img));
   end Initialize;

   -----------------------
   -- Is_Boot_Partition --
   -----------------------

   function Is_Boot_Partition return Boolean is
   begin
      return Is_Boot;
   end Is_Boot_Partition;

   -----------------------------
   -- Is_Shutdown_In_Progress --
   -----------------------------

   function Is_Shutdown_In_Progress return Boolean is
   begin
      return Shutdown_In_Progress;
   end Is_Shutdown_In_Progress;

   -----------------------------
   -- Local_Partition_ID_Type --
   -----------------------------

   protected body Local_Partition_ID_Type is

      ---------
      -- Get --
      ---------

      entry Get (Partition : out Partition_ID)
      when Local_Partition /= Null_Partition_ID or not In_Progress is
      begin
         if Local_Partition = Null_Partition_ID then
            In_Progress := True;
         end if;
         Partition := Local_Partition;
      end Get;

      ---------------------
      -- Get_Immediately --
      ---------------------

      function Get_Immediately return Partition_ID is
      begin
         return Local_Partition;
      end Get_Immediately;

      ---------
      -- Set --
      ---------

      procedure Set (Partition : in Partition_ID) is
      begin
         In_Progress := False;
         Local_Partition := Partition;
      end Set;

   end Local_Partition_ID_Type;

   --------------
   -- Location --
   --------------

   function Location (Partition : Partition_ID) return Location_Type is
   begin
      return Get_Public_Data (Partition) .Location;
   end Location;

   ----------
   -- Name --
   ----------

   function Name (Partition : Partition_ID) return Name_Id is
   begin
      return Get_Public_Data (Partition) .Name;
   end Name;

   ----------
   -- Name --
   ----------

   function Name (Partition : Partition_ID) return String is
   begin
      return Get (Name (Partition));
   end Name;

   -----------------
   -- Opcode_Read --
   -----------------

   function Opcode_Read (Operation : Stream_Element) return Opcode is
   begin
      return Opcode'Val (Operation);
   end Opcode_Read;

   ------------------
   -- Opcode_Write --
   ------------------

   function Opcode_Write (Operation : Opcode) return Stream_Element is
   begin
      return Opcode'Pos (Operation);
   end Opcode_Write;

   ----------------------------
   -- Partition_RPC_Receiver --
   ----------------------------

   procedure Partition_RPC_Receiver
     (Params : access Streams.Params_Stream_Type;
      Result : access Streams.Params_Stream_Type) is
      Receiver : RPC_Receiver;
   begin
      RPC_Receiver'Read (Params, Receiver);
      Receiver (Params, Result);
   end Partition_RPC_Receiver;

   -------------
   -- Receive --
   -------------

   procedure Receive (Operation : in Opcode; Receiver : in Public_Receiver) is
   begin
      pragma Debug
        (D (D_Garlic,
            "Receiver for operation " & Operation'Img & " is now registered"));
      Receiver_Map.Set (Operation, Receiver);
   end Receive;

   ------------------
   -- Receiver_Map --
   ------------------

   protected body Receiver_Map_Type is

      ---------
      -- Get --
      ---------

      entry Get (for Operation in Opcode) (Receiver : out Public_Receiver)
      when Receiver_Data (Operation) /= null is
      begin
         Receiver := Receiver_Data (Operation);
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (Operation : in Opcode; Receiver : in Public_Receiver) is
      begin
         Receiver_Data (Operation) := Receiver;
      end Set;

   end Receiver_Map_Type;

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

   procedure Remote_Partition_Error (Partition : in Partition_ID) is
      Data : Partition_Data;
   begin
      pragma Debug
        (D (D_Communication,
            "It looks like partition" & Partition'Img & " is dead"));
      pragma Debug (D (D_Debug, "Registering the partition as not alive"));
      Data := Partition_Map.Get_Data (Partition);
      Data.Public.Alive := False;
      Partition_Map.Set_Data (Partition, Data);
      if Shutdown_Policy = Shutdown_On_Any_Partition_Error then
         pragma Debug
            (D (D_Communication, "Due to the policy, I will shutdown"));
         Soft_Shutdown;
      end if;
      if Partition = Server_Partition_ID and then
        Shutdown_Policy = Shutdown_On_Boot_Partition_Error then
         pragma Debug
           (D (D_Communication, "I cannot live without a boot partition"));
         Soft_Shutdown;
      end if;
      if Partition_Error_Notification /= null then
         pragma Debug (D (D_Debug, "Calling the registered callback"));
         Partition_Error_Notification (Partition);
      end if;
   end Remote_Partition_Error;

   -------------------------
   -- Reconnection_Policy --
   -------------------------

   function Reconnection_Policy
     (Partition : Partition_ID)
      return Reconnection_Type is
   begin
      return Get_Public_Data (Partition) .Reconnection;
   end Reconnection_Policy;

   ----------
   -- Send --
   ----------

   procedure Send (Partition : in Partition_ID;
                   Operation : in Opcode;
                   Params    : access Params_Stream_Type)
   is
      Filtered_Data : Stream_Element_Access;
      Length        : Stream_Element_Offset;
      Packet        : Stream_Element_Access;
   begin
      --  Filter the data according to the remote partition and the opcode

      Filtered_Data := Filter_Outgoing (Partition, Operation, Params);

      --  Compute the length of the packet: this is the length of the
      --  unused space that will be used by the protocol to stick its own
      --  data at the beginning + 1 for the opcode + the length of the
      --  unfiltered data. Allocate a packet of the right length.

      Length := Protocols.Unused_Space + 1 + Filtered_Data'Length;
      Packet := new Stream_Element_Array (1 .. Length);

      --  Put the opcode at the beginning of the reserved section,
      --  then the filtered data, which can then be deallocated.

      Packet (Protocols.Unused_Space + 1) := Opcode_Write (Operation);
      Packet (Protocols.Unused_Space + 2 .. Packet'Last) := Filtered_Data.all;
      Free (Filtered_Data);

      --  If the data is for a remote partition, send it using the right
      --  protocol. Otherwise, make a local call to Has_Arrived (this can
      --  happen for a call on which pragma All_Calls_Remote applies) without
      --  the extra space.

      if Partition = Get_My_Partition_ID_Immediately then
         pragma Debug (D (D_Debug, "Handling a All_Calls_Remote case"));
         Has_Arrived (Partition,
                      Packet,
                      Protocols.Unused_Space);
      else
         pragma Debug (D (D_Debug, "Calling the right protocol"));
         Protocols.Send (Get_Protocol (Partition), Partition, Packet);
      end if;

      --  Free the data, even if an exception occurs

      Free (Packet);

   exception
      when others =>
         Free (Packet);
         raise;
   end Send;

   -----------------------
   -- Set_Boot_Location --
   -----------------------

   procedure Set_Boot_Location (Location : in Location_Type)
   is
      Public : constant Public_Data :=
        (Location     => Location,
         Name         => Null_Name,
         Reconnection => Rejected_On_Restart,
         Termination  => Global_Termination,
         Alive        => False);
      Data : Partition_Data :=
        (Public  => Public,
         Queried => False,
         Known   => False);
   begin
      if Is_Boot_Partition then
         Data.Public := My_Public_Data;
         Data.Known  := True;
      end if;
      Partition_Map.Set_Data (Server_Partition_ID, Data);
   end Set_Boot_Location;

   ---------------------------
   -- Set_Is_Boot_Partition --
   ---------------------------

   procedure Set_Is_Boot_Partition (Yes : in Boolean) is
   begin
      Is_Boot := Yes;
      if Is_Boot then
         Local_Partition_ID.Set (Server_Partition_ID);
      end if;
   end Set_Is_Boot_Partition;

   ---------------------
   -- Set_My_Location --
   ---------------------

   procedure Set_My_Location (Location : in Location_Type)
   is
   begin
      pragma Debug (D (D_Debug,
                       "Setting my location to " & To_String (Location)));
      My_Public_Data.Location := Location;
   end Set_My_Location;

   -------------------------
   -- Set_My_Partition_ID --
   -------------------------

   procedure Set_My_Partition_ID (Partition : in Partition_ID) is
      use System.Garlic.Trace;
   begin
      System.Standard_Library.Local_Partition_ID := Natural (Partition);
      Local_Partition_ID.Set (Partition);
      if Options.Execution_Mode = Trace_Mode then
         Trace_Partition_ID (Partition);
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
      Trace.Shutdown;
      Soft_Links.Termination_Shutdown;
      Physical_Location.Shutdown;
      RPC_Shutdown;
      Free (Local_Partition_ID);
      Free (Partition_Map);
      Free (Receiver_Map);
      Delete_Termination_Sanity_File;
   end Shutdown;

   -------------------
   -- Soft_Shutdown --
   -------------------

   procedure Soft_Shutdown is
   begin
      Shutdown_In_Progress := True;
      if Is_Boot_Partition then
         for Partition in
           Server_Partition_ID + 1 .. Latest_Allocated_Partition_ID loop
            if Termination_Policy (Partition) /= Local_Termination then
               declare
                  Empty : aliased Params_Stream_Type (0);
               begin
                  Send (Partition, Shutdown, Empty'Access);
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
      return Get_Public_Data (Partition) .Termination;
   end Termination_Policy;

   ------------------------------------------
   -- Wait_Until_Elaboration_Is_Terminated --
   ------------------------------------------

   procedure Wait_Until_Elaboration_Is_Terminated is
   begin
      pragma Debug (D (D_Debug, "Checking that elaboration is terminated"));
      Elaboration_Barrier.Wait;
      pragma Debug
         (D (D_Debug, "Confirmation that elaboration is terminated"));
   end Wait_Until_Elaboration_Is_Terminated;

end System.Garlic.Heart;
