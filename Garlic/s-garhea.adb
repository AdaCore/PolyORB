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
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Deallocation;

with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Filters;           use System.Garlic.Filters;
with System.Garlic.Name_Table;        use System.Garlic.Name_Table;
with System.Garlic.Options;
with System.Garlic.Physical_Location; use System.Garlic.Physical_Location;
with System.Garlic.Protocols;
with System.Garlic.Streams;           use System.Garlic.Streams;
with System.Garlic.Termination;
with System.Garlic.Trace;             use System.Garlic.Trace;
with System.Garlic.Utils;
with System.RPC.Initialization;
with System.Standard_Library;

package body System.Garlic.Heart is

   --  The protocol used is:
   --
   --   - <QUERY_LOCATION> <PARTITION_ID>
   --   - <QUERY_LOCATION_ANSWER> <PARTITION_ID> <LOCATION> <NAME>
   --   - <SET_LOCATION> <LOCATION> <NAME>
   --   - <SHUTDOWN>

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("HEART", "(s-garhea): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use System.RPC, System.Garlic.Utils;
   use type Ada.Exceptions.Exception_Id;

   subtype Valid_Partition_ID is Partition_ID
     range Partition_ID'Succ (Null_Partition_ID) .. Partition_ID'Last;
   --  A partition whose ID fits in Valid_Partition_ID is a real partition

   Server_Partition_ID : constant Valid_Partition_ID :=
     Valid_Partition_ID'First;
   --  The partition ID server does have this partition ID

   Reconnection_Policy : Reconnection_Type := Immediately;
   Shutdown_Policy     : Shutdown_Type     := Shutdown_On_Boot_Partition_Error;
   --  These parameters control how Garlic will act in face of errors.
   --  They don't need extra protection because they should not be modified
   --  by more than one task (in fact, they should not be modified after
   --  the elaboration is terminated).

   Elaboration_Barrier : Barrier_Type;
   --  This barrier will be no longer blocking when the elaboration is
   --  terminated.

   function Get_Protocol
     (Partition : Partition_ID)
      return Protocols.Protocol_Access;
   pragma Inline (Get_Protocol);
   --  Return the protocol of a partition using a cache whenever possible

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

   Opcode_Size : Ada.Streams.Stream_Element_Count;
   --  Set in 'Initialize': length of result of an Opcode'Write

   type Partition_Data_Array is array (Valid_Partition_ID) of Partition_Data;

   protected type Partition_Map_Type is
      procedure Set_Data
        (Partition : in Partition_ID;
         Data      : in Partition_Data);
      function Get_Data (Partition : Partition_ID) return Partition_Data;
      entry Wait_For_Data (Partition : in  Partition_ID;
                           Data      : out Partition_Data);

   private
      entry Queue (Partition : in  Partition_ID;
                   Data      : out Partition_Data);

      Map : Partition_Data_Array;
      New_Data : Boolean := False;
      --  Local barrier

   end Partition_Map_Type;
   --  Data available for a partition. When the data is not available for
   --  a given partition, Wait_For_Data will block, unless the caller *has*
   --  to ask for information about the given partition (in this case,
   --  the Queried field is set to True). If we are on the server, Queried
   --  is never set to True since we wait for the partition itself to
   --  register (this should have occurred already in a normal utilization
   --  since there is no way for a partition to ask for another one if
   --  the name server has not mapped the name of the wanted package on the
   --  partition number).

   Partition_Map_Cache : Partition_Data_Array;
   --  This acts as a Cache for Partition_Map, this means that if Known
   --  is True for a given partition, there is no need to use the overhead
   --  of the protected type to query a partition location.

   Protocols_Cache : array (Partition_ID) of Protocols.Protocol_Access;
   --  Copy of the protocol type of the Partition_Map_Cache

   type Partition_Map_Access is access Partition_Map_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Partition_Map_Type,
                                      Partition_Map_Access);
   Partition_Map : Partition_Map_Access := new Partition_Map_Type;
   --  Same kludge as above to raise Program_Error at deallocation time ???

   My_Location : Location_Type;
   --  Location of the current partition

   type Allocated_Map is array (Partition_ID range <>) of Boolean;
   --  Type of allocated partitions

   protected type Partition_ID_Allocation_Type is
      procedure Allocate (Partition : out Partition_ID);
      procedure Free (Partition : in Partition_ID);
      function Latest return Partition_ID;
   private
      Latest_Partition : Valid_Partition_ID :=
        Server_Partition_ID;
      Allocated        : Allocated_Map (Valid_Partition_ID) :=
        (Server_Partition_ID => True,
         others              => False);
   end Partition_ID_Allocation_Type;

   type Partition_ID_Allocation_Access is access Partition_ID_Allocation_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Partition_ID_Allocation_Type,
                                      Partition_ID_Allocation_Access);
   Partition_ID_Allocation : Partition_ID_Allocation_Access :=
     new Partition_ID_Allocation_Type;
   --  Same kludge as above ???

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

   --------------------------
   -- Add_New_Partition_ID --
   --------------------------

   procedure Add_New_Partition_ID (Partition : in Partition_ID) is
      Empty : aliased Params_Stream_Type (0);
   begin
      if Reconnection_Policy = Immediately then

         --  Send a NOP to establish the connection

         pragma Debug (D (D_Debug, "Sending a No_Operation"));
         Send (Partition, No_Operation, Empty'Access);
      end if;
   end Add_New_Partition_ID;

   ---------------------------
   -- Allocate_Partition_ID --
   ---------------------------

   function Allocate_Partition_ID return Partition_ID is
      Partition : Partition_ID;
   begin
      Partition_ID_Allocation.Allocate (Partition);
      pragma Debug (D (D_Server, "Allocating partition" & Partition'Img));
      return Partition;
   end Allocate_Partition_ID;

   -------------------------------
   -- Elaboration_Is_Terminated --
   -------------------------------

   procedure Elaboration_Is_Terminated is
   begin
      pragma Debug
        (D (D_Elaborate, "Signaling that elaboration is terminated"));
      Elaboration_Barrier.Signal_All (Permanent => True);
   end Elaboration_Is_Terminated;

   -----------------
   -- Fatal_Error --
   -----------------

   protected body Fatal_Error is

      --------------
      -- Occurred --
      --------------

      entry Occurred
        (What    : out Ada.Exceptions.Exception_Id;
         Message : out String_Ptr)
      when Exc /= Ada.Exceptions.Null_Id is
      begin
         What    := Exc;
         Message := new String'(Msg.all);
      end Occurred;

      ------------
      -- Signal --
      ------------

      procedure Signal
        (What    : in Ada.Exceptions.Exception_Id;
         Message : in String := "")
      is
         procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);
      begin
         Free (Msg);
         Exc := What;
         Msg := new String'(Message);
      end Signal;

   end Fatal_Error;

   ---------------------
   -- Get_Boot_Server --
   ---------------------

   function Get_Boot_Server return Partition_ID is
   begin
      return Server_Partition_ID;
   end Get_Boot_Server;

   ---------------------
   -- Get_Boot_Server --
   ---------------------

   function Get_Boot_Server return String is
      Data : Partition_Data;
   begin
      Partition_Map.Wait_For_Data (Server_Partition_ID, Data);
      return To_String (Data.Location);
   end Get_Boot_Server;

   ---------------------
   -- Get_My_Location --
   ---------------------

   function Get_My_Location return Location_Type is
   begin
      return My_Location;
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

            --  We will send a Set_Location to the server. This will cause
            --  a dialog to be established and a new Partition_ID to be
            --  allocated, and our location will be registered into
            --  the server's base.

            Location_Type'Write (Params'Access, My_Location);
            String'Output (Params'Access, Get (My_Partition_Name));
            Send (Server_Partition_ID, Set_Location, Params'Access);
            Local_Partition_ID.Get (Partition);
         end;
      end if;

      return Partition;
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

   ------------------------
   -- Get_Partition_Data --
   ------------------------

   function Get_Partition_Data (Partition : Partition_ID)
     return Partition_Data is
      Data : Partition_Data;

   begin
      pragma Debug
        (D (D_Table,
            "Looking locally for information on partition" & Partition'Img));

      --  If the partition location is in the cache, then get it from
      --  there instead of using the protected type.

      if Partition_Map_Cache (Partition) .Known then
         return Partition_Map_Cache (Partition);
      end if;

      Partition_Map.Wait_For_Data (Partition, Data);
      if Data.Queried then

         --  We have to query the server for the location.

         declare
            Params : aliased Params_Stream_Type (0);

         begin
            pragma Debug
              (D (D_Garlic,
                  "Asking for information on partition" & Partition'Img));
            Partition_ID'Write (Params'Access, Partition);
            Send (Server_Partition_ID, Query_Location, Params'Access);
         end;

         Partition_Map.Wait_For_Data (Partition, Data);

         pragma Debug
           (D (D_Table,
               "Caching information on partition" & Partition'Img));
      end if;

      return Data;
   end Get_Partition_Data;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
     (Partition : Partition_ID)
      return Protocols.Protocol_Access is
   begin
      if not Partition_Map_Cache (Partition) .Known then
         declare
            Dummy : constant Partition_Data := Get_Partition_Data (Partition);
         begin
            null;
         end;
      end if;

      return Protocols_Cache (Partition);
   end Get_Protocol;

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

      Termination.Activity_Detected;

      case Operation is

         when No_Operation => null;

         when Set_Location =>

            pragma Debug
              (D (D_Server,
                  "Receive information on partition" & Partition'Img));

            Location_Type'Read (Params, Data.Location);
            if Options.Execution_Mode = Replay_Mode then
               Data.Location := To_Location ("replay://");
            end if;

            Data.Name    := Get (String'Input (Params));
            Data.Known   := True;
            Data.Queried := False;

            pragma Debug
              (D (D_Server,
                  "Receive that partition" & Partition'Img &
                  " is named " & Get (Data.Name) &
                  " and is located at " & To_String (Data.Location)));

            Partition_Map.Set_Data (Partition, Data);

         when Query_Location =>
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
                     "Reply that partition" & Asked'Img &
                     " is named " & Get (Data.Name) &
                     " and is located at " & To_String (Data.Location)));

               Partition_ID'Write (Answer'Access, Asked);
               Location_Type'Write (Answer'Access, Data.Location);
               String'Output (Answer'Access, Get (Data.Name));
               Send (Partition, Query_Location_Answer, Answer'Access);
            end;

         when Query_Location_Answer =>

            Partition_ID'Read (Params, Asked);
            if not Asked'Valid then
               pragma Debug (D (D_Debug, "Invalid partition ID"));
               raise Constraint_Error;
            end if;

            pragma Debug
              (D (D_Garlic,
                  "Receive query for information on partition" & Asked'Img));

            Location_Type'Read (Params, Data.Location);
            if Options.Execution_Mode = Replay_Mode then
               Data.Location := To_Location ("replay://");
            end if;

            Data.Name    := Get (String'Input (Params));
            Data.Known   := True;
            Data.Queried := False;
            Partition_Map.Set_Data (Asked, Data);

         when Shutdown =>

            pragma Debug
              (D (D_Garlic,
                  "Receive shutdown request from partition" & Partition'Img));

            Heart.Shutdown;

      end case;

      exception
         when others =>
            pragma Debug (D (D_Garlic, "Handle internal: fatal error"));
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
         Termination.Activity_Detected;
      end if;
      Receiver_Map.Get (Operation) (Receiver);
      Receiver (Partition, Operation, Params);
   end Handle_Public;

   -----------------
   -- Has_Arrived --
   -----------------

   procedure Has_Arrived
     (Partition : in Partition_ID;
      Data      : in Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Count;

      Operation : Opcode;

   begin
      if Options.Execution_Mode = Trace_Mode then
         Trace_Data (Partition, Data);
      end if;

      declare
         Params : aliased Params_Stream_Type (Opcode_Size);
      begin
         To_Params_Stream_Type
            (Data (Data'First .. Data'First + Opcode_Size - 1),
             Params'Access);
         Opcode'Read (Params'Access, Operation);
      end;

      if not Operation'Valid then
         pragma Debug
           (D (D_Debug, "Received unknown opcode"));
         raise Constraint_Error;
      end if;
      pragma Debug
        (D (D_Debug,
            "Received request with opcode " & Operation'Img &
            " from partition" & Partition_ID'Image (Partition)));

      declare
         Filtered_Data   : Stream_Element_Access :=
           Filter_Incoming
             (Partition, Operation,
              Data (Data'First + Opcode_Size .. Data'Last));
         Filtered_Params : aliased Params_Stream_Type (Filtered_Data'Length);
      begin
         To_Params_Stream_Type (Filtered_Data.all, Filtered_Params'Access);
         Free (Filtered_Data);
         if Operation in Internal_Opcode then
            Handle_Internal (Partition, Operation, Filtered_Params'Access);
         elsif Operation in Public_Opcode then
            Handle_Public (Partition, Operation, Filtered_Params'Access);
         else
            pragma Debug (D (D_Debug, "Aborting due to invalid opcode"));
            raise Constraint_Error;
         end if;
      end;
   end Has_Arrived;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Stream : aliased Params_Stream_Type (32);
      --  Some size that certainly is enough
   begin
      My_Partition_Name := Get (Options.Partition_Name.all);
      Opcode'Write (Stream'Access, Opcode'Last);
      declare
         Buffer : Ada.Streams.Stream_Element_Array
           := To_Stream_Element_Array (Stream'Access);
      begin
         Opcode_Size := Buffer'Length;
      end;
   end Initialize;

   -----------------------
   -- Is_Boot_Partition --
   -----------------------

   function Is_Boot_Partition return Boolean is
   begin
      return Is_Boot;
   end Is_Boot_Partition;

   -----------------------------------
   -- Latest_Allocated_Partition_ID --
   -----------------------------------

   function Latest_Allocated_Partition_ID return Partition_ID is
   begin
      return Partition_ID_Allocation.Latest;
   end Latest_Allocated_Partition_ID;

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

   ----------------------------------
   -- Partition_ID_Allocation_Type --
   ----------------------------------

   protected body Partition_ID_Allocation_Type is

      --------------
      -- Allocate --
      --------------

      procedure Allocate (Partition : out Partition_ID) is
      begin
         for I in Allocated'Range loop
            if not Allocated (I) then
               Partition := I;
               Allocated (I) := True;
               if I > Latest_Partition then
                  Latest_Partition := I;
               end if;
               return;
            end if;
         end loop;
         raise Constraint_Error;
      end Allocate;

      ----------
      -- Free --
      ----------

      procedure Free (Partition : in Partition_ID) is
      begin
         Allocated (Partition) := False;
         if Partition = Latest_Partition then
            for I in reverse Allocated'First .. Partition - 1 loop
               if Allocated (I) then
                  Latest_Partition := I;
                  return;
               end if;
            end loop;

            --  We should not be here, since the server partition id
            --  has to stay allocated.

            pragma Assert (False);
            raise Program_Error;

         end if;
      end Free;

      ------------
      -- Latest --
      ------------

      function Latest return Partition_ID is
      begin
         return Latest_Partition;
      end Latest;

   end Partition_ID_Allocation_Type;

   ------------------------
   -- Partition_Map_Type --
   ------------------------

   protected body Partition_Map_Type is

      --------------
      -- Get_Data --
      --------------

      function Get_Data (Partition : Partition_ID) return Partition_Data is
      begin
         return Map (Partition);
      end Get_Data;

      --------------
      -- Set_Data --
      --------------

      procedure Set_Data
        (Partition : in Partition_ID;
         Data      : in Partition_Data) is
      begin
         Map (Partition) := Data;
         Partition_Map_Cache (Partition) := Data;
         Protocols_Cache (Partition) :=
           Physical_Location.Get_Protocol (Data.Location);
         if Queue'Count > 0 then
            New_Data := True;
         end if;
      end Set_Data;

      -------------------
      -- Wait_For_Data --
      -------------------

      entry Wait_For_Data
         (Partition : in Partition_ID;
          Data      : out Partition_Data)
         when not New_Data is
      begin
         if Map (Partition).Known then
            Data := Map (Partition);
         elsif Map (Partition).Queried then
            requeue Queue with abort;
         else
            Map (Partition).Queried := True;
            Data := Map (Partition);
         end if;
      end Wait_For_Data;

      --  Local entries implementing wait queues below.

      entry Queue
         (Partition : in Partition_ID;
          Data      : out Partition_Data)
         when New_Data is
      begin
         if Queue'Count = 0 then
            New_Data := False;
         end if;
         requeue Wait_For_Data with abort;
      end Queue;

   end Partition_Map_Type;

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
   begin
      pragma Debug
        (D (D_Communication,
            "It seems that partition" & Partition'Img & " is dead"));
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
      Partition_Error_Notification (Partition);
   end Remote_Partition_Error;

   ----------
   -- Send --
   ----------

   procedure Send
     (Partition : in     Partition_ID;
      Operation : in     Opcode;
      Params    : access System.RPC.Params_Stream_Type) is
      Protocol  : constant Protocols.Protocol_Access :=
        Get_Protocol (Partition);
      Op_Params : aliased Params_Stream_Type (Opcode_Size);
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;

   begin
      Opcode'Write (Op_Params'Access, Operation);

      declare
         Filtered_Data : Stream_Element_Access :=
           Filter_Outgoing (Partition, Operation, Params);
         Header : constant Ada.Streams.Stream_Element_Array :=
           To_Stream_Element_Array (Op_Params'Access);
         Length : constant Ada.Streams.Stream_Element_Offset :=
           Protocols.Unused_Space + Header'Length + Filtered_Data'Length;
         Packet : aliased Ada.Streams.Stream_Element_Array :=
           (1 .. Length => 0);

         --  We can't just declare 'Packet' with the correct size here:
         --  this would make Packet's type a constrained subtype, while
         --  the 'Send' below expects an access to an unconstrained
         --  subtype. Result: since the two types do not "statically
         --  match" (RM 4.9.1), the parameter types are different, and
         --  the compiler will issue an error message in the call to
         --  'Protocols.Send' below.

      begin
         --  Stuff the opcode (unfiltered) in front of the data.
         Packet
           (Packet'First + Protocols.Unused_Space ..
            Packet'First + Protocols.Unused_Space + Header'Length - 1) :=
           Header;
         Packet
           (Packet'First + Protocols.Unused_Space + Header'Length ..
            Packet'Last) :=
           Filtered_Data.all;
         Free (Filtered_Data);
         pragma Debug
           (D (D_Debug, "Sending an operation with opcode " & Operation'Img));
         Protocols.Send (Protocol, Partition, Packet'Access);
      end;
   end Send;

   -----------------------
   -- Set_Boot_Location --
   -----------------------

   procedure Set_Boot_Location (Location : in Location_Type) is
      Data : constant Partition_Data := (Location, Null_Name, True, False);

   begin
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
      My_Location := Location;
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
     (Reconnection : Reconnection_Type := Immediately;
      Shutdown     : Shutdown_Type     := Shutdown_On_Boot_Partition_Error)
   is
   begin
      Reconnection_Policy := Reconnection;
      Shutdown_Policy     := Shutdown;
   end Set_Policy;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Shutdown_Keeper.Signal;
      Trace.Shutdown;
      Termination.Shutdown;
      Physical_Location.Shutdown;
      RPC.Initialization.Shutdown;
      Free (Local_Partition_ID);
      Free (Partition_Map);
      Free (Partition_ID_Allocation);
      Free (Receiver_Map);
   end Shutdown;

   ---------------------
   -- Shutdown_Keeper --
   ---------------------

   protected body Shutdown_Keeper is

      --------------------
      -- Is_In_Progress --
      --------------------

      function Is_In_Progress return Boolean is
      begin
         return In_Progress;
      end Is_In_Progress;

      ------------
      -- Signal --
      ------------

      procedure Signal is
      begin
         In_Progress := True;
      end Signal;

      ----------
      -- Wait --
      ----------

      entry Wait when In_Progress is
      begin
         null;
      end Wait;

   end Shutdown_Keeper;

   -------------------
   -- Soft_Shutdown --
   -------------------

   procedure Soft_Shutdown is
   begin
      Shutdown_Keeper.Signal;
      if Is_Boot_Partition then
         for Partition in
           Server_Partition_ID + 1 .. Partition_ID_Allocation.Latest loop
            declare
               Empty : aliased Params_Stream_Type (0);
            begin
               Send (Partition, Shutdown, Empty'Access);
            exception
               when Communication_Error => null;
            end;
         end loop;
      end if;
      Heart.Shutdown;
   end Soft_Shutdown;

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
