------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                    S Y S T E M . G A R L I C . T C P                     --
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

with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
pragma Warnings (Off, Interfaces.C.Strings);
with System.Garlic.Constants;             use System.Garlic.Constants;
with System.Garlic.Debug;                 use System.Garlic.Debug;
with System.Garlic.Exceptions;            use System.Garlic.Exceptions;
with System.Garlic.Heart;                 use System.Garlic.Heart;
with System.Garlic.Naming;                use System.Garlic.Naming;
with System.Garlic.TCP.Operations;
with System.Garlic.Network_Utilities;     use System.Garlic.Network_Utilities;
with System.Garlic.Options;
with System.Garlic.Physical_Location;     use System.Garlic.Physical_Location;
with System.Garlic.Priorities;
with System.Garlic.Soft_Links;            use System.Garlic.Soft_Links;
with System.Garlic.Streams;               use System.Garlic.Streams;
with System.Garlic.Thin;                  use System.Garlic.Thin;
with System.Garlic.Types;                 use System.Garlic.Types;
with System.Garlic.Utils;                 use System.Garlic.Utils;
with System.Garlic.TCP_Platform_Specific;
pragma Warnings (Off, System.Garlic.TCP_Platform_Specific);
with System.Storage_Elements;             use System.Storage_Elements;

package body System.Garlic.TCP is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARTCP", "(s-gartcp): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use Ada.Streams, System.Garlic.Protocols, System.Garlic.Types;

   package C renames Interfaces.C;
   package Strings renames C.Strings;
   use type C.int;
   use type C.unsigned_short;
   --  Shortcuts

   package Net renames System.Garlic.TCP.Operations;

   In_Address_Any : constant Naming.Address := Any_Address;

   type Host_Location is record
      Addr : Naming.Address   := In_Address_Any;
      Port : C.unsigned_short := 0;
   end record;

   function Split_Data (Data : String) return Host_Location;
   --  Split a data given as <machine> or <machine>:<port>

   type Socket_Type is record
      Location : Host_Location;
      Socket   : C.int;
      Defined  : Boolean := False;
      Locked   : Boolean := False;
   end record;

   Socket_Table : array (Boot_PID .. Last_PID) of Socket_Type;
   Socket_Table_Mutex   : Mutex_Access := Create;
   Socket_Table_Watcher : Watcher_Access := Create;

   type Banner_Kind is (Junk_Banner, Data_Banner, Quit_Banner);
   --  Various headers that can be performed on a communication link

   Banner_Size : constant := 4;
   --  Size of a header when it is encoded as a stream

   subtype Banner_Stream is Stream_Element_Array (1 .. Banner_Size);
   --  Constrained subtype for headers

   function Read_Banner (Peer : C.int) return Banner_Kind;
   pragma Inline (Read_Banner);
   --  Read header from a file descriptor or return Junk_Banner if the
   --  header is not understood.

   SEC_Size : constant := 4;
   --  Size of a Stream_Element_Count when it is encoded as a stream

   subtype SEC_Stream is Stream_Element_Array (1 .. SEC_Size);
   --  Constrained subtype for stream element counts

   function Read_SEC (Peer : C.int)  return Stream_Element_Count;
   --  Read a stream element count from a file descriptor and check that
   --  it is valid. Raise Communication_Error otherwise.

   function To_Stream_Element_Array (Count : Stream_Element_Count)
     return SEC_Stream;
   --  Return the stream element array corresponding to this count

   function Do_Connect (Location : Host_Location) return C.int;
   --  Establish a socket to a remote location and return the file descriptor

   procedure Do_Listen;
   --  Establish a socket according to the information in Self_Host (and
   --  complete it if needed).

   procedure Enter (PID : Partition_ID);
   procedure Leave (PID : Partition_ID);

   procedure Free is
     new Ada.Unchecked_Deallocation (Sockaddr_In, Sockaddr_In_Access);

   procedure Physical_Receive
     (Peer : in C.int;
      Data : out Stream_Element_Array);
   pragma Inline (Physical_Receive);
   procedure Physical_Send
     (Peer   : in C.int;
      Stream : access Stream_Element_Array;
      First  : in Stream_Element_Count);
   pragma Inline (Physical_Send);
   --  Receive and send data. Physical_Receive loops as long as Data has
   --  not been filled and Physical_Send as long as everything has not been
   --  sent.

   Data_Stream : aliased Stream_Element_Array
     :=  (1 .. Banner_Size => Banner_Kind'Pos (Data_Banner));

   Quit_Stream : aliased Stream_Element_Array
     := (1 .. Banner_Size => Banner_Kind'Pos (Quit_Banner));

   task type Accept_Handler is
      pragma Priority (Priorities.RPC_Priority);
   end Accept_Handler;
   type Accept_Handler_Access is access Accept_Handler;
   Acceptor : Accept_Handler_Access;
   --  Accept new connections

   task type Connection_Handler
     (Peer : C.int;
      PID  : Partition_ID) is
      pragma Priority (Priorities.RPC_Priority);
   end Connection_Handler;
   type Connection_Handler_Access is
      access Connection_Handler;
   --  Handle incoming connection

   --------------------
   -- Accept_Handler --
   --------------------

   task body Accept_Handler is
   begin
      --  Infinite loop on C_Accept

      Accept_Loop :
      loop
         declare
            Self    : Socket_Type renames Socket_Table (Last_PID);
            Sin     : Sockaddr_In_Access := new Sockaddr_In;
            Length  : aliased C.int := Sin.all'Size / 8;
            Peer    : C.int;
            Handler : Connection_Handler_Access;
            Banner  : Banner_Kind;
            Result  : C.int;
         begin
            Sin.Sin_Family := Constants.Af_Inet;
            Add_Non_Terminating_Task;
            pragma Debug (D (D_Debug, "Before Net.C_Accept"));
            Peer := Net.C_Accept
              (Self.Socket, To_Sockaddr_Access (Sin), Length'Access);
            pragma Debug (D (D_Debug, "After Net.C_Accept"));
            Sub_Non_Terminating_Task;
            if Peer = Failure then
               Raise_Communication_Error;
            end if;

            --  Read a code from the file descriptor to know what to do
            --  next.

            pragma Debug (D (D_Debug, "Reading header"));
            Banner := Read_Banner (Peer);
            case Banner is

               when Junk_Banner =>
                  pragma Debug (D (D_Debug,
                                   "Unknown header received, closing socket"));
                  Result := Net.C_Close (Peer);

               when Data_Banner =>
                  pragma Debug (D (D_Debug, "Data header received"));
                  --  Create a new task to handle this new connection

                  Handler := new Connection_Handler (Peer, Null_PID);

               when Quit_Banner =>
                  pragma Debug (D (D_Debug, "Quitting accept handler"));
                  Result := Net.C_Close (Peer);
                  exit Accept_Loop;

            end case;

         end;
      end loop Accept_Loop;
   end Accept_Handler;

   ------------
   -- Create --
   ------------

   function Create return Protocol_Access is
      Self : constant Protocol_Access := new TCP_Protocol;
   begin
      Register_Protocol (Self);
      return Self;
   end Create;

   ----------------
   -- Do_Connect --
   ----------------

   function Do_Connect (Location  : Host_Location) return C.int
   is
      Peer : C.int;
      Sin  : Sockaddr_In_Access;
      Code : C.int;
   begin
      Peer := C_Socket (Af_Inet, Sock_Stream, 0);
      if Peer /= Failure then
         Sin := new Sockaddr_In;
         Sin.Sin_Family := Constants.Af_Inet;
         Sin.Sin_Addr := To_In_Addr (Location.Addr);
         Sin.Sin_Port := Port_To_Network (Location.Port);
         Code := Net.C_Connect
           (Peer, To_Sockaddr_Access (Sin), Sin.all'Size / 8);
         if Code = Failure then
            pragma Debug
              (D (D_Debug,
                  "Cannot connect to host " & Image (Location.Addr) &
                  " on port" & Location.Port'Img));
            Free (Sin);
            Code := Net.C_Close (Peer);
            Peer := Failure;
         end if;
      end if;
      return Peer;
   end Do_Connect;

   ---------------
   -- Do_Listen --
   ---------------

   procedure Do_Listen is
      Self  : Socket_Type renames Socket_Table (Last_PID);
      Port  : C.unsigned_short renames Self.Location.Port;
      Sin   : Sockaddr_In_Access := new Sockaddr_In;
      Check : Sockaddr_In_Access := new Sockaddr_In;
      Size  : aliased C.int := Check.all'Size / 8;
      Code  : C.int;
      One   : aliased C.int := 1;
   begin
      Enter (Last_PID);
      Self.Socket := C_Socket (Af_Inet, Sock_Stream, 0);
      if Self.Socket = Failure then
         Free (Sin);
         Free (Check);
         Leave (Last_PID);
         Raise_Communication_Error;
      end if;

      Code := C_Setsockopt
        (Self.Socket, Sol_Socket, So_Reuseaddr, One'Address, One'Size / 8);

      Sin.Sin_Family := Constants.Af_Inet;
      Sin.Sin_Port := Port_To_Network (Port);
      Code := C_Bind (Self.Socket, To_Sockaddr_Access (Sin), Sin.all'Size / 8);
      if Code = Failure then
         Free (Sin);
         Free (Check);
         Leave (Last_PID);
         Raise_Communication_Error;
      end if;

      if C_Listen (Self.Socket, 15) = Failure then
         Leave (Last_PID);
         Raise_Communication_Error;
      end if;

      if Port = 0 then
         Code := C_Getsockname
           (Self.Socket, To_Sockaddr_Access (Check), Size'Access);
         if Code = Failure then
            Free (Sin);
            Free (Check);
            Leave (Last_PID);
            Raise_Communication_Error;
         end if;
         Port := Network_To_Port (Check.Sin_Port);
      end if;

      Free (Check);
      Self.Defined := True;
      Leave (Last_PID);

      pragma Debug (D (D_Communication, "Listen on port" & Port'Img));
   end Do_Listen;

   -----------
   -- Enter --
   -----------

   procedure Enter (PID : Partition_ID) is
      Version : Version_Id;
   begin
      loop
         Enter (Socket_Table_Mutex);
         if not Socket_Table (PID).Locked then
            Socket_Table (PID).Locked := True;
            pragma Debug (D (D_Debug, "Enter lock of partition" & PID'Img));
            Leave (Socket_Table_Mutex);
            exit;
         end if;
         pragma Debug (D (D_Debug, "Postpone lock of partition" & PID'Img));
         Commit (Socket_Table_Watcher, Version);
         Leave  (Socket_Table_Mutex);
         Differ (Socket_Table_Watcher, Version);
      end loop;
   end Enter;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (P : access TCP_Protocol) return String is
      Location : Host_Location renames Socket_Table (Last_PID).Location;
      Port     : constant String := Location.Port'Img;
   begin
      if Can_Have_A_Light_Runtime then
         return "";
      end if;
      return Name_Of (Image (Location.Addr)) & ":" & Port (2 .. Port'Last);
   end Get_Info;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (P : access TCP_Protocol) return String is
   begin
      return "tcp";
   end Get_Name;

   ---------------------------------
   -- Connection_Handler --
   ---------------------------------

   task body Connection_Handler is
      Unknown    : Boolean := (PID = Null_PID);
      Partition  : Partition_ID := PID;
      Length     : Stream_Element_Count;
      Filtered   : Stream_Element_Access;
      Unfiltered : Stream_Element_Access;
      Opcode     : Any_Opcode;
      Banner     : Banner_Kind;
   begin
      pragma Debug (D (D_Communication, "New communication task started"));

      loop
         if Partition /= Null_PID then
            pragma Debug (D (D_Debug, "Reading header"));

            Add_Non_Terminating_Task;
            Banner := Read_Banner (Peer);
            Sub_Non_Terminating_Task;

            case Banner is
               when Junk_Banner =>
                  pragma Debug (D (D_Debug, "Unknown header received"));
                  Raise_Communication_Error ("Received a bad header");

               when Data_Banner =>
                  pragma Debug (D (D_Debug, "Received a data header"));
                  null;

               when Quit_Banner =>
                  pragma Debug (D (D_Debug, "Quitting incoming handler"));
                  Raise_Communication_Error ("Received a quit header");

            end case;
         end if;

         Length := Read_SEC (Peer);

         pragma Debug (D (D_Debug, "Receive a packet of length" & Length'Img));

         pragma Debug (D (D_Debug, "Allocate stream"));
         Filtered := new Stream_Element_Array (1 .. Length);

         pragma Debug (D (D_Debug, "Receive stream physically"));
         Physical_Receive (Peer, Filtered.all);

         pragma Debug (D (D_Debug, "Analyse stream"));
         Analyze_Stream (Partition, Opcode, Unfiltered, Filtered);

         if Unknown then
            Activity_Detected;
            pragma Debug
              (D (D_Communication, "Task handling partition" & Partition'Img));

            Enter (Partition);
            if not Socket_Table (Partition).Defined then
               Socket_Table (Partition).Socket  := Peer;
               Socket_Table (Partition).Defined := True;
            end if;
            Leave (Partition);
            Unknown := False;
         end if;

         pragma Debug (D (D_Debug, "Process stream"));
         Process_Stream (Partition, Opcode, Unfiltered);

         pragma Debug (D (D_Debug, "Deallocate streams"));
         Free (Filtered);
         Free (Unfiltered);
      end loop;

   exception

      when Communication_Error =>

         --  The remote end has closed the socket or a communication error
         --  occurred. In the case, the entry will be freed, except if we
         --  are terminating since this is not worth freeing anything.

         if Partition /= Null_PID
           and then not Shutdown_In_Progress
         then
            --  Set the entry in the table correctly only we are not
            --  executing a shutdown operation. If we are, then
            --  it is likely that Partition_Map is already deallocated.

            Enter (Partition);
            Socket_Table (Partition).Defined := False;
            Leave (Partition);

         end if;

         declare
            Dummy : C.int;
         begin
            Dummy := Net.C_Close (Peer);
         end;

         --  Signal to the heart that we got an error on this partition

         if Partition = Null_PID then
            pragma Debug
              (D (D_Garlic,
                  "Task dying before determining remote Partition_ID"));
            null;
         elsif not Shutdown_In_Progress then
            pragma Debug
              (D (D_Garlic,
                  "Signaling that partition" & Partition'Img &
                  " is now unavailable"));
            Remote_Partition_Error (Partition);
         else
            pragma Debug (D (D_Garlic,
                             "Partition" & Partition'Img &
                             " is now unavailable (because of shutdown)"));
            null;
         end if;

      when others =>
         pragma Debug (D (D_Garlic, "Fatal error in connection handler"));
         declare
            Dummy : C.int;
         begin
            Dummy := Net.C_Close (Peer);
         end;
   end Connection_Handler;

   -----------
   -- Leave --
   -----------

   procedure Leave (PID : Partition_ID) is
   begin
      Enter (Socket_Table_Mutex);
      pragma Debug (D (D_Debug, "Enter unlock of partition" & PID'Img));
      Socket_Table (PID).Locked := False;
      Update (Socket_Table_Watcher);
      Leave (Socket_Table_Mutex);
   end Leave;

   ----------------------
   -- Physical_Receive --
   ----------------------

   procedure Physical_Receive
     (Peer : in C.int;
      Data : out Stream_Element_Array)
   is
      Addr : System.Address := Data'Address;
      Size : C.int          := Data'Length;
      Code : C.int;
   begin
      while Size > 0 loop
         Code := Net.C_Recv (Peer, To_Chars_Ptr (Addr), Size, 0);
         if Code <= 0 then
            Code := Net.C_Close (Peer);
            Raise_Communication_Error ("Read error");
         end if;

         Addr := Addr + Storage_Offset (Code);
         Size := Size - Code;
      end loop;
   end Physical_Receive;

   -------------------
   -- Physical_Send --
   -------------------

   procedure Physical_Send
     (Peer   : in C.int;
      Stream : access Stream_Element_Array;
      First  : in Stream_Element_Count)
   is
      Addr : System.Address := Stream (First)'Address;
      Size : C.int          := C.int (Stream'Last - First + 1);
      Code : C.int;
   begin
      while Size > 0 loop
         Code := Net.C_Send (Peer, To_Chars_Ptr (Addr), Size, 0);
         if Code <= 0 then
            Code := Net.C_Close (Peer);
            Raise_Communication_Error ("Write error");
         end if;

         Addr := Addr + Storage_Offset (Code);
         Size := Size - Code;
      end loop;
   end Physical_Send;

   -----------------
   -- Read_Banner --
   -----------------

   function Read_Banner (Peer : C.int)
     return Banner_Kind
   is
      Stream : Banner_Stream;
      Result : Banner_Kind;
   begin
      pragma Debug (D (D_Debug, "Will receive code from peer" & Peer'Img));
      Physical_Receive (Peer, Stream);
      Result := Banner_Kind'Val (Stream (1));
      if not Result'Valid then
         Result := Junk_Banner;
      end if;
      for I in 2 .. Stream'Last loop
         if Stream (I) /= Stream (1) then
            Result := Junk_Banner;
            exit;
         end if;
      end loop;
      pragma Debug (D (D_Debug, "Received from peer code " & Result'Img));
      return Result;
   exception
      when Constraint_Error =>
         return Junk_Banner;
   end Read_Banner;

   --------------
   -- Read_SEC --
   --------------

   function Read_SEC (Peer : C.int) return Stream_Element_Count
   is
      Stream : SEC_Stream;
   begin
      Physical_Receive (Peer, Stream);
      return
        Stream_Element_Count (Stream (1)) * 256 ** 3 +
        Stream_Element_Count (Stream (2)) * 256 ** 2 +
        Stream_Element_Count (Stream (3)) * 256 +
        Stream_Element_Count (Stream (4));
   end Read_SEC;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access TCP_Protocol;
      Partition : in Partition_ID;
      Data      : access Stream_Element_Array)
   is
      Peer  : Socket_Type renames Socket_Table (Partition);
      Hits  : Natural := 1;
      First : Stream_Element_Count := Data'First + Unused_Space;
      Count : Stream_Element_Count;
   begin
      Enter (Partition);
      if not Peer.Defined then
         Peer.Location := Split_Data (Get_Data (Location (Partition)));

         pragma Debug
           (D (D_Communication,
               "Try to connect to " & Image (Peer.Location.Addr) &
               " port" & Peer.Location.Port'Img));

         if Partition = Boot_PID then
            Hits := Options.Connection_Hits;
         end if;

         while Hits > 0 loop
            pragma Debug
              (D (D_Communication,
                  "Trying to connect to partition" & Partition'Img));

            Peer.Socket  := Do_Connect (Peer.Location);
            if Peer.Socket /= Failure then
               Peer.Defined := True;
               exit;
            end if;

            delay 2.0;
            Hits := Hits - 1;
         end loop;

         if Hits = 0 then
            pragma Debug
              (D (D_Communication,
                  "Cannot connect to partition" & Partition'Img));
            Leave (Partition);

            Raise_Communication_Error
              ("Cannot connect to partition" & Partition'Img);
         end if;

         pragma Debug
           (D (D_Communication,
               "Connected to partition" & Partition'Img));

         --  Now create a task to get data on this connection

         declare
            Handler : Connection_Handler_Access;
         begin
            Handler := new Connection_Handler (Peer.Socket, Partition);
         end;

      end if;

      --  Write length at the beginning of the data, then the header.

      Count := Banner_Size;
      First := First - Count;
      Data (First .. First + Count - 1)
        := To_Stream_Element_Array (Data'Length - Unused_Space);

      Count := Banner_Size;
      First := First - Count;
      Data (First .. First + Count - 1) := Data_Stream;

      pragma Debug
        (D (D_Debug,
            "Sending packet of length" &
            Stream_Element_Count'Image (Data'Last - First + 1) &
            " (content of" &
            Stream_Element_Count'Image (Data'Length - Unused_Space) &
            ")"));

      Dump (D_Debug, Data, Private_Debug_Key);
      Physical_Send (Peer.Socket, Data, First);

      Leave (Partition);
   end Send;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol         : access TCP_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String := "")
   is
      Self : Socket_Type renames Socket_Table (Last_PID);
      Boot : Socket_Type renames Socket_Table (Boot_PID);
   begin
      Enter (Last_PID);
      Enter (Boot_PID);
      Self.Location := Split_Data (Host_Name);
      if Is_Boot_Protocol then
         Boot.Location := Split_Data (Boot_Data);
         if Options.Boot_Partition then
            Self.Location.Port := Boot.Location.Port;
         end if;
      end if;
      Leave (Last_PID);
      Leave (Boot_PID);
      if not Can_Have_A_Light_Runtime then
         pragma Debug (D (D_Debug, "Starting an acceptor task"));
         Do_Listen;
         Acceptor := new Accept_Handler;
      else
         pragma Debug (D (D_Debug, "No acceptor task, light runtime"));
         null;
      end if;
   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access TCP_Protocol) is
      Peer : C.int;
   begin

      pragma Debug (D (D_Debug, "Initiating connections shutdown"));

      --  The following loop tries to send a QUIT message to any known
      --  partition so that it releases its socket to be able to perform
      --  the shutdown operation on its end.

      Enter (Socket_Table_Mutex);

      for P in Boot_PID .. Last_PID - 1 loop
         if Socket_Table (P).Defined then
            Peer := Socket_Table (P).Socket;
            begin
               Physical_Send (Peer, Quit_Stream'Access, Quit_Stream'First);
               Peer := Net.C_Close (Peer);
            exception
               when Communication_Error => null;
            end;
         end if;
      end loop;

      Leave (Socket_Table_Mutex);

      pragma Debug (D (D_Debug, "Sending a close message to myself"));

      --  Send the same message on a new connection to the partition itself
      --  so that the accept gets the message.

      declare
         Back : C.int;
         Self : Socket_Type renames Socket_Table (Last_PID);
      begin
         for I in 1 .. 5 loop
            pragma Debug (D (D_Debug, "Connect to signal termination"));
            Back := Do_Connect (Self.Location);
            exit when Back /= Failure;
            delay 1.0;
         end loop;
         Physical_Send (Back, Quit_Stream'Access, Quit_Stream'First);
         Back := Net.C_Close (Back);
      exception
         when Communication_Error => null;
      end;

      Destroy (Socket_Table_Mutex);
      Shutdown_Completed := True;

      pragma Debug (D (D_Debug, "Shutdown completed"));
   end Shutdown;

   ----------------
   -- Split_Data --
   ----------------

   function Split_Data (Data : String) return Host_Location is
      Result : Host_Location;
   begin
      if Data = "" then
         return Result;
      end if;
      for I in Data'Range loop
         if Data (I) = ':' then
            Result.Addr := Address_Of (Data (Data'First .. I - 1));
            Result.Port := C.unsigned_short'Value (Data (I + 1 .. Data'Last));
            return Result;
         end if;
      end loop;
      Result.Addr := Address_Of (Data);
      return Result;
   end Split_Data;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (Count : Stream_Element_Count)
     return SEC_Stream
   is
   begin
      return (1 => Stream_Element (Count / 256 ** 3),
              2 => Stream_Element ((Count / 256 ** 2) mod 256),
              3 => Stream_Element ((Count / 256) mod 256),
              4 => Stream_Element (Count mod 256));
   end To_Stream_Element_Array;

end System.Garlic.TCP;
