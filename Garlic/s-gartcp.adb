------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                    S Y S T E M . G A R L I C . T C P                     --
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
      Socket   : C.int   := Failure;
      Locked   : Boolean := False;
   end record;

   Socket_Table : array (Valid_Partition_ID) of Socket_Type;
   Socket_Table_Mutex   : Mutex_Access   := Create;
   Socket_Table_Watcher : Watcher_Access := Create;

   Self : Socket_Type;

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
     := (1 .. Banner_Size => Banner_Kind'Pos (Data_Banner));

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
      PID  : Partition_ID)
   is
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
      pragma Debug (D (D_Debug, "Task Accept Handler is running"));

      --  Infinite loop on C_Accept

      Accept_Loop :
      loop
         declare
            Sin     : Sockaddr_In_Access := new Sockaddr_In;
            Length  : aliased C.int := Sin.all'Size / 8;
            Peer    : C.int;
            Handler : Connection_Handler_Access;
            Banner  : Banner_Kind;
            Result  : C.int;
         begin
            Sin.Sin_Family := Constants.Af_Inet;
            Add_Non_Terminating_Task;
            Peer := Net.C_Accept
              (Self.Socket, To_Sockaddr_Access (Sin), Length'Access);
            pragma Debug (D (D_Warning, "Accept new connection"));
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
                  pragma Debug
                    (D (D_Debug, "Accept Handler: Receive a junk banner"));
                  Result := Net.C_Close (Peer);

               when Data_Banner =>
                  pragma Debug
                    (D (D_Debug, "Accept Handler: Receive a data banner"));

                  --  Create a new task to handle this new connection

                  Handler := new Connection_Handler (Peer, Null_PID);

               when Quit_Banner =>
                  pragma Debug
                    (D (D_Debug, "Accept Handler: Receive a quit banner"));
                  Result := Net.C_Close (Peer);
                  exit Accept_Loop;

            end case;

         end;
      end loop Accept_Loop;
   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D (D_Exception, "Accept Handler: " &
                          Exception_Information (E)));
         raise;
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
      Port  : C.unsigned_short renames Self.Location.Port;
      Sin   : Sockaddr_In_Access := new Sockaddr_In;
      Check : Sockaddr_In_Access := new Sockaddr_In;
      Size  : aliased C.int := Check.all'Size / 8;
      Code  : C.int;
      One   : aliased C.int := 1;
   begin
      Self.Socket := C_Socket (Af_Inet, Sock_Stream, 0);
      if Self.Socket = Failure then
         Free (Sin);
         Free (Check);
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
         Raise_Communication_Error;
      end if;

      if C_Listen (Self.Socket, 15) = Failure then
         Raise_Communication_Error;
      end if;

      if Port = 0 then
         Code := C_Getsockname
           (Self.Socket, To_Sockaddr_Access (Check), Size'Access);
         if Code = Failure then
            Free (Sin);
            Free (Check);
            Raise_Communication_Error;
         end if;
         Port := Network_To_Port (Check.Sin_Port);
      end if;

      Free (Check);

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
            Leave (Socket_Table_Mutex);
            exit;
         end if;
         pragma Debug (D (D_Debug, "Postpone lock of partition" & PID'Img));
         Commit (Socket_Table_Watcher, Version);
         Leave  (Socket_Table_Mutex);
         Differ (Socket_Table_Watcher, Version);
      end loop;
      pragma Debug (D (D_Debug, "Lock partition" & PID'Img));
   end Enter;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (Protocol  : access TCP_Protocol) return String
   is
      Location : Host_Location renames Self.Location;
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

   ------------------------
   -- Connection_Handler --
   ------------------------

   task body Connection_Handler is
      Old_PID    : Partition_ID := PID;
      New_PID    : Partition_ID := PID;
      Length     : Stream_Element_Count;
      Filtered   : Stream_Element_Access;
      Unfiltered : Stream_Element_Access;
      Opcode     : Any_Opcode;
      Banner     : Banner_Kind;
   begin
      pragma Debug (D (D_Debug, "Task Connection Handler is running"));

      loop
         if New_PID /= Null_PID then
            Add_Non_Terminating_Task;
            Banner := Read_Banner (Peer);
            Sub_Non_Terminating_Task;

            case Banner is
               when Junk_Banner =>
                  Raise_Communication_Error
                    ("Connection Handler: Receive a junk banner");

               when Data_Banner =>
                  null;

               when Quit_Banner =>
                  Raise_Communication_Error
                    ("Connection Handler: Receive a quit banner");

            end case;
         end if;

         Length := Read_SEC (Peer);

         pragma Debug (D (D_Debug, "Receive a packet of length" & Length'Img));

         Filtered := new Stream_Element_Array (1 .. Length);
         Physical_Receive (Peer, Filtered.all);
         Old_PID := New_PID;
         Analyze_Stream (New_PID, Opcode, Unfiltered, Filtered);

         if Old_PID /= New_PID then
            Activity_Detected;

            if Old_PID = Null_PID then
               pragma Debug
                 (D (D_Warning, "Task handling partition" & New_PID'Img));
               null;
            else
               pragma Debug
                 (D (D_Warning,
                     "Task handling partition" & New_PID'Img &
                     " and no longer handling partition" & Old_PID'Img));
               null;
            end if;

            if Old_PID = Null_PID then
               Enter (New_PID);
               Socket_Table (New_PID).Socket := Peer;
               Leave (New_PID);
            else
               Enter (Old_PID);
               Enter (New_PID);
               Socket_Table (New_PID) := Socket_Table (Old_PID);
               Socket_Table (Old_PID).Socket := Failure;
               Leave (New_PID);
               Leave (Old_PID);
            end if;
         end if;

         Process_Stream (New_PID, Opcode, Unfiltered);
         Free (Filtered);
         Free (Unfiltered);
      end loop;

   exception

      when Communication_Error =>

         --  If this connection is broken before partition identification,
         --  then try to rescue New_PID especially for the boot server.

         if New_PID = Last_PID then
            New_PID := Boot_PID;
         end if;

         if New_PID /= Null_PID
           and then not Shutdown_In_Progress
         then
            --  Set the entry in the table correctly only we are not
            --  executing a shutdown operation. If we are, then
            --  it is likely that Partition_Map is already deallocated.

            Enter (New_PID);
            Socket_Table (New_PID).Socket := Failure;
            Leave (New_PID);

         end if;

         --  The remote end has closed the socket or a communication error
         --  occurred. In the case, the entry will be freed, except if we
         --  are terminating since this is not worth freeing anything.

         declare
            Dummy : C.int;
         begin
            Dummy := Net.C_Close (Peer);
         end;

         --  Signal to the heart that we got an error on this partition

         if New_PID = Null_PID then
            pragma Debug
              (D (D_Garlic,
                  "Task dying before determining remote Partition_ID"));
            null;
         elsif not Shutdown_In_Progress then
            pragma Debug
              (D (D_Garlic,
                  "Signaling that partition" & New_PID'Img &
                  " is now unavailable"));
            Remote_Partition_Error (New_PID);
         else
            pragma Debug (D (D_Garlic,
                             "Partition" & New_PID'Img &
                             " is now unavailable (because of shutdown)"));
            null;
         end if;

      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D (D_Exception, "Connection_Handler: " &
                          Exception_Information (E)));
         declare
            Dummy : C.int;
         begin
            Dummy := Net.C_Close (Peer);
         end;
   end Connection_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Protocol : access TCP_Protocol;
      Default  : in Utils.String_Access := null;
      Bootmode : in Boolean := False)
   is
   begin
      pragma Debug (D (D_Warning, "Initialize protocol TCP"));

      if Acceptor = null then
         Self.Location := Split_Data (Host_Name);
      end if;

      if Bootmode then
         Enter (Boot_PID);
         if Default /= null then
            Socket_Table (Boot_PID).Location := Split_Data (Default.all);
         end if;

         --  If this partition is the lead partition, then the bootmode is
         --  a normal mode. The default should be also used for the local
         --  connection.

         if Options.Boot_Partition then
            Self.Location.Port := Socket_Table (Boot_PID).Location.Port;
         end if;
         Leave (Boot_PID);
      end if;

      --  If this is the first time we initialize the protocol, start the
      --  Accept_Handler.

      if Acceptor = null  then
         if not Can_Have_A_Light_Runtime then
            pragma Debug (D (D_Warning, "Start acceptor task"));
            Do_Listen;
            Acceptor := new Accept_Handler;
         else
            pragma Debug (D (D_Warning, "No acceptor task, light runtime"));
            null;
         end if;
      end if;
      pragma Debug (D (D_Debug, "Protocol TCP initialized"));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (PID : Partition_ID) is
   begin
      Enter (Socket_Table_Mutex);
      pragma Debug (D (D_Debug, "Unlock partition" & PID'Img));
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
      pragma Debug (D (D_Debug, "Reading banner from peer" & Peer'Img));
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
      pragma Debug (D (D_Warning, "Read banner " & Result'Img));
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
      pragma Debug (D (D_Warning, "Send to partition" & Partition'Img));
      Enter (Partition);
      if Peer.Socket = Failure then
         Peer.Location := Split_Data (Get_Data (Location (Partition)).all);

         if Partition = Boot_PID then
            Hits := Options.Connection_Hits;
         end if;

         while Hits > 0 loop
            Peer.Socket  := Do_Connect (Peer.Location);
            exit when Peer.Socket /= Failure;

            delay 2.0;
            Hits := Hits - 1;
         end loop;

         if Hits = 0 then
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
        (D (D_Warning,
            "Send" &
            Stream_Element_Count'Image (Data'Last - First + 1) &
            " bytes (content of" &
            Stream_Element_Count'Image (Data'Length - Unused_Space) &
            " bytes) to partition" & Partition'Img &
            " on peer" & Peer.Socket'Img));

      Dump (D_Dump, Data, Private_Debug_Key);
      Physical_Send (Peer.Socket, Data, First);

      Leave (Partition);
   end Send;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access TCP_Protocol) is
      Peer : C.int;
   begin

      pragma Debug (D (D_Warning, "Remote connections shutdown"));

      --  The following loop tries to send a QUIT message to any known
      --  partition so that it releases its socket to be able to perform
      --  the shutdown operation on its end.

      Enter (Socket_Table_Mutex);

      for P in Socket_Table'Range loop
         if Socket_Table (P).Socket /= Failure then
            pragma Debug
              (D (D_Debug, "Partition" & P'Img & " peer is still alive"));
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

      pragma Debug (D (D_Warning, "Local connection shutdown"));

      --  Send the same message on a new connection to the partition itself
      --  so that the accept gets the message.

      if not Can_Have_A_Light_Runtime then
         declare
            Back : C.int;
         begin
            for I in 1 .. 5 loop
               Back := Do_Connect (Self.Location);
               exit when Back /= Failure;
               delay 1.0;
            end loop;
            Physical_Send (Back, Quit_Stream'Access, Quit_Stream'First);
            Back := Net.C_Close (Back);
         exception
            when Communication_Error => null;
         end;
      end if;

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
