------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--          S Y S T E M . G A R L I C . P R O T O C O L S . T C P           --
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

with GNAT.OS_Lib;                         use GNAT.OS_Lib;
with GNAT.Sockets;                        use GNAT.Sockets;

with Ada.Exceptions;                      use Ada.Exceptions;

with System.Garlic.Debug;                 use System.Garlic.Debug;
with System.Garlic.Exceptions;            use System.Garlic.Exceptions;
with System.Garlic.Heart;                 use System.Garlic.Heart;
with System.Garlic.Options;
with System.Garlic.Partitions;            use System.Garlic.Partitions;
with System.Garlic.Physical_Location;     use System.Garlic.Physical_Location;
with System.Garlic.Platform_Specific;
with System.Garlic.Protocols;             use System.Garlic.Protocols;
with System.Garlic.Soft_Links;
with System.Garlic.Streams;               use System.Garlic.Streams;
with System.Garlic.Table;
with System.Garlic.Types;                 use System.Garlic.Types;
with System.Garlic.Utils;                 use System.Garlic.Utils;

with System.Storage_Elements;             use System.Storage_Elements;

package body System.Garlic.Protocols.Tcp is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GAPRTC", "(s-gaprtc): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use Ada.Streams, System.Garlic.Protocols, System.Garlic.Types;

   subtype Error_Type is Exceptions.Error_Type;

   --  Initialize can be invoked several times to register several
   --  self locations. But some initializations of this unit should
   --  happen only once. Initialize has also to be invoked at least
   --  once.

   Activated   : Boolean := False;
   Initialized : Boolean := False;

   type Socket_Info is record
      Sock_Addr : Sock_Addr_Type;
      Socket    : Socket_Type;
      Any_Port  : Boolean;
   end record;

   Null_Socket_Info : constant Socket_Info
     := (No_Sock_Addr, No_Socket, False);

   --  We need a table to maintain all the outgoing connections. In case
   --  of Shutdown and concurrent connections, we may have concurrent
   --  accesses. We need a special table for this.

   package Outgoings is
     new System.Garlic.Table.Medium
        (Partition_ID,
         Null_PID,
         First_PID,
         Partition_ID_Increment,
         Partition_ID_Increment,
         Socket_Info,
         Null_Socket_Info);

   --  There should not be any concurrent access on this table.

   Max_Incoming   : constant := 5;
   Null_Incoming  : constant := 0;
   First_Incoming : constant := 1;
   Incomings      : array (First_Incoming .. Max_Incoming) of Socket_Info;
   Last_Incoming  : Natural := Null_Incoming;

   Allocate_Acceptor_Task  : Allocate_Acceptor_Procedure;
   Allocate_Connector_Task : Allocate_Connector_Procedure;

   type Banner_Kind is (Junk_Banner, Data_Banner, Quit_Banner);
   --  Various headers that can be performed on a communication link

   Banner_Size : constant := 4;
   --  Size of a header when it is encoded as a stream

   subtype Banner_Stream is Stream_Element_Array (1 .. Banner_Size);
   --  Constrained subtype for headers

   procedure Read_Banner
     (Peer   : in Socket_Type;
      Banner : out Banner_Kind);
   pragma Inline (Read_Banner);
   --  Read header from a file descriptor or return Junk_Banner if the
   --  header is not understood.

   SEC_Size : constant := 4;
   --  Size of a Stream_Element_Count when it is encoded as a stream

   subtype SEC_Stream is Stream_Element_Array (1 .. SEC_Size);
   --  Constrained subtype for stream element counts

   procedure Read_SEC
     (Peer  : in Socket_Type;
      Count : out Stream_Element_Count;
      Error : in out Error_Type);
   --  Read a stream element count from a file descriptor and check that
   --  it is valid.

   procedure Close_Socket_Ignoring_Error (S : Socket_Type);
   --  Close socket and ignore any exception.

   function To_Stream_Element_Array (Count : Stream_Element_Count)
     return SEC_Stream;
   --  Return the stream element array corresponding to this count

   function Do_Connect (Sock_Addr : Sock_Addr_Type) return Socket_Type;
   --  Establish a socket to a remote location and return the file descriptor

   procedure Do_Listen
     (Index : in Natural;
      Error : in out Error_Type);
   --  Establish a socket according to the information in Self_Host (and
   --  complete it if needed).

   procedure Receive_One_Stream
     (Peer  : in Socket_Type;
      PID   : in out Partition_ID;
      Error : in out Error_Type);

   procedure Receive
     (Peer  : in Socket_Type;
      Data  : access Stream_Element_Array;
      Error : in out Error_Type);
   pragma Inline (Receive);
   pragma Export (Ada, Receive, "GLADE_Physical_Receive");

   procedure Send
     (Peer  : in Socket_Type;
      Data  : access Stream_Element_Array;
      From  : in Stream_Element_Count;
      Error : in out Error_Type);
   pragma Inline (Send);
   pragma Export (Ada, Send, "GLADE_Physical_Send");
   --  Receive and send data. Receive loops as long as Data has not
   --  been filled and Send as long as everything has not
   --  been sent. These two procedures are exported so that they can
   --  be used in profiling programs without being in the spec.

   function Value (Image : String) return Sock_Addr_Type;

   Data_Stream : aliased Stream_Element_Array
     := (1 .. Banner_Size => Banner_Kind'Pos (Data_Banner));

   Quit_Stream : aliased Stream_Element_Array
     := (1 .. Banner_Size => Banner_Kind'Pos (Quit_Banner));

   Self_Reference   : Protocol_Access;

   -------------------------
   -- Accept_Until_Closed --
   -------------------------

   procedure Accept_Until_Closed
     (Incoming : in Natural) is
   begin
      loop
         declare
            Peer    : Socket_Type;
            Banner  : Banner_Kind;
            Address : Sock_Addr_Type := No_Sock_Addr;
         begin
            Soft_Links.Add_Non_Terminating_Task;
            begin
               Accept_Socket (Incomings (Incoming).Socket, Peer, Address);
            exception when Socket_Error =>
               Peer := No_Socket;
            end;
            Soft_Links.Sub_Non_Terminating_Task;
            exit when Peer = No_Socket;
            pragma Debug (D ("Accept Handler: have new peer" & Image (Peer)));

            --  Read a code from the file descriptor to know what to do
            --  next.

            Read_Banner (Peer, Banner);
            case Banner is
               when Junk_Banner =>
                  --  Acceptor can safely close this peer because it
                  --  is still local to acceptor.

                  pragma Debug (D ("Accept Handler: receive junk banner"));
                  Close_Socket_Ignoring_Error (Peer);

               when Data_Banner =>
                  --  Get a new task to handle this new connection

                  pragma Debug (D ("Accept Handler: receive data banner"));
                  Set_Socket_Option (Peer, Option => (Keep_Alive, True));
                  Allocate_Connector_Task (Peer, Null_PID);

               when Quit_Banner =>
                  --  Acceptor can safely close this peer because it
                  --  is still local to acceptor.

                  pragma Debug (D ("Accept Handler: receive quit banner"));
                  Close_Socket_Ignoring_Error (Peer);
                  exit;
            end case;
         end;
      end loop;

      pragma Debug (D ("close incoming" & Incoming'Img));

      --  Protect against multiple attempts to close socket and
      --  prevent any other task from using Incomings (Incoming).Socket
      --  value. The issue is that a task may keep a copy of this file
      --  descriptor. In the meantime, if this file descriptor is
      --  closed and then reallocated for a connection with another
      --  connection, the task will use its copy to communicate with
      --  the wrong partition.

      Outgoings.Enter;
      if Incomings (Incoming).Socket /= No_Socket then
         Close_Socket_Ignoring_Error (Incomings (Incoming).Socket);
         Incomings (Incoming).Socket := No_Socket;
         Outgoings.Update;
      end if;
      Outgoings.Leave;
   end Accept_Until_Closed;

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Protocol : access TCP_Protocol;
      Error    : in out Error_Type)
   is
   begin
      if Activated then
         return;
      end if;

      Activated := True;

      if Options.Is_Pure_Client then
         return;
      end if;

      for I in First_Incoming .. Last_Incoming loop
         pragma Debug (D ("Start acceptor task on" &
                          Image (Incomings (I).Sock_Addr)));

         Allocate_Acceptor_Task (I);
      end loop;

      pragma Debug (D ("Protocol tcp activated"));
   end Activate;

   ---------------------------------
   -- Close_Socket_Ignoring_Error --
   ---------------------------------

   procedure Close_Socket_Ignoring_Error (S : Socket_Type) is
   begin
      Close_Socket (S);
   exception when others =>
      null;
   end Close_Socket_Ignoring_Error;

   ------------
   -- Create --
   ------------

   function Create return Protocol_Access is
   begin
      if Self_Reference /= null then
         return null;
      end if;
      Self_Reference := new TCP_Protocol;
      return Self_Reference;
   end Create;

   ----------------
   -- Do_Connect --
   ----------------

   function Do_Connect (Sock_Addr : Sock_Addr_Type) return Socket_Type
   is
      Peer : Socket_Type    := No_Socket;
      Addr : Sock_Addr_Type := Sock_Addr;

   begin
      begin
         Create_Socket (Peer);
         Connect_Socket (Peer, Addr);
      exception when Socket_Error =>
         pragma Debug (D ("Cannot connect to " & Image (Sock_Addr)));
         if Peer /= No_Socket then
            Close_Socket_Ignoring_Error (Peer);
         end if;
         Peer := No_Socket;
      end;
      return Peer;
   end Do_Connect;

   ---------------
   -- Do_Listen --
   ---------------

   procedure Do_Listen
     (Index : in Natural;
      Error : in out Error_Type)
   is
      Self  : Socket_Info renames Incomings (Index);

   begin
      begin
         Create_Socket (Self.Socket);
      exception when Socket_Error =>
         Throw (Error, "Do_Listen: tcp socket error");
         return;
      end;

      Set_Socket_Option (Self.Socket, Option => (Reuse_Address, True));

      begin
         Bind_Socket (Self.Socket, Self.Sock_Addr);
      exception when Socket_Error =>
         Close_Socket_Ignoring_Error (Self.Socket);
         Self.Socket := No_Socket;
         Throw (Error, "Do_Listen: tcp bind error");
         return;
      end;

      begin
         Listen_Socket (Self.Socket);
      exception when Socket_Error =>
         Close_Socket_Ignoring_Error (Self.Socket);
         Self.Socket := No_Socket;
         Throw (Error, "Do_Listen: tcp listen error");
         return;
      end;

      if Self.Sock_Addr.Port = Any_Port then
         begin
            Self.Any_Port       := True;
            Self.Sock_Addr.Port := Get_Socket_Name (Self.Socket).Port;
         exception when Socket_Error =>
            Close_Socket_Ignoring_Error (Self.Socket);
            Self.Socket := No_Socket;
            Throw (Error, "Do_Listen: tcp getsockname error");
            return;
         end;
      end if;

      pragma Debug (D ("Listen on port" & Self.Sock_Addr.Port'Img));
   end Do_Listen;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Protocol : access TCP_Protocol)
     return String_Array_Access
   is
      Result : String_Array_Access;
   begin
      if Options.Is_Pure_Client
        or else Last_Incoming = Null_Incoming
      then
         return null;
      end if;
      Result := new String_Array (First_Incoming .. Last_Incoming);
      for I in Result'Range loop
         Result (I) := new String'(Image (Incomings (I).Sock_Addr));
      end loop;
      return Result;
   end Get_Data;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Protocol : access TCP_Protocol)
     return String is
   begin
      return "tcp";
   end Get_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Protocol  : access TCP_Protocol;
      Self_Data : in String;
      Required  : in Boolean;
      Performed : out Boolean;
      Error     : in out Error_Type)
   is
      Host  : constant Sock_Addr_Type := Value (Host_Name);
      Self  : Socket_Info := Null_Socket_Info;
      Index : Natural;

   begin
      pragma Debug (D ("Try to initialize tcp with self data """ &
                       Self_Data & """"));

      --  If this new data is not really needed and if there are
      --  already other data available then skip this one.

      if not Required
        and then Last_Incoming /= Null_Incoming
      then
         pragma Debug (D ("Initialization not required and tcp empty"));

         Performed := False;
         return;
      end if;

      --  If anything goes wrong then the registration is not
      --  performed.

      Performed := False;

      if not Initialized then
         pragma Debug (D ("Initialize GNAT.sockets for protocol tcp"));
         Outgoings.Initialize;
         GNAT.Sockets.Initialize (Platform_Specific.Process_Blocking_IO);
         Initialized := True;
      end if;

      if Self_Data'Length /= 0 then

         Self.Sock_Addr := Value (Self_Data);

         --  When there is a self location to bind on, check that this
         --  location concerns the current host. Otherwise, there is a
         --  real problem.

         declare
            N1 : constant Inet_Addr_Type
              := Addresses (Get_Host_By_Address (Self.Sock_Addr.Addr), 1);
            N2 : constant Inet_Addr_Type
              := Addresses (Get_Host_By_Name ("localhost"), 1);
            N3 : constant Inet_Addr_Type
              := Addresses (Get_Host_By_Address (Host.Addr), 1);
         begin
            if N1 /= N2
              and then N1 /= N3
            then
               pragma Debug (D ("Address " & Image (N1) & " does not match:"));
               pragma Debug (D (" - " & Image (N2)));
               pragma Debug (D (" - " & Image (N3)));
               Ada.Exceptions.Raise_Exception
                 (Program_Error'Identity,
                  "Incorrect tcp self location: " & Self_Data);
            end if;
         end;

      else
         Self.Sock_Addr := Host;
      end if;

      if Options.Is_Pure_Client then
         return;
      end if;

      --  Skip this incoming location if it does already exist.

      Index := Null_Incoming;

      for I in First_Incoming .. Last_Incoming loop
         if Incomings (I).Sock_Addr.Port = Self.Sock_Addr.Port then
            pragma Debug
              (D ("Use already port" & Self.Sock_Addr.Port'Img));
            return;

         elsif Incomings (I).Any_Port then
            if Self.Sock_Addr.Port = Any_Port then
               pragma Debug
                 (D ("Use already port" & Self.Sock_Addr.Port'Img));
               return;
            end if;

            pragma Debug
              (D ("Close port" & Self.Sock_Addr.Port'Img));

            begin
               Index := I;
               Close_Socket_Ignoring_Error (Incomings (I).Socket);
            exception when others =>
               null;
            end;
         end if;
      end loop;

      if Index = Null_Incoming then
         Last_Incoming := Last_Incoming + 1;
         Index := Last_Incoming;
      end if;

      Incomings (Index) := Self;
      Do_Listen (Index, Error);

      if Found (Error) then
         return;
      end if;

      Performed := True;

      pragma Debug (D ("New self data """ & Self_Data & """ accepted"));
   end Initialize;

   -----------------
   -- Read_Banner --
   -----------------

   procedure Read_Banner
     (Peer   : in Socket_Type;
      Banner : out Banner_Kind)
   is
      Data   : aliased Stream_Element_Array := (1 .. Banner_Size => 0);
      Result : Banner_Kind;
      Error  : Error_Type;

   begin
      Receive (Peer, Data'Access, Error);
      if Found (Error) then
         Catch (Error);
         Banner := Junk_Banner;
         return;
      end if;
      Result := Banner_Kind'Val (Data (1));
      if not Result'Valid then
         Result := Junk_Banner;
         return;
      end if;
      for I in 2 .. Data'Last loop
         if Data (I) /= Data (1) then
            Result := Junk_Banner;
            exit;
         end if;
      end loop;
      pragma Debug (D ("Read banner " & Result'Img &
                       " from peer " & Image (Peer)));
      Banner := Result;
   exception
      when Constraint_Error =>
         Banner := Junk_Banner;
   end Read_Banner;

   --------------
   -- Read_SEC --
   --------------

   procedure Read_SEC
     (Peer   : in Socket_Type;
      Count  : out Stream_Element_Count;
      Error  : in out Error_Type)
   is
      Data : aliased Stream_Element_Array := (1 .. SEC_Size => 0);
   begin
      Receive (Peer, Data'Access, Error);
      if not Found (Error) then
         Count := Stream_Element_Count (Data (1)) * 256 ** 3 +
           Stream_Element_Count (Data (2)) * 256 ** 2 +
           Stream_Element_Count (Data (3)) * 256 +
           Stream_Element_Count (Data (4));
      end if;
   end Read_SEC;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Peer  : in Socket_Type;
      Data  : access Stream_Element_Array;
      Error : in out Error_Type)
   is
      First : Ada.Streams.Stream_Element_Offset := Data'First;
      Index : Ada.Streams.Stream_Element_Offset := First - 1;
      Last  : constant Ada.Streams.Stream_Element_Offset := Data'Last;

   begin
      loop
         begin
            Receive_Socket (Peer, Data (First .. Last), Index);

            --  Exit when all or zero data received. Zero means that
            --  the socket is closed.

            exit when Index < First or else Index = Last;
            First := Index + 1;
         exception when Socket_Error =>
            exit;
         end;
      end loop;
      if Index /= Last then
         Throw (Error,
                "Receive: peer =" & Image (Peer) &
                " and errno =" & Errno'Img);
      end if;
   end Receive;

   -------------
   -- Receive --
   -------------

   function Receive
     (Protocol  : access TCP_Protocol;
      Timeout   : Duration)
     return Boolean
   is
      RSet     : Socket_Set_Type;
      WSet     : Socket_Set_Type;
      Info     : Socket_Info;
      Done     : Boolean := False;
      Error    : Error_Type;
      PID      : Partition_ID;
      Selector : Selector_Type;
      Status   : Selector_Status;

   begin
      --  This procedure is called in a No_Tasking env. No need to
      --  protect against concurrent access.

      Empty (RSet);
      Empty (WSet);

      for O in First_PID .. Outgoings.Last loop
         Info := Outgoings.Get_Component (O);
         if Info.Socket /= No_Socket then
            pragma Debug (D ("include socket " & Image (Info.Socket)));

            Set (RSet, Info.Socket);
         end if;
      end loop;

      if Is_Empty (RSet) then
         return True;
      end if;

      Create_Selector (Selector);
      Check_Selector  (Selector, RSet, WSet, Status, Timeout);
      Close_Selector  (Selector);
      pragma Debug (D ("select returned with status " & Status'Img));

      if Status = Expired then
         Empty (RSet);
         Empty (WSet);
         return False;
      end if;

      for O in First_PID .. Outgoings.Last loop
         Info := Outgoings.Get_Component (O);
         if Info.Socket /= No_Socket
           and then Is_Set (RSet, Info.Socket)
         then
            pragma Debug (D ("something ready from partition" & O'Img));

            Done := True;
            PID  := O;
            Receive_One_Stream (Info.Socket, PID, Error);
            if Found (Error) then
               Close_Socket_Ignoring_Error (Info.Socket);
               Info.Socket := No_Socket;
               Outgoings.Set_Component (PID, Info);
               Set_Online (PID, False);
               Outgoings.Update;
               Notify_Partition_Error (PID);
            end if;
         end if;
      end loop;

      Empty (RSet);
      Empty (WSet);
      return Done;
   end Receive;

   ------------------------
   -- Receive_One_Stream --
   ------------------------

   procedure Receive_One_Stream
     (Peer  : in Socket_Type;
      PID   : in out Partition_ID;
      Error : in out Error_Type)
  is
      Old_PID    : Partition_ID;
      Length     : Stream_Element_Count;
      Filtered   : Stream_Element_Access;
      Unfiltered : Stream_Element_Access;
      Opcode     : Any_Opcode;
      Banner     : Banner_Kind;

   begin
      if PID /= Null_PID then
         Soft_Links.Add_Non_Terminating_Task;
         Read_Banner (Peer, Banner);
         Soft_Links.Sub_Non_Terminating_Task;

         case Banner is
            when Junk_Banner =>
               Throw (Error, "Connect_Handler: junk banner");

            when Data_Banner =>
               null;

            when Quit_Banner =>
               Throw (Error, "Connect_Handler: quit banner");

         end case;
      end if;
      if Found (Error) then
         return;
      end if;

      Read_SEC (Peer, Length, Error);
      if Found (Error) then
         return;
      end if;

      pragma Debug (D ("Recv" & Length'Img & " bytes from peer " &
                       Image (Peer) & " (pid =" & PID'Img & ")"));

      Filtered := new Stream_Element_Array (1 .. Length);
      Receive (Peer, Filtered, Error);
      if Found (Error) then
         return;
      end if;

      Old_PID := PID;
      Analyze_Stream
        (PID,
         Self_Reference,
         Opcode,
         Unfiltered,
         Filtered,
         0, Error);
      if Found (Error) then
         return;
      end if;

      if Old_PID /= PID then
         Soft_Links.Activity_Detected;

         Outgoings.Enter;

         if Old_PID = Null_PID then
            pragma Debug (D ("Task handling partition" & PID'Img));

            declare
               New_Info : Socket_Info;
            begin
               New_Info := Outgoings.Get_Component (PID);
               New_Info.Socket := Peer;
               Outgoings.Set_Component (PID, New_Info);
               Set_Online (PID, True);
            end;

         else
            pragma Debug
              (D ("Task handling partition" & PID'Img &
                  " and no longer handling partition" & Old_PID'Img));

            declare
               Old_Info : Socket_Info;
            begin
               Old_Info := Outgoings.Get_Component (Old_PID);
               Outgoings.Set_Component (PID, Old_Info);
               Old_Info.Socket := No_Socket;
               Outgoings.Set_Component (Old_PID, Old_Info);
               Set_Online (Old_PID, False);
               Set_Online (PID, True);
            end;
         end if;

         Outgoings.Update;
         Outgoings.Leave;
      end if;

      Process_Stream (PID, Opcode, Unfiltered, Error);

      if Filtered /= null then
         Free (Filtered);
      end if;
      if Unfiltered /= null then
         Free (Unfiltered);
      end if;
   end Receive_One_Stream;

   --------------------------
   -- Receive_Until_Closed --
   --------------------------

   procedure Receive_Until_Closed
     (Peer : in Socket_Type;
      PID  : in out Partition_ID)
   is
      Error      : Error_Type;

   begin
      while not Found (Error) loop
         Receive_One_Stream (Peer, PID, Error);
      end loop;
      Catch (Error);

      --  If this connection is broken before partition
      --  identification, then try to rescue PID especially for the
      --  boot server. ???: When does this happen?

      if PID = Null_PID then
         PID := Boot_PID;
      end if;

      --  Protect against multiple attempts to close socket and
      --  prevent any other task from using Incomings (N).Socket
      --  value. The issue is that a task may keep a copy of this file
      --  descriptor. In the meantime, if this file descriptor is
      --  closed and then reallocated for a connection with another
      --  connection, the task will use its copy to communicate with
      --  the wrong partition.

      declare
         Info  : Socket_Info;
      begin
         Outgoings.Enter;
         Info := Outgoings.Get_Component (PID);
         if Info.Socket /= No_Socket then
            Close_Socket_Ignoring_Error (Info.Socket);
            Info.Socket := No_Socket;
            Outgoings.Set_Component (PID, Info);
            Set_Online (PID, False);
            Outgoings.Update;
         end if;
         Outgoings.Leave;
      end;

      --  Signal to the heart that we got an error on this partition

      pragma Debug
        (D ("Notify error on pid" & PID'Img));

      Notify_Partition_Error (PID);
   end Receive_Until_Closed;

   ------------------------
   -- Register_Task_Pool --
   ------------------------

   procedure Register_Task_Pool
     (Allocate_Acceptor  : in Allocate_Acceptor_Procedure;
      Allocate_Connector : in Allocate_Connector_Procedure) is
   begin
      Allocate_Acceptor_Task  := Allocate_Acceptor;
      Allocate_Connector_Task := Allocate_Connector;
   end Register_Task_Pool;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access TCP_Protocol;
      Partition : in Partition_ID;
      Data      : access Stream_Element_Array;
      Error     : in out Error_Type)
   is
      Info     : Socket_Info;
      Hits     : Natural := 1;
      First    : Stream_Element_Count := Data'First + Unused_Space;
      Count    : Stream_Element_Count;
      Location : Location_Type;
   begin
      Outgoings.Enter;
      Info := Outgoings.Get_Component (Partition);
      if Info.Socket = No_Socket then
         Get_Net_Location (Partition, Location, Error);
         if Found (Error) then
            Outgoings.Leave;
            return;
         end if;

         Info.Sock_Addr := Value (Get_Data (Location));

         if Info.Sock_Addr = No_Sock_Addr then
            Throw (Error, "Send: Cannot connect with peer without location");
            return;
         end if;

         if Partition = Boot_PID then
            Hits := Options.Connection_Hits;
         end if;

         while not Shutdown_Activated
           and then Hits > 0
         loop
            Info.Socket := Do_Connect (Info.Sock_Addr);
            exit when Info.Socket /= No_Socket;

            delay 4 * Polling;
            Hits := Hits - 1;
         end loop;

         if Info.Socket = No_Socket then
            Outgoings.Leave;
            if Options.Partition_Name /= null then
               Throw (Error, "Send: Cannot connect to" & Partition'Img &
                      " " & Options.Partition_Name.all);
            else
               Throw (Error, "Send: Cannot connect to" & Partition'Img);
            end if;
            return;
         end if;

         Set_Socket_Option (Info.Socket, Option => (Keep_Alive, True));

         Outgoings.Set_Component (Partition, Info);
         Set_Online (Partition, True);
         Outgoings.Update;

         if Allocate_Connector_Task /= null then
            --  Now create a task to get data on this connection

            Allocate_Connector_Task (Info.Socket, Partition);
         end if;
      end if;

      --  Write length at the beginning of the data, then the header.

      Count := SEC_Size;
      First := First - Count;
      Data (First .. First + Count - 1)
        := To_Stream_Element_Array (Data'Length - Unused_Space);

      Count := Banner_Size;
      First := First - Count;
      Data (First .. First + Count - 1) := Data_Stream;

      pragma Debug
        (D ("Send bytes" & Stream_Element_Count'Image (Data'Last - First + 1) &
            " on peer " & Image (Info.Socket) &
            " (pid =" & Partition'Img & ")"));

      pragma Debug (Dump (Data, Private_Debug_Key));

      Send (Info.Socket, Data, First, Error);
      if Found (Error) then
         --  Protect against multiple attempts to close socket and
         --  prevent any other task from using Incomings (N).Socket
         --  value. The issue is that a task may keep a copy of this file
         --  descriptor. In the meantime, if this file descriptor is
         --  closed and then reallocated for a connection with another
         --  connection, the task will use its copy to communicate with
         --  the wrong partition.

         Info := Outgoings.Get_Component (Partition);
         if Info.Socket /= No_Socket then
            Close_Socket_Ignoring_Error (Info.Socket);
            Outgoings.Set_Component (Partition, Info);
            Set_Online (Partition, False);
            Outgoings.Update;
         end if;
      end if;

      Outgoings.Leave;
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Peer  : in Socket_Type;
      Data  : access Stream_Element_Array;
      From  : in Stream_Element_Count;
      Error : in out Error_Type)
   is
      First : Ada.Streams.Stream_Element_Offset := From;
      Index : Ada.Streams.Stream_Element_Offset := First - 1;
      Last  : constant Ada.Streams.Stream_Element_Offset := Data'Last;

   begin
      loop
         begin
            Send_Socket (Peer, Data (First .. Last), Index);

            --  Exit when all or zero data sent. Zero means that
            --  the socket is closed.

            exit when Index < First or else Index = Last;
            First := Index + 1;
         exception when Socket_Error =>
            exit;
         end;
      end loop;
      if Index /= Last then
         Throw (Error,
                "Send: peer =" & Image (Peer) &
                " and errno =" & Errno'Img);
      end if;
   end Send;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol  : access TCP_Protocol;
      Boot_Data : in String;
      Error     : in out Error_Type) is
   begin
      if not Initialized then
         pragma Debug (D ("Initialize protocol tcp"));
         Outgoings.Initialize;
         GNAT.Sockets.Initialize (Platform_Specific.Process_Blocking_IO);
         Initialized := True;
      end if;

      pragma Debug (D ("Setting boot data for protocol tcp"));

      --  If the Boot_PID (Boot_First) changes because the current
      --  partition is in fact a boot mirror (not Boot_First), then
      --  the connection task is in charge of the update. Note that
      --  this occurs as soon as the task receives a request from the
      --  real boot server, that means during initialization when
      --  there is no concurrent activity yet.

      Outgoings.Enter;
      declare
         Boot_Info : Socket_Info;
      begin
         Boot_Info := Outgoings.Get_Component (Boot_PID);
         if Boot_Data'Length /= 0 then
            Boot_Info.Sock_Addr := Value (Boot_Data);
         end if;
         Outgoings.Set_Component (Boot_PID, Boot_Info);
      end;
      Outgoings.Leave;
      pragma Debug (D ("Boot data set for protocol tcp"));
   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (Protocol : access TCP_Protocol)
   is
      Count   : Natural;
      Socket  : Socket_Type;
      Info    : Socket_Info;
      Error   : Error_Type;
      Version : Version_Id;

   begin
      if not Initialized then
         return;
      end if;

      --  Close all the incoming sockets. This will resume all the
      --  the pending accept and receive operations.

      Count := 0;
      Outgoings.Enter;
      for I in First_Incoming .. Last_Incoming loop
         if Incomings (I).Socket /= No_Socket then
            pragma Debug
              (D ("Connect peer to acceptor " &
                  Image (Incomings (I).Socket) &
                  " in order to shutdown"));

            for H in 1 .. Options.Connection_Hits loop
               Socket := Do_Connect (Incomings (I).Sock_Addr);
               exit when Socket /= No_Socket;
               delay 4 * Polling;
            end loop;

            pragma Debug
              (D ("Send QUIT to " &
                  Image (Incomings (I).Socket) &
                  " acceptor peer"));

            Send
              (Socket, Quit_Stream'Access, Quit_Stream'First, Error);
            Catch (Error);

            delay Polling;

            Close_Socket_Ignoring_Error (Socket);
            Count := Count + 1;
         end if;
      end loop;
      Outgoings.Leave (Version);

      --  We cannot have a rendez-vous with the acceptors to check
      --  that it has terminated because we do not maintain the list
      --  of acceptors. An acceptor is in charge of setting field
      --  Socket to No_Socket. This way it signals its termination.

      loop
         pragma Debug (D ("There are" & Count'Img & " acceptors alive"));
         Count := 0;
         Outgoings.Enter;
         for I in First_Incoming .. Last_Incoming loop
            if Incomings (I).Socket /= No_Socket then
               Count := Count + 1;
            end if;
         end loop;
         Outgoings.Leave (Version);
         exit when Count = 0;
         Outgoings.Differ (Version);
      end loop;

      pragma Debug (D ("Shutdown connectors"));

      --  Close all the outgoing sockets. This will resume all the
      --  the pending receive operations.

      Outgoings.Enter;
      for P in First_PID .. Outgoings.Last loop
         Info := Outgoings.Get_Component (P);
         if Info.Socket /= No_Socket then
            pragma Debug (D ("Send QUIT to connector " & Image (Info.Socket)));

            Send
              (Info.Socket,
               Quit_Stream'Access,
               Quit_Stream'First,
               Error);
            Catch (Error);

            delay Polling;

            pragma Debug (D ("Shutdown connector " & Image (Info.Socket)));

            Close_Socket_Ignoring_Error (Info.Socket);
            Info.Socket := No_Socket;
            Outgoings.Set_Component (P, Info);
         end if;
      end loop;
      Outgoings.Leave;

      pragma Debug (D ("Shutdown TCP acceptors"));

      Shutdown_Completed := True;

      pragma Debug (D ("TCP Shutdown completed"));
   end Shutdown;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (Count : Stream_Element_Count)
     return SEC_Stream is
   begin
      return (1 => Stream_Element (Count / 256 ** 3),
              2 => Stream_Element ((Count / 256 ** 2) mod 256),
              3 => Stream_Element ((Count / 256) mod 256),
              4 => Stream_Element (Count mod 256));
   end To_Stream_Element_Array;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Sock_Addr_Type is
   begin
      for J in reverse Image'Range loop
         if Image (J) = ':' then
            declare
               Host : constant String := Image (Image'First .. J - 1);
               Addr : Inet_Addr_Type;
               Port : Port_Type;

            begin
               Addr := Addresses (Get_Host_By_Name (Host), 1);
               Port := Port_Type'Value (Image (J +  1 .. Image'Last));
               return (Addr.Family, Addr, Port);
            end;
         end if;
      end loop;

      declare
         Addr : Inet_Addr_Type := Addresses (Get_Host_By_Name (Image), 1);

      begin
         return (Addr.Family, Addr, Any_Port);
      end;
   end Value;

end System.Garlic.Protocols.Tcp;
