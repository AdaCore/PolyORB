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
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with GNAT.OS_Lib;                     use GNAT.OS_Lib;
with Ada.Exceptions;                  use Ada.Exceptions;
with Interfaces.C.Strings;
pragma Warnings (Off, Interfaces.C.Strings);
with System.Garlic.Constants;             use System.Garlic.Constants;
with System.Garlic.Debug;                 use System.Garlic.Debug;
with System.Garlic.Exceptions;            use System.Garlic.Exceptions;
with System.Garlic.Heart;                 use System.Garlic.Heart;
with System.Garlic.Naming;                use System.Garlic.Naming;
with System.Garlic.TCP_Operations;
with System.Garlic.Network_Utilities;     use System.Garlic.Network_Utilities;
with System.Garlic.Options;
with System.Garlic.Partitions;            use System.Garlic.Partitions;
with System.Garlic.Physical_Location;     use System.Garlic.Physical_Location;
with System.Garlic.Protocols;             use System.Garlic.Protocols;
with System.Garlic.Soft_Links;
with System.Garlic.Streams;               use System.Garlic.Streams;
with System.Garlic.Table;
with System.Garlic.Thin;                  use System.Garlic.Thin;
with System.Garlic.Types;                 use System.Garlic.Types;
with System.Garlic.Utils;                 use System.Garlic.Utils;
with System.Garlic.TCP_Platform_Specific;
pragma Warnings (Off, System.Garlic.TCP_Platform_Specific);
with System.Storage_Elements;             use System.Storage_Elements;

package body System.Garlic.Protocols.Xyz is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GAPRTC", "(s-gaprtc): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use Ada.Streams, System.Garlic.Protocols, System.Garlic.Types;

   package C renames Interfaces.C;
   package Strings renames C.Strings;
   use type C.int;
   use type C.unsigned_short;
   --  Shortcuts

   package Net renames System.Garlic.TCP_Operations;

   --  Initialize can be invoked several times to register several
   --  self locations. But some initializations of this unit should
   --  happen only once. Initialize has also to be invoked at least
   --  once.

   Initialized : Boolean := False;

   In_Address_Any : constant Naming.Address := Any_Address;

   type Host_Location is record
      Addr : Naming.Address   := In_Address_Any;
      Port : C.unsigned_short := 0;
   end record;

   Null_Location : constant Host_Location := (In_Address_Any, 0);

   function Split_Data (Data : String) return Host_Location;
   --  Split a data given as <machine> or <machine>:<port>

   type Socket_Info is record
      Location : Host_Location;
      Socket   : C.int;
      Closed   : Boolean;
      Locked   : Boolean;
   end record;

   Null_Socket : constant Socket_Info := (Null_Location, Failure, True, False);

   --  We need a table to maintain all the outgoing connections. In case
   --  of Shutdown and concurrent connections, we may have concurrent
   --  accesses. We need a special table for this.

   package Outgoings is
     new System.Garlic.Table.Complex
        (Partition_ID,
         Null_PID,
         First_PID,
         Partition_ID_Increment,
         Partition_ID_Increment,
         Socket_Info,
         Null_Socket);

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
     (Peer   : in C.int;
      Banner : out Banner_Kind);
   pragma Inline (Read_Banner);
   --  Read header from a file descriptor or return Junk_Banner if the
   --  header is not understood.

   SEC_Size : constant := 4;
   --  Size of a Stream_Element_Count when it is encoded as a stream

   subtype SEC_Stream is Stream_Element_Array (1 .. SEC_Size);
   --  Constrained subtype for stream element counts

   procedure Read_SEC
     (Peer  : in C.int;
      Count : out Stream_Element_Count;
      Error : in out Error_Type);
   --  Read a stream element count from a file descriptor and check that
   --  it is valid.

   function To_Stream_Element_Array (Count : Stream_Element_Count)
     return SEC_Stream;
   --  Return the stream element array corresponding to this count

   function Do_Connect (Location : Host_Location) return C.int;
   --  Establish a socket to a remote location and return the file descriptor

   procedure Do_Listen
     (Index : in Natural;
      Error : in out Error_Type);
   --  Establish a socket according to the information in Self_Host (and
   --  complete it if needed).

   procedure Enter (PID : Partition_ID);
   procedure Leave (PID : Partition_ID);

   procedure Physical_Receive
     (Peer  : in C.int;
      Data  : access Stream_Element_Array;
      Error : in out Error_Type);
   pragma Inline (Physical_Receive);

   procedure Physical_Send
     (Peer   : in C.int;
      Stream : access Stream_Element_Array;
      First  : in Stream_Element_Count;
      Error  : in out Error_Type);
   pragma Inline (Physical_Send);
   --  Receive and send data. Physical_Receive loops as long as Data has
   --  not been filled and Physical_Send as long as everything has not been
   --  sent. These two procedures are exported so that they can be used in
   --  profiling programs without being in the spec.

   procedure Receive_One_Stream
     (Peer  : in C.int;
      PID   : in out Partition_ID;
      Error : in out Error_Type);

   Data_Stream : aliased Stream_Element_Array
     := (1 .. Banner_Size => Banner_Kind'Pos (Data_Banner));

   Quit_Stream : aliased Stream_Element_Array
     := (1 .. Banner_Size => Banner_Kind'Pos (Quit_Banner));

   Self_Reference   : Protocol_Access;

   ------------
   -- Create --
   ------------

   function Create return Protocol_Access is
   begin
      if Self_Reference /= null then
         return null;
      end if;
      Self_Reference := new XYZ_Protocol;
      return Self_Reference;
   end Create;

   -------------------------
   -- Accept_Until_Closed --
   -------------------------

   procedure Accept_Until_Closed
     (Incoming : in Natural) is
   begin
      loop
         declare
            Sin       : aliased Sockaddr_In;
            Length    : aliased C.int := Sin'Size / 8;
            Peer      : C.int;
            Banner    : Banner_Kind;
            Result    : C.int;
         begin
            Sin.Sin_Family := Constants.Af_Inet;
            Soft_Links.Add_Non_Terminating_Task;
            Peer := Net.C_Accept
              (Incomings (Incoming).Socket,
               Sin'Address,
               Length'Access);
            Soft_Links.Sub_Non_Terminating_Task;
            if Peer = Failure then
               exit;
            end if;
            pragma Debug (D ("Accept Handler: have new peer" & Peer'Img));

            --  Read a code from the file descriptor to know what to do
            --  next.

            Read_Banner (Peer, Banner);
            case Banner is
               when Junk_Banner =>
                  --  Acceptor can safely close this peer because it
                  --  is still local to acceptor.

                  pragma Debug (D ("Accept Handler: receive junk banner"));
                  Result := Net.C_Close (Peer);

               when Data_Banner =>
                  --  Get a new task to handle this new connection

                  pragma Debug (D ("Accept Handler: receive data banner"));
                  Allocate_Connector_Task (Peer, Null_PID);

               when Quit_Banner =>
                  --  Acceptor can safely close this peer because it
                  --  is still local to acceptor.

                  pragma Debug (D ("Accept Handler: receive quit banner"));
                  Result := Net.C_Close (Peer);
                  exit;
            end case;
         end;
      end loop;

      --  Protect against multiple attempts to close socket and
      --  prevent any other task from using Incomings (Incoming).Socket
      --  value. The issue is that a task may keep a copy of this file
      --  descriptor. In the meantime, if this file descriptor is
      --  closed and then reallocated for a connection with another
      --  connection, the task will use its copy to communicate with
      --  the wrong partition.

      declare
         Result : C.int;
      begin
         Outgoings.Enter;
         if not Incomings (Incoming).Closed then
            Result := Net.C_Close (Incomings (Incoming).Socket);
            Incomings (Incoming).Closed := True;
         end if;
         Incomings (Incoming).Socket := Failure;
         Outgoings.Update;
         Outgoings.Leave;
      end;
   end Accept_Until_Closed;

   ----------------
   -- Do_Connect --
   ----------------

   function Do_Connect (Location  : Host_Location) return C.int
   is
      Peer : C.int;
      Sin  : aliased Sockaddr_In;
      Code : C.int;
   begin
      Peer := Net.C_Socket (Af_Inet, Sock_Stream, 0);
      if Peer /= Failure then
         Sin.Sin_Family := Constants.Af_Inet;
         Sin.Sin_Addr := To_In_Addr (Location.Addr);
         Sin.Sin_Port := Port_To_Network (Location.Port);
         Code := Net.C_Connect
           (Peer, Sin'Address, Sin'Size / 8);
         if Code = Failure then
            pragma Debug
              (D ("Cannot connect to host " & Image (Location.Addr) &
                  " on port" & Location.Port'Img));
            Code := Net.C_Close (Peer);
            Peer := Failure;
         end if;
      end if;
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
      Port  : C.unsigned_short renames Self.Location.Port;
      Sin   : aliased Sockaddr_In;
      Check : aliased Sockaddr_In;
      Size  : aliased C.int := Check'Size / 8;
      Code  : C.int;
      One   : aliased C.int := 1;

   begin
      Self.Socket := Net.C_Socket (Af_Inet, Sock_Stream, 0);
      if Self.Socket = Failure then
         Outgoings.Update;
         Throw (Error, "Do_Listen: xyz socket error");
         return;
      end if;

      Code := C_Setsockopt
        (Self.Socket, Sol_Socket, So_Reuseaddr, One'Address, One'Size / 8);

      Sin.Sin_Family := Constants.Af_Inet;
      Sin.Sin_Port := Port_To_Network (Port);
      Code := C_Bind (Self.Socket, Sin'Address, Sin'Size / 8);
      if Code = Failure then
         Throw (Error, "Do_Listen: xyz bind error");
         return;
      end if;

      if C_Listen (Self.Socket, 15) = Failure then
         Throw (Error, "Do_Listen: xyz listen error");
         return;
      end if;

      if Port = 0 then
         Code := C_Getsockname
           (Self.Socket, Check'Address, Size'Access);
         if Code = Failure then
            Throw (Error, "Do_Listen: xyz getsockname error");
            return;
         end if;
         Port := Network_To_Port (Check.Sin_Port);
      end if;

      Self.Closed := False;

      pragma Debug (D ("Listen on port" & Port'Img));
   end Do_Listen;

   -------------
   -- Receive --
   -------------

   function Receive
     (Protocol  : access XYZ_Protocol;
      Timeout   : Milliseconds)
     return Boolean
   is
      Dummy   : C.int;
      Rfds    : aliased Fd_Set := 0;
      Sfds    : aliased Fd_Set := 0;
      Last    : C.int := 0;
      TVal    : aliased Timeval := (0, C.int (Timeout * 1000));
      TPtr    : Timeval_Access;
      Info    : Socket_Info;
      Done    : Boolean := False;
      Error   : Error_Type;
      PID     : Partition_ID;

   begin
      if Timeout = 0 then
         TPtr := null;
      else
         TPtr := TVal'Unchecked_Access;
      end if;
      Outgoings.Enter;
      for O in First_PID .. Outgoings.Last loop
         Info := Outgoings.Get_Component (O);
         if Info.Socket /= Failure then
            if Info.Closed then
               Info.Socket := Failure;
               Outgoings.Set_Component (O, Info);

            else
               pragma Debug (D ("include socket" & Info.Socket'Img));

               if Last < Info.Socket then
                  Last := Info.Socket;
               end if;
               Rfds := Rfds + 2 ** Integer (Info.Socket);
            end if;
         end if;
      end loop;
      Outgoings.Leave;

      if Last = 0 then
         return True;
      end if;

      Dummy := C_Select (Last + 1,
                         Rfds'Unchecked_Access,
                         Sfds'Unchecked_Access,
                         null,
                         TPtr);
      pragma Debug (D ("select returned error code" & Dummy'Img));

      if Dummy = Failure then
         return False;
      end if;

      for O in First_PID .. Outgoings.Last loop
         Info := Outgoings.Get_Component (O);
         if Info.Socket /= Failure
           and then (Rfds / 2 ** Natural (Info.Socket)) mod 2 = 1
         then
            pragma Debug (D ("something available from partition" & O'Img));

            Done := True;
            PID := O;
            Receive_One_Stream (Info.Socket, PID, Error);
            Catch (Error);
         end if;
      end loop;
      return Done;
   end Receive;

   ------------------------
   -- Receive_One_Stream --
   ------------------------

   procedure Receive_One_Stream
     (Peer  : in C.int;
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

      pragma Debug (D ("Recv" & Length'Img & " bytes from peer" &
                       Peer'Img & " (pid =" & PID'Img & ")"));

      Filtered := new Stream_Element_Array (1 .. Length);
      Physical_Receive (Peer, Filtered, Error);
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

         if Old_PID = Null_PID then
            pragma Debug (D ("Task handling partition" & PID'Img));

            Enter (PID);
            declare
               New_Info : Socket_Info;
            begin
               New_Info := Outgoings.Get_Component (PID);
               New_Info.Socket := Peer;
               New_Info.Closed := False;
               Outgoings.Set_Component (PID, New_Info);
               Set_Online (PID, True);
            end;
            Leave (PID);
         else
            pragma Debug
              (D ("Task handling partition" & PID'Img &
                  " and no longer handling partition" & Old_PID'Img));

            Enter (Old_PID);
            Enter (PID);
            declare
               Old_Info : Socket_Info;
            begin
               Old_Info := Outgoings.Get_Component (Old_PID);
               Outgoings.Set_Component (PID, Old_Info);
               Old_Info.Socket := Failure;
               Old_Info.Closed := True;
               Outgoings.Set_Component (Old_PID, Old_Info);
               Set_Online (Old_PID, False);
               Set_Online (PID, True);
            end;
            Outgoings.Update;
            Leave (PID);
            Leave (Old_PID);
         end if;
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
     (Peer : in C.int;
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

      Outgoings.Enter;
      declare
         Info  : Socket_Info;
         Dummy : C.int;
      begin
         Info := Outgoings.Get_Component (PID);
         if not Info.Closed then
            Dummy := Net.C_Close (Info.Socket);
            Info.Closed := True;
         end if;
         Info.Socket := Failure;
         Outgoings.Set_Component (PID, Info);
         Outgoings.Update;
         Set_Online (PID, False);
      end;
      Outgoings.Leave;

      --  Signal to the heart that we got an error on this partition

      pragma Debug
        (D ("Notify error on pid" & PID'Img));

      Notify_Partition_Error (PID);
   end Receive_Until_Closed;

   -----------
   -- Enter --
   -----------

   procedure Enter (PID : Partition_ID) is
      Version : Version_Id;
      Info    : Socket_Info;
   begin
      loop
         Outgoings.Enter;
         Info := Outgoings.Get_Component (PID);
         if not Info.Locked then
            Info.Locked := True;
            Outgoings.Set_Component (PID, Info);
            Outgoings.Leave;
            exit;
         end if;
         pragma Debug (D ("Postpone lock of partition" & PID'Img));
         Outgoings.Leave (Version);
         Outgoings.Differ (Version);
         pragma Debug (D ("Release postponed lock of partition" & PID'Img));
      end loop;
      pragma Debug (D ("Lock partition" & PID'Img));
   end Enter;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Protocol : access XYZ_Protocol)
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
         declare
            L : Host_Location renames Incomings (I).Location;
            P : constant String := L.Port'Img;
         begin
            Result (I)
              := new String'(Name_Of (Image (L.Addr)) & ":" & P (2 .. P'Last));
         end;
      end loop;
      return Result;
   end Get_Data;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Protocol : access XYZ_Protocol)
     return String is
   begin
      return "xyz";
   end Get_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Protocol  : access XYZ_Protocol;
      Self_Data : in String;
      Required  : in Boolean;
      Performed : out Boolean;
      Error     : in out Error_Type)
   is
      Host     : constant Host_Location := Split_Data (Host_Name);
      Self     : Socket_Info := Null_Socket;
   begin
      if not Required and then Last_Incoming /= Null_Incoming then
         Performed := False;
         return;
      end if;

      Performed := False;

      if Self_Data'Length /= 0 then
         --  When there is a self location to bind on, check that this
         --  location concerns the current host. Otherwise, there is a
         --  real problem.

         Self.Location := Split_Data (Self_Data);
         declare
            N1 : String := Name_Of (Image (Self.Location.Addr));
            N2 : String := Name_Of ("localhost");
            N3 : String := Name_Of (Image (Host.Addr));
         begin
            if N1 /= N2
              and then N1 /= N3
            then
               pragma Debug (D ("Self_Data " & N1 & " does not match:"));
               pragma Debug (D (" - " & N2));
               pragma Debug (D (" - " & N3));
               Ada.Exceptions.Raise_Exception
                 (Program_Error'Identity,
                  "Incorrect xyz self location: " & Self_Data);
            end if;
         end;
      else
         Self.Location := Host;
      end if;

      if not Initialized then
         pragma Debug (D ("Initialize protocol xyz"));
         Outgoings.Initialize;
         TCP_Operations.Initialize;
         Initialized := True;
      end if;

      if not Options.Is_Pure_Client then

         pragma Debug (D ("Start acceptor task"));

         --  Skip this incoming location if it does already exist.

         for I in First_Incoming .. Last_Incoming loop
            if Incomings (I).Location.Port = Self.Location.Port then
               return;
            end if;
         end loop;

         Last_Incoming := Last_Incoming + 1;
         Incomings (Last_Incoming) := Self;
         Do_Listen (Last_Incoming, Error);

         if Found (Error) then
            return;
         end if;

         Allocate_Acceptor_Task (Last_Incoming);
         Performed := True;
      else
         pragma Debug (D ("No acceptor task, light runtime"));
         null;
      end if;
      pragma Debug (D ("Protocol xyz initialized"));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (PID : in Partition_ID) is
      Info : Socket_Info;
   begin
      Outgoings.Enter;
      pragma Debug (D ("Unlock partition" & PID'Img));
      Info := Outgoings.Get_Component (PID);
      Info.Locked := False;
      Outgoings.Set_Component (PID, Info);
      Outgoings.Update;
      Outgoings.Leave;
   end Leave;

   ----------------------
   -- Physical_Receive --
   ----------------------

   procedure Physical_Receive
     (Peer  : in C.int;
      Data  : access Stream_Element_Array;
      Error : in out Error_Type)
   is
      Addr : System.Address := Data.all'Address;
      Size : C.int          := Data'Length;
      Code : C.int;
   begin
      while Size > 0 loop
         Code := Net.C_Recv (Peer, Addr, Size, 0);
         if Code <= 0 then
            Throw (Error,
                   "Physical_Receive: peer =" & Peer'Img &
                   " and errno =" & Errno'Img);
            return;
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
      First  : in Stream_Element_Count;
      Error  : in out Error_Type)
   is
      Addr : System.Address := Stream (First)'Address;
      Size : C.int          := C.int (Stream'Last - First + 1);
      Code : C.int;
   begin
      while Size > 0 loop
         Code := Net.C_Send (Peer, Addr, Size, 0);
         if Code <= 0 then
            Throw (Error,
                   "Physical_Send: peer =" & Peer'Img &
                   " and errno =" & Errno'Img);
            return;
         end if;

         Addr := Addr + Storage_Offset (Code);
         Size := Size - Code;
      end loop;
   end Physical_Send;

   -----------------
   -- Read_Banner --
   -----------------

   procedure Read_Banner
     (Peer   : in C.int;
      Banner : out Banner_Kind)
   is
      Stream : aliased Stream_Element_Array := (1 .. Banner_Size => 0);
      Result : Banner_Kind;
      Error  : Error_Type;
   begin
      Physical_Receive (Peer, Stream'Access, Error);
      if Found (Error) then
         Catch (Error);
         Banner := Junk_Banner;
         return;
      end if;
      Result := Banner_Kind'Val (Stream (1));
      if not Result'Valid then
         Result := Junk_Banner;
         return;
      end if;
      for I in 2 .. Stream'Last loop
         if Stream (I) /= Stream (1) then
            Result := Junk_Banner;
            exit;
         end if;
      end loop;
      pragma Debug (D ("Read banner " & Result'Img &
                       " from peer" & Peer'Img));
      Banner := Result;
   exception
      when Constraint_Error =>
         Banner := Junk_Banner;
   end Read_Banner;

   --------------
   -- Read_SEC --
   --------------

   procedure Read_SEC
     (Peer   : in C.int;
      Count  : out Stream_Element_Count;
      Error  : in out Error_Type)
   is
      Stream : aliased Stream_Element_Array := (1 .. SEC_Size => 0);
   begin
      Physical_Receive (Peer, Stream'Access, Error);
      if not Found (Error) then
         Count := Stream_Element_Count (Stream (1)) * 256 ** 3 +
           Stream_Element_Count (Stream (2)) * 256 ** 2 +
           Stream_Element_Count (Stream (3)) * 256 +
           Stream_Element_Count (Stream (4));
      end if;
   end Read_SEC;

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
     (Protocol  : access XYZ_Protocol;
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
      Enter (Partition);
      Info := Outgoings.Get_Component (Partition);
      if Info.Socket = Failure then
         Get_Net_Location (Partition, Location, Error);
         if Found (Error) then
            Leave (Partition);
            return;
         end if;

         Info.Location := Split_Data (Get_Data (Location));

         if Info.Location = Null_Location then
            Outgoings.Set_Component (Partition, Info);
            Throw (Error, "Send: Cannot connect with peer without location");
            return;
         end if;

         if Partition = Boot_PID then
            Hits := Options.Connection_Hits;
         end if;

         while not Shutdown_Activated
           and then Hits > 0
         loop
            Info.Socket := Do_Connect (Info.Location);
            exit when Info.Socket /= Failure;

            delay 2.0;
            Hits := Hits - 1;
         end loop;

         if Info.Socket = Failure then
            Leave (Partition);
            if Options.Partition_Name /= null then
               Throw (Error, "Send: Cannot connect to" & Partition'Img &
                      " " & Options.Partition_Name.all);
            else
               Throw (Error, "Send: Cannot connect to" & Partition'Img);
            end if;
            return;
         end if;

         Outgoings.Enter;
         Info.Closed := False;
         Outgoings.Set_Component (Partition, Info);
         Set_Online (Partition, True);
         Outgoings.Leave;

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
            " on peer" & Info.Socket'Img & " (pid =" & Partition'Img & ")"));

      pragma Debug (Dump (Data, Private_Debug_Key));

      Physical_Send (Info.Socket, Data, First, Error);
      if Found (Error) then
         --  Protect against multiple attempts to close socket and
         --  prevent any other task from using Incomings (N).Socket
         --  value. The issue is that a task may keep a copy of this file
         --  descriptor. In the meantime, if this file descriptor is
         --  closed and then reallocated for a connection with another
         --  connection, the task will use its copy to communicate with
         --  the wrong partition.

         declare
            Dummy : C.int;
         begin
            Outgoings.Enter;
            Info := Outgoings.Get_Component (Partition);
            if not Info.Closed then
               Dummy := Net.C_Close (Info.Socket);
               Info.Closed := True;
            end if;
            Outgoings.Set_Component (Partition, Info);
            Set_Online (Partition, False);
            Outgoings.Leave;
         end;
      end if;

      Leave (Partition);
   end Send;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol  : access XYZ_Protocol;
      Boot_Data : in String;
      Error     : in out Error_Type) is
   begin
      if not Initialized then
         Outgoings.Initialize;
         TCP_Operations.Initialize;
         Initialized := True;
      end if;

      pragma Debug (D ("Setting boot data for protocol xyz"));

      --  If the Boot_PID (Boot_First) changes because the current
      --  partition is in fact a boot mirror (not Boot_First), then
      --  the connection task is in charge of the update. Note that
      --  this occurs as soon as the task receives a request from the
      --  real boot server, that means during initialization when
      --  there is no concurrent activity yet.

      Enter (Boot_PID);
      declare
         Boot_Info : Socket_Info := Outgoings.Get_Component (Boot_PID);
      begin
         if Boot_Data'Length /= 0 then
            Boot_Info.Location := Split_Data (Boot_Data);
         end if;
         Outgoings.Set_Component (Boot_PID, Boot_Info);
      end;
      Leave (Boot_PID);
      pragma Debug (D ("Boot data set for protocol xyz"));
   end Set_Boot_Data;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
     (Protocol : access XYZ_Protocol)
   is
      Count   : Natural;
      FD      : C.int;
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
         if not Incomings (I).Closed then
            pragma Debug (D ("Connect peer" & Incomings (I).Socket'Img &
                             " to shutdown"));
            for H in 1 .. Options.Connection_Hits loop
               FD := Do_Connect (Incomings (I).Location);
               exit when FD /= Failure;
               delay 0.5;
            end loop;
            pragma Debug (D ("Send peer" & Incomings (I).Socket'Img &
                             " quit banner"));
            Physical_Send (FD, Quit_Stream'Access, Quit_Stream'First, Error);
            Catch (Error);
            delay 1.0;
            FD := Net.C_Close (FD);
            FD := Net.C_Close (Incomings (I).Socket);
            Incomings (I).Closed := True;
         end if;
         if Incomings (I).Socket /= Failure then
            pragma Debug
              (D ("Acceptor" & Incomings (I).Socket'Img & " is still alive"));
            Count := Count + 1;
         end if;
      end loop;
      Outgoings.Leave (Version);

      --  We cannot have a rendez-vous with the acceptors to
      --  check that it has terminated because we do not
      --  maintain the list of acceptors. An acceptor is in
      --  charge of setting field Socket to Failure. This way it
      --  signals its termination.

      loop
         pragma Debug (D ("There are" & Count'Img & " acceptors alive"));
         Count := 0;
         Outgoings.Enter;
         for I in First_Incoming .. Last_Incoming loop
            if Incomings (I).Socket /= Failure then
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

      Count := 0;
      Outgoings.Enter;
      for P in First_PID .. Outgoings.Last loop
         Info := Outgoings.Get_Component (P);
         if not Info.Closed then
            pragma Debug (D ("Partition" & P'Img & " peer is still alive"));
            Physical_Send
              (Info.Socket,
               Quit_Stream'Access,
               Quit_Stream'First,
               Error);
            Catch (Error);
            FD := Net.C_Close (Info.Socket);
            Info.Closed := True;
            Outgoings.Set_Component (P, Info);
         end if;
         if Info.Socket /= Failure then
            pragma Debug (D ("Connector to" & Info.Socket'Img &
                             " still alive"));
            Count := Count + 1;
         end if;
      end loop;
      Outgoings.Leave (Version);

      loop
         pragma Debug (D ("There are " & Count'Img & " connectors alive"));
         Count := 0;
         Outgoings.Enter;
         for P in First_PID .. Outgoings.Last loop
            Info := Outgoings.Get_Component (P);
            if Info.Socket /= Failure then
               Count := Count + 1;
            end if;
         end loop;
         Outgoings.Leave (Version);
         exit when Count = 0;
         Outgoings.Differ (Version);
      end loop;

      pragma Debug (D ("Shutdown TCP acceptors"));

      TCP_Operations.Shutdown;
      Shutdown_Completed := True;

      pragma Debug (D ("TCP Shutdown completed"));
   end Shutdown;

   ----------------
   -- Split_Data --
   ----------------

   function Split_Data
     (Data : String)
     return Host_Location
   is
      Result : Host_Location;
   begin
      if Data'Length = 0 then
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
     return SEC_Stream is
   begin
      return (1 => Stream_Element (Count / 256 ** 3),
              2 => Stream_Element ((Count / 256 ** 2) mod 256),
              3 => Stream_Element ((Count / 256) mod 256),
              4 => Stream_Element (Count mod 256));
   end To_Stream_Element_Array;

end System.Garlic.Protocols.Xyz;
