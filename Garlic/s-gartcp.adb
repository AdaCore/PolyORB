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
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
pragma Warnings (Off, Interfaces.C.Strings);
with System.Garlic.Constants;             use System.Garlic.Constants;
with System.Garlic.Debug;                 use System.Garlic.Debug;
with System.Garlic.Heart;                 use System.Garlic.Heart;
with System.Garlic.Naming;                use System.Garlic.Naming;
with System.Garlic.TCP.Operations;
with System.Garlic.Network_Utilities;     use System.Garlic.Network_Utilities;
with System.Garlic.Options;
with System.Garlic.Partitions;            use System.Garlic.Partitions;
with System.Garlic.Physical_Location;     use System.Garlic.Physical_Location;
with System.Garlic.Priorities;
with System.Garlic.Soft_Links;            use System.Garlic.Soft_Links;
with System.Garlic.Streams;               use System.Garlic.Streams;
with System.Garlic.Table;
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
     (Message : in String;
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

   Null_Location : constant Host_Location := (In_Address_Any, 0);

   function Split_Data (Data : String) return Host_Location;
   --  Split a data given as <machine> or <machine>:<port>

   type Socket_Info is record
      Location : Host_Location;
      Socket   : C.int;
      Locked   : Boolean;
   end record;
   Null_Socket : constant Socket_Info := (Null_Location, Failure, False);

   package Sockets is
      new System.Garlic.Table.Complex
        (Partition_ID,
         Null_PID,
         First_PID,
         Partition_ID_Increment,
         Partition_ID_Increment,
         Socket_Info,
         Null_Socket);

   Self : Socket_Info := Null_Socket;

   type Banner_Kind is (Junk_Banner, Data_Banner, Quit_Banner);
   --  Various headers that can be performed on a communication link

   Banner_Size : constant := 4;
   --  Size of a header when it is encoded as a stream

   subtype Banner_Stream is Stream_Element_Array (1 .. Banner_Size);
   --  Constrained subtype for headers

   procedure Read_Banner
     (Peer   : in C.int;
      Banner : out Banner_Kind;
      Error  : in out Error_Type);
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

   procedure Do_Listen (Error : in out Error_Type);
   --  Establish a socket according to the information in Self_Host (and
   --  complete it if needed).

   procedure Enter (PID : Partition_ID);
   procedure Leave (PID : Partition_ID);

   procedure Free is
     new Ada.Unchecked_Deallocation (Sockaddr_In, Sockaddr_In_Access);

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

   type Connection_Record;
   type Connection_Access is access Connection_Record;
   task type Connection_Handler
   is
      pragma Priority (Priorities.RPC_Priority);
      entry Initialize
        (My_Peer : C.int;
         My_PID  : Partition_ID;
         My_Self : Connection_Access);
   end Connection_Handler;
   type Connection_Handler_Access is access Connection_Handler;
   type Connection_Record is record
      Next : Connection_Access;
      Self : Connection_Handler_Access;
   end record;
   --  Handle incoming connection

   procedure Dequeue_Connection (Connection : in out Connection_Access);
   procedure Enqueue_Connection (Connection : in out Connection_Access);
   Connection_List : Connection_Access;

   --------------------
   -- Accept_Handler --
   --------------------

   task body Accept_Handler is
   begin
      pragma Debug (D ("Task Accept Handler is running"));

      --  Infinite loop on C_Accept

      Accept_Loop :
      loop
         declare
            Sin        : Sockaddr_In_Access := new Sockaddr_In;
            Length     : aliased C.int := Sin.all'Size / 8;
            Peer       : C.int;
            Connection : Connection_Access;
            Banner     : Banner_Kind;
            Result     : C.int;
            Error      : Error_Type;
         begin
            Sin.Sin_Family := Constants.Af_Inet;
            Add_Non_Terminating_Task;
            Peer := Net.C_Accept
              (Self.Socket, To_Sockaddr_Access (Sin), Length'Access);
            pragma Debug (D ("Accept new connection"));
            Sub_Non_Terminating_Task;
            if Peer = Failure then
               exit Accept_Loop;
            end if;

            --  Read a code from the file descriptor to know what to do
            --  next.

            pragma Debug (D ("Reading header"));
            Read_Banner (Peer, Banner, Error);
            exit when Found (Error);

            case Banner is

               when Junk_Banner =>
                  pragma Debug (D ("Accept Handler: Receive a junk banner"));
                  Result := Net.C_Close (Peer);

               when Data_Banner =>
                  pragma Debug (D ("Accept Handler: Receive a data banner"));

                  --  Get a new task to handle this new connection

                  Dequeue_Connection (Connection);
                  Connection.Self.Initialize (Peer, Null_PID, Connection);

               when Quit_Banner =>
                  pragma Debug (D ("Accept Handler: Receive a quit banner"));
                  Result := Net.C_Close (Peer);
                  exit Accept_Loop;

            end case;

         end;
      end loop Accept_Loop;
   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D ("Accept Handler: " & Exception_Information (E)));
         raise;
   end Accept_Handler;

   ------------------------
   -- Connection_Handler --
   ------------------------

   task body Connection_Handler is
      Old_PID    : Partition_ID;
      New_PID    : Partition_ID;
      Length     : Stream_Element_Count;
      Filtered   : Stream_Element_Access;
      Unfiltered : Stream_Element_Access;
      Opcode     : Any_Opcode;
      Banner     : Banner_Kind;
      Error      : Error_Type;
      Peer       : C.int;
      Self       : Connection_Access;
   begin
      loop
         select
            accept Initialize
              (My_Peer : C.int;
               My_PID  : Partition_ID;
               My_Self : Connection_Access)
            do
               Peer    := My_Peer;
               Old_PID := My_PID;
               New_PID := My_PID;
               Self    := My_Self;
            end Initialize;
         or
            terminate;
         end select;

         pragma Debug (D ("Task Connection Handler is running"));

         loop
            if New_PID /= Null_PID then
               Add_Non_Terminating_Task;
               Read_Banner (Peer, Banner, Error);
               Sub_Non_Terminating_Task;
               exit when Found (Error);

               case Banner is
                  when Junk_Banner =>
                     Throw (Error, "Connection_Handler: junk banner");
                     exit;

                  when Data_Banner =>
                     null;

                  when Quit_Banner =>
                     Throw (Error, "Connection_Handler: quit banner");
                     exit;

               end case;
            end if;

            Read_SEC (Peer, Length, Error);
            exit when Found (Error);

            pragma Debug (D ("Receive a packet of length" & Length'Img));

            Filtered := new Stream_Element_Array (1 .. Length);
            Physical_Receive (Peer, Filtered, Error);
            exit when Found (Error);

            Old_PID := New_PID;
            Analyze_Stream (New_PID, Opcode, Unfiltered, Filtered, 0, Error);
            exit when Found (Error);

            if Old_PID /= New_PID then
               Activity_Detected;

               if Old_PID = Null_PID then
                  pragma Debug (D ("Task handling partition" & New_PID'Img));
                  null;
               else
                  pragma Debug
                    (D ("Task handling partition" & New_PID'Img &
                        " and no longer handling partition" & Old_PID'Img));
                  null;
               end if;

               if Old_PID = Null_PID then
                  Enter (New_PID);
                  declare
                     New_Info : Socket_Info;
                  begin
                     New_Info := Sockets.Get_Component (New_PID);
                     New_Info.Socket := Peer;
                     Sockets.Set_Component (New_PID, New_Info);
                  end;
                  Leave (New_PID);
               else
                  Enter (Old_PID);
                  Enter (New_PID);
                  declare
                     Old_Info : Socket_Info;
                  begin
                     Old_Info := Sockets.Get_Component (Old_PID);
                     Sockets.Set_Component (New_PID, Old_Info);
                     Old_Info.Socket := Failure;
                     Sockets.Set_Component (Old_PID, Old_Info);
                  end;
                  Leave (New_PID);
                  Leave (Old_PID);
               end if;

               Catch (Error);
            end if;

            Process_Stream (New_PID, Opcode, Unfiltered, Error);
            exit when Found (Error);

            Free (Filtered);
            Free (Unfiltered);
         end loop;

         --  We always exit with an error. Catch it to print the error
         --  message and to deallocate it.
         Catch (Error);

         --  If this connection is broken before partition
         --  identification, then try to rescue New_PID especially for
         --  the boot server. ???: When does this happen?

         if New_PID = Null_PID then
            New_PID := Boot_PID;
         end if;

         --  Set the entry in the table correctly when we are not
         --  executing a shutdown operation. If we are, then it is
         --  likely that Partition_Map is already deallocated.

         if not Shutdown_In_Progress then
            Enter (New_PID);
            declare
               New_Info : Socket_Info;
            begin
               New_Info := Sockets.Get_Component (New_PID);
               New_Info.Socket := Failure;
               Sockets.Set_Component (New_PID, New_Info);
            end;
            Leave (New_PID);
         end if;

         --  The remote end has closed the socket or a communication
         --  error occurred. In the case, the entry will be freed,
         --  except if we are terminating since this is not worth
         --  freeing anything.

         declare
            Dummy : C.int;
         begin
            Dummy := Net.C_Close (Peer);
         end;

         --  Signal to the heart that we got an error on this partition

         if New_PID = Null_PID then
            pragma Debug
              (D ("Task dying before determining remote Partition_ID"));
            null;
         elsif not Shutdown_In_Progress then
            pragma Debug
              (D ("Signaling that partition" & New_PID'Img &
                  " is now unavailable"));

            Notify_Partition_Error (New_PID);

         else
            pragma Debug (D ("Partition" & New_PID'Img &
                             " is now unavailable (because of shutdown)"));
            null;
         end if;

         Enqueue_Connection (Self);
      end loop;
   end Connection_Handler;

   ------------
   -- Create --
   ------------

   function Create return Protocol_Access is
      Self : constant Protocol_Access := new TCP_Protocol;
   begin
      Register_Protocol (Self);
      return Self;
   end Create;

   ------------------------
   -- Dequeue_Connection --
   ------------------------

   procedure Dequeue_Connection
     (Connection : in out Connection_Access)
   is
   begin
      Sockets.Enter;
      if Connection_List = null then
         pragma Debug (D ("Create a new connection handler"));
         Connection      := new Connection_Record;
         Connection.Self := new Connection_Handler;
      else
         pragma Debug (D ("Reuse an old connection handler"));
         Connection      := Connection_List;
         Connection_List := Connection.Next;
      end if;
      Sockets.Leave;
   end Dequeue_Connection;

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
              (D ("Cannot connect to host " & Image (Location.Addr) &
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

   procedure Do_Listen (Error : in out Error_Type)
   is
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
         Throw (Error, "Do_Listen: TCP socket error");
         return;
      end if;

      Code := C_Setsockopt
        (Self.Socket, Sol_Socket, So_Reuseaddr, One'Address, One'Size / 8);

      Sin.Sin_Family := Constants.Af_Inet;
      Sin.Sin_Port := Port_To_Network (Port);
      Code := C_Bind (Self.Socket, To_Sockaddr_Access (Sin), Sin.all'Size / 8);
      if Code = Failure then
         Free (Sin);
         Free (Check);
         Throw (Error, "Do_Listen: TCP bind error");
         return;
      end if;

      if C_Listen (Self.Socket, 15) = Failure then
         Throw (Error, "Do_Listen: TCP listen error");
         return;
      end if;

      if Port = 0 then
         Code := C_Getsockname
           (Self.Socket, To_Sockaddr_Access (Check), Size'Access);
         if Code = Failure then
            Free (Sin);
            Free (Check);
            Throw (Error, "Do_Listen: TCP getsockname error");
            return;
         end if;
         Port := Network_To_Port (Check.Sin_Port);
      end if;

      Free (Check);

      pragma Debug (D ("Listen on port" & Port'Img));
   end Do_Listen;

   ------------------------
   -- Enqueue_Connection --
   ------------------------

   procedure Enqueue_Connection
     (Connection : in out Connection_Access)
   is
   begin
      pragma Debug (D ("Queue an old connection handler"));
      Sockets.Enter;
      Connection.Next := Connection_List;
      Connection_List := Connection;
      Sockets.Leave;
   end Enqueue_Connection;

   -----------
   -- Enter --
   -----------

   procedure Enter (PID : Partition_ID) is
      Version : Version_Id;
      Info    : Socket_Info;
   begin
      loop
         Sockets.Enter;
         Info := Sockets.Get_Component (PID);
         if not Info.Locked then
            Info.Locked := True;
            Sockets.Set_Component (PID, Info);
            Sockets.Leave;
            exit;
         end if;
         pragma Debug (D ("Postpone lock of partition" & PID'Img));
         Sockets.Leave (Version);
         Sockets.Differ (Version);
      end loop;
      pragma Debug (D ("Lock partition" & PID'Img));
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Protocol  : access TCP_Protocol;
      Self_Data : in Utils.String_Access := null;
      Boot_Data : in Utils.String_Access := null;
      Boot_Mode : in Boolean := False;
      Error     : in out Error_Type)
   is
      Current_Host : constant Host_Location := Split_Data (Host_Name);
   begin
      pragma Debug (D ("Initialize protocol TCP"));

      if Acceptor = null then
         if Self_Data /= null
           and then Self_Data.all /= ""
         then
            --  When there is a self location to bind on, check that
            --  this location concerns the current host. Otherwise,
            --  there is a real problem.

            Self.Location := Split_Data (Self_Data.all);
            declare
               N1 : String := Name_Of (Image (Self.Location.Addr));
               N2 : String := Name_Of ("localhost");
               N3 : String := Name_Of (Image (Current_Host.Addr));
            begin
               if N1 /= N2
                 and then N1 /= N3
               then
                  pragma Debug (D ("Self_Data " & N1 & " does not match:"));
                  pragma Debug (D (" - " & N2));
                  pragma Debug (D (" - " & N3));
                  Ada.Exceptions.Raise_Exception
                    (Program_Error'Identity,
                     "Incorrect tcp self location: " & Self_Data.all);
               end if;
            end;
         else
            Self.Location := Current_Host;
         end if;
      end if;

      if Boot_Mode then

         --  If the Boot_PID (Boot_First) changes because the current
         --  partition is in fact a boot mirror (not Boot_First), then
         --  the connection task in charge of the update. Note that
         --  this occurs as soon as the task receives a request from
         --  the real boot server, that means during initialization.

         Enter (Boot_PID);
         declare
            Boot_Info : Socket_Info := Sockets.Get_Component (Boot_PID);
         begin
            if Boot_Data /= null
              and then Boot_Data.all /= ""
            then
               Boot_Info.Location := Split_Data (Boot_Data.all);
            end if;

            --  If this partition is the lead partition, then the
            --  bootmode is a normal mode. The default should be also
            --  used for the local connection.

            if Options.Is_Boot_Server then
               Self.Location.Port := Boot_Info.Location.Port;
            end if;
            Sockets.Set_Component (Boot_PID, Boot_Info);
         end;
         Leave (Boot_PID);
      end if;

      --  If this is the first time we initialize the protocol, start the
      --  Accept_Handler.

      if Acceptor = null  then
         if not Can_Have_A_Light_Runtime then
            pragma Debug (D ("Start acceptor task"));
            Do_Listen (Error);

            if Found (Error) then
               return;
            end if;

            Acceptor := new Accept_Handler;
         else
            pragma Debug (D ("No acceptor task, light runtime"));
            null;
         end if;
      end if;

      pragma Debug (D ("Protocol TCP initialized"));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (PID : in Partition_ID) is
      Info : Socket_Info;
   begin
      Sockets.Enter;
      pragma Debug (D ("Unlock partition" & PID'Img));
      Info := Sockets.Get_Component (PID);
      Info.Locked := False;
      Sockets.Set_Component (PID, Info);
      Sockets.Update;
      Sockets.Leave;
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
         Code := Net.C_Recv (Peer, To_Chars_Ptr (Addr), Size, 0);
         if Code <= 0 then
            Code := Net.C_Close (Peer);
            Throw (Error, "Physical_Receive: error");
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
         Code := Net.C_Send (Peer, To_Chars_Ptr (Addr), Size, 0);
         if Code <= 0 then
            Code := Net.C_Close (Peer);
            Throw (Error, "Physical_Send: error");
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
      Banner : out Banner_Kind;
      Error  : in out Error_Type)
   is
      Stream : aliased Stream_Element_Array := (1 .. Banner_Size => 0);
      Result : Banner_Kind;
   begin
      pragma Debug (D ("Reading banner from peer" & Peer'Img));
      Physical_Receive (Peer, Stream'Access, Error);
      if Found (Error) then
         return;
      end if;
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
      pragma Debug (D ("Read banner " & Result'Img));
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

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access TCP_Protocol;
      Partition : in Partition_ID;
      Data      : access Stream_Element_Array;
      Error     : in out Error_Type)
   is
      Peer     : Socket_Info;
      Hits     : Natural := 1;
      First    : Stream_Element_Count := Data'First + Unused_Space;
      Count    : Stream_Element_Count;
      Location : Location_Type;
   begin
      pragma Debug (D ("Send to partition" & Partition'Img));
      Enter (Partition);
      Peer := Sockets.Get_Component (Partition);
      if Peer.Socket = Failure then
         Get_Location (Partition, Location, Error);
         if Found (Error) then
            Leave (Partition);
            return;
         end if;

         Peer.Location := Split_Data (Get_Data (Location).all);

         if Peer.Location = Null_Location then
            Sockets.Set_Component (Partition, Peer);
            Throw (Error, "Send: Cannot connect with peer without location");
            return;
         end if;

         if Partition = Boot_PID then
            Hits := Options.Connection_Hits;
         end if;

         while Hits > 0 loop
            Peer.Socket  := Do_Connect (Peer.Location);
            exit when Peer.Socket /= Failure;

            delay 2.0;
            Hits := Hits - 1;
         end loop;

         Sockets.Set_Component (Partition, Peer);

         if Hits = 0 then
            Leave (Partition);
            Throw (Error, "Send: Cannot connect to" & Partition'Img);
            return;
         end if;

         pragma Debug (D ("Connected to partition" & Partition'Img));

         --  Now create a task to get data on this connection

         declare
            Connection : Connection_Access;
         begin
            Dequeue_Connection (Connection);
            Connection.Self.Initialize (Peer.Socket, Partition, Connection);
         end;

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
        (D ("Send" &
            Stream_Element_Count'Image (Data'Last - First + 1) &
            " bytes (content of" &
            Stream_Element_Count'Image (Data'Length - Unused_Space) &
            " bytes) to partition" & Partition'Img &
            " on peer" & Peer.Socket'Img));

      pragma Debug (Dump (Data, Private_Debug_Key));
      Physical_Send (Peer.Socket, Data, First, Error);

      Leave (Partition);
   end Send;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Protocol : access TCP_Protocol) is
      Peer  : C.int;
      Error : Error_Type;
      Info  : Socket_Info;
   begin
      if Shutdown_Completed then
         return;
      end if;

      pragma Debug (D ("Remote connections shutdown"));

      --  The following loop tries to send a QUIT message to any known
      --  partition so that it releases its socket to be able to perform
      --  the shutdown operation on its end.

      Sockets.Enter;

      for P in First_PID .. Sockets.Last loop
         Info := Sockets.Get_Component (P);
         if Info.Socket /= Failure then
            pragma Debug (D ("Partition" & P'Img & " peer is still alive"));
            Peer := Info.Socket;
            Physical_Send (Peer, Quit_Stream'Access, Quit_Stream'First, Error);
            Catch (Error);
            Peer := Net.C_Close (Peer);
         end if;
      end loop;

      Sockets.Leave;

      if Found (Error) then
         return;
      end if;

      pragma Debug (D ("Local connection shutdown"));

      --  Send the same message on a new connection to the partition itself
      --  so that the accept gets the message.

      if not Can_Have_A_Light_Runtime then
         declare
            Back : C.int;
            Fail : Error_Type;
         begin
            for I in 1 .. 5 loop
               Back := Do_Connect (Self.Location);
               exit when Back /= Failure;
               delay 1.0;
            end loop;
            Physical_Send (Back, Quit_Stream'Access, Quit_Stream'First, Fail);
            Catch (Fail);
            Back := Net.C_Close (Back);
         end;
      end if;

      --  Destroy (Socket_Table_Mutex);
      Shutdown_Completed := True;

      pragma Debug (D ("Shutdown completed"));
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
