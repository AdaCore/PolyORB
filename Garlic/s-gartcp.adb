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

with Ada.Exceptions;                      use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
pragma Warnings (Off, Interfaces.C.Strings);
with System.Garlic.Constants;             use System.Garlic.Constants;
with System.Garlic.Debug;                 use System.Garlic.Debug;
with System.Garlic.Exceptions;            use System.Garlic.Exceptions;
with System.Garlic.Heart;                 use System.Garlic.Heart;
with System.Garlic.Name_Server;           use System.Garlic.Name_Server;
with System.Garlic.Naming;                use System.Garlic.Naming;
with System.Garlic.TCP.Operations;
with System.Garlic.Network_Utilities;     use System.Garlic.Network_Utilities;
with System.Garlic.Options;
with System.Garlic.Physical_Location;     use System.Garlic.Physical_Location;
with System.Garlic.PID_Server;            use System.Garlic.PID_Server;
with System.Garlic.Priorities;
with System.Garlic.Soft_Links;            use System.Garlic.Soft_Links;
with System.Garlic.Streams;               use System.Garlic.Streams;
with System.Garlic.Thin;                  use System.Garlic.Thin;
with System.Garlic.TCP_Platform_Specific;
pragma Warnings (Off, System.Garlic.TCP_Platform_Specific);
with System.Storage_Elements;             use System.Storage_Elements;

package body System.Garlic.TCP is

   --  This system implements TCP communication. The connection is established
   --  as follow: (C = caller, R = receiver)
   --    - If the partition_ID is not known:
   --      C->R : Null_Partition_ID
   --      R->C : <new caller Partition_ID> <boot partition name>
   --    - After this, in any case: (C & R may be reversed)
   --      C->R : <Length (Stream_Element_Count)> <Packet>

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

   type Host_Data is record
      Location  : Host_Location;
      FD        : C.int;
      Connected : Boolean := False;
      Known     : Boolean := False;
      Queried   : Boolean := False;
      Locked    : Boolean := False;
   end record;

   Self_Host : Host_Data;

   type Partition_Array is array (Partition_ID) of Host_Data;

   protected type Partition_Map_Type is
      procedure Set_Locked
        (Partition : in Partition_ID; Data : in Host_Data);
      entry Get (Partition_ID) (Data : out Host_Data);
      entry Lock (Partition_ID);
      procedure Unlock (Partition : in Partition_ID);
      function Get_Immediate (Partition : Partition_ID) return Host_Data;
   private
      Partitions  : Partition_Array;
   end Partition_Map_Type;
   --  Get returns immediately if Known is True or else Queried is False,
   --  and sets Queried to True, meaning that the calling task should ask
   --  for the coordinates.

   type Partition_Map_Access is access Partition_Map_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Partition_Map_Type,
                                      Partition_Map_Access);
   Partition_Map : Partition_Map_Access :=
     new Partition_Map_Type;
   --  Kludge to raise Program_Error at deallocation time. Should be removed
   --  in the future ???

   type Operation_Code is (Open_Code, Data_Code, Quit_Code);
   --  Various operations that can be performed on a communication link

   Operation_Code_Length : constant := 4;
   --  Size of an operation code when it is encoded as a stream

   subtype Operation_Code_Array is
     Stream_Element_Array (1 .. Operation_Code_Length);
   --  Constrained subtype for operation codes

   function Read_Code (FD : C.int) return Operation_Code;
   pragma Inline (Read_Code);
   --  Read an operation code from a file descriptor or raise
   --  Communication_Error if the code is not understood.

   function To_Stream_Element_Array (Code : Operation_Code)
     return Operation_Code_Array;
   pragma Inline (To_Stream_Element_Array);
   --  Return the stream element array corresponding to the code

   Stream_Element_Count_Length : constant := 4;
   --  Size of a Stream_Element_Count when it is encoded as a stream

   subtype Stream_Element_Count_Array is
     Stream_Element_Array (1 .. Stream_Element_Count_Length);
   --  Constrained subtype for stream element counts

   function Read_Stream_Element_Count (FD : C.int) return Stream_Element_Count;
   --  Read a stream element count from a file descriptor and check that
   --  it is valid. Raise Communication_Error otherwise.

   function To_Stream_Element_Array (Count : Stream_Element_Count)
     return Stream_Element_Count_Array;
   --  Return the stream element array corresponding to this count

   Partition_ID_Length : constant := 2;
   --  Length of a partition ID when it is encoded as a stream

   subtype Partition_ID_Array is
     Stream_Element_Array (1 .. Partition_ID_Length);
   --  Constrained subtype for partition ID

   function Read_Partition_ID (FD : C.int) return Partition_ID;
   --  Read a Partition_ID from a file descriptor or raise Constraint_Error
   --  if it is not valid.

   procedure Write_Partition_ID (FD        : in C.int;
                                 Partition : in Partition_ID);
   --  Write a Partition_ID on a file descriptor

   function Establish_Connection (Location  : Host_Location;
                                  Operation : Operation_Code)
      return C.int;
   --  Establish a socket to a remote location and return the file descriptor

   procedure Establish_Listening_Socket;
   --  Establish a socket according to the information in Self_Host (and
   --  complete it if needed).

   procedure Free is
     new Ada.Unchecked_Deallocation (Sockaddr_In, Sockaddr_In_Access);

   function Ask_For_Partition_ID (FD : C.int) return Partition_ID;
   --  Ask for a partition ID on the given file descriptor (which is
   --  of course bound to the server, since we cannot contact other
   --  partitions if we haven't our partition ID). Get also the boot
   --  partition name.

   procedure Physical_Receive
     (FD : in C.int; Data : out Stream_Element_Array);
   pragma Inline (Physical_Receive);
   procedure Physical_Send (FD : in C.int; Data : in Stream_Element_Array);
   pragma Inline (Physical_Send);
   --  Receive and send data. Physical_Receive loops as long as Data has
   --  not been filled and Physical_Send as long as everything has not been
   --  sent.

   procedure Physical_Send
     (FD   : in C.int;
      Data : in System.Address;
      Len  : in C.int);
   procedure Physical_Receive
     (FD   : in C.int;
      Data : in System.Address;
      Len  : in C.int);
   --  Same procedures as above, at a lower level (and does not require a
   --  copy on the stack).

   procedure Send_My_Partition_ID (FD : in C.int);
   --  Transmit my partition ID to the remote end

   procedure Receive_And_Send_To_Heart
     (Length    : in Stream_Element_Count;
      FD        : in C.int;
      Partition : in Partition_ID);
   --  Receive some data from FD and send it to Garlic heart

   task type Accept_Handler is
      pragma Priority (Priorities.RPC_Priority);
   end Accept_Handler;
   type Accept_Handler_Access is access Accept_Handler;
   Acceptor : Accept_Handler_Access;
   --  The task which will accept new connections

   task type Incoming_Connection_Handler (FD        : C.int;
                                          Receiving : Boolean;
                                          Remote    : Partition_ID) is
      pragma Priority (Priorities.RPC_Priority);
   end Incoming_Connection_Handler;
   type Incoming_Connection_Handler_Access is
      access Incoming_Connection_Handler;
   --  Handler for an incoming connection

   --------------------
   -- Accept_Handler --
   --------------------

   task body Accept_Handler is
   begin
      --  Infinite loop on C_Accept

      loop
         declare
            Sin    : Sockaddr_In_Access := new Sockaddr_In;
            Length : aliased C.int := Sin.all'Size / 8;
            FD     : C.int;
            NT     : Incoming_Connection_Handler_Access;
            Code   : Operation_Code;
            Result : C.int;
         begin
            Sin.Sin_Family := Constants.Af_Inet;
            Add_Non_Terminating_Task;
            pragma Debug (D (D_Debug, "Before Net.C_Accept"));
            FD := Net.C_Accept (Self_Host.FD, To_Sockaddr_Access (Sin),
                                Length'Access);
            pragma Debug (D (D_Debug, "After Net.C_Accept"));
            Sub_Non_Terminating_Task;
            if FD = Failure then
               Raise_Communication_Error;
            end if;


            --  Read a code from the file descriptor to know what to do
            --  next.

            pragma Debug (D (D_Debug, "Reading code"));
            Code := Read_Code (FD);
            case Code is

               when Open_Code =>
                  pragma Debug (D (D_Debug, "Open code received"));
                  null;

               when Quit_Code =>
                  pragma Debug (D (D_Debug, "Quitting accept handler"));
                  Result := Net.C_Close (FD);
                  Raise_Communication_Error ("Quit code received");

               when Data_Code =>
                  pragma Debug (D (D_Debug, "Unexpected code received " &
                                   Operation_Code'Image (Code)));
                  Raise_Communication_Error ("Unexpected code received " &
                                             Operation_Code'Image (Code));
                  Result := Net.C_Close (FD);

            end case;

            --  Create a new task to handle this new connection

            NT :=
             new Incoming_Connection_Handler (FD,
                                              Receiving => True,
                                              Remote => Null_Partition_ID);
         end;
      end loop;
   end Accept_Handler;

   --------------------------
   -- Ask_For_Partition_ID --
   --------------------------

   function Ask_For_Partition_ID (FD : C.int) return Partition_ID is
      Partition : Partition_ID;
   begin
      pragma Debug (D (D_Garlic, "Asking for a Partition_ID"));
      Write_Partition_ID (FD, Null_Partition_ID);
      Partition := Read_Partition_ID (FD);
      if not Partition'Valid then
         pragma Debug (D (D_Garlic, "Invalid partition ID"));
         Raise_Exception (Constraint_Error'Identity, "Invalid partition ID");
      end if;
      pragma Debug (D (D_Garlic, "My Partition_ID is" & Partition'Img));
      return Partition;
   end Ask_For_Partition_ID;

   ------------
   -- Create --
   ------------

   function Create return Protocol_Access is
      Self : constant Protocol_Access := new TCP_Protocol;
   begin
      Register_Protocol (Self);
      return Self;
   end Create;

   --------------------------
   -- Establish_Connection --
   --------------------------

   function Establish_Connection (Location  : Host_Location;
                                  Operation : Operation_Code)
     return C.int is
      FD   : C.int;
      Sin  : Sockaddr_In_Access := new Sockaddr_In;
      Code : C.int;
   begin
      FD := C_Socket (Af_Inet, Sock_Stream, 0);
      if FD = Failure then
         Free (Sin);
         Raise_Communication_Error;
      end if;
      Sin.Sin_Family := Constants.Af_Inet;
      Sin.Sin_Addr := To_In_Addr (Location.Addr);
      Sin.Sin_Port := Port_To_Network (Location.Port);
      Code := Net.C_Connect (FD,
                             To_Sockaddr_Access (Sin), Sin.all'Size / 8);
      if Code = Failure then
         Code := Net.C_Close (FD);
         Free (Sin);
         Raise_Communication_Error;
      end if;
      Physical_Send (FD, To_Stream_Element_Array (Operation));
      return FD;
   end Establish_Connection;

   --------------------------------
   -- Establish_Listening_Socket --
   --------------------------------

   procedure Establish_Listening_Socket is
      Port  : C.unsigned_short renames Self_Host.Location.Port;
      FD    : C.int renames Self_Host.FD;
      Sin   : Sockaddr_In_Access := new Sockaddr_In;
      Check : Sockaddr_In_Access := new Sockaddr_In;
      Dummy : aliased C.int := Check.all'Size / 8;
   begin
      FD := C_Socket (Af_Inet, Sock_Stream, 0);
      if FD = Failure then
         Free (Sin);
         Free (Check);
         Raise_Communication_Error;
      end if;
      declare
         One   : aliased C.int := 1;
         Dummy : C.int;
      begin
         Dummy := C_Setsockopt (FD, Sol_Socket,
                                So_Reuseaddr,
                                One'Address,
                                One'Size / 8);
      end;
      Sin.Sin_Family := Constants.Af_Inet;
      Sin.Sin_Port := Port_To_Network (Port);
      if C_Bind (FD,
                 To_Sockaddr_Access (Sin),
                 Sin.all'Size / 8) = Failure then
         Free (Sin);
         Free (Check);
         Raise_Communication_Error;
      end if;
      if C_Listen (FD, 15) = Failure then
         Raise_Communication_Error;
      end if;
      if Port = 0 then
         if C_Getsockname (FD,
                           To_Sockaddr_Access (Check),
                           Dummy'Access) =  Failure then
            Free (Sin);
            Free (Check);
            Raise_Communication_Error;
         end if;
         Port := Network_To_Port (Check.Sin_Port);
      end if;
      Free (Check);
      Self_Host.Connected := True;
      pragma Debug
        (D (D_Communication,
            "Listening on port" & C.unsigned_short'Image (Port)));
   end Establish_Listening_Socket;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (P : access TCP_Protocol) return String is
      Port : constant String :=
        C.unsigned_short'Image (Self_Host.Location.Port);
   begin
      if Can_Have_A_Light_Runtime then
         return "";
      else
         return Name_Of (Image (Self_Host.Location.Addr)) &
           ":" & Port (2 .. Port'Last);
      end if;
   end Get_Info;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (P : access TCP_Protocol) return String is
   begin
      return "tcp";
   end Get_Name;

   ---------------------------------
   -- Incoming_Connection_Handler --
   ---------------------------------

   task body Incoming_Connection_Handler is
      Data      : Host_Data;
      Partition : Partition_ID := Null_Partition_ID;

      procedure Handle_Transaction;
      --  Handle one transaction

      ------------------------
      -- Handle_Transaction --
      ------------------------

      procedure Handle_Transaction is
         Length : Stream_Element_Count;
      begin
         begin
            Add_Non_Terminating_Task;
            pragma Debug
              (D (D_Debug, "Reading operation code"));

            case Read_Code (FD) is

               when Data_Code =>
                  pragma Debug (D (D_Debug, "Received a data code"));
                  null;

               when Quit_Code =>
                  pragma Debug (D (D_Debug, "Quitting one incoming handler"));
                  Raise_Communication_Error ("Received a quit code");

               when Open_Code =>
                  pragma Debug (D (D_Debug, "Open code received, error"));
                  Raise_Communication_Error ("Bogus open code received");

            end case;

            Length := Read_Stream_Element_Count (FD);

            Sub_Non_Terminating_Task;

         exception
            when Communication_Error =>
               Sub_Non_Terminating_Task;
               raise;
         end;

         pragma Debug (D (D_Debug,
                          "Will receive a packet of length" & Length'Img));
         Receive_And_Send_To_Heart (Length, FD, Partition);
      end Handle_Transaction;

   begin

      if Receiving then

         --  The first thing we will receive is the Partition_ID. As an
         --  exception, Null_Partition_ID means that the remote side hasn't
         --  got a Partition_ID.

         pragma Debug
           (D (D_Communication, "New communication task started"));

         --  We do not call Add_Non_Terminating_Task since we want to
         --  receive the whole partition ID. Moreover, we will signal
         --  that data has arrived.

         Activity_Detected;
         Partition := Read_Partition_ID (FD);
         if Partition = Null_Partition_ID then
            Partition := Allocate_Partition_ID;
            Write_Partition_ID (FD, Partition);
         end if;
         pragma Debug
           (D (D_Communication,
                  "This task is in charge of partition" & Partition'Img));
         Partition_Map.Lock (Partition);
         Data           := Partition_Map.Get_Immediate (Partition);
         Data.FD        := FD;
         Data.Connected := True;
         Partition_Map.Set_Locked (Partition, Data);
         Partition_Map.Unlock (Partition);

      else

         Partition := Remote;
         pragma Debug
           (D (D_Communication,
               "New task to handle partition" & Partition'Img));

      end if;

      --  Then we have an (almost) infinite loop to get requests or
      --  answers.

      loop
         Handle_Transaction;
      end loop;

   exception

      when Communication_Error =>

         --  The remote end has closed the socket or a communication error
         --  occurred. In the case, the entry will be freed, except if we
         --  are terminating since this is not worth freeing anything.

         if Partition /= Null_Partition_ID
           and then not Is_Shutdown_In_Progress
         then
            --  Set the entry in the table correctly only we are not
            --  executing a shutdown operation. If we are, then
            --  it is likely that Partition_Map is already deallocated.

            Partition_Map.Lock (Partition);
            Data := Partition_Map.Get_Immediate (Partition);
            Data.Connected := False;
            Partition_Map.Set_Locked (Partition, Data);
            Partition_Map.Unlock (Partition);

         end if;

         declare
            Dummy : C.int;
         begin
            Dummy := Net.C_Close (FD);
         end;

         --  Signal to the heart that we got an error on this partition

         if Partition = Null_Partition_ID then
            pragma Debug
              (D (D_Garlic,
                  "Task dying before determining remote Partition_ID"));
            null;
         elsif not Is_Shutdown_In_Progress then
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
            Dummy := Net.C_Close (FD);
         end;
   end Incoming_Connection_Handler;

   ------------------------
   -- Partition_Map_Type --
   ------------------------

   protected body Partition_Map_Type is

      ---------
      -- Get --
      ---------

      entry Get (for Partition in Partition_ID) (Data : out Host_Data)
      when Partitions (Partition).Known or else
        not Partitions (Partition).Queried is
      begin
         if Partitions (Partition).Known then
            Data := Partitions (Partition);
         else
            Partitions (Partition).Queried := True;
            Data := Partitions (Partition);
         end if;
      end Get;

      -------------------
      -- Get_Immediate --
      -------------------

      function Get_Immediate (Partition : Partition_ID) return Host_Data is
      begin
         return Partitions (Partition);
      end Get_Immediate;

      ----------
      -- Lock --
      ----------

      entry Lock (for P in Partition_ID)
      when not Partitions (P).Locked is
      begin
         Partitions (P).Locked := True;
      end Lock;

      ----------------
      -- Set_Locked --
      ----------------

      procedure Set_Locked
        (Partition : in Partition_ID; Data : in Host_Data)
      is
      begin
         Partitions (Partition) := Data;
      end Set_Locked;

      ------------
      -- Unlock --
      ------------

      procedure Unlock (Partition : in Partition_ID) is
      begin
         Partitions (Partition).Locked := False;
      end Unlock;

   end Partition_Map_Type;

   ----------------------
   -- Physical_Receive --
   ----------------------

   procedure Physical_Receive (FD : in C.int; Data : out Stream_Element_Array)
   is
   begin
      Physical_Receive (FD, Data'Address, Data'Length);
   end Physical_Receive;

   ----------------------
   -- Physical_Receive --
   ----------------------

   procedure Physical_Receive
     (FD   : in C.int;
      Data : in System.Address;
      Len  : in C.int)
   is
      Current : System.Address := Data;
      Rest    : C.int          := Len;
      Code    : C.int;
   begin
      while Rest > 0 loop

         Code := Net.C_Recv (FD, To_Chars_Ptr (Current), Rest, 0);
         if Code <= 0 then
            Code := Net.C_Close (FD);
            Raise_Communication_Error ("Read error");
         end if;

         Current := Current + Storage_Offset (Code);
         Rest := Rest - Code;
      end loop;
   end Physical_Receive;

   -------------------
   -- Physical_Send --
   -------------------

   procedure Physical_Send (FD : in C.int; Data : in Stream_Element_Array)
   is
   begin
      Physical_Send (FD, Data'Address, Data'Length);
   end Physical_Send;

   -------------------
   -- Physical_Send --
   -------------------

   procedure Physical_Send
     (FD   : in C.int;
      Data : in System.Address;
      Len  : in C.int)
   is
      Current : System.Address := Data;
      Rest    : C.int          := Len;
      Code    : C.int;
   begin
      while Rest > 0 loop
         Code := Net.C_Send (FD, To_Chars_Ptr (Current), Rest, 0);
         if Code <= 0 then
            Code := Net.C_Close (FD);
            Raise_Communication_Error ("Write error");
         end if;

         Current := Current + Storage_Offset (Code);
         Rest := Rest - Code;
      end loop;
   end Physical_Send;

   ---------------
   -- Read_Code --
   ---------------

   function Read_Code (FD : in C.int) return Operation_Code is
      Code   : Operation_Code_Array;
      Result : Operation_Code;
   begin
      Physical_Receive (FD, Code);
      Result := Operation_Code'Val (Code (1));
      if not Result'Valid then
         pragma Debug (D (D_Exception, "Unknown Operation_Code received"));
         Raise_Communication_Error ("Unknown Operation_Code received");
      end if;
      for I in 2 .. Code'Last loop
         if Code (I) /= Code (1) then
            pragma Debug (D (D_Exception, "Invalid Operation_Code received"));
            Raise_Communication_Error ("Invalid Operation_Code received");
         end if;
      end loop;
      return Result;
   end Read_Code;

   -----------------------
   -- Read_Partition_ID --
   -----------------------

   function Read_Partition_ID (FD : C.int) return Partition_ID is
      Stream : Partition_ID_Array;
      Result : Natural;
   begin
      Physical_Receive (FD, Stream);
      Result := Natural (Stream (1)) * 256 + Natural (Stream (2));
      if Result < Natural (Partition_ID'First)
        or else Result > Natural (Partition_ID'Last)
      then
         raise Constraint_Error;
      end if;
      return Partition_ID (Result);
   end Read_Partition_ID;

   -------------------------------
   -- Read_Stream_Element_Count --
   -------------------------------

   function Read_Stream_Element_Count (FD : C.int) return Stream_Element_Count
   is
      Stream : Stream_Element_Count_Array;
   begin
      Physical_Receive (FD, Stream);
      return
        Stream_Element_Count (Stream (1)) * 256 ** 3 +
        Stream_Element_Count (Stream (2)) * 256 ** 2 +
        Stream_Element_Count (Stream (3)) * 256 +
        Stream_Element_Count (Stream (4));
   exception
      when Constraint_Error =>
         Raise_Communication_Error ("Bad Stream_Element_Count received");
   end Read_Stream_Element_Count;

   -------------------------------
   -- Receive_And_Send_To_Heart --
   -------------------------------

   procedure Receive_And_Send_To_Heart
     (Length    : in Stream_Element_Count;
      FD        : in C.int;
      Partition : in Partition_ID)
   is
      Buffer : Stream_Element_Access;
   begin
      pragma Debug (D (D_Debug, "Creating buffer"));
      Buffer := new Stream_Element_Array (1 .. Length);
      pragma Debug (D (D_Debug, "Calling Physical_Receive"));
      Physical_Receive (FD, Buffer.all'Address, C.int (Length));
      pragma Debug (D (D_Debug, "Calling Has_Arrived"));
      Has_Arrived (Partition, Buffer);
      pragma Debug (D (D_Debug, "Freeing buffer"));
      Free (Buffer);
      pragma Debug (D (D_Debug, "Packet received and sent to heart"));
   end Receive_And_Send_To_Heart;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol  : access TCP_Protocol;
      Partition : in Partition_ID;
      Data      : access Stream_Element_Array)
   is
      Remote_Data : Host_Data;
   begin
      Partition_Map.Lock (Partition);
      Partition_Map.Get (Partition) (Remote_Data);
      if Remote_Data.Queried and then not Remote_Data.Known then
         Partition_Map.Unlock (Partition);
         declare
            Temp : Host_Location;
         begin
            Temp := Split_Data (Get_Data (Location (Partition)));
            Partition_Map.Lock (Partition);
            Remote_Data := Partition_Map.Get_Immediate (Partition);
            Remote_Data.Location := Temp;
            Remote_Data.Known := True;
            Remote_Data.Queried := False;
            Partition_Map.Set_Locked (Partition, Remote_Data);
         end;
      end if;
      begin
         if not Remote_Data.Connected then

            pragma Debug
              (D (D_Communication,
                  "Willing to connect to " &
                  Image (Remote_Data.Location.Addr) & " port" &
                  C.unsigned_short'Image (Remote_Data.Location.Port)));

            declare
               Retries : Natural := 1;
            begin
               if Partition = Get_Boot_Server then
                  Retries := Options.Connection_Hits;
               end if;
               for I in 1 .. Retries loop
                  begin
                     pragma Debug
                       (D (D_Communication,
                           "Trying to connect to partition" &
                           Partition'Img));
                     Remote_Data.FD :=
                       Establish_Connection (Remote_Data.Location,
                                             Open_Code);
                     Remote_Data.Connected := True;
                     exit;
                  exception
                     when Communication_Error =>
                        if I = Retries then
                           pragma Debug
                             (D (D_Communication,
                                 "Cannot connect to partition" &
                                 Partition'Img));
                           Raise_Communication_Error
                             ("Cannot connect to partition" &
                              Partition_ID'Image (Partition));
                        else
                           delay 2.0;
                        end if;
                  end;
               end loop;
               pragma Debug
                 (D (D_Communication,
                     "Connected to partition" & Partition'Img));
            end;

            Partition_Map.Set_Locked (Partition, Remote_Data);
            if Get_My_Partition_ID_Immediately = Null_Partition_ID then
               Set_My_Partition_ID (Ask_For_Partition_ID (Remote_Data.FD));
            else
               Send_My_Partition_ID (Remote_Data.FD);
            end if;

            --  Now create a task to get data on this connection

            declare
               NT : Incoming_Connection_Handler_Access;
            begin
               NT := new Incoming_Connection_Handler
                 (Remote_Data.FD, Receiving => False, Remote => Partition);
            end;

         end if;
         declare
            Offset : constant Stream_Element_Offset :=
              Data'First + Unused_Space - Stream_Element_Count_Length;
            Code   : constant Stream_Element_Offset :=
              Offset - Operation_Code_Length;
         begin
            --  Write length at the beginning of the data, then the operation
            --  code.

            Data (Offset .. Offset + Stream_Element_Count_Length - 1) :=
              To_Stream_Element_Array (Data'Length - Unused_Space);
            Data (Code .. Offset - 1) :=
              To_Stream_Element_Array (Data_Code);
            pragma Debug
              (D (D_Debug,
                  "Sending packet of length" &
                  Stream_Element_Count'Image (Data'Last - Code + 1) &
                  " (content of" &
                  Stream_Element_Count'Image (Data'Length - Unused_Space) &
                  ")"));
            Physical_Send (Remote_Data.FD, Data (Code) 'Address,
                           C.int (Data'Last - Code + 1));
         end;
         Partition_Map.Unlock (Partition);
      exception
         when Communication_Error =>
            pragma Debug (D (D_Debug, "Error detected in Send"));
            Partition_Map.Unlock (Partition);
            raise;
      end;
   end Send;

   --------------------------
   -- Send_My_Partition_ID --
   --------------------------

   procedure Send_My_Partition_ID (FD : in C.int) is
   begin
      Write_Partition_ID (FD, Get_My_Partition_ID);
   end Send_My_Partition_ID;

   -------------------
   -- Set_Boot_Data --
   -------------------

   procedure Set_Boot_Data
     (Protocol         : access TCP_Protocol;
      Is_Boot_Protocol : in Boolean := False;
      Boot_Data        : in String := "";
      Is_Master        : in Boolean := False)
   is
      Boot_Host : Host_Data;
   begin
      Self_Host.Location := Split_Data (Host_Name);
      if Is_Boot_Protocol then
         Boot_Host.Location := Split_Data (Boot_Data);
         Boot_Host.Known    := True;
         Partition_Map.Lock (Get_Boot_Server);
         Partition_Map.Set_Locked (Get_Boot_Server, Boot_Host);
         Partition_Map.Unlock (Get_Boot_Server);
         if Is_Master then
            Self_Host.Location.Port := Boot_Host.Location.Port;
         end if;
      end if;
      if not Can_Have_A_Light_Runtime then
         pragma Debug (D (D_Debug, "Starting an acceptor task"));
         Establish_Listening_Socket;
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
      Data : Host_Data;
   begin

      pragma Debug (D (D_Debug, "Initiating connections shutdown"));

      --  The following loop tries to send a QUIT message to any known
      --  partition so that it releases its socket to be able to perform
      --  the shutdown operation on its end.

      for Partition in Partition_ID loop
         Data := Partition_Map.Get_Immediate (Partition);
         if Data.Known and then Data.Connected then
            begin
               Physical_Send (Data.FD, To_Stream_Element_Array (Quit_Code));
            exception
               when Communication_Error => null;
            end;
         end if;
      end loop;

      pragma Debug (D (D_Debug, "Sending a close message to myself"));

      --  Send the same message on a new connection to the partition itself
      --  so that the accept gets the message.

      declare
         FD : C.int;
      begin
         FD := Establish_Connection (Self_Host.Location, Quit_Code);
         FD := Net.C_Close (FD);
      exception
         when Communication_Error => null;
      end;

      Free (Partition_Map);
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

   function To_Stream_Element_Array (Code : Operation_Code)
     return Operation_Code_Array
   is
   begin
      return (others => Operation_Code'Pos (Code));
   end To_Stream_Element_Array;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (Count : Stream_Element_Count)
     return Stream_Element_Count_Array
   is
   begin
      return (1 => Stream_Element (Count / 256 ** 3),
              2 => Stream_Element ((Count / 256 ** 2) mod 256),
              3 => Stream_Element ((Count / 256) mod 256),
              4 => Stream_Element (Count mod 256));
   end To_Stream_Element_Array;

   ------------------------
   -- Write_Partition_ID --
   ------------------------

   procedure Write_Partition_ID (FD        : in C.int;
                                 Partition : in Partition_ID)
   is
      Stream : constant Partition_ID_Array :=
        (1 => Stream_Element (Natural (Partition) / 256),
         2 => Stream_Element (Natural (Partition) mod 256));
   begin
      Physical_Send (FD, Stream);
   end Write_Partition_ID;

end System.Garlic.TCP;
