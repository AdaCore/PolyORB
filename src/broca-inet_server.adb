------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                    B R O C A . I N E T _ S E R V E R                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Broca.Buffers; use Broca.Buffers;
with CORBA; use CORBA;
with Broca.CDR;
with Broca.IOP;
with Broca.IIOP;
with Sockets.Thin; use Sockets.Thin;
with Sockets.Constants; use Sockets.Constants;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Broca.Exceptions;
with Broca.GIOP;
with Broca.Stream; use Broca.Stream;
with Broca.Flags;
with Broca.Server;
with Broca.Sequences;
with Ada.Text_IO;
pragma Elaborate_All (Broca.Exceptions);
pragma Elaborate_All (Broca.Server);
pragma Elaborate_All (CORBA);

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Inet_Server is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.inet_server");
   procedure O is new Broca.Debug.Output (Flag);

   --  The number of simultaneously open streams.
   Simultaneous_Streams : Positive := 10;

   My_Addr : Sockets.Thin.In_Addr;
   IIOP_Host : CORBA.String;
   IIOP_Port : CORBA.Unsigned_Short;
   Listening_Socket : Interfaces.C.int;

   --  Find an IPv4 address for this host.
   procedure Get_Host_Address;

   function Htons (Val : Natural) return Interfaces.C.unsigned_short;
   pragma Inline (Htons);
   function Ntohs (Val : Interfaces.C.unsigned_short) return Natural;
   pragma Inline (Ntohs);

   procedure Signal_Poll_Set_Change;
   procedure Accept_Poll_Set_Change;

   procedure Initialize;

   --  Calling this function will cause the BOA to start accepting requests
   --  from other address spaces.
   procedure Wait_Fd_Request;

   --  Convert an IN_ADDR to the decimal string representation.
   function In_Addr_To_Str (Addr : Sockets.Thin.In_Addr) return String;

   function In_Addr_To_Str (Addr : Sockets.Thin.In_Addr) return String
   is
      function Image (Val : Interfaces.C.unsigned_char) return String;
      function Image (Val : Interfaces.C.unsigned_char) return String is
         Res : String := Interfaces.C.unsigned_char'Image (Val);
      begin
         return Res (Res'First + 1 .. Res'Last);
      end Image;
   begin
      return Image (Addr.S_B1) & '.' & Image (Addr.S_B2) & '.' &
        Image (Addr.S_B3) & '.' & Image (Addr.S_B4);
   end In_Addr_To_Str;

   ----------------------
   -- Get_Host_Address --
   ----------------------

   procedure Get_Host_Address is
      use Interfaces.C;
      use Sockets.Thin;
      Host_Name_Acc : Interfaces.C.Strings.char_array_access;
      Host_Name_Ptr : Interfaces.C.Strings.chars_ptr;
      Len : int;
      Host : Hostent_Access;
   begin
      --  Get the name of the host.
      Host_Name_Acc := new Interfaces.C.char_array (0 .. 63);
      Host_Name_Ptr :=
        Interfaces.C.Strings.To_Chars_Ptr (Host_Name_Acc, False);
      Len := C_Gethostname (Host_Name_Ptr, Host_Name_Acc.all'Length);
      if Len < 0 or Len > Host_Name_Acc.all'Length then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  Get the host entry corresponding to the name.
      Host := C_Gethostbyname (Host_Name_Ptr);
      if Host = null then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  Get the first IP address.
      My_Addr := In_Addr_Access_Pointers.Value (Host.H_Addr_List)(1).all;
   end Get_Host_Address;

   -----------
   -- Htons --
   -----------

   function Htons (Val : Natural) return Interfaces.C.unsigned_short is
   begin
      if Broca.Buffers.Host_Order = Little_Endian then
         return Interfaces.C.unsigned_short
           ((Val / 256) + (Val mod 256) * 256);
      else
         return Interfaces.C.unsigned_short (Val);
      end if;
   end Htons;

   -----------
   -- Ntohs --
   -----------

   function Ntohs (Val : Interfaces.C.unsigned_short) return Natural
   is
      use Interfaces.C;
   begin
      if Broca.Buffers.Host_Order = Little_Endian then
         return Natural
           ((Val / 256) + (Val mod 256) * 256);
      else
         return Natural (Val);
      end if;
   end Ntohs;

   -------------------------------------------
   --
   -------------------------------------------

   subtype Pollfd_Array_Index is Integer range 0 .. Simultaneous_Streams;

   type Job_Kind is (No_Event, Fd_Event);
   type Job (Kind : Job_Kind := No_Event;
             Pollfd_Array_Size : Pollfd_Array_Index := 0)
   is record
      case Kind is
         when No_Event =>
            Poll_Set : aliased Pollfd_Array (1 .. Pollfd_Array_Size);
         when Fd_Event =>
            Fd : Interfaces.C.int := -1;
            Stream : Stream_Ptr := null;
      end case;
   end record;

   protected Lock is

      --  Initialize the work scheduler.
      procedure Initialize (Listening_Socket : Interfaces.C.int;
                            Signal_Fd_Read   : Interfaces.C.int);

      entry Get_Job_And_Lock (A_Job : out Job);
      --  Obtain a pending job to perform. The scheduler is
      --  then locked until Unlock is called.

      procedure Set_Pending_Jobs
        (Returned_Poll_Set : Pollfd_Array);
      --  Set new pending jobs.

      procedure Insert_Descriptor (Sock : Interfaces.C.int);
      --  Insert a descriptor for a newly-opened connection.
      --  Clear events on listening socket.

      procedure Mask_Descriptor (Fd : Interfaces.C.int);
      --  Mask descriptor Fd.
      --  The descriptor is now managed by a server task,
      --  until it calls Delete_Descriptor or Unmask_Descriptor.
      --  This must be called with the scheduler locked.

      procedure Unlock;
      --  Unlock the scheduler.

      procedure Delete_Descriptor (Fd : Interfaces.C.int);
      --  Destroy a descriptor associated with a closed
      --  connection.
      procedure Unmask_Descriptor (Fd : Interfaces.C.int);
      --  Resume waitiong for events on descriptor.

   private

      Locked : Boolean := False;

      Polls : Pollfd_Array (1 .. Simultaneous_Streams)
        := (others => (Fd => Failure, Events => 0, Revents => 0));
      Streams : Stream_Ptr_Array (3 .. Simultaneous_Streams)
        := (others => null);

      Nbr_Fd : Integer;
      Fd_Pos : Integer;
      --  These variables must be accessed under mutual exclusion
      --  Polls represents current tasks-to-perform.
      --  Fd_Pos is used to implement a round-robin scheme
      --  between all open connections.

   end Lock;

   protected body Lock is

      procedure Initialize (Listening_Socket : Interfaces.C.int;
                            Signal_Fd_Read   : Interfaces.C.int)
      is
      begin
         Polls (1).Fd := Listening_Socket;
         Polls (1).Events := Pollin;
         Polls (1).Revents := 0;
         Polls (2).Fd := Signal_Fd_Read;
         Polls (2).Events := Pollin;
         Polls (2).Revents := 0;
         Nbr_Fd := 2;
         Fd_Pos := 1;
         Locked := False;
      end Initialize;

      entry Get_Job_And_Lock (A_Job : out Job) when not Locked is
         Current_Fd_Pos : Integer := Fd_Pos;
      begin
         Locked := True;

         loop
            --  Exit if there is work to perform
            exit when Polls (Current_Fd_Pos).Revents >= Pollin;

            --  Go to the next descriptor
            Current_Fd_Pos := Current_Fd_Pos + 1;
            if Current_Fd_Pos > Nbr_Fd then
               Current_Fd_Pos := 1;
            end if;

            --  If there was no work at all, ask the
            --  caller to wait for an external event.
            if Current_Fd_Pos = Fd_Pos then
               A_Job := (Kind => No_Event,
                         Pollfd_Array_Size => Nbr_Fd,
                         Poll_Set => Polls (1 .. Nbr_Fd));
               return;
            end if;
         end loop;

         Polls (Current_Fd_Pos).Revents := 0;
         --  Clear pending events.

         A_Job := (Kind => Fd_Event,
                   Pollfd_Array_Size => 0,
                   Fd => Polls (Current_Fd_Pos).Fd,
                   Stream => null);
         --  Prepare job structure for calling task.

         if Current_Fd_Pos in Streams'First .. Nbr_Fd then
            A_Job.Stream := Streams (Current_Fd_Pos);
         end if;

         --  Go to the next descriptor
         Fd_Pos := Current_Fd_Pos + 1;
         if Fd_Pos > Nbr_Fd then
            Fd_Pos := 1;
         end if;

         return;
      end Get_Job_And_Lock;

      procedure Set_Pending_Jobs
        (Returned_Poll_Set : Pollfd_Array) is
      begin
         for I in Returned_Poll_Set'Range loop
            Polls (I).Revents := Returned_Poll_Set (I).Revents;
         end loop;
         Fd_Pos := 1;
      end Set_Pending_Jobs;

      procedure Insert_Descriptor (Sock : Interfaces.C.int) is
      begin
         pragma Assert (Nbr_Fd < Polls'Last);

         Nbr_Fd := Nbr_Fd + 1;
         Polls (Nbr_Fd).Fd := Sock;
         Polls (Nbr_Fd).Events := Pollin;
         Polls (Nbr_Fd).Revents := 0;
         Streams (Nbr_Fd) := Broca.Stream.Create_Fd_Stream (Sock);
         Fd_Pos := 2;

         Polls (1).Revents := 0;
         if Nbr_Fd = Polls'Last then
            Polls (1).Events := 0;
         end if;
      end Insert_Descriptor;

      procedure Mask_Descriptor (Fd : Interfaces.C.int) is
      begin
         pragma Assert (Locked);

         for I in Polls'Range loop
            if Polls (I).Fd = Fd then
               Polls (I).Events := 0;
               return;
            end if;
         end loop;

         pragma Assert (False);
      end Mask_Descriptor;

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

      procedure Unmask_Descriptor (Fd : Interfaces.C.int) is
      begin
         for I in Polls'Range loop
            if Polls (I).Fd = Fd then
               pragma Assert (Polls (I).Events = 0);

               Polls (I).Events := Pollin;

               return;
            end if;
         end loop;

         pragma Assert (False);
      end Unmask_Descriptor;

      procedure Delete_Descriptor (Fd : Interfaces.C.int) is
      begin
         for I in Polls'Range loop
            if Polls (I).Fd = Fd then
               pragma Assert (Polls (I).Events = 0);
               --  A descriptor can be deleted only when
               --  masked.

               --  FIXME: remove suspended requests.
               Broca.Stream.Free (Streams (I));
               Streams (I) := Streams (Nbr_Fd);
               Streams (Nbr_Fd) := null;
               Polls (I) := Polls (Nbr_Fd);

               Nbr_Fd := Nbr_Fd - 1;
               if Fd_Pos > Nbr_Fd then
                  Fd_Pos := 1;
               end if;

               if Nbr_Fd < Polls'Last then
                  Polls (1).Events := Pollin;
               end if;

               return;
            end if;
         end loop;

         pragma Assert (False);
         --  Tried to delete an unknown fd.

      end Delete_Descriptor;
   end Lock;

   -------------------------------------------
   -- The core of the Broca Internet server --
   -------------------------------------------

   Signal_Fds : aliased Two_Int;
   Signal_Fd_Read : Interfaces.C.int renames Signal_Fds (0);
   Signal_Fd_Write : Interfaces.C.int renames Signal_Fds (1);

   Fd_Server_Id : Broca.Server.Server_Id_Type;

   procedure Signal_Poll_Set_Change
   is
      C : aliased constant Interfaces.Unsigned_32 := 0;
   begin
      pragma Debug (O ("Signalling poll set change."));
      if C_Send (Signal_Fd_Write, C'Address, 4, 0) = Failure then
         pragma Debug (O ("--> failed!"));
         null;
      end if;
   end Signal_Poll_Set_Change;

   procedure Accept_Poll_Set_Change
   is
      C : aliased Interfaces.Unsigned_32;
   begin
      pragma Debug (O ("Accepting poll set change."));
      if C_Recv (Signal_Fd_Read, C'Address, 4, 0) /= 4 then
         pragma Debug (O ("--> failed!"));
         null;
      end if;
   end Accept_Poll_Set_Change;

   --  Create a listening socket.
   procedure Initialize is
      Sock : Interfaces.C.int;
      Sock_Name : Sockaddr_In;
      Sock_Name_Size : aliased Interfaces.C.int;
      Result : Interfaces.C.int;
   begin
      --  Create the socket.
      Sock := C_Socket (Af_Inet, Sock_Stream, 0);
      if Sock = Failure then
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  Find an address for this host.
      Get_Host_Address;

      --  Bind this socket.
      --  Do not use the default address (security issue), but the address
      --  found.
      Sock_Name.Sin_Family := Af_Inet;
      Sock_Name.Sin_Port := Htons (Broca.Flags.Port);
      Sock_Name.Sin_Addr := My_Addr;
      Result := C_Bind (Sock, Sock_Name'Address, Sock_Name'Size / 8);
      if Result = Failure then
         C_Close (Sock);
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  Listen
      Result := C_Listen (Sock, 8);
      if Result = Failure then
         C_Close (Sock);
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      --  Retrieve the port.
      Sock_Name_Size := Sock_Name'Size / 8;
      Result := C_Getsockname (Sock, Sock_Name'Address, Sock_Name_Size'Access);
      if Result = Failure then
         C_Close (Sock);
         Broca.Exceptions.Raise_Comm_Failure;
      end if;
      IIOP_Port := CORBA.Unsigned_Short
        (Ntohs (Sock_Name.Sin_Port));

      Listening_Socket := Sock;

      --  Stringification of the address.
      declare
         use Ada.Text_IO;
         Addr_Str : String := In_Addr_To_Str (My_Addr);
      begin
         IIOP_Host := CORBA.To_CORBA_String (Addr_Str);
         if Broca.Flags.Verbose then
            Put ("listening on host: ");
            Put (Addr_Str);
            Put (", port: ");
            Put_Line (CORBA.Unsigned_Short'Image (IIOP_Port));
         end if;
      end;

      --  Create signalling socket pair.
      Result := C_Socketpair (Af_Unix, Sock_Stream, 0, Signal_Fds'Address);
      if Result = Failure then
         C_Close (Sock);
         Broca.Exceptions.Raise_Comm_Failure;
      end if;

      Lock.Initialize (Listening_Socket, Signal_Fd_Read);
   end Initialize;

   --  Calling this function will cause the BOA to start accepting requests
   --  from other address spaces.
   procedure Wait_Fd_Request is
      use Interfaces.C;
      use Sockets.Constants;

      use Broca.Opaque;
      use Broca.GIOP;

      Buffer : aliased Buffer_Type;
      Res : C.int;
      Sock : Interfaces.C.int;
      Sock_Name : Sockaddr_In;
      Size : aliased C.int;
      Bytes : Octet_Array (1 .. Message_Header_Size);
      A_Job : Job;

   begin
      Broca.Server.Log ("Enter Wait_Fd_Request");
      Broca.Server.New_Request (Fd_Server_Id);

      --  Try to obtain some work to be done.
      Broca.Server.Log ("Waiting for something to do");
      Lock.Get_Job_And_Lock (A_Job);
      Broca.Server.Log ("Got something to do");

      --  The pending work repository is now locked.

      if A_Job.Kind = No_Event then
         --  There was no work available. Wait for some.

         Broca.Server.Log ("polling"
                           & A_Job.Poll_Set'Length'Img & " fds:");
         for I in 1 .. A_Job.Poll_Set'Length loop
            if A_Job.Poll_Set (I).Events >= Pollin then
               Broca.Server.Log
                 (" waiting on " & A_Job.Poll_Set (I).Fd'Img);
            end if;
         end loop;

         Res := C_Poll (A_Job.Poll_Set'Address, A_Job.Poll_Set'Length, -1);
         Broca.Server.Log ("poll returned " & Res'Img);
         if Res = 0 then
            --  This should never happen.
            pragma Assert (False);
            null;

         elsif Res < 0 then
            Broca.Exceptions.Raise_Comm_Failure;
         end if;

         Lock.Set_Pending_Jobs (A_Job.Poll_Set);
         Lock.Unlock;

      elsif A_Job.Fd = Listening_Socket then

         --  Accepting a connection.
         Broca.Server.Log ("accepting");

         Size := 0;
         Size := Sock_Name'Size / 8;
         Sock :=
           C_Accept (Listening_Socket, Sock_Name'Address, Size'Access);
         if Sock = Failure then
            Broca.Exceptions.Raise_Comm_Failure;
         end if;

         Broca.Server.Log
           ("accepting a connection of fd" & C.int'Image (Sock)
            & " from " & In_Addr_To_Str (Sock_Name.Sin_Addr)
            & " port" & Natural'Image (Ntohs (Sock_Name.Sin_Port)));

         Lock.Insert_Descriptor (Sock);
         Lock.Unlock;

      elsif A_Job.Fd = Signal_Fd_Read then
         Broca.Server.Log ("poll set change");

         Accept_Poll_Set_Change;
         Lock.Unlock;

      else
         Broca.Server.Log ("data on fd" & A_Job.Fd'Img);

         --  Prevent poll from accepting messages from this file
         --  descriptor, because the server is now in charge
         --  of conducting the dialog, until the incoming
         --  request is handled.

         Lock.Mask_Descriptor (A_Job.Fd);
         Lock.Unlock;

         Sock := A_Job.Fd;

         --  Receive a message header
         Res := C_Recv (Sock, Bytes'Address, Message_Header_Size, 0);
         if Res <= 0 then
            if Broca.Flags.Log then
               if Res < 0 then
                  Broca.Server.Log
                    ("error " & C.int'Image (Res)
                     & " on fd" & C.int'Image (Sock));
               else
                  Broca.Server.Log
                    ("connection closed on fd" & C.int'Image (Sock));
               end if;
            end if;

            --  The fd was closed
            C_Close (Sock);

            Broca.Server.Log ("Deleting fd" & A_Job.Fd'Img);
            Lock.Delete_Descriptor (A_Job.Fd);
            Signal_Poll_Set_Change;

         elsif Res /= Broca.GIOP.Message_Header_Size then
            Broca.Exceptions.Raise_Comm_Failure;
         else
            declare
               Endianness : Endianness_Type;
               Byte_Order_Offset : constant := 6;
               --  The offset of the byte_order boolean field in
               --  a GIOP message header.

               --  FIXME: This really belongs only in Broca.GIOP
            begin
               pragma Debug (O ("Received request"));

               if CORBA.Boolean'Val
                 (CORBA.Octet (Bytes (Bytes'First
                  + Byte_Order_Offset)) and 1) then
                  Endianness := Little_Endian;
               else
                  Endianness := Big_Endian;
               end if;

               --  A message header was correctly received.
               Initialize_Buffer
                 (Buffer'Access,
                  Bytes'Length,
                  Bytes (Bytes'First)'Address,
                  Endianness,
                  0);
            end;

            Broca.Server.Log
              ("message received from fd" & Interfaces.C.int'Image (Sock));
            Broca.Stream.Lock_Receive (A_Job.Stream);
            Broca.Server.Handle_Message
              (A_Job.Stream, Buffer'Access);
            Release (Buffer);

            Broca.Server.Log ("Request done for" & A_Job.Fd'Img);
            Lock.Unmask_Descriptor (A_Job.Fd);
            Broca.Server.Log ("Unmasked fd" & A_Job.Fd'Img);
            Signal_Poll_Set_Change;
         end if;
      end if;
   end Wait_Fd_Request;

   type Fd_Server_Type is
     new Broca.Server.Server_Type with null record;

   procedure Perform_Work
     (Server : access Fd_Server_Type);

   function Can_Create_Profile
     (Server : access Fd_Server_Type)
     return Boolean;

   procedure Marshall_Profile
     (Server     : access Fd_Server_Type;
      IOR        : access Buffer_Type;
      Object_Key : Encapsulation);

   procedure Perform_Work (Server : access Fd_Server_Type) is
   begin
      Wait_Fd_Request;
   end Perform_Work;

   --------------------------------
   -- Server profile marshalling --
   --------------------------------

   function Can_Create_Profile
     (Server : access Fd_Server_Type)
     return Boolean is
   begin
      return True;
      --  The Fd server can associate a TAG_INTERNET_IOP
      --  profile to an object.
   end Can_Create_Profile;

   ----------------------
   -- Marshall_Profile --
   ----------------------

   --  Marshall a TAG_INTERNET_IOP TaggedProfile into buffer
   --  IOR.

   procedure Marshall_Profile
     (Server     : access Fd_Server_Type;
      IOR        : access Broca.Buffers.Buffer_Type;
      Object_Key : Encapsulation)
   is
      use Broca.CDR;
      use Broca.Sequences;

      Server_Profile : aliased Broca.IIOP.Profile_IIOP_Type;
   begin
      Server_Profile.Host := IIOP_Host;
      Server_Profile.Port := IIOP_Port;
      Server_Profile.Object_Key := Octet_Sequences.To_Sequence
        (To_CORBA_Octet_Array (Object_Key));

      Marshall (IOR, Broca.IOP.Tag_Internet_IOP);
      Broca.IOP.Callbacks (Broca.IOP.Tag_Internet_IOP).Marshall_Profile_Body
        (IOR, Server_Profile'Access);
   end Marshall_Profile;

   ----------------------
   -- Elaboration code --
   ----------------------

   type Fd_Server_Ptr is access Fd_Server_Type;
   The_Fd_Server : Fd_Server_Ptr;
begin
   Initialize;

   The_Fd_Server := new Fd_Server_Type;
   Broca.Server.Register
     (Broca.Server.Server_Ptr (The_Fd_Server), Fd_Server_Id);

   --  There is always one request to handle: accepting a connection.
   Broca.Server.New_Request (Fd_Server_Id);
exception
   when CORBA.Comm_Failure =>
      Broca.Server.Log ("Failed to initialize Inet server.");
end Broca.Inet_Server;
