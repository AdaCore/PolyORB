with Broca.Buffers; use Broca.Buffers;
with CORBA; use CORBA;
with CORBA.IOP;
with Sockets.Thin; use Sockets.Thin;
with Sockets.Constants; use Sockets.Constants;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Broca.Exceptions;
with Broca.Marshalling;
with Broca.GIOP;
with Broca.Stream; use Broca.Stream;
with Broca.Flags;
with Broca.Server;
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
   function Ntohs (Val : Interfaces.C.unsigned_short) return Natural;

   procedure Signal_Poll_Set_Change;
   procedure Accept_Poll_Set_Change;

   procedure Initialize;

   --  Calling this function will cause the BOA to start accepting requests
   --  from other address spaces.
   procedure Wait_Fd_Request (Buffer : in out Buffer_Descriptor);

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
      if Broca.Buffers.Is_Little_Endian then
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
      if Broca.Buffers.Is_Little_Endian then
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
            Pos : Integer := -1;
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
      --  then locked until a call to a _Unlock subprogram is
      --  made.

      procedure Set_Pending_Jobs
        (Returned_Poll_Set : Pollfd_Array);
      --  Set new pending jobs.

      procedure Insert_Descriptor (Sock : Interfaces.C.int);
      --  Insert a descriptor for a newly-opened connection.
      --  Clear events on listening socket.

      procedure Mask_Descriptor (Pos : Integer);
      --  Mask descriptor and clear events. The descriptor
      --  is now managed by a server task, until it calls
      --  Delete_Descriptor or Unmask_Descriptor.

      procedure Unlock;
      --  Unlock the scheduler.

      procedure Delete_Descriptor (Pos : Integer);
      --  Destroy a descriptor associated with a closed connection.

      procedure Unmask_Descriptor (Pos : Integer);
      --  Resume waitiong for events on descriptor.

   private

      Locked : Boolean;
      Polls : Pollfd_Array (1 .. Simultaneous_Streams);
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
         Polls (2).Fd := Signal_Fd_Read;
         Polls (2).Events := Pollin;
         Nbr_Fd := 2;
         Fd_Pos := 1;
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
         --  Clear pending job.

         A_Job := (Kind => Fd_Event,
                   Pollfd_Array_Size => 0,
                   Pos => Current_Fd_Pos,
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

      procedure Mask_Descriptor (Pos : Integer) is
      begin
         Polls (Pos).Events := 0;
      end Mask_Descriptor;

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

      procedure Unmask_Descriptor (Pos : Integer) is
      begin
         Polls (Pos).Events := Pollin;
      end Unmask_Descriptor;

      procedure Delete_Descriptor (Pos : Integer) is
      begin
         pragma Assert (Pos in 1 .. Nbr_Fd);

         --  FIXME: remove suspended requests.
         Broca.Stream.Unchecked_Deallocation (Streams (Pos));
         Streams (Pos) := Streams (Nbr_Fd);
         Streams (Nbr_Fd) := null;
         Polls (Pos) := Polls (Nbr_Fd);

         Nbr_Fd := Nbr_Fd - 1;
         if Fd_Pos > Nbr_Fd then
            Fd_Pos := 1;
         end if;

         if Nbr_Fd < Polls'Last then
            Polls (1).Events := Pollin;
         end if;
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
      C : aliased constant Character := Character'Val (0);
   begin
      pragma Debug (O ("Signalling poll set change."));
      if C_Send (Signal_Fd_Write, C'Address, 1, 0) = Failure then
         pragma Debug (O ("--> failed!"));
         null;
      end if;
   end Signal_Poll_Set_Change;

   procedure Accept_Poll_Set_Change
   is
      C : aliased Character;
   begin
      pragma Debug (O ("Accepting poll set change."));
      if C_Recv (Signal_Fd_Read, C'Address, 1, 0) = Failure then
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
      IIOP_Port := CORBA.Unsigned_Short (Sock_Name.Sin_Port);

      --  Network to host byte order conversion.
      if Broca.Buffers.Is_Little_Endian then
         IIOP_Port := (IIOP_Port / 256) + (IIOP_Port mod 256) * 256;
      end if;

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
   procedure Wait_Fd_Request (Buffer : in out Buffer_Descriptor)
   is
      use Broca.Marshalling;
      use Sockets.Constants;
      use Interfaces.C;
      use Broca.GIOP;
      Res : C.int;
      Sock : Interfaces.C.int;
      Sock_Name : Sockaddr_In;
      Size : aliased C.int;
      Bytes : Buffer_Type (0 .. Message_Header_Size - 1);
      A_Job : Job;

   begin
      Broca.Server.Log ("Enter Wait_Fd_Request");
      Broca.Server.New_Request (Fd_Server_Id);

      --  Try to obtain some work to be done.
      Lock.Get_Job_And_Lock (A_Job);

      --  The pending work repository is now locked.

      if A_Job.Kind = No_Event then
         --  There was no work available. Wait for some.

         Broca.Server.Log ("polling"
                           & A_Job.Poll_Set'Length'Img & " fds:");
         for I in 1 .. A_Job.Poll_Set'Length loop
            if A_Job.Poll_Set (I).Events >= Pollin then
               Broca.Server.Log (" waiting on " & I'Img);
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

      elsif A_Job.Pos = 1 then

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

      elsif A_Job.Pos = 2 then
         Broca.Server.Log ("poll set change");

         Accept_Poll_Set_Change;
         Lock.Unlock;

      else
         Broca.Server.Log ("data at position " & A_Job.Pos'Img);

         --  There is work to be done at position A_Job.Pos
         --  Prevent poll from accepting messages from this file
         --  descriptor, because the server is now in charge
         --  of conducting the dialog, until the incoming
         --  request is handled.

         Lock.Mask_Descriptor (A_Job.Pos);
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

            Broca.Server.Log ("Deleting fd" & A_Job.Fd'Img
                              & " at" & A_Job.Pos'Img);
            Lock.Delete_Descriptor (A_Job.Pos);
            Signal_Poll_Set_Change;

         elsif Res /= Broca.GIOP.Message_Header_Size then
            Broca.Exceptions.Raise_Comm_Failure;
         else
            --  A message header was correctly received.
            Allocate_Buffer_And_Clear_Pos
              (Buffer, Broca.GIOP.Message_Header_Size);
            Write (Buffer, Bytes);

            Broca.Server.Log
              ("message received from fd" & Interfaces.C.int'Image (Sock)
               & " at" & A_Job.Pos'Img);
            Broca.Stream.Lock_Receive (A_Job.Stream);
            Broca.Server.Handle_Message (A_Job.Stream, Buffer);

            Broca.Server.Log ("Request done for" & A_Job.Pos'Img);
            Lock.Unmask_Descriptor (A_Job.Pos);
            Broca.Server.Log ("Unmasked descriptor for" & A_Job.Pos'Img);
            Signal_Poll_Set_Change;
         end if;
      end if;
   end Wait_Fd_Request;

   type Fd_Server_Type is new Broca.Server.Server_Type with null record;
   procedure Perform_Work (Server : access Fd_Server_Type;
                           Buffer : in out Broca.Buffers.Buffer_Descriptor);
   procedure Marshall_Size_Profile
     (Server : access Fd_Server_Type;
      IOR : in out Broca.Buffers.Buffer_Descriptor;
      Object_Key : Broca.Buffers.Buffer_Descriptor);
   procedure Marshall_Profile (Server : access Fd_Server_Type;
                               IOR : in out Broca.Buffers.Buffer_Descriptor;
                               Object_Key : Broca.Buffers.Buffer_Descriptor);

   procedure Perform_Work (Server : access Fd_Server_Type;
                           Buffer : in out Broca.Buffers.Buffer_Descriptor) is
   begin
      Wait_Fd_Request (Buffer);
   end Perform_Work;

   --------------------------------
   -- Server profile marshalling --
   --------------------------------

   procedure Marshall_Size_Profile
     (Server     : access Fd_Server_Type;
      IOR        : in out Broca.Buffers.Buffer_Descriptor;
      Object_Key : in Broca.Buffers.Buffer_Descriptor)
   is
      use Broca.Marshalling;
   begin
      --  Tag
      Compute_New_Size (IOR, UL_Size, UL_Size);

      --  Profile_Data length
      Compute_New_Size (IOR, UL_Size, UL_Size);

      --  Flag
      Compute_New_Size (IOR, O_Size, O_Size);

      --  Version
      Compute_New_Size (IOR, O_Size, O_Size);
      Compute_New_Size (IOR, O_Size, O_Size);

      --  Host string.
      Compute_New_Size (IOR, IIOP_Host);

      --  Port
      Compute_New_Size (IOR, US_Size, US_Size);
      Align_Size       (IOR, UL_Size);

      --  Key.
      Compute_New_Size (IOR, Object_Key);
   end Marshall_Size_Profile;

   ----------------------
   -- Marshall_Profile --
   ----------------------

   procedure Marshall_Profile
     (Server     : access Fd_Server_Type;
      IOR        : in out Broca.Buffers.Buffer_Descriptor;
      Object_Key : in Broca.Buffers.Buffer_Descriptor)
   is
      use Broca.Marshalling;
   begin
      Marshall (IOR, CORBA.IOP.Tag_Internet_IOP);

      Marshall (IOR, CORBA.Unsigned_Long (Size_Left (IOR) - UL_Size));

      --  Endianess
      Marshall (IOR, Is_Little_Endian);

      --  Version
      Marshall (IOR, CORBA.Octet'(1));
      Marshall (IOR, CORBA.Octet'(0));

      --  Host
      Marshall (IOR, IIOP_Host);

      --  Port
      Marshall (IOR, IIOP_Port);

      Align_Size (IOR, UL_Size);

      --  Object key
      Append_Buffer (IOR, Object_Key);
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
