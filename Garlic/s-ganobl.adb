------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--           S Y S T E M . G A R L I C . N O N _ B L O C K I N G            --
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

with Ada.Exceptions;                      use Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions);
with Ada.Interrupts.Names;
with GNAT.OS_Lib;                         use GNAT.OS_Lib;
with System.Garlic.Constants;             use System.Garlic.Constants;
with System.Garlic.Debug;                 use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Priorities;
with System.Garlic.Soft_Links;
with System.Garlic.Thin;                  use System.Garlic.Thin;

package body System.Garlic.Non_Blocking is

   --  This package works by maintaining two global file descriptors set
   --  and using protected types (with entry families).

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GANOBL", "(s-ganobl): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use C, Strings;

   Use_Poll : constant Boolean := False;
   --  We HAVE to solve this issue later (???)

   Safety_Delay : constant Duration := 1.0;
   --  A SIGIO will be simulated every Safety_Delay seconds, to make
   --  sure we do not get stuned because we have missed one of them.

   subtype Descriptors is int range 0 .. 127;
   --  At most 128 file descriptors are available. This is used to limit
   --  the size of entry families.

   Initialized : Natural := 0;

   type Desc_Set is array (Descriptors) of Boolean;

   protected Asynchronous is

      entry Close (Descriptors);

      function Is_Open (Socket : Descriptors) return Boolean;

      procedure Open  (Socket : in Descriptors);

      entry Recv (Descriptors)
        (Buffer : in System.Address;
         Length : in int;
         Flags  : in int;
         Result : out int);
      --  You may wait for data to be here by setting Nbyte to zero

      entry Send (Descriptors)
        (Buffer : in System.Address;
         Length : in int;
         Flags  : in int;
         Result : out int);
      --  You may wait for data to be written by setting Nbyte to zero

      procedure Get_Masks
        (Recv_M : out Desc_Set;
         Send_M : out Desc_Set;
         Max    : out int);
      --  Get the masks on which select should apply

      procedure Set_Masks
        (Recv_M : in Desc_Set;
         Send_M : in Desc_Set);
      --  Set the masks as returned by C_Select

   private

      entry Close_Requeue (Descriptors);

      entry Recv_Requeue (Descriptors)
        (Buffer : in System.Address;
         Length : in int;
         Flags  : in int;
         Result : out int);

      entry Send_Requeue (Descriptors)
        (Buffer : in System.Address;
         Length : in int;
         Flags  : in int;
         Result : out int);

      Open_Desc_Set   : Desc_Set       := (others => False);
      Recv_Mask       : Desc_Set       := (others => False);
      Send_Mask       : Desc_Set       := (others => False);
      Ready_Recv_Mask : Desc_Set       := (others => False);
      Ready_Send_Mask : Desc_Set       := (others => False);
      Rfds            : Fd_Set_Access  := new Fd_Set;
      Sfds            : Fd_Set_Access  := new Fd_Set;
      Timeout         : Timeval_Access := new Timeval;
      Max_FD          : int            := -1;

   end Asynchronous;

   PID : constant Thin.pid_t := Thin.C_Getpid;

   procedure Dump_Masks
     (Recv_Mask : in Desc_Set;
      Send_Mask : in Desc_Set;
      Last_Mask : in int);

   procedure Set_Asynchronous_Non_Blocking (FD : in Descriptors);
   pragma Inline (Set_Asynchronous_Non_Blocking);
   --  Set a file descriptor to be asynchronous and non-blocking

   task type Sigio_Simulation_Type is
      entry Shutdown;
      pragma Priority (Priorities.Polling_Priority);
   end Sigio_Simulation_Type;
   --  This task will simulate SIGIOs every <N> seconds

   type Sigio_Simulation_Access is access Sigio_Simulation_Type;

   Sigio_Simulation : Sigio_Simulation_Access;

   task type Selection_Type is
      entry Shutdown;
      pragma Priority (Priorities.Polling_Priority);
   end Selection_Type;
   --  This task is in charge of calling C_Select

   type Selection_Access is access Selection_Type;

   Selection : Selection_Access;

   procedure Check
     (Socket : in     Descriptors;
      R_Mask : in out Boolean;
      S_Mask : in out Boolean);
   --  Check if socket could be used to recv data or to send data. This
   --  procedure should be used in a protected object because the variables
   --  above are used as shared variables.

   SVR4_Stream_IO : Boolean := False;
   --  Will be set to True if SVR4 stream io operations are detected

   ------------------
   -- Asynchronous --
   ------------------

   protected body Asynchronous is

      ------------------------
      -- Asynchronous.Close --
      ------------------------

      entry Close (for Socket in Descriptors) when True is
      begin
         pragma Assert (Open_Desc_Set (Socket));
         Open_Desc_Set (Socket)   := False;
         Ready_Recv_Mask (Socket) := Recv_Mask (Socket);
         Ready_Send_Mask (Socket) := Send_Mask (Socket);
         if Recv_Mask (Socket)
           or else Send_Mask (Socket)
           or else Recv (Socket)'Count /= 0
           or else Send (Socket)'Count /= 0
         then
            requeue Close_Requeue (Socket);
         end if;
      end Close;

      --------------------------------
      -- Asynchronous.Close_Requeue --
      --------------------------------

      entry Close_Requeue (for Socket in Descriptors)
      when not Recv_Mask (Socket)
        and then not Recv_Mask (Socket)
        and then Recv (Socket)'Count = 0
        and then Send (Socket)'Count = 0
      is
      begin
         null;
      end Close_Requeue;

      ----------------------------
      -- Asynchronous.Get_Masks --
      ----------------------------

      procedure Get_Masks
        (Recv_M : out Desc_Set;
         Send_M : out Desc_Set;
         Max    : out int) is
      begin
         Recv_M := Recv_Mask;
         Send_M := Send_Mask;
         Max := Max_FD;
         pragma Debug (Dump_Masks (Recv_Mask, Send_Mask, Max_FD));
      end Get_Masks;

      --------------------------
      -- Asynchronous.Is_Open --
      --------------------------

      function Is_Open (Socket : Descriptors) return Boolean is
      begin
         return Open_Desc_Set (Socket);
      end Is_Open;

      -----------------------
      -- Asynchronous.Open --
      -----------------------

      procedure Open (Socket : in Descriptors) is
      begin
         pragma Assert (not Open_Desc_Set (Socket));
         Open_Desc_Set   (Socket) := True;
         Recv_Mask       (Socket) := False;
         Send_Mask       (Socket) := False;
         Ready_Recv_Mask (Socket) := False;
         Ready_Send_Mask (Socket) := False;
      end Open;

      -----------------------
      -- Asynchronous.Recv --
      -----------------------

      entry Recv (for RRFD in Descriptors)
        (Buffer : in System.Address;
         Length : in int;
         Flags  : in int;
         Result : out int)
      when True is
         Dummy  : int;
         Empty  : Boolean;
         R_Mask : Boolean := True;
         S_Mask : Boolean := False;
      begin
         pragma Assert (not Recv_Mask (RRFD));
         pragma Debug  (D ("Try to recv" & Length'Img &
                           " bytes on peer" & RRFD'Img));

         if not Open_Desc_Set (RRFD) then
            Result := Failure;
            return;
         end if;

         --  We have to handle len = 0 separatly because recv does
         --  not behave the same way on different platforms.

         if Length = 0 then
            Check (RRFD, R_Mask, S_Mask);
            if R_Mask then
               Dummy  := Thin.C_Recv (RRFD, Buffer, Length, Flags);
            end if;
            Empty := not R_Mask;

         else
            Dummy  := Thin.C_Recv (RRFD, Buffer, Length, Flags);
            pragma Debug (D ("Recv length = " & Length'Img));
            pragma Debug (D ("Recv return = " & Dummy'Img));
            if Dummy < 0 then
               pragma Debug (D ("Recv errno  = " & Errno'Img));
               Empty := Errno = Ewouldblock or else Errno = Eintr;
            else
               Empty := False;
            end if;
         end if;

         if Empty then
            Recv_Mask (RRFD) := True;
            if RRFD > Max_FD then
               Max_FD := RRFD;
            end if;
            Ready_Recv_Mask (RRFD) := False;
            pragma Debug (Dump_Masks (Recv_Mask, Send_Mask, Max_FD));
            requeue Recv_Requeue (RRFD) with abort;

         else
            Result := Dummy;
         end if;
      end Recv;

      -------------------------------
      -- Asynchronous.Recv_Requeue --
      -------------------------------

      entry Recv_Requeue (for RRFD in Descriptors)
        (Buffer : in System.Address;
         Length : in int;
         Flags  : in int;
         Result : out int)
      when Ready_Recv_Mask (RRFD) is
         Dummy : int;
         Empty : Boolean;
      begin
         pragma Debug
           (D ("Retry to recv" & Length'Img & " bytes on peer" & RRFD'Img));

         if not Open_Desc_Set (RRFD) then
            Recv_Mask (RRFD)       := False;
            Ready_Recv_Mask (RRFD) := False;
            Result := Failure;
            return;
         end if;

         if Length = 0 then
            Dummy  := Thin.C_Recv (RRFD, Buffer, Length, Flags);
            Empty  := False;

         else
            Dummy  := Thin.C_Recv (RRFD, Buffer, Length, Flags);
            pragma Debug (D ("Recv length = " & Length'Img));
            pragma Debug (D ("Recv return = " & Dummy'Img));
            if Dummy < 0 then
               pragma Debug (D ("Recv errno  = " & Errno'Img));
               Empty := Errno = Ewouldblock or else Errno = Eintr;
            else
               Empty := False;
            end if;
         end if;

         Ready_Recv_Mask (RRFD) := False;
         if Empty then
            requeue Recv_Requeue (RRFD) with abort;

         else
            Result := Dummy;
            Recv_Mask (RRFD) := False;
            if Max_FD = RRFD then
               Max_FD := -1;
               for I in reverse Descriptors'First .. RRFD loop
                  if Recv_Mask (I) or else Send_Mask (I) then
                     Max_FD := I;
                     exit;
                  end if;
               end loop;
            end if;
            pragma Debug (Dump_Masks (Recv_Mask, Send_Mask, Max_FD));
         end if;
      end Recv_Requeue;

      -----------------------
      -- Asynchronous.Send --
      -----------------------

      entry Send (for RSFD in Descriptors)
        (Buffer : in System.Address;
         Length : in int;
         Flags  : in int;
         Result : out int)
      when True is
         Dummy  : int;
         Empty  : Boolean;
         R_Mask : Boolean := False;
         S_Mask : Boolean := True;
      begin
         pragma Assert (not Send_Mask (RSFD));
         pragma Debug
           (D ("Try to send" & Length'Img & " bytes on peer" & RSFD'Img));

         if not Open_Desc_Set (RSFD) then
            Result := Failure;
            return;
         end if;

         --  We have to handle len = 0 separatly because send does
         --  not behave the same way on different platforms.

         if Length = 0 then
            Check (RSFD, R_Mask, S_Mask);
            Dummy := 0;
            Empty := not S_Mask;

         else
            --  Check (RSFD, R_Mask, S_Mask);
            --  if not S_Mask then
            --     Result := -1;
            --     return;
            --  end if;
            Dummy  := Thin.C_Send (RSFD, Buffer, Length, Flags);
            pragma Debug (D ("Send length = " & Length'Img));
            pragma Debug (D ("Send return = " & Dummy'Img));
            if Dummy > 0 then
               Empty := False;
            elsif Dummy < 0 then
               pragma Debug (D ("Send errno  = " & Errno'Img));
               Empty := Errno = Ewouldblock; --  or else Errno = Eagain;
            else
               Empty := True;
            end if;
         end if;

         if Empty then
            Send_Mask (RSFD) := True;
            if RSFD > Max_FD then
               Max_FD := RSFD;
            end if;
            Ready_Send_Mask (RSFD) := False;
            pragma Debug (Dump_Masks (Recv_Mask, Send_Mask, Max_FD));
            requeue Send_Requeue (RSFD) with abort;

         else
            Result := Dummy;
         end if;
      end Send;

      -------------------------------
      -- Asynchronous.Send_Requeue --
      -------------------------------

      entry Send_Requeue (for RSFD in Descriptors)
        (Buffer : in System.Address;
         Length : in int;
         Flags  : in int;
         Result : out int)
      when Ready_Send_Mask (RSFD) is
         Dummy  : int;
         Empty  : Boolean;
         --  R_Mask : Boolean := False;
         --  S_Mask : Boolean := True;
      begin
         pragma Debug
           (D ("Retry to send" & Length'Img & " bytes on peer" & RSFD'Img));

         if not Open_Desc_Set (RSFD) then
            Send_Mask (RSFD)       := False;
            Ready_Send_Mask (RSFD) := False;
            Result := Failure;
            return;
         end if;

         if Length = 0 then
            Dummy := 0;
            Empty := False;

         else
            --  Check (RSFD, R_Mask, S_Mask);
            --  if not S_Mask then
            --     Result := -1;
            --     return;
            --  end if;
            Dummy  := Thin.C_Send (RSFD, Buffer, Length, Flags);
            pragma Debug (D ("Send length = " & Length'Img));
            pragma Debug (D ("Send return = " & Dummy'Img));
            if Dummy > 0 then
               Empty := False;
            elsif Dummy < 0 then
               pragma Debug (D ("Send errno  = " & Errno'Img));
               Empty := Errno = Ewouldblock; --  or else Errno = Eagain;
            else
               Empty := True;
            end if;
         end if;

         Ready_Send_Mask (RSFD) := False;
         if Empty then
            requeue Send_Requeue (RSFD) with abort;

         else
            Result := Dummy;
            Send_Mask (RSFD) := False;
            if Max_FD = RSFD then
               Max_FD := -1;
               for I in reverse Descriptors'First .. RSFD loop
                  if Recv_Mask (I) or else Send_Mask (I) then
                     Max_FD := I;
                     exit;
                  end if;
               end loop;
            end if;
            pragma Debug (Dump_Masks (Recv_Mask, Send_Mask, Max_FD));
         end if;
      end Send_Requeue;

      ----------------------------
      -- Asynchronous.Set_Masks --
      ----------------------------

      procedure Set_Masks
        (Recv_M : in Desc_Set;
         Send_M : in Desc_Set)
      is
      begin
         for I in Recv_M'Range loop
            if Recv_M (I) then
               Ready_Recv_Mask (I) := True;
            end if;
            if Send_M (I) then
               Ready_Send_Mask (I) := True;
            end if;
         end loop;
      end Set_Masks;
   end Asynchronous;

   --------------
   -- C_Accept --
   --------------

   function C_Accept
     (Socket  : int;
      Addr    : System.Address;
      Addrlen : access int)
     return int
   is
      Dummy     : int;
      Dummy_CP  : constant System.Address := Null_Address;
      Return_FD : int;
   begin
      pragma Assert (Asynchronous.Is_Open (Socket));
      Set_Asynchronous_Non_Blocking (Socket);
      loop
         Asynchronous.Recv (Socket) (Dummy_CP, 0, 0, Dummy);
         pragma Debug (D ("There is something to accept"));
         Return_FD := Thin.C_Accept (Socket, Addr, Addrlen);
         exit when Return_FD /= Failure or else Errno /= Eagain;
         pragma Debug (D ("Cannot accept on peer" & Socket'Img &
                          " (errno =" & Errno'Img & ")"));
         delay 0.2;
      end loop;
      if Return_FD /= Failure then
         Asynchronous.Open (Return_FD);
         Set_Asynchronous_Non_Blocking (Return_FD);
      end if;
      return Return_FD;
   end C_Accept;

   -------------
   -- C_Close --
   -------------

   function C_Close (Socket : C.int) return C.int is
   begin
      pragma Debug (D ("Close peer" & Socket'Img));
      Asynchronous.Close (Socket);
      return Thin.C_Close (Socket);
   end C_Close;

   ---------------
   -- C_Connect --
   ---------------

   function C_Connect
     (Socket  : int;
      Name    : System.Address;
      Namelen : int)
     return int
   is
      Dummy    : int;
      Dummy_CP : constant System.Address := Null_Address;
   begin
      pragma Assert (Asynchronous.Is_Open (Socket));
      Set_Asynchronous_Non_Blocking (Socket);
      Dummy := Thin.C_Connect (Socket, Name, Namelen);
      pragma Debug
        (D ("Try to connect (result = " & C.int'Image (Dummy) &
            " - errno =" & Integer'Image (Errno) & ")"));
      if Dummy /= Thin.Failure
        or else Errno /= Einprogress
      then
         return Dummy;
      end if;
      Asynchronous.Send (Socket) (Dummy_CP, 0, 0, Dummy);
      Dummy := Thin.C_Connect (Socket, Name, Namelen);
      pragma Debug
        (D ("Retry to connect (result = " & C.int'Image (Dummy) &
            " - errno =" & Integer'Image (Errno) & ")"));
      if Dummy = Thin.Failure
        and then Errno = Eisconn
      then
         return Thin.Success;
      else
         return Dummy;
      end if;
   end C_Connect;

   ------------
   -- C_Recv --
   ------------

   function C_Recv
     (Socket : int;
      Buffer : System.Address;
      Length : int;
      Flags  : int)
     return int
   is
      Count : int;
   begin
      pragma Debug (D ("Recv on peer" & Socket'Img));
      Asynchronous.Recv (Socket) (Buffer, Length, Flags, Count);
      return Count;
   end C_Recv;

   ------------
   -- C_Send --
   ------------

   function C_Send
     (Socket : C.int;
      Buffer : System.Address;
      Length : C.int;
      Flags  : C.int)
     return int
   is
      Count : int;
   begin
      pragma Debug (D ("Send to peer" & Socket'Img));
      Asynchronous.Send (Socket) (Buffer, Length, Flags, Count);
      return Count;
   end C_Send;

   --------------
   -- C_Socket --
   --------------

   function C_Socket
     (Domain, Typ, Protocol : C.int)
     return C.int
   is
      Result : C.int;
   begin
      Result := Thin.C_Socket (Domain, Typ, Protocol);
      if Result /= Failure then
         Asynchronous.Open (Result);
      end if;
      return Result;
   end C_Socket;

   -----------
   -- Check --
   -----------

   procedure Check
     (Socket : in     Descriptors;
      R_Mask : in out Boolean;
      S_Mask : in out Boolean)
   is
      Dummy   : int;
      Rfds    : aliased Fd_Set;
      Sfds    : aliased Fd_Set;
      Pfd     : aliased Pollfd;
      Timeout : aliased Timeval;
   begin
      if Use_Poll then

         --  There is something to do on this file descriptor

         Pfd.Fd := Socket;
         Pfd.Events := 0;

         if R_Mask then
            Pfd.Events := Pfd.Events + Pollin;
         end if;

         if S_Mask then
            Pfd.Events := Pfd.Events + Pollout;
         end if;

         Dummy := Thin.C_Poll (Pfd'Address, 1, 0);

         R_Mask := (Pfd.Revents / Pollin) mod 2 = 1;
         S_Mask := (Pfd.Revents / Pollout) mod 2 = 1;

      else

         Clear (Rfds);
         Clear (Sfds);

         if R_Mask then
            Set (Rfds, Socket_Fd (Socket));
         end if;

         if S_Mask then
            Set (Sfds, Socket_Fd (Socket));
         end if;

         Timeout := Immediat;
         Dummy   := C_Select (Socket + 1,
                              Rfds'Unchecked_Access,
                              Sfds'Unchecked_Access,
                              null,
                              Timeout'Unchecked_Access);

         if Dummy = Failure then
            R_Mask := False;
            R_Mask := False;
         else
            R_Mask := Is_Set (Rfds, Socket_Fd (Socket));
            S_Mask := Is_Set (Sfds, Socket_Fd (Socket));
         end if;
         pragma Debug (D ("C_Select (F: " & Socket'Img &
                          ", R: " & R_Mask'Img &
                          ", S: " & S_Mask'Img &
                          ") = "  & Dummy'Img));
      end if;
   end Check;

   ----------------
   -- Dump_Masks --
   ----------------

   procedure Dump_Masks
     (Recv_Mask : in Desc_Set;
      Send_Mask : in Desc_Set;
      Last_Mask : in int) is
   begin
      if Debug_Mode (Private_Debug_Key)
        and then Last_Mask >= 0
      then
         declare
            R : String (1 .. Natural (Last_Mask + 1)) := (others => '0');
            S : String (1 .. Natural (Last_Mask + 1)) := (others => '0');
         begin
            for I in R'Range loop
               if Recv_Mask (Descriptors (I - 1)) then
                  R (I) := '1';
               end if;
               if Send_Mask (Descriptors (I - 1)) then
                  S (I) := '1';
               end if;
            end loop;
            pragma Debug (D ("Recv_Mask = " & R));
            pragma Debug (D ("Send_Mask = " & S));
         end;
      end if;
   end Dump_Masks;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Initialized = 0 then
         pragma Debug (D ("Initialize thread blocking IO unit"));
         if Selection = null then
            Selection := new Selection_Type;
         end if;
         if Sigio_Simulation = null then
            Sigio_Simulation := new Sigio_Simulation_Type;
         end if;
      end if;
      Initialized := Initialized + 1;
   end Initialize;

   --------------------
   -- Selection_Type --
   --------------------

   task body Selection_Type is
      RFD, SFD     : Desc_Set;
      Max          : aliased int;
      Dummy        : int;
      Terminated   : Boolean;
   begin
      Soft_Links.Add_Non_Terminating_Task;
      loop
         Sigio.Wait (Terminated);
         pragma Debug (D ("Activate selection"));

         exit when Terminated;

         Asynchronous.Get_Masks (RFD, SFD, Max);

         if Max > -1 then
            for I in RFD'First .. Max loop
               if RFD (I) or else SFD (I) then
                  pragma Debug (D ("Selection checks peer" & I'Img));
                  Check (I, RFD (I), SFD (I));
               end if;
            end loop;
            Asynchronous.Set_Masks (RFD, SFD);
         end if;
      end loop;
      pragma Debug (D ("Selection terminated"));
      Soft_Links.Sub_Non_Terminating_Task;
      accept Shutdown;
   exception
      when E : others =>
         --  Because Shutdown has to be synchronous, an exception
         --  should not be raised in this task.
         pragma Warnings (Off, E);
         pragma Debug (D ("Selection: " & Exception_Name (E)));
         pragma Debug (D ("Selection: " & Exception_Information (E)));
         null;
   end Selection_Type;

   -----------------------------------
   -- Set_Asynchronous_Non_Blocking --
   -----------------------------------

   procedure Set_Asynchronous_Non_Blocking (FD : in Descriptors) is
      Dummy : int;
   begin
      if not SVR4_Stream_IO then
         if Thin.C_Fcntl (FD, F_Setown, int (PID)) = Thin.Failure then
            SVR4_Stream_IO := True;
         end if;
      end if;

      if not SVR4_Stream_IO then
         --  Use an alternate method for SVR4

         Dummy := Thin.C_Fcntl (FD, F_Setfl, Fasync + Fndelay);
         pragma Debug (D ("Set fasync/fndelay (return = " & Dummy'Img & ")"));

      elsif I_Setsig /= -1
        and then S_Rdnorm /= -1
        and then S_Wrnorm /= -1
      then
         --  Use an alternate method for SVR4

         Dummy := Thin.C_Fcntl (FD, F_Setfl, Fndelay);
         pragma Debug (D ("Set fasync/fndelay (return = " & Dummy'Img & ")"));
         Dummy := Thin.C_Ioctl (FD, I_Setsig, S_Rdnorm + S_Wrnorm);
         pragma Debug (D ("Set signal W/R (return = " & Dummy'Img & ")"));
      end if;
   end Set_Asynchronous_Non_Blocking;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Initialized := Initialized - 1;
      if Initialized = 0 then
         Sigio.Shutdown;
         for D in Descriptors loop
            if Asynchronous.Is_Open (D) then
               Asynchronous.Close (D);
            end if;
         end loop;
         if Sigio_Simulation /= null then
            Sigio_Simulation.Shutdown;
            Selection.Shutdown;
         end if;
      end if;
   end Shutdown;

   -----------
   -- Sigio --
   -----------

   protected body Sigio is

      --------------------
      -- Sigio.Shutdown --
      --------------------

      procedure Shutdown is
      begin
         Occurred  := True;
         Terminated := True;
      end Shutdown;

      ------------------
      -- Sigio.Signal --
      ------------------

      procedure Signal is
      begin
         Signals  := Signals + 1;
         Occurred := True;
      end Signal;

      -----------------
      -- Sigio.Stats --
      -----------------

      procedure Stats (S, T : out Natural) is
      begin
         S := Signals;
         T := Timeouts;
      end Stats;

      -------------------
      -- Sigio.Timeout --
      -------------------

      procedure Timeout (Stop : out Boolean) is
      begin
         Timeouts := Timeouts + 1;
         Occurred := True;
         Stop     := Terminated;
      end Timeout;

      ----------------
      -- Sigio.Wait --
      ----------------

      entry Wait (Stop : out Boolean) when Occurred is
      begin
         Stop     := Terminated;
         Occurred := Terminated;
      end Wait;

   end Sigio;

   ----------------------
   -- Sigio_Simulation --
   ----------------------

   task body Sigio_Simulation_Type is
      Signals   : Natural;
      Timeouts  : Natural;
      Terminated : Boolean := False;
   begin
      Soft_Links.Add_Non_Terminating_Task;
      while not Terminated loop
         delay Safety_Delay;
         pragma Debug (D ("Simulate sigio"));
         Sigio.Timeout (Terminated);
      end loop;
      Sigio.Stats (Signals, Timeouts);
      pragma Debug (D ("signals  =" & Signals'Img));
      pragma Debug (D ("timeouts =" & Timeouts'Img));
      Soft_Links.Sub_Non_Terminating_Task;
      accept Shutdown;
   exception
      when E : others =>
         --  Because Shutdown has to be synchronous, an exception
         --  should not be raised in this task.
         pragma Warnings (Off, E);
         pragma Debug (D ("Sigio_Simulation: " & Exception_Name (E)));
         pragma Debug (D ("Sigio_Simulation: " & Exception_Information (E)));
         null;
   end Sigio_Simulation_Type;

end System.Garlic.Non_Blocking;
