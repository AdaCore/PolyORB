------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--           S Y S T E M . G A R L I C . N O N _ B L O C K I N G            --
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
with Ada.Interrupts.Names;
with GNAT.OS_Lib;                         use GNAT.OS_Lib;
with System.Garlic.Constants;             use System.Garlic.Constants;
with System.Garlic.Debug;                 use System.Garlic.Debug;
with System.Garlic.Priorities;
with System.Garlic.Soft_Links;
with System.Garlic.TCP;
pragma Elaborate (System.Garlic.TCP);
with System.Garlic.TCP.Platform_Specific;
with System.Garlic.Thin;                  use System.Garlic.Thin;

package body System.Garlic.Non_Blocking is

   --  This package works by maintaining two global file descriptors set
   --  and using protected types (with entry families).

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("NONBLOCKING", "(s-ganobl): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use C, Strings;

   Safety_Delay : constant Duration := 1.0;
   --  A SIGIO will be simulated every Safety_Delay seconds, to make
   --  sure we do not get stuned because we have missed one of them.

   subtype Descriptors is int range 0 .. 127;
   --  At most 128 file descriptors are available. This is used to limit
   --  the size of entry families.

   type Desc_Set is array (Descriptors) of Boolean;

   In_Use   : Desc_Set := (others => False);
   Shutdown : Boolean  := False;

   protected type Asynchronous_Type is

      entry Recv (Descriptors)
        (Buf    : in chars_ptr;
         Len    : in int;
         Flags  : in int;
         Result : out int);
      --  You may wait for data to be here by setting Nbyte to zero

      entry Send (Descriptors)
        (Buf    : in chars_ptr;
         Len    : in int;
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

      entry Recv_Requeue (Descriptors)
        (Buf    : in chars_ptr;
         Len    : in int;
         Flags  : in int;
         Result : out int);
      entry Send_Requeue (Descriptors)
        (Buf    : in chars_ptr;
         Len    : in int;
         Flags  : in int;
         Result : out int);

      Recv_Mask       : Desc_Set := (others => False);
      Send_Mask       : Desc_Set := (others => False);
      Ready_Recv_Mask : Desc_Set := (others => False);
      Ready_Send_Mask : Desc_Set := (others => False);
      Rfds            : Fd_Set_Access := new Fd_Set;
      Sfds            : Fd_Set_Access := new Fd_Set;
      Timeout         : Timeval_Access := new Timeval;
      Max_FD          : int := -1;

   end Asynchronous_Type;

   type Asynchronous_Access is access Asynchronous_Type;

   Asynchronous : Asynchronous_Access := new Asynchronous_Type;

   PID : constant Thin.pid_t := Thin.C_Getpid;

   procedure Set_Asynchronous_Non_Blocking (FD : in Descriptors);
   pragma Inline (Set_Asynchronous_Non_Blocking);
   --  Set a file descriptor to be asynchronous and non-blocking

   task Sigio_Simulation is
      pragma Priority (Priorities.Polling_Priority);
   end Sigio_Simulation;
   --  This task will simulate SIGIOs every <N> seconds

   task Selection is
      pragma Priority (Priorities.Polling_Priority);
   end Selection;
   --  This task is in charge of calling C_Select

   SVR4_Stream_IO : Boolean := False;
   --  Will be set to True if SVR4 stream io operations are detected

   -----------------------
   -- Asynchronous_Type --
   -----------------------

   protected body Asynchronous_Type is

      ---------------
      -- Get_Masks --
      ---------------

      procedure Get_Masks
        (Recv_M : out Desc_Set;
         Send_M : out Desc_Set;
         Max    : out int) is
      begin
         Recv_M := Recv_Mask;
         Send_M := Send_Mask;
         Max := Max_FD;
      end Get_Masks;

      ----------
      -- Recv --
      ----------

      entry Recv (for RRFD in Descriptors)
        (Buf    : in chars_ptr;
         Len    : in int;
         Flags  : in int;
         Result : out int)
      when True is
         Dummy : int;
         Empty : Boolean;
      begin

         --  We have to handle len = 0 separatly because recv does
         --  not behave the same way on different platforms.

         if Len = 0 then
            Timeout.all  := Immediat;
            Rfds.all := 2 ** Integer (RRFD);
            Dummy := C_Select (RRFD + 1, Rfds, null, null, Timeout);
            Empty := Rfds.all = 0;
         else
            Dummy := Thin.C_Recv (RRFD, Buf, Len, Flags);
            Empty := Dummy = Thin.Failure and then Errno = Ewouldblock;
         end if;
         if Empty then
            Recv_Mask (RRFD) := True;
            if RRFD > Max_FD then
               Max_FD := RRFD;
            end if;
            requeue Recv_Requeue (RRFD) with abort;
         else
            Result := Dummy;
         end if;
      end Recv;

      ------------------
      -- Recv_Requeue --
      ------------------

      entry Recv_Requeue (for RRFD in Descriptors)
        (Buf    : in chars_ptr;
         Len    : in int;
         Flags  : in int;
         Result : out int)
      when Ready_Recv_Mask (RRFD) is
      begin
         if Len > 0 then
            Result := Thin.C_Recv (RRFD, Buf, Len, Flags);
         else
            Result := 0;
         end if;
         Recv_Mask (RRFD) := False;
         Ready_Recv_Mask (RRFD) := False;
         if Max_FD = RRFD then
            Max_FD := -1;
            for I in reverse Descriptors'First .. RRFD loop
               if Recv_Mask (I) or Send_Mask (I) then
                  Max_FD := I;
                  exit;
               end if;
            end loop;
         end if;
      end Recv_Requeue;

      ---------------
      -- Set_Masks --
      ---------------

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

      ----------
      -- Send --
      ----------

      entry Send (for RSFD in Descriptors)
        (Buf    : in chars_ptr;
         Len    : in int;
         Flags  : in int;
         Result : out int)
      when True is
         Dummy : int;
         Empty : Boolean;
      begin

         --  We have to handle len = 0 separatly because send does
         --  not behave the same way on different platforms.

         if Len = 0 then
            Timeout.all  := Immediat;
            Sfds.all := 2 ** Integer (RSFD);
            Dummy := C_Select (RSFD + 1, null, Sfds, null, Timeout);
            Empty := Sfds.all = 0;
         else
            Dummy := Thin.C_Send (RSFD, Buf, Len, Flags);
            Empty := Dummy = Thin.Failure and then Errno = Ewouldblock;
         end if;
         if Empty then
            Send_Mask (RSFD) := True;
            if RSFD > Max_FD then
               Max_FD := RSFD;
            end if;
            requeue Send_Requeue (RSFD) with abort;
         else
            Result := Dummy;
         end if;
      end Send;

      ------------------
      -- Send_Requeue --
      ------------------

      entry Send_Requeue (for RSFD in Descriptors)
        (Buf    : in chars_ptr;
         Len    : in int;
         Flags  : in int;
         Result : out int)
      when Ready_Send_Mask (RSFD) is
      begin
         if Len > 0 then
            Result := Thin.C_Send (RSFD, Buf, Len, Flags);
         else
            Result := 0;
         end if;
         Send_Mask (RSFD) := False;
         Ready_Send_Mask (RSFD) := False;
         if Max_FD = RSFD then
            Max_FD := -1;
            for I in reverse Descriptors'First .. RSFD loop
               if Recv_Mask (I) or Send_Mask (I) then
                  Max_FD := I;
                  exit;
               end if;
            end loop;
         end if;
      end Send_Requeue;

   end Asynchronous_Type;

   --------------
   -- C_Accept --
   --------------

   function C_Accept
     (S       : int;
      Addr    : Thin.Sockaddr_Access;
      Addrlen : access int)
     return int
   is
      Dummy    : int;
      Dummy_CP : chars_ptr := Null_Ptr;
   begin
      Set_Asynchronous_Non_Blocking (S);
      pragma Debug
        (D (D_Debug, "Blocking until there is something to accept"));
      Asynchronous.Recv (S) (Dummy_CP, 0, 0, Dummy);
      pragma Debug
        (D (D_Debug, "There is something to accept"));
      return Thin.C_Accept (S, Addr, Addrlen);
   end C_Accept;

   -------------
   -- C_Close --
   -------------

   function C_Close (Fildes : C.int) return C.int is
   begin
      In_Use (Fildes) := False;
      pragma Debug (D (D_Debug, "Close on " & Fildes'Img));
      return Thin.C_Close (Fildes);
   end C_Close;

   ---------------
   -- C_Connect --
   ---------------

   function C_Connect
     (S       : int;
      Name    : Thin.Sockaddr_Access;
      Namelen : int)
     return int
   is
      Dummy    : int;
      Dummy_CP : chars_ptr := Null_Ptr;
   begin
      Set_Asynchronous_Non_Blocking (S);
      Dummy := Thin.C_Connect (S, Name, Namelen);
      pragma Debug
        (D (D_Debug,
            "Connect (first) return code is" & C.int'Image (Dummy) &
            " and errno is" & Integer'Image (Errno)));
      if Dummy /= Thin.Failure or else
        Errno /= Einprogress then
         return Dummy;
      end if;
      Asynchronous.Send (S) (Dummy_CP, 0, 0, Dummy);
      Dummy := Thin.C_Connect (S, Name, Namelen);
      pragma Debug
        (D (D_Debug,
            "Connect return code is" & C.int'Image (Dummy) &
            " and errno is" & Integer'Image (Errno)));
      if Dummy = Thin.Failure and then Errno = Eisconn then
         In_Use (S) := True;
         return Thin.Success;
      else
         In_Use (S) := False;
         return Dummy;
      end if;
   end C_Connect;

   ------------
   -- C_Recv --
   ------------

   function C_Recv
     (S      : int;
      Buf    : chars_ptr;
      Len    : int;
      Flags  : int)
     return int is
      Count : int;
   begin
      pragma Debug (D (D_Debug, "Recv on " & S'Img));
      In_Use (S) := True;
      Asynchronous.Recv (S) (Buf, Len, Flags, Count);
      return Count;
   end C_Recv;

   ------------
   -- C_Send --
   ------------

   function C_Send
     (S      : C.int;
      Buf    : Strings.chars_ptr;
      Len    : C.int;
      Flags  : C.int)
     return C.int is
      Count : int;
   begin
      pragma Debug (D (D_Debug, "Send on " & S'Img));
      In_Use (S) := True;
      Asynchronous.Send (S) (Buf, Len, Flags, Count);
      return Count;
   end C_Send;

   ---------------
   -- Selection --
   ---------------

   task body Selection is
      RFD, SFD     : Desc_Set;
      Max          : aliased int;
      Dummy        : int;
      Pfd          : aliased Thin.Pollfd;
      Rfds         : Fd_Set_Access := new Fd_Set;
      Sfds         : Fd_Set_Access := new Fd_Set;
      Timeout      : Timeval_Access := new Timeval;
      Continue     : Boolean := True;
   begin
      Soft_Links.Add_Non_Terminating_Task;
      while Continue loop
         pragma Debug (D (D_Debug, "Before SIGIO"));
         Sigio.Wait;
         pragma Debug (D (D_Debug, "After  SIGIO"));

         Asynchronous.Get_Masks (RFD, SFD, Max);

         if Max > -1 then
            for I in RFD'First .. Max loop

               if RFD (I) or else SFD (I) then

                  if TCP.Platform_Specific.Use_Poll then

                     --  There is something to do on this file descriptor

                     Pfd.Fd := I;
                     Pfd.Events := 0;

                     if RFD (I) then
                        Pfd.Events := Pfd.Events + Pollin;
                     end if;

                     if SFD (I) then
                        Pfd.Events := Pfd.Events + Pollout;
                     end if;

                     Dummy := Thin.C_Poll (Pfd'Address, 1, 0);

                     RFD (I) := (Pfd.Revents / Pollin) mod 2 = 1;
                     SFD (I) := (Pfd.Revents / Pollout) mod 2 = 1;

                  else

                     Timeout.all  := Immediat;

                     if RFD (I) then
                        Rfds.all := 2 ** Integer (I);
                        pragma Debug
                          (D (D_Debug, "select recv :" & I'Img));
                     else
                        Rfds.all := 0;
                     end if;

                     if SFD (I) then
                        Sfds.all := 2 ** Integer (I);
                        pragma Debug
                          (D (D_Debug, "select send :" & I'Img));
                     else
                        Sfds.all := 0;
                     end if;

                     Dummy := C_Select (I + 1, Rfds, Sfds, null, Timeout);
                     pragma Debug (D (D_Debug, "select value :" & Dummy'Img));

                     RFD (I) := Rfds.all /= 0;
                     SFD (I) := Sfds.all /= 0;

                  end if;

               end if;

            end loop;
            Asynchronous.Set_Masks (RFD, SFD);

         elsif TCP.Shutdown_Completed then
            Continue := False;
            for FD in In_Use'Range loop
               if In_Use (FD) then
                  pragma Debug (D (D_Debug, "Use " & FD'Img));
                  Continue := True;
                  exit;
               end if;
            end loop;
            Shutdown := not Continue;
         end if;
      end loop;
      pragma Debug (D (D_Debug, "Selection terminated"));
      Soft_Links.Sub_Non_Terminating_Task;

   exception
      when E : others =>
         pragma Debug
           (D (D_Debug, Exception_Name (E) & " received in Selection"));
         Soft_Links.Sub_Non_Terminating_Task;
   end Selection;

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

      pragma Debug (D (D_Debug, "Set asynchronous non-blocking"));
      if not SVR4_Stream_IO then
         --  Use an alternate method for SVR4

         pragma Debug (D (D_Debug, "Set flag fasync or fndelay"));
         Dummy := Thin.C_Fcntl (FD, F_Setfl, Fasync + Fndelay);
         pragma Debug (D (D_Debug, "Return " & Dummy'Img));

      elsif I_Setsig /= -1
        and then S_Rdnorm /= -1
        and then S_Wrnorm /= -1
      then
         --  Use an alternate method for SVR4

         pragma Debug (D (D_Debug, "Set non-blocking with alternate method"));
         Dummy := Thin.C_Fcntl (FD, F_Setfl, Fndelay);
         pragma Debug (D (D_Debug, "Return " & Dummy'Img));
         pragma Debug (D (D_Debug, "Set asynchronous with alternate method"));
         Dummy := Thin.C_Ioctl (FD, I_Setsig, S_Rdnorm + S_Wrnorm);
         pragma Debug (D (D_Debug, "Return " & Dummy'Img));
      end if;
   end Set_Asynchronous_Non_Blocking;

   -----------
   -- Sigio --
   -----------

   protected body Sigio is

      -----------
      -- Stats --
      -----------

      procedure Stats (S, T : out Natural) is
      begin
         S := Signals;
         T := Timeouts;
      end Stats;

      ------------
      -- Signal --
      ------------

      procedure Signal is
      begin
         Signals  := Signals + 1;
         Occurred := True;
      end Signal;

      -------------
      -- Timeout --
      -------------

      procedure Timeout is
      begin
         Timeouts := Timeouts + 1;
         Occurred := True;
      end Timeout;

      ----------
      -- Wait --
      ----------

      entry Wait when Occurred is
      begin
         Occurred := False;
      end Wait;

   end Sigio;

   ----------------------
   -- Sigio_Simulation --
   ----------------------

   task body Sigio_Simulation is
      Signals, Timeouts : Natural;
   begin
      Soft_Links.Add_Non_Terminating_Task;
      while not Shutdown loop
         delay Safety_Delay;
         pragma Debug (D (D_Debug, "Simulate SIGIO"));
         Sigio.Timeout;
      end loop;
      Sigio.Stats (Signals, Timeouts);
      pragma Debug (D (D_Debug, "signals  =" & Signals'Img));
      pragma Debug (D (D_Debug, "timeouts =" & Timeouts'Img));
      Soft_Links.Sub_Non_Terminating_Task;

   exception
      when E : others =>
         pragma Debug
           (D (D_Debug, Exception_Name (E) & " received in Sigio_Simulation"));
         Soft_Links.Sub_Non_Terminating_Task;
   end Sigio_Simulation;

end System.Garlic.Non_Blocking;
