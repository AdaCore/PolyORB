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
with System.Garlic.Heart;                 use System.Garlic.Heart;
with System.Garlic.Priorities;
with System.Garlic.TCP;
pragma Elaborate (System.Garlic.TCP);
with System.Garlic.TCP.Platform_Specific;
with System.Garlic.Thin;                  use System.Garlic.Thin;
with System.Garlic.Termination;

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

      entry Read (Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int);
      --  You may wait for data to be here by setting Nbyte to zero

      entry Write (Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int);
      --  You may wait for data to be written by setting Nbyte to zero

      procedure Get_Masks
        (Read_M  : out Desc_Set;
         Write_M : out Desc_Set;
         Max     : out int);
      --  Get the masks on which select should apply

      procedure Set_Masks
        (Read_M  : in Desc_Set;
         Write_M : in Desc_Set);
      --  Set the masks as returned by C_Select

   private

      entry Read_Requeue (Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int);
      entry Write_Requeue (Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int);

      Read_Mask        : Desc_Set := (others => False);
      Write_Mask       : Desc_Set := (others => False);
      Ready_Read_Mask  : Desc_Set := (others => False);
      Ready_Write_Mask : Desc_Set := (others => False);
      Max_FD     : int := -1;

   end Asynchronous_Type;

   type Asynchronous_Access is access Asynchronous_Type;

   Asynchronous : Asynchronous_Access := new Asynchronous_Type;

   PID : constant Thin.pid_t := Thin.C_Getpid;

   procedure Set_Asynchronous (FD : in Descriptors);
   pragma Inline (Set_Asynchronous);
   --  Set a file descriptor to be asynchronous

   procedure Set_Non_Blocking (FD : in Descriptors);
   pragma Inline (Set_Non_Blocking);
   --  Set a file descriptor to be non-blocking

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
        (Read_M  : out Desc_Set;
         Write_M : out Desc_Set;
         Max     : out int) is
      begin
         Read_M := Read_Mask;
         Write_M := Write_Mask;
         Max := Max_FD;
      end Get_Masks;

      ----------
      -- Read --
      ----------

      entry Read (for RFD in Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int)
      when True is
         Dummy : int;
      begin
         Set_Asynchronous (RFD);
         Dummy := Thin.C_Read (RFD, Buf, Nbyte);
         if Dummy = Thin.Failure and then Errno = Eagain then
            Read_Mask (RFD) := True;
            if RFD > Max_FD then
               Max_FD := RFD;
            end if;
            requeue Read_Requeue (RFD) with abort;
         else
            Result := Dummy;
         end if;
      end Read;

      ------------------
      -- Read_Requeue --
      ------------------

      entry Read_Requeue (for RRFD in Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int)
      when Ready_Read_Mask (RRFD) is
      begin
         if Nbyte > 0 then
            Result := Thin.C_Read (RRFD, Buf, Nbyte);
         else
            Result := 0;
         end if;
         Read_Mask (RRFD) := False;
         Ready_Read_Mask (RRFD) := False;
         if Max_FD = RRFD then
            Max_FD := -1;
            for I in reverse Descriptors'First .. RRFD loop
               if Read_Mask (I) or Write_Mask (I) then
                  Max_FD := I;
                  exit;
               end if;
            end loop;
         end if;
      end Read_Requeue;

      ---------------
      -- Set_Masks --
      ---------------

      procedure Set_Masks
        (Read_M  : in Desc_Set;
         Write_M : in Desc_Set)
      is
      begin
         for I in Read_M'Range loop
            if Read_M (I) then
               Ready_Read_Mask (I) := True;
            end if;
            if Write_M (I) then
               Ready_Write_Mask (I) := True;
            end if;
         end loop;
      end Set_Masks;

      -----------
      -- Write --
      -----------

      entry Write (for WFD in Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int)
      when True is
         Dummy : int;
      begin
         Set_Asynchronous (WFD);
         Dummy := Thin.C_Write (WFD, Buf, Nbyte);
         if Dummy = Thin.Failure and then Errno = Eagain then
            Write_Mask (WFD) := True;
            if WFD > Max_FD then
               Max_FD := WFD;
            end if;
            requeue Write_Requeue (WFD) with abort;
         else
            Result := Dummy;
         end if;
      end Write;

      -------------------
      -- Write_Requeue --
      -------------------

      entry Write_Requeue (for RWFD in Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int)
      when Ready_Write_Mask (RWFD) is
      begin
         if Nbyte > 0 then
            Result := Thin.C_Write (RWFD, Buf, Nbyte);
         else
            Result := 0;
         end if;
         Write_Mask (RWFD) := False;
         Ready_Write_Mask (RWFD) := False;
         if Max_FD = RWFD then
            Max_FD := -1;
            for I in reverse Descriptors'First .. RWFD loop
               if Read_Mask (I) or Write_Mask (I) then
                  Max_FD := I;
                  exit;
               end if;
            end loop;
         end if;
      end Write_Requeue;

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
      Set_Non_Blocking (S);
      pragma Debug
        (D (D_Debug, "Blocking until there is something to accept"));
      Asynchronous.Read (S) (Dummy_CP, 0, Dummy);
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
      Set_Non_Blocking (S);
      Dummy := Thin.C_Connect (S, Name, Namelen);
      pragma Debug
        (D (D_Debug,
            "Connect (first) return code is" & C.int'Image (Dummy) &
            " and errno is" & Integer'Image (Errno)));
      if Dummy /= Thin.Failure or else
        Errno /= Einprogress then
         return Dummy;
      end if;
      Asynchronous.Write (S) (Dummy_CP, 0, Dummy);
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
   -- C_Read --
   ------------

   function C_Read
     (Fildes : int;
      Buf    : chars_ptr;
      Nbyte  : int)
     return int is
      Count : int;
   begin
      pragma Debug (D (D_Debug, "Read on " & Fildes'Img));
      In_Use (Fildes) := True;
      Asynchronous.Read (Fildes) (Buf, Nbyte, Count);
      return Count;
   end C_Read;

   -------------
   -- C_Write --
   -------------

   function C_Write
     (Fildes : C.int;
      Buf    : Strings.chars_ptr;
      Nbyte  : C.int)
     return C.int is
      Count : int;
   begin
      pragma Debug (D (D_Debug, "Write on " & Fildes'Img));
      In_Use (Fildes) := True;
      Asynchronous.Write (Fildes) (Buf, Nbyte, Count);
      return Count;
   end C_Write;

   ---------------
   -- Selection --
   ---------------

   task body Selection is
      RFD, WFD     : Desc_Set;
      Max          : aliased int;
      Dummy        : int;
      Pfd          : aliased Thin.Pollfd;
      Rfds         : Fd_Set_Access := new Fd_Set;
      Wfds         : Fd_Set_Access := new Fd_Set;
      Timeout      : Timeval_Access := new Timeval;
      Continue     : Boolean := True;
   begin
      Termination.Add_Non_Terminating_Task;
      while Continue loop
         pragma Debug (D (D_Debug, "Before SIGIO"));
         Sigio.Wait;
         pragma Debug (D (D_Debug, "After  SIGIO"));

         Asynchronous.Get_Masks (RFD, WFD, Max);

         if Max > -1 then
            for I in RFD'First .. Max loop

               if RFD (I) or else WFD (I) then

                  if Platform_Specific.Use_Poll then

                     --  There is something to do on this file descriptor

                     Pfd.Fd := I;
                     Pfd.Events := 0;

                     if RFD (I) then
                        Pfd.Events := Pfd.Events + Pollin;
                     end if;

                     if WFD (I) then
                        Pfd.Events := Pfd.Events + Pollout;
                     end if;

                     Dummy := Thin.C_Poll (Pfd'Address, 1, 0);

                     RFD (I) := (Pfd.Revents / Pollin) mod 2 = 1;
                     WFD (I) := (Pfd.Revents / Pollout) mod 2 = 1;

                  else

                     Timeout.all  := Immediat;

                     if RFD (I) then
                        Rfds.all := 2 ** Integer (I);
                        pragma Debug
                          (D (D_Debug, "select read :" & I'Img));
                     else
                        Rfds.all := 0;
                     end if;

                     if WFD (I) then
                        Wfds.all := 2 ** Integer (I);
                        pragma Debug
                          (D (D_Debug, "select write :" & I'Img));
                     else
                        Wfds.all := 0;
                     end if;

                     Dummy := C_Select (I + 1, Rfds, Wfds, null, Timeout);
                     pragma Debug (D (D_Debug, "select value :" & Dummy'Img));

                     RFD (I) := Rfds.all /= 0;
                     WFD (I) := Wfds.all /= 0;

                  end if;

               end if;

            end loop;
            Asynchronous.Set_Masks (RFD, WFD);

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
      Termination.Sub_Non_Terminating_Task;

   exception
      when E : others =>
         pragma Debug
           (D (D_Debug, Exception_Name (E) & " received in Selection"));
         Termination.Sub_Non_Terminating_Task;
   end Selection;

   ----------------------
   -- Set_Asynchronous --
   ----------------------

   procedure Set_Asynchronous (FD : in Descriptors) is
      Dummy : int;
   begin
      if not (SVR4_Stream_IO
               or else Thin.C_Fcntl (FD, F_Setown, int (PID)) = Thin.Failure)
      then
         Dummy := Thin.C_Fcntl (FD, F_Setfl, Fasync);
      elsif I_Setsig /= -1
        and then S_Rdnorm /= -1
        and then S_Wrnorm /= -1
      then
         --  Use an alternate method for SVR4

         SVR4_Stream_IO := True;
         Dummy          := Thin.C_Ioctl (FD, I_Setsig, S_Rdnorm + S_Wrnorm);
      end if;
   end Set_Asynchronous;

   ----------------------
   -- Set_Non_Blocking --
   ----------------------

   procedure Set_Non_Blocking (FD : in Descriptors) is
      Dummy : int;
   begin
      Dummy := Thin.C_Fcntl (FD, F_Setfl, Fndelay);
   end Set_Non_Blocking;

   -----------
   -- Sigio --
   -----------

   protected body Sigio is

      ------------
      -- Signal --
      ------------

      procedure Signal is
      begin
         Occurred := True;
      end Signal;

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
   begin
      Termination.Add_Non_Terminating_Task;
      while not Shutdown loop
         delay Safety_Delay;
         pragma Debug (D (D_Debug, "Simulate SIGIO"));
         Sigio.Signal;
      end loop;
      Termination.Sub_Non_Terminating_Task;

   exception
      when E : others =>
         pragma Debug
           (D (D_Debug, Exception_Name (E) & " received in Sigio_Simulation"));
         Termination.Sub_Non_Terminating_Task;
   end Sigio_Simulation;

end System.Garlic.Non_Blocking;
