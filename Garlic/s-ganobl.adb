------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--          S Y S T E M . G A R L I C . N O N _ B L O C K I N G             --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Interrupts.Names;
with System.Garlic.Constants; use System.Garlic.Constants;
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Heart; use System.Garlic.Heart;
with System.Garlic.OS_Lib;
with System.Garlic.Priorities;
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

   Safety_Delay : constant Duration := 0.5;
   --  A SIGIO will be simulated every Safety_Delay seconds, to make
   --  sure we do not get stuned because we have missed one of them.

   subtype Descriptors is int range 0 .. 127;
   --  At most 128 file descriptors are available. This is used to limit
   --  the size of entry families.

   type Desc_Set is array (Descriptors) of Boolean;
   pragma Pack (Desc_Set);

   protected type Asynchronous_Type is

      entry Read (Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int);
      --  You may wait for data to be here by setting Nbyte to zero.

      entry Write (Descriptors)
        (Buf    : in chars_ptr;
         Nbyte  : in int;
         Result : out int);
      --  You may wait for data to be written by setting Nbyte to zero.

      procedure Get_Masks
        (Read_M  : out Desc_Set;
         Write_M : out Desc_Set;
         Max     : out int);
      --  Get the masks on which select should apply.

      procedure Set_Masks
        (Read_M  : in Desc_Set;
         Write_M : in Desc_Set);
      --  Set the masks as returned by C_Select.

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
   --  Set a file descriptor to be asynchronous.

   procedure Set_Non_Blocking (FD : in Descriptors);
   pragma Inline (Set_Non_Blocking);
   --  Set a file descriptor to be non-blocking.

   protected Sigio_Keeper is
      entry Wait;
      procedure Signal;
      pragma Attach_Handler (Signal, Ada.Interrupts.Names.SIGIO);
   private
      Occurred : Boolean := False;
   end Sigio_Keeper;

   task Sigio_Simulation is
      pragma Priority (Priorities.Polling_Priority);
   end Sigio_Simulation;
   --  This task will simulate SIGIOs every <N> seconds.

   task Selection is
      pragma Priority (Priorities.Polling_Priority);
   end Selection;
   --  This task is in charge of calling C_Select.

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
         Max     : out int)
      is
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
      begin
         Read_Mask (RFD) := True;
         if RFD > Max_FD then
            Max_FD := RFD;
         end if;
         Set_Asynchronous (RFD);
         requeue Read_Requeue (RFD) with abort;
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
      begin
         Write_Mask (WFD) := True;
         if WFD > Max_FD then
            Max_FD := WFD;
         end if;
         Set_Asynchronous (WFD);
         requeue Write_Requeue (WFD) with abort;
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
            " and errno is" & C.int'Image (OS_Lib.C_Errno)));
      if Dummy /= Thin.Failure or else
        OS_Lib.C_Errno /= Einprogress then
         return Dummy;
      end if;
      Asynchronous.Write (S) (Dummy_CP, 0, Dummy);
      Dummy := Thin.C_Connect (S, Name, Namelen);
      pragma Debug
        (D (D_Debug,
            "Connect return code is" & C.int'Image (Dummy) &
            " and errno is" & C.int'Image (OS_Lib.C_Errno)));
      if Dummy = Thin.Failure and then OS_Lib.C_Errno = Eisconn then
         return Thin.Success;
      else
         return Dummy;
      end if;
   end C_Connect;

   ------------
   -- C_Read --
   ------------

   function C_Read
     (Filedes : int;
      Buf     : chars_ptr;
      Nbyte   : int)
     return int
   is
      Count : int;
   begin
      Asynchronous.Read (Filedes) (Buf, Nbyte, Count);
      return Count;
   end C_Read;

   -------------
   -- C_Write --
   -------------

   function C_Write
     (Fildes : C.int;
      Buf    : Strings.chars_ptr;
      Nbyte  : C.int)
     return C.int
   is
      Count : int;
   begin
      Asynchronous.Write (Fildes) (Buf, Nbyte, Count);
      return Count;
   end C_Write;

   ---------------
   -- Selection --
   ---------------

   task body Selection is
      Read_Fd_Set,
      RFD, WFD     : Desc_Set;
      Max          : aliased int;
      Dummy        : int;
      Pfd          : aliased Thin.Pollfd;
   begin
      Termination.Add_Non_Terminating_Task;
      loop
         select
            Shutdown_Keeper.Wait;
            pragma Debug
              (D (D_Debug, "Selection exiting because of Shutdown_Keeper"));
            exit;
         else
            null;
         end select;
         pragma Debug (D (D_Debug, "Waiting for SIGIO"));
         Sigio_Keeper.Wait;
         pragma Debug (D (D_Debug, "SIGIO (or pseudo SIGIO) received"));
         Asynchronous.Get_Masks (RFD, WFD, Max);
         if Max > -1 then
            for I in RFD'First .. Max loop

               if RFD (I) or else WFD (I) then

                  --  There is something to do on this file descriptor.

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

               end if;

            end loop;
            Asynchronous.Set_Masks (RFD, WFD);
         end if;
      end loop;
      Termination.Sub_Non_Terminating_Task;
   end Selection;

   ----------------------
   -- Set_Asynchronous --
   ----------------------

   procedure Set_Asynchronous (FD : in Descriptors) is
      Dummy : int;
   begin
      Dummy := Thin.C_Fcntl (FD, F_Setfl, Fasync);
      Dummy := Thin.C_Fcntl (FD, F_Setown, int (PID));
   end Set_Asynchronous;

   ----------------------
   -- Set_Non_Blocking --
   ----------------------

   procedure Set_Non_Blocking (FD : in Descriptors) is
      Dummy : int;
   begin
      Dummy := Thin.C_Fcntl (FD, F_Setfl, Fndelay);
   end Set_Non_Blocking;

   ------------------
   -- Sigio_Keeper --
   ------------------

   protected body Sigio_Keeper is

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

   end Sigio_Keeper;

   ----------------------
   -- Sigio_Simulation --
   ----------------------

   task body Sigio_Simulation is
   begin
      Termination.Add_Non_Terminating_Task;
      loop
         Sigio_Keeper.Signal;
         select
            Shutdown_Keeper.Wait;
            pragma Debug
              (D (D_Debug, "Simulation exiting because of Shutdown_Keeper"));
            exit;
         else
            delay Safety_Delay;
         end select;
      end loop;
      Termination.Sub_Non_Terminating_Task;
   end Sigio_Simulation;

end System.Garlic.Non_Blocking;
