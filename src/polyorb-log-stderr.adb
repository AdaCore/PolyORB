------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . L O G . S T D E R R                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with System;

with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Utils.Strings;

package body PolyORB.Log.Stderr is

   use PolyORB.Parameters;

   Enable_Timestamps   : Boolean := False;
   --  If set true, all messages are prefixed with a timestamp

   Pid_Info            : Utils.Strings.String_Ptr;
   --  If Pid display is enabled, pointer to the PID string, else pointer to
   --  an empty string.

   Failed_Message      : constant String :=
                           "polyorb.log.stderr: write failed" & ASCII.LF;
   Interrupted_Message : constant String :=
                           "polyorb.log.stderr: write interrupted" & ASCII.LF;

   type Write_Status is (Success, Interrupted, Failed);

   function Write (S : String) return Write_Status;
   --  Outputs string to standard error. Output operation can be interrupted
   --  by a signal, in which case we try to complete output and return
   --  Interrupted status.

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String);
   --  Write S to output, possibly prefixed with a timestamp.
   --  If write operation fails or is interrupted, generate an additional
   --  informational message.

   procedure Put_Line (S : String) is
      function Timestamp return String;
      --  If timestamps are enabled, return a timestamp for this message,
      --  else return an empty string.

      ---------------
      -- Timestamp --
      ---------------

      function Timestamp return String is
         Result : String := "0000-00-00 00:00:00  ";
         --  Note additional empty space at end of string to account for the
         --  fact that we will use strftime(2) to fill in this string, which
         --  will append a NUL character.

         procedure C_Timestamp
           (Buf     : System.Address;
            Bufsize : Interfaces.C.int);
         pragma Import (C, C_Timestamp, "__PolyORB_timestamp");

      begin
         if Enable_Timestamps then
            C_Timestamp (Result'Address, Result'Length);
            return Result (Result'First .. Result'Last - 1);
         else
            return "";
         end if;
      end Timestamp;

      SS : aliased constant String := Timestamp & Pid_Info.all & S & ASCII.LF;
      X  : Write_Status;
      pragma Unreferenced (X);

   --  Start of processing for Put_Line

   begin
      case Write (SS) is
         when Success =>
            null;

         when Interrupted =>
            X := Write (Interrupted_Message);

         when Failed =>
            X := Write (Failed_Message);
      end case;
   end Put_Line;

   -----------
   -- Write --
   -----------

   function Write (S : String) return Write_Status is
      use type Interfaces.C.int;
      use type Interfaces.C.size_t;

      function C_Write
        (Fd  : Interfaces.C.int;
         P   : System.Address;
         Len : Interfaces.C.int) return Interfaces.C.size_t;
      pragma Import (C, C_Write, "write");
      --  write(2) system call

      P : Natural          := 0;
      C : Interfaces.C.int := 0;
      R : Interfaces.C.size_t;
      --  Comments needed???

   --  Start of processing for Write

   begin
      loop
         R := C_Write (2, S (S'First + Integer (C))'Address, S'Length - C);
         P := P + 1;

         if R = -1 then
            return Failed;
         end if;

         C := C + Interfaces.C.int (R);

         if C = S'Length then
            --  Output complete

            if P = 1 then
               return Success;

            else
               return Interrupted;
            end if;
         end if;
      end loop;
   end Write;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use type PolyORB.Log.Internals.Log_Hook_T;
   begin
      if PolyORB.Log.Internals.Log_Hook = null then
         PolyORB.Log.Internals.Log_Hook := Put_Line'Access;
      end if;

      Enable_Timestamps := Get_Conf ("log", "timestamp", Default => False);
      if Get_Conf ("log", "pid", Default => False) then
         declare
            function getpid return Integer;
            pragma Import (C, getpid, "getpid");

            Pid : constant String := getpid'Img;
         begin
            Pid_Info :=
              new String'("[" & Pid (Pid'First + 1 .. Pid'Last) & "] ");
         end;
      else
         Pid_Info := new String'("");
      end if;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"log.stderr",
       Conflicts => Empty,
       Depends   => +"parameters",
       Provides  => +"log_sink",
       Implicit  => True,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Log.Stderr;
