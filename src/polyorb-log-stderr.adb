------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . L O G . S T D E R R                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with System;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Log.Stderr is

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

   procedure Put_Line (S : String) is
      SS : aliased constant String := S & ASCII.LF;
      X  : Write_Status;
      pragma Unreferenced (X);

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
            --  Operation was failed.

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
