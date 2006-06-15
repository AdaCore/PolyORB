------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . D S A _ P . R E M O T E _ L A U N C H           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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
with PolyORB.Log;
with PolyORB.Parameters;
with System;

package body PolyORB.DSA_P.Remote_Launch is

   use Interfaces.C;
   use PolyORB.Log;
   use PolyORB.Parameters;

   package L is new PolyORB.Log.Facility_Log ("polyorb.dsa_p.remote_launch");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;

   package C renames Interfaces.C;

   procedure C_Dup2 (Fd1, Fd2 : C.int);
   pragma Import (C, C_Dup2, "dup2");

   function C_Open
     (Path  : C.char_array;
      Oflag : C.int;
      Mode  : C.int := 0)
     return C.int;
   pragma Import (C, C_Open, "open");

   procedure C_Setsid;
   pragma Import (C, C_Setsid, "setsid");

   function C_System (Command : System.Address) return C.int;
   pragma Import (C, C_System, "system");

   procedure Spawn (Command : String);
   --  Spawn a system command

   ------------
   -- Detach --
   ------------

   procedure Detach
   is
      Dev_Null      : C.int;
      Dev_Null_Name : constant C.char_array := To_C ("/dev/null");

      Do_Detach : constant String :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "detach",
           Default => "false");

   begin
      if Do_Detach = "true" then
         Dev_Null := C_Open (Dev_Null_Name, 2);
         C_Dup2 (Dev_Null, 0);
         C_Dup2 (Dev_Null, 1);
         C_Dup2 (Dev_Null, 2);
         C_Setsid;
      end if;
   end Detach;

   ----------------------
   -- Launch_Partition --
   ----------------------

   procedure Launch_Partition (Host : String; Command : String)
   is
      Rsh_Options : constant String :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "rsh_options",
           Default => "-f");

      Rsh_Command : constant String :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "rsh_command",
           Default => "ssh");

      C1 : constant String := '"' & Command & '"';
      C2 : constant String := Host & ' ' & Rsh_Options;
      C3 : constant String := Rsh_Command & ' ' & C2;
      C4 : constant String := C3 & ' ' & C1;
      C5 : constant String := C4 & " < /dev/null > /dev/null 2>&1";
   begin
      pragma Debug (O ("Enter Launch Partition"));
      pragma Debug (O ("Run Spawn: " & C5));
      Spawn (C5);
      pragma Debug (O ("Leave Launch Partition"));
   end Launch_Partition;

   -----------
   -- Spawn --
   -----------

   procedure Spawn (Command : String) is
      C_Command : aliased String := Command & ASCII.NUL;
   begin
      if C_System (C_Command'Address) / 256 /= 0 then
         raise Program_Error;
      end if;
   end Spawn;

end PolyORB.DSA_P.Remote_Launch;
