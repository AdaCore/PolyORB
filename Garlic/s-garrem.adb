------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . R E M O T E                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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

with GNAT.IO;
with Interfaces.C.Strings;
with System.Garlic.Constants; use System.Garlic.Constants;
with System.Garlic.Thin; use System.Garlic.Thin;
with System.RPC;

package body System.Garlic.Remote is

   use Interfaces.C, System.RPC;
   --  Shortcuts

   procedure Rsh_Launcher
     (Launcher : in String;
      Host     : in String;
      Command  : in String);
   --  Build full launcher command.

   Current_Launcher : Launcher_Type := Rsh_Launcher'Access;
   --  The current launcher

   ------------
   -- Detach --
   ------------

   procedure Detach is
      Dummy         : C.int;
      Dummy_P       : pid_t;
      Dev_Null      : C.int;
      Dev_Null_Name : C.Strings.chars_ptr :=
        C.Strings.New_String ("/dev/null");
   begin
      Dummy_P := C_Setsid;
      Dummy := C_Close (0);
      Dev_Null := C_Open (Dev_Null_Name, O_Rdonly);
      pragma Assert (Dev_Null = 0);
      Dummy := C_Close (1);
      Dev_Null := C_Open (Dev_Null_Name, O_Wronly);
      pragma Assert (Dev_Null = 1);
      Dummy := C_Close (2);
      Dev_Null := C_Open (Dev_Null_Name, O_Wronly);
      pragma Assert (Dev_Null = 2);
      C.Strings.Free (Dev_Null_Name);
   end Detach;

   ----------------------
   -- Install_Launcher --
   ----------------------

   procedure Install_Launcher (Launcher : in Launcher_Type) is
   begin
      Current_Launcher := Launcher;
   end Install_Launcher;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Launcher : in String;
      Host     : in String;
      Command  : in String)
   is
   begin
      Current_Launcher (Launcher, Host, Command);
   end Launch;

   ------------------
   -- Rsh_Launcher --
   ------------------

   procedure Rsh_Launcher
     (Launcher : in String;
      Host     : in String;
      Command  : in String)
   is

      Rsh_Full_Command : constant String :=
        Launcher & " " & Host & " """ & Command & """ >/dev/null";

      C_Command : C.Strings.chars_ptr :=
        C.Strings.New_String (Rsh_Full_Command);

      function System (Command : C.Strings.chars_ptr) return int;
      pragma Import (C, System);

      Return_Code : int;

   begin
      Return_Code := System (C_Command);
      C.Strings.Free (C_Command);
      if Return_Code = -1 then
         raise Program_Error;
         --  This is allowed because any exception may be raised.
      end if;
   end Rsh_Launcher;

   -----------------
   -- Full_Launch --
   -----------------

   procedure Full_Launch
     (Launcher        : in String;
      Host            : in String;
      Executable_Name : in String;
      Boot_Server     : in String)
   is
      Full_Command : constant String :=
        Executable_Name & " " & "--detach --slave --boot_server " &
        Boot_Server & " &";
   begin
      Launch (Launcher, Host, Full_Command);
   end Full_Launch;

   --------------
   -- Get_Host --
   --------------

   function Get_Host (Partition : String) return String is
      Buffer : String (1 .. 64);
      Last   : Natural;
   begin
      GNAT.IO.Put ("Host for """ & Partition & """: ");
      GNAT.IO.Get_Line (Buffer, Last);
      return Buffer (1 .. Last);
   end Get_Host;

end System.Garlic.Remote;
