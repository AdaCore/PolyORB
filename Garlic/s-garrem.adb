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

with GNAT.IO;
with Interfaces.C.Strings;
with System.Garlic.Constants;         use System.Garlic.Constants;
with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Heart;             use System.Garlic.Heart;
with System.Garlic.Naming;            use System.Garlic.Naming;
with System.Garlic.Platform_Specific; use System.Garlic.Platform_Specific;
with System.Garlic.Thin;              use System.Garlic.Thin;

package body System.Garlic.Remote is

   use Interfaces.C;
   --  Shortcuts

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARREM", "(s-garrem): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   function Is_Local_Host (Host : String) return Boolean;
   --  Return True if the Host we are trying to contact is the same as the
   --  local host.

   procedure Local_Launcher (Command  : in String);
   --  Local launcher

   procedure Rsh_Launcher
     (Launcher : in String;
      Host     : in String;
      Command  : in String);
   --  RSH launcher

   function System (Command : C.Strings.chars_ptr) return int;
   pragma Import (C, System);

   procedure Launch
     (Launcher : in String;
      Host     : in String;
      Command  : in String);
   --  Launch Command on Host using Launcher

   function Background
     (Command  : String;
      Platform : String := Platform_Name)
      return String;
   --  Return the command necessary to launch Command in background on
   --  Platform.

   ----------------
   -- Background --
   ----------------

   function Background
     (Command  : String;
      Platform : String := Platform_Name)
      return String
   is
   begin
      if Platform_Name = "Open NT" or else Platform_Name = "Windows NT" then
         return "start /B " & Command;
      else
         return Command & " &";
      end if;
   end Background;

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

      --  The following sequence is a hack to simulate calling dup2() on
      --  file descriptors 0, 1 and 2 (stdin, stdout and stderr); dup2()
      --  is broken in some Linux threads packages (MIT threads for example)
      --  and this results into system crashes. The use of dup2() should be
      --  restored sometimes in the future ???

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

   -----------------
   -- Full_Launch --
   -----------------

   procedure Full_Launch
     (Launcher        : in String;
      Host            : in String;
      Executable_Name : in String)
   is
      Full_Command : constant String :=
        Executable_Name & " " & "--detach --slave --boot_server " &
        Get_Boot_Server;
   begin
      pragma Debug (D (D_Debug, "Full_Launch: " & Full_Command));

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

   -------------------
   -- Is_Local_Host --
   -------------------

   function Is_Local_Host (Host : String) return Boolean is
      Name_Of_Host : constant String := Name_Of (Host);
   begin
      return Host = "localhost"
        or else Name_Of_Host = "localhost"
        or else Name_Of_Host = Name_Of (Host_Name);
   end Is_Local_Host;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Launcher : in String;
      Host     : in String;
      Command  : in String)
   is
   begin
      if Supports_Local_Launch
        and then Host (Host'First) /= '`'
        and then Is_Local_Host (Host)
      then
         Local_Launcher (Command);
      else
         Rsh_Launcher (Launcher, Host, Command);
      end if;
   end Launch;

   --------------------
   -- Local_Launcher --
   --------------------

   procedure Local_Launcher (Command  : in String)
   is
      C_Command   : C.Strings.chars_ptr :=
        C.Strings.New_String (Background (Command));
      Return_Code : int;
   begin
      pragma Debug (D (D_Debug, "Local Launch: " & Command));

      Return_Code := System (C_Command);
      C.Strings.Free (C_Command);
   end Local_Launcher;

   ------------------
   -- Rsh_Launcher --
   ------------------

   procedure Rsh_Launcher
     (Launcher : in String;
      Host     : in String;
      Command  : in String)
   is
      --  RSH-like commands target Unix systems, so use the backgrounding
      --  specific to Unix.

      B_Command        : constant String := Background (Command, "unix");
      Rsh_Full_Command : constant String :=
        Launcher & " " & Host & " """ & B_Command & """ >/dev/null";
      Return_Code      : int;
      C_Command        : C.Strings.chars_ptr :=
        C.Strings.New_String (Rsh_Full_Command);
   begin
      Return_Code := System (C_Command);
      C.Strings.Free (C_Command);
      if Return_Code = -1 then

         --  Since any exception may be raised here, we choose to
         --  raise Program_Error since the elaboration won't take
         --  be able to finish properly.

         raise Program_Error;
      end if;
   end Rsh_Launcher;

end System.Garlic.Remote;
