------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . R E M O T E                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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

with Ada.Unchecked_Deallocation;
with GNAT.IO;
with GNAT.OS_Lib;                     use GNAT.OS_Lib;
with Interfaces.C.Strings;            use Interfaces.C, Interfaces.C.Strings;
with System.Garlic.Constants;         use System.Garlic.Constants;
with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Naming;            use System.Garlic.Naming;
with System.Garlic.Partitions;        use System.Garlic.Partitions;
with System.Garlic.Platform_Specific; use System.Garlic.Platform_Specific;
with System.Garlic.Thin;              use System.Garlic.Thin;

package body System.Garlic.Remote is

   use Interfaces.C;
   --  Shortcuts

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARREM", "(s-garrem): ");
   procedure D
     (Message : in String;
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

   function Strip_Pwd (Command : String) return String;
   --  Remove the "`pwd`" construct that may have been added by the code
   --  generator, and replace it by ".". This is equivalent when launching
   --  partitions locally and will work on platforms that don't support
   --  this form of commands, such as Windows NT.

   function Split (Command : String) return Argument_List;
   --  Return an argument list corresponding to Command. Honors double-quote
   --  escaped commands correctly.

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

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
        Executable_Name & " " & "--detach --boot_location """ &
        Get_Boot_Locations & """";
   begin
      pragma Debug (D ("Launch Command: " & Full_Command));

      Launch (Launcher, Host, Full_Command);
   end Full_Launch;

   --------------
   -- Get_Host --
   --------------

   function Get_Host
     (Partition : String)
     return String
   is
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

   function Is_Local_Host
     (Host : String)
     return Boolean
   is
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
      Command  : in String) is
   begin
      if Supports_Local_Launch
        and then Host (Host'First) /= '`'
        and then Is_Local_Host (Host)
      then
         pragma Debug (D ("Run Spawn: " & Command));

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
      Args : Argument_List       := Split (Strip_Pwd (Command));
      PID  : constant Process_Id :=
        Non_Blocking_Spawn (Args (1).all, Args (2 .. Args'Last));
   begin
      for I in Args'Range loop
         Free (Args (I));
      end loop;
      if PID = Invalid_Pid then
         raise Program_Error;
      end if;
   end Local_Launcher;

   ------------------
   -- Rsh_Launcher --
   ------------------

   procedure Rsh_Launcher
     (Launcher : in String;
      Host     : in String;
      Command  : in String)
   is
      function C_System (Command : Address) return int;
      pragma Import (C, C_System, "system");

      Rsh_Full_Command : constant String     :=
        Launcher & " " & Host & " """ & Command &
        " &"" < /dev/null > /dev/null";
      C_Command        : aliased char_array := To_C (Rsh_Full_Command);
   begin
      pragma Debug (D ("Run System: " & Rsh_Full_Command));

      if C_System (C_Command'Address) / 256 /= 0 then
         raise Program_Error;
      end if;
   end Rsh_Launcher;

   -----------
   -- Split --
   -----------

   function Split
     (Command : String)
     return Argument_List
   is
      Result   : Argument_List (1 .. 50);
      Last     : Natural := 0;
      Current  : String_Access;
      Old      : String_Access;
      In_Quote : Boolean := False;
   begin
      for I in Command'Range loop
         declare
            Char : Character renames Command (I);
         begin
            if Char = '"' then                -- "
               In_Quote := not In_Quote;
            elsif Char = ' ' and then not In_Quote then
               if Current /= null then
                  Last := Last + 1;
                  Result (Last) := Current;
                  Current := null;
               end if;
            else
               if Current = null then
                  Current := new String'(1 => Char);
               else
                  Old     := Current;
                  Current := new String'(Old.all & Char);
                  Free (Old);
               end if;
            end if;
         end;
      end loop;
      if Current /= null then
         Last := Last + 1;
         Result (Last) := Current;
      end if;
      pragma Assert (not In_Quote);
      return Result (1 .. Last);
   end Split;

   ---------------
   -- Strip_Pwd --
   ---------------

   function Strip_Pwd (Command : String) return String is
   begin
      if Command'Length > 5
        and then Command (Command'First .. Command'First + 4) = "`pwd`"
      then
         return '.' & Command (Command'First + 5 .. Command'Last);
      else
         return Command;
      end if;
   end Strip_Pwd;

end System.Garlic.Remote;
