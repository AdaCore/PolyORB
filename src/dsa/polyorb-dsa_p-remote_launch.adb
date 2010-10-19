------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . D S A _ P . R E M O T E _ L A U N C H           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2010, Free Software Foundation, Inc.          --
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

with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps;

with GNAT.Expect;
with GNAT.OS_Lib;

with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Platform;
with PolyORB.Sockets;
with PolyORB.Utils.Strings.Lists;

package body PolyORB.DSA_P.Remote_Launch is

   use GNAT.OS_Lib;

   use PolyORB.Sockets;
   use PolyORB.Log;
   use PolyORB.Parameters;

   package L is new PolyORB.Log.Facility_Log ("polyorb.dsa_p.remote_launch");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   function Windows_To_Unix (S : String) return String;
   --  Translate Windows-style pathnames to Unix-style by changing '\' to '/'.
   --  ???This is a temporary kludge, but we're assuming the existence of a
   --  Unix-like shell anyway (see below). The goal is to get tests working
   --  under Windows using Cygwin. The problem is that Cygwin's 'sh' interprets
   --  '\' as a Unix escape, rather than as a directory separator.
   --  This should be made more portable.
   --  This is a no-op on non-Windows systems.

   function Escape_Spaces (S : String) return String;
   --  Protect spaces and shell metacharacters in S with a backslash
   --  ??? Assumes a UNIX shell

   procedure Initialize;
   --  Retrieve rsh command and options from configuration

   Sh_Command  : String_Access;
   Rsh_Command : String_Access;
   Rsh_Options : String_Access;
   Rsh_Args    : String_List_Access;

   -------------------
   -- Escape_Spaces --
   -------------------

   function Escape_Spaces (S : String) return String is
      R    : String (1 .. 2 * S'Length);
      Last : Natural := 0;
   begin
      for J in S'Range loop
         case S (J) is
            when ' ' | ASCII.HT |
                 ''' | '"' | '*' | '?' | '|' |
                 '[' | ']' | '(' | ')' | '{' | '}' | '<' | '>' =>
               Last := Last + 1;
               R (Last) := '\';
            when others =>
               null;
         end case;
         Last := Last + 1;
         R (Last) := S (J);
      end loop;
      return R (1 .. Last);
   end Escape_Spaces;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Sh_Command  := Locate_Exec_On_Path ("sh");

      --  Rsh_Command and Rsh_Options are always provided by gnatdist, so no
      --  default value is required here.

      Rsh_Command := Locate_Exec_On_Path (Parameters.Get_Conf
                       (Section => "dsa",
                        Key     => "rsh_command",
                        Default => ""));

      Rsh_Options := new String'(Parameters.Get_Conf
                       (Section => "dsa",
                        Key     => "rsh_options",
                        Default => ""));

      Rsh_Args := Argument_String_To_List (Rsh_Options.all);
   end Initialize;

   -------------------
   -- Is_Local_Host --
   -------------------

   function Is_Local_Host (Host : String) return Boolean;
   --  True if Host designates the local machine and we can avoid a remote
   --  shell execution.

   function Is_Local_Host (Host : String) return Boolean
   is
      Name_Of_Host : constant String
                       := Official_Name (Get_Host_By_Name (Host));
   begin
      --  If force_rsh is True, never optimize away rsh call

      if Parameters.Get_Conf
           (Section => "dsa",
            Key     => "force_rsh",
            Default => False)
      then
         return False;
      end if;

      return Host = "localhost"
        or else Name_Of_Host = "localhost"
        or else Name_Of_Host = Official_Name (Get_Host_By_Name (Host_Name));
   end Is_Local_Host;

   ----------------------
   -- Launch_Partition --
   ----------------------

   procedure Launch_Partition
     (Host : String; Command : String; Env_Vars : String)
   is
      U_Command : constant String :=
                    Escape_Spaces (Windows_To_Unix (Command))
                                     & " --polyorb-dsa-name_service="
                                     & Get_Conf ("dsa", "name_service", "");
      Pid       : Process_Id;
      pragma Unreferenced (Pid);

      Remote_Host : String_Access;
   begin
      pragma Debug (C, O ("Launch_Partition: enter"));

      --  ??? This is implemented assuming a UNIX-like shell on both the master
      --  and the slave hosts. This should be made more portable. If the
      --  configuration file specified a shell script for the 'Host, then the
      --  Host will look like `shell-script blah` (using back-quote notation),
      --  so we have to strip off the back-quotes, and split it into command
      --  name and arguments. We're using Argument_String_To_List to split off
      --  the command name as well, which is also kludgy.

      if Host (Host'First) = '`' then
         declare
            Argv : Argument_List_Access :=
              Argument_String_To_List (Host (Host'First + 1 .. Host'Last - 1));
            Command   : String renames Argv (Argv'First).all;
            --  First "argument" found by Argument_String_To_List is the name
            --  of the shell script.
            Arguments : constant Argument_List (1 .. Argv'Length - 1) :=
              Argv (2 .. Argv'Last);
            --  Get_Command_Output requires a 1-based array, so we need to
            --  slide Arguments.
            Status    : aliased Integer;
         begin
            Remote_Host :=
              new String'(GNAT.Expect.Get_Command_Output
                            (Command,
                             Arguments,
                             Input => "",
                             Status => Status'Access));
            if Status /= 0 then
               raise Program_Error with "Unable to launch " & Command;
            end if;
            for J in Argv'Range loop
               Free (Argv (J));
            end loop;
            Free (Argv);
         end;

      --  Otherwise (no back-quote), we can just use Host as is.

      else
         Remote_Host := new String'(Host);
      end if;

      pragma Debug (C, O ("Remote_Host: " & Remote_Host.all));

      --  Local spawn

      if Is_Local_Host (Remote_Host.all) then

         declare
            Args : Argument_List :=
                     (new String'("-c"), new String'(U_Command));
         begin
            pragma Debug (C, O ("Enter Spawn (local): " & U_Command));
            Pid := Non_Blocking_Spawn (Sh_Command.all, Args);
            for J in Args'Range loop
               Free (Args (J));
            end loop;
         end;

      --  Remote spawn

      else
         Remote_Spawn : declare
            function Expand_Env_Vars (Vars : String) return String;
            --  Given a space separated list of environment variable names,
            --  return a space separated list of assigments of the form:
            --  VAR='value'.

            function Expand_Env_Vars (Vars : String) return String is
               use Ada.Environment_Variables;
               use Ada.Strings.Unbounded;

               First, Last : Integer;
               Result : Unbounded_String;
            begin
               First := Vars'First;

               loop
                  --  Find first character of name

                  while First <= Env_Vars'Last
                          and then Env_Vars (First) = ' '
                  loop
                     First := First + 1;
                  end loop;
                  exit when First > Env_Vars'Last;

                  --  Find last character of name

                  Last := First;
                  while Last < Env_Vars'Last
                          and then Env_Vars (Last + 1) /= ' '
                  loop
                     Last := Last + 1;
                  end loop;

                  declare
                     Var_Name : String renames Vars (First .. Last);
                  begin
                     if Exists (Var_Name) then
                        if Length (Result) = 0 then
                           Result := To_Unbounded_String ("env ");
                        end if;
                        Result := Result &
                                  Var_Name & "='" & Value (Var_Name) & "' ";
                     end if;
                  end;

                  First := Last + 1;
               end loop;
               return To_String (Result);
            end Expand_Env_Vars;

            Remote_Command : String_Access :=
                               new String'(Expand_Env_Vars (Env_Vars)
                                             & U_Command
                                             & " --polyorb-dsa-detach");

         --  Start of processing for Remote_Spawn

         begin
            pragma Debug
              (C, O ("Enter Spawn (remote: "
                       & Rsh_Command.all & " "
                       & Rsh_Options.all
                       & Remote_Host.all & "): "
                       & Remote_Command.all));
            Pid := Non_Blocking_Spawn
                     (Rsh_Command.all,
                      Remote_Host & Rsh_Args.all & Remote_Command);
            Free (Remote_Command);
         end Remote_Spawn;
      end if;
      Free (Remote_Host);

      pragma Debug (C, O ("Launch_Partition: leave"));
   end Launch_Partition;

   ---------------------
   -- Windows_To_Unix --
   ---------------------

   function Windows_To_Unix (S : String) return String is
      use Ada.Strings.Fixed, Ada.Strings.Maps;
   begin
      if Platform.Windows_On_Target then
         return Translate (S, To_Mapping ("\", "/"));
      else
         return S;
      end if;
   end Windows_To_Unix;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"dsa_p.remote_launch",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.DSA_P.Remote_Launch;
