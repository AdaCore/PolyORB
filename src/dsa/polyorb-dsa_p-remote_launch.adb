------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . D S A _ P . R E M O T E _ L A U N C H           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2008, Free Software Foundation, Inc.          --
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
with PolyORB.Sockets;
with PolyORB.Log;
with PolyORB.Parameters;
with System;

package body PolyORB.DSA_P.Remote_Launch is

   use Interfaces.C;
   use PolyORB.Sockets;
   use PolyORB.Log;
   use PolyORB.Parameters;

   package L is new PolyORB.Log.Facility_Log ("polyorb.dsa_p.remote_launch");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   package IC renames Interfaces.C;

   function C_System (Command : System.Address) return IC.int;
   pragma Import (C, C_System, "system");

   procedure Spawn (Command : String);
   --  Spawn a system command

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

   procedure Launch_Partition (Host : String; Command : String) is
   begin
      pragma Debug (C, O ("Launch_Partition: enter"));

      --  ??? This is implemented assuming a UNIX-like shell on both the master
      --  and the slave hosts. This should be made more portable.

      --  Local spawn

      if Host (Host'First) /= '`' and then Is_Local_Host (Host) then

         declare
            Spawn_Local : constant String :=
              "sh -c """ & Command & """ &";
         begin
            pragma Debug (C, O ("Enter Spawn (local): " & Spawn_Local));
            Spawn (Spawn_Local);
         end;

      --  Remote spawn

      else
         declare
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

            Remote_Command : constant String :=
              Rsh_Command & ' ' & Host & ' ' & Rsh_Options;

            Spawn_Remote : constant String :=
              Remote_Command & " """ & Command & " --polyorb-dsa-detach "" ";
         begin
            pragma Debug (C, O ("Enter Spawn (remote): " & Spawn_Remote));
            Spawn (Spawn_Remote);
         end;
      end if;

      pragma Debug (C, O ("Launch_Partition: leave"));
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
