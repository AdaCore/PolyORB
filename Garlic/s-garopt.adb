------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . O P T I O N S                 --
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

with Ada.Command_Line;         use Ada.Command_Line;
with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.OS_Lib;     use System.Garlic.OS_Lib;
with System.Garlic.Thin;       use System.Garlic.Thin;

package body System.Garlic.Options is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("GAROPT", "(s-garopt): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;
   --  Debugging stuff.

   type String_Access is access String;

   Boot_Server_Default     : String_Access := new String'("tcp");
   Connection_Hits_Default : Natural := 128;
   Detach_Default          : Boolean := False;
   Is_Slave_Default        : Boolean := False;
   Nolaunch_Default        : Boolean := False;
   Permanent_Default       : Boolean := False;

   ---------------------
   -- Get_Boot_Server --
   ---------------------

   function Get_Boot_Server return String is
   begin
      for Index in 1 .. Argument_Count - 1 loop
         if Argument (Index) = "--boot_server" then
            return Argument (Index + 1);
         end if;
      end loop;
      declare
         EV : constant String := Getenv ("BOOT_SERVER");
      begin
         if EV = "" then
            return Boot_Server_Default.all;
         end if;
         return EV;
      end;
   end Get_Boot_Server;

   -------------------------
   -- Get_Connection_Hits --
   -------------------------

   function Get_Connection_Hits return Natural is
   begin
      for Index in 1 .. Argument_Count - 1 loop
         if Argument (Index) = "--connection_hits" then
            pragma Debug
              (D (D_Debug, "--connection_hits available on command line"));
            return Natural'Value (Argument (Index + 1));
         end if;
      end loop;
      declare
         EV : constant String := Getenv ("CONNECTION_HITS");
      begin
         if EV = "" then
            return Connection_Hits_Default;
         end if;
         pragma Debug (D (D_Debug, "CONNECTION_HITS env. variable available"));
         return Natural'Value (EV);
      end;
   end Get_Connection_Hits;

   ----------------
   -- Get_Detach --
   ----------------

   function Get_Detach return Boolean is
   begin
      for Index in 1 .. Argument_Count loop
         if Argument (Index) = "--detach" then
            pragma Debug (D (D_Debug, "--detach available on command line"));
            return True;
         end if;
      end loop;
      if Getenv ("DETACH") /= "" then
         return True;
      end if;
      return Detach_Default;
   end Get_Detach;

   ------------------
   -- Get_Is_Slave --
   ------------------

   function Get_Is_Slave return Boolean is
   begin
      for Index in 1 .. Argument_Count loop
         if Argument (Index) = "--slave" then
            pragma Debug (D (D_Debug, "--slave available on command line"));
            return True;
         end if;
      end loop;
      if Getenv ("SLAVE") /= "" then
         return True;
      end if;
      return Is_Slave_Default;
   end Get_Is_Slave;

   -------------------
   -- Get_Permanent --
   -------------------

   function Get_Permanent return Boolean is
   begin
      for Index in 1 .. Argument_Count loop
         if Argument (Index) = "--permanent" then
            pragma Debug (D (D_Debug,
                             "--permanent available on command line"));
            return True;
         end if;
      end loop;
      if Getenv ("PERMANENT") /= "" then
         return True;
      end if;
      return Permanent_Default;
   end Get_Permanent;

   ------------------
   -- Get_Nolaunch --
   ------------------

   function Get_Nolaunch return Boolean is
   begin
      for Index in 1 .. Argument_Count loop
         if Argument (Index) = "--nolaunch" then
            pragma Debug (D (D_Debug, "--nolaunch available on command line"));
            return True;
         end if;
      end loop;
      if Getenv ("NOLAUNCH") /= "" then
         return True;
      end if;
      return Nolaunch_Default;
   end Get_Nolaunch;

   ---------------------
   -- Set_Boot_Server --
   ---------------------

   procedure Set_Boot_Server (Default : in String) is
   begin
      Boot_Server_Default := new String'(Default);
   end Set_Boot_Server;

   -------------------------
   -- Set_Connection_Hits --
   -------------------------

   procedure Set_Connection_Hits (Default : in Natural) is
   begin
      Connection_Hits_Default := Default;
   end Set_Connection_Hits;

   ----------------
   -- Set_Detach --
   ----------------

   procedure Set_Detach (Default : in Boolean) is
   begin
      Detach_Default := Default;
   end Set_Detach;

   ------------------
   -- Set_Is_Slave --
   ------------------

   procedure Set_Is_Slave (Default : in Boolean) is
   begin
      Is_Slave_Default := Default;
   end Set_Is_Slave;

   ------------------
   -- Set_Nolaunch --
   ------------------

   procedure Set_Nolaunch (Default : in Boolean) is
   begin
      Nolaunch_Default := Default;
   end Set_Nolaunch;

   -------------------
   -- Set_Permanent --
   -------------------

   procedure Set_Permanent (Default : in Boolean) is
   begin
      Permanent_Default := Default;
   end Set_Permanent;

end System.Garlic.Options;
