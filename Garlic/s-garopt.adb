------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . O P T I O N S                  --
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

with System.Garlic.Debug;      use System.Garlic.Debug;
with Ada.Command_Line;         use Ada.Command_Line;
with GNAT.OS_Lib;              use GNAT.OS_Lib;

package body System.Garlic.Options is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("GAROPT", "(s-garopt): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;
   --  Debugging stuff.

   ------------------------
   -- Get_Boot_Server --
   ------------------------

   function Get_Boot_Server (Default : String := "tcp") return String is
      EV : String_Access;
   begin
      for Index in 1 .. Argument_Count - 1 loop
         if Argument (Index) = "--boot_server" then
            return Argument (Index + 1);
         end if;
      end loop;
      EV := Getenv ("BOOT_SERVER");
      if EV = null or else EV.all = "" then
         return Default;
      end if;
      return EV.all;
   end Get_Boot_Server;

   -------------------------
   -- Get_Connection_Hits --
   -------------------------

   function Get_Connection_Hits (Default : Natural := 128) return Natural is
      EV : String_Access;
   begin
      for Index in 1 .. Argument_Count - 1 loop
         if Argument (Index) = "--connection_hits" then
            pragma Debug
              (D (D_Debug, "--connection_hits available on command line"));
            return Natural'Value (Argument (Index + 1));
         end if;
      end loop;
      EV := Getenv ("CONNECTION_HITS");
      if EV = null  or else EV.all = "" then
         return Default;
      end if;
      pragma Debug (D (D_Debug, "CONNECTION_HITS env. variable available"));
      return Natural'Value (EV.all);
   exception when others =>
      pragma Debug (D (D_Exception, "(get_connection_hits) Get lost ..."));
      raise;
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
      declare
         EV : constant String_Access := Getenv ("DETACH");
      begin
         return EV /= null and then EV.all /= "";
      end;
   exception
      when others =>
         pragma Debug (D (D_Exception, "(get_detach) Get lost ..."));
         raise;
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
      declare
         EV : constant String_Access := Getenv ("SLAVE");
      begin
         return EV /= null and then EV.all /= "";
      end;
   exception
      when others =>
         pragma Debug (D (D_Exception, "(get_is_slave) Get lost ..."));
         raise;
   end Get_Is_Slave;

   -------------------
   -- Get_No_Launch --
   -------------------

   function Get_Nolaunch return Boolean is
   begin
      for Index in 1 .. Argument_Count loop
         if Argument (Index) = "--nolaunch" then
            pragma Debug (D (D_Debug, "--nolaunch available on command line"));
            return True;
         end if;
      end loop;
      declare
         EV : constant String_Access := Getenv ("NOLAUNCH");
      begin
         return EV /= null and then EV.all /= "";
      end;
   exception
      when others =>
         pragma Debug (D (D_Exception, "(get_nolaunch) Get lost ..."));
         raise;
   end Get_Nolaunch;

end System.Garlic.Options;
