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
with System.Garlic.Heart;      use System.Garlic.Heart;
with System.Garlic.OS_Lib;     use System.Garlic.OS_Lib;
with System.Garlic.Utils;      use System.Garlic.Utils;

package body System.Garlic.Options is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("GAROPT", "(s-garopt): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;
   --  Debugging stuff.

   function Value (S : String) return Termination_Type;

   -----------
   -- Value --
   -----------

   function Value (S : String) return Termination_Type is
   begin
      if S = "local" then
         pragma Debug (D (D_Debug,
                          "local termination is selected"));
         return Local_Termination;
      elsif S = "global" then
         pragma Debug (D (D_Debug,
                          "global termination is selected"));
         return Global_Termination;
      elsif S = "deferred" then
         pragma Debug (D (D_Debug,
                          "deferred termination is selected"));
         return Deferred_Termination;
      else
         return Unknown_Termination;
      end if;
   end Value;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      Index : Natural := 1;

   begin

      Execution_Mode := Normal_Mode;

      declare
         EV : constant String := Getenv ("BOOT_SERVER");
      begin
         if EV /= "" then
            Set_Boot_Server (EV);
         else
            Set_Boot_Server ("tcp");
         end if;
      end;

      declare
         EV : constant String := Getenv ("CONNECTION_HITS");
      begin
         if EV /= "" then
            Set_Connection_Hits (Natural'Value (EV));
         else
            Set_Connection_Hits (128);
         end if;
      end;

      if Getenv ("DETACH") /= "" then
         Set_Detach (True);
      else
         Set_Detach (False);
      end if;

      if Getenv ("SLAVE") /= "" then
         Set_Is_Slave (True);
      else
         Set_Is_Slave (False);
      end if;

      declare
         EV : constant String := Getenv ("TERMINATE");
      begin
         if EV /= "" then
            Set_Termination (Value (EV));
         else
            Set_Termination (Global_Termination);
         end if;
      end;

      if Getenv ("NOLAUNCH") /= "" then
         Set_Nolaunch (True);
      else
         Set_Nolaunch (False);
      end if;

      while Index <= Argument_Count loop

         if Argument (Index) = "--boot_server" then

            pragma Debug
              (D (D_Debug, "--boot_server available on command line"));

            --  Need a second argument.
            if Index = Argument_Count then
               raise Program_Error;
            end if;

            Index := Index + 1;
            Set_Boot_Server (Argument (Index));

         elsif Argument (Index) = "--connection_hits" then

            pragma Debug
              (D (D_Debug, "--connection_hits available on command line"));


            --  Need a second argument.
            if Index = Argument_Count then
               raise Program_Error;
            end if;

            Index := Index + 1;
            Set_Connection_Hits (Natural'Value (Argument (Index)));

         elsif Argument (Index) = "--detach" then

            pragma Debug (D (D_Debug, "--detach available on command line"));

            Set_Detach (True);

         elsif Argument (Index) = "--slave" then

            pragma Debug (D (D_Debug, "--slave available on command line"));

            Set_Is_Slave (True);

         elsif Argument (Index) = "--terminate" then

            pragma Debug (D (D_Debug,
                             "--terminate available on command line"));

            if Index = Argument_Count then
               raise Program_Error;
            end if;

            Index := Index + 1;
            Set_Termination (Value (Argument (Index)));

         elsif Argument (Index) = "--nolaunch" then

            pragma Debug (D (D_Debug, "--nolaunch available on command line"));

            Set_Nolaunch (True);

         elsif Argument (Index) = "--trace" then

            pragma Debug (D (D_Debug, "--trace available on command line"));

            Set_Execution_Mode (Trace_Mode);

         elsif Argument (Index) = "--replay" then

            pragma Debug (D (D_Debug, "--replay available on command line"));

            Set_Execution_Mode (Replay_Mode);

         end if;

         Index := Index + 1;

      end loop;

   end Initialize;

   ---------------------
   -- Set_Boot_Server --
   ---------------------

   procedure Set_Boot_Server (Default : in String) is
   begin
      if Boot_Server /= null then
         Free (Boot_Server);
      end if;
      Boot_Server := new String'(Default);
   end Set_Boot_Server;

   -------------------------
   -- Set_Connection_Hits --
   -------------------------

   procedure Set_Connection_Hits (Default : in Natural) is
   begin
      Connection_Hits := Default;
   end Set_Connection_Hits;

   ----------------
   -- Set_Detach --
   ----------------

   procedure Set_Detach (Default : in Boolean) is
   begin
      Detach := Default;
   end Set_Detach;

   ------------------------
   -- Set_Execution_Mode --
   ------------------------

   procedure Set_Execution_Mode (Default : in Execution_Mode_Type) is
   begin
      if Execution_Mode /= Normal_Mode then
         pragma Debug (D (D_Debug, "Execution mode has already been set"));
         raise Program_Error;
      end if;
      Execution_Mode := Default;
   end Set_Execution_Mode;

   ------------------
   -- Set_Is_Slave --
   ------------------

   procedure Set_Is_Slave (Default : in Boolean) is
   begin
      Is_Slave := Default;
   end Set_Is_Slave;

   ------------------
   -- Set_Nolaunch --
   ------------------

   procedure Set_Nolaunch (Default : in Boolean) is
   begin
      Nolaunch := Default;
   end Set_Nolaunch;

   ------------------------
   -- Set_Partition_Name --
   ------------------------

   procedure Set_Partition_Name (Name : in String) is
   begin
      if Partition_Name /= null then
         Free (Partition_Name);
      end if;
      Partition_Name := new String'(Name);
      Set_Trace_File_Name (Name & ".ptf");
   end Set_Partition_Name;

   ---------------------
   -- Set_Termination --
   ---------------------

   procedure Set_Termination (Default : in Termination_Type) is
   begin
      Termination := Default;
   end Set_Termination;

   -------------------------
   -- Set_Trace_File_Name --
   -------------------------

   procedure Set_Trace_File_Name (Name : in String) is
   begin
      if Trace_File_Name /= null then
         Free (Trace_File_Name);
      end if;
      Trace_File_Name := new String'(Name);
   end Set_Trace_File_Name;

end System.Garlic.Options;
