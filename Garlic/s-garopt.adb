------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . O P T I O N S                 --
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

with Ada.Command_Line;                use Ada.Command_Line;
with Ada.Exceptions;                  use Ada.Exceptions;
with System.Garlic.Debug;             use System.Garlic.Debug;
with System.Garlic.Platform_Specific; use System.Garlic.Platform_Specific;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;
with GNAT.OS_Lib;
with Unchecked_Deallocation;

package body System.Garlic.Options is

   use System.Garlic.Types;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GAROPT", "(s-garopt): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;
   --  Debugging stuff

   procedure Next_Argument (Index : in out Natural);

   function Value (S : String) return Reconnection_Type;
   function Value (S : String) return Termination_Type;

   procedure Free is
     new Unchecked_Deallocation (String, GNAT.OS_Lib.String_Access);

   --------------------------------
   -- Initialize_Default_Options --
   --------------------------------

   procedure Initialize_Default_Options is
   begin
      Execution_Mode  := Normal_Mode;
      Connection_Hits := 128;
      Detach          := False;
      Mirror_Expected := False;
      Has_Light_PCS   := not Has_RCI_Pkg_Or_RACW_Var;
      Is_Boot_Mirror  := False;
      Is_Boot_Server  := False;
      Nolaunch        := False;
      Reconnection    := Rejected_On_Restart;
      Termination     := Global_Termination;
   end Initialize_Default_Options;

   -----------------------------
   -- Initialize_User_Options --
   -----------------------------

   procedure Initialize_User_Options is
      Index : Natural := 1;
      EV    : GNAT.OS_Lib.String_Access;
   begin
      EV := GNAT.OS_Lib.Getenv ("BOOT_SERVER");
      if EV.all /= "" then
         Set_Boot_Server (EV.all);
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("CONNECTION_HITS");
      if EV.all /= "" then
         Set_Connection_Hits (Natural'Value (EV.all));
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("DETACH");
      if EV.all /= "" then
         Set_Detach (True);
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("SELF_LOCATION");
      if EV.all /= "" then
         Set_Self_Location (EV.all);
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("BOOT_MIRROR");
      if EV.all /= "" then
         Set_Boot_Mirror (True);
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("NOLAUNCH");
      if EV.all /= "" then
         Set_Nolaunch (True);
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("SLAVE");
      if EV.all /= "" then
         Set_Slave (True);
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("MIRROR_EXPECTED");
      if EV.all /= "" then
         Set_Mirror_Expected (True);
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("RECONNECTION");
      if EV.all /= "" then
         Set_Reconnection (Value (EV.all));
      end if;
      Free (EV);

      EV := GNAT.OS_Lib.Getenv ("TERMINATION");
      if EV.all /= "" then
         Set_Termination (Value (EV.all));
      end if;
      Free (EV);

      while Index <= Argument_Count loop

         if Argument (Index) = "--boot_server" then

            pragma Debug (D ("--boot_server available on command line"));

            Next_Argument (Index);
            Set_Boot_Server (Argument (Index));

         elsif Argument (Index) = "--boot_mirror" then

            pragma Debug (D ("--boot_mirror available on command line"));

            Set_Boot_Mirror (True);

         elsif Argument (Index) = "--mirror_expected" then

            pragma Debug (D ("mirror_expected-- available on command line"));

            Set_Mirror_Expected (True);

         elsif Argument (Index) = "--self_location" then

            pragma Debug (D ("--self_location available on command line"));

            Next_Argument (Index);
            Set_Self_Location (Argument (Index));

         elsif Argument (Index) = "--connection_hits" then

            pragma Debug (D ("--connection_hits available on command line"));

            Next_Argument (Index);
            Set_Connection_Hits (Natural'Value (Argument (Index)));

         elsif Argument (Index) = "--detach" then

            pragma Debug (D ("--detach available on command line"));
            Set_Detach (True);

         elsif Argument (Index) = "--slave" then

            pragma Debug (D ("--slave available on command line"));
            Set_Slave (True);

         elsif Argument (Index) = "--reconnection" then

            pragma Debug (D ("--reconnection available on command line"));

            Next_Argument (Index);
            Set_Reconnection (Value (Argument (Index)));

         elsif Argument (Index) = "--termination" then

            pragma Debug (D ("--termination available on command line"));

            Next_Argument (Index);
            Set_Termination (Value (Argument (Index)));

         elsif Argument (Index) = "--nolaunch" then

            pragma Debug (D ("--nolaunch available on command line"));
            Set_Nolaunch (True);

         elsif Argument (Index) = "--trace" then

            pragma Debug (D ("--trace available on command line"));
            Set_Execution_Mode (Trace_Mode);

         elsif Argument (Index) = "--replay" then

            pragma Debug (D ("--replay available on command line"));
            Set_Execution_Mode (Replay_Mode);

         end if;

         Index := Index + 1;
      end loop;

      if Boot_Location = null then
         if (Is_Boot_Server and then not Nolaunch)
           or else Default_Boot_Server = ""
         then
            Set_Boot_Server ("tcp");
         else
            Set_Boot_Server (Default_Boot_Server);
         end if;
      end if;

      if Self_Location = null then
         if Is_Boot_Server then
            Self_Location := Boot_Location;
         else
            Self_Location := new String'("");
         end if;
      end if;

      if Is_Boot_Server then
         Is_Boot_Mirror := True;
      end if;
   end Initialize_User_Options;

   -------------------
   -- Next_Argument --
   -------------------

   procedure Next_Argument (Index : in out Natural) is
   begin
      if Index = Argument_Count then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Missing argument on command line");
      end if;
      Index := Index + 1;
   end Next_Argument;

   ---------------------
   -- Set_Boot_Mirror --
   ---------------------

   procedure Set_Boot_Mirror (Default : in Boolean) is
   begin
      if Is_Boot_Mirror then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition is already a boot mirror partition");
      end if;
      Is_Boot_Mirror := True;
   end Set_Boot_Mirror;

   ---------------------
   -- Set_Boot_Server --
   ---------------------

   procedure Set_Boot_Server (Default : in String) is
   begin
      if Boot_Location /= null then
         Free (Boot_Location);
      end if;
      Boot_Location := new String'(Default);
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
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition has already set its execution mode");
      end if;
      Execution_Mode := Default;
   end Set_Execution_Mode;

   -------------------
   -- Set_Light_PCS --
   -------------------

   procedure Set_Light_PCS (Default : in Boolean) is
   begin
      if Has_Light_PCS and then not Default then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition is configured for a light PCS");
      end if;
      Has_Light_PCS := True;
   end Set_Light_PCS;

   -------------------------
   -- Set_Mirror_Expected --
   -------------------------

   procedure Set_Mirror_Expected (Default : in Boolean) is
   begin
      Mirror_Expected := Default;
   end Set_Mirror_Expected;

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

   ----------------------
   -- Set_Reconnection --
   ----------------------

   procedure Set_Reconnection (Default : in Reconnection_Type) is
   begin
      Reconnection := Default;
   end Set_Reconnection;

   -----------------------
   -- Set_Self_Location --
   -----------------------

   procedure Set_Self_Location (Default : in String) is
   begin
      if Self_Location /= null then
         Free (Self_Location);
      end if;
      Self_Location := new String'(Default);
   end Set_Self_Location;

   ---------------
   -- Set_Slave --
   ---------------

   procedure Set_Slave (Default : in Boolean) is
   begin
      Is_Boot_Server := not Default;
   end Set_Slave;

   --------------------------
   -- Set_Task_Pool_Bounds --
   --------------------------

   procedure Set_Task_Pool_Bounds (Low, High, Max : in Positive) is
   begin
      Task_Pool_Low_Bound  := Low;
      Task_Pool_High_Bound := High;
      Task_Pool_Max_Bound  := Max;
   end Set_Task_Pool_Bounds;

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

   -----------
   -- Value --
   -----------

   function Value (S : String) return Reconnection_Type is
   begin
      return Reconnection_Type'Value (S);
   exception when others =>
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity,
         "Unknown reconnection mode """ & S & """");
   end Value;

   -----------
   -- Value --
   -----------

   function Value (S : String) return Termination_Type is
   begin
      return Termination_Type'Value (S);
   exception when others =>
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity,
         "Unknown termination mode """ & S & """");
   end Value;

end System.Garlic.Options;
