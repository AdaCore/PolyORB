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
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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
with System.Garlic.Platform_Specific; use System.Garlic.Platform_Specific;
with System.Garlic.Types;             use System.Garlic.Types;
with System.Garlic.Utils;             use System.Garlic.Utils;
with GNAT.OS_Lib;

package body System.Garlic.Options is

   use System.Garlic.Types;

   --  Debugging stuff

   procedure Next_Argument (Index : in out Natural);

   function Value (S : String) return Reconnection_Type;
   function Value (S : String) return Termination_Type;

   --------------------------------
   -- Initialize_Default_Options --
   --------------------------------

   procedure Initialize_Default_Options is
   begin
      Task_Pool_Low_Bound  := Def_Task_Pool_Low_Bound;
      Task_Pool_High_Bound := Def_Task_Pool_High_Bound;
      Task_Pool_Max_Bound  := Def_Task_Pool_Max_Bound;

      Execution_Mode  := Normal_Mode;
      Connection_Hits := Def_Connection_Hits;
      Detach          := False;
      Mirror_Expected := False;
      Has_A_Light_PCS := False;
      Is_Pure_Client  := False;
      Is_Boot_Mirror  := False;
      Is_Boot_Server  := False;
      Local_Launch    := Supports_Local_Launch;
      Nolaunch        := False;
      Reconnection    := Reject_On_Restart;
      Termination     := Global_Termination;
      Rsh_Command     := new String'(Platform_Specific.Rsh_Command);
      Rsh_Options     := new String'(Platform_Specific.Rsh_Options);
   end Initialize_Default_Options;

   -----------------------------
   -- Initialize_User_Options --
   -----------------------------

   procedure Initialize_User_Options is
      Index : Natural := 1;
      EV    : GNAT.OS_Lib.String_Access;
   begin
      EV := GNAT.OS_Lib.Getenv ("BOOT_SERVER");
      if EV'Length /= 0 then
         Set_Boot_Location (Unquote (EV.all));
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("BOOT_LOCATION");
      if EV'Length /= 0 then
         Set_Boot_Location (Unquote (EV.all));
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("DATA_LOCATION");
      if EV'Length /= 0 then
         Set_Data_Location (Unquote (EV.all));
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("CONNECTION_HITS");
      if EV'Length /= 0 then
         Set_Connection_Hits (Natural'Value (EV.all));
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("DETACH");
      if EV'Length /= 0 then
         Set_Detach (True);
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("SELF_LOCATION");
      if EV'Length /= 0 then
         Set_Self_Location (Unquote (EV.all));
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("BOOT_MIRROR");
      if EV'Length /= 0 then
         Set_Boot_Mirror (True);
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("NOLAUNCH");
      if EV'Length /= 0 then
         Set_Nolaunch (True);
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("SLAVE");
      if EV'Length /= 0 then
         Set_Slave (True);
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("MIRROR_EXPECTED");
      if EV'Length /= 0 then
         Set_Mirror_Expected (True);
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("RECONNECTION");
      if EV'Length /= 0 then
         Set_Reconnection (Value (EV.all));
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("TERMINATION");
      if EV'Length /= 0 then
         Set_Termination (Value (EV.all));
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("RSH_COMMAND");
      if EV'Length /= 0 then
         Set_Rsh_Command (EV.all);
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("RSH_OPTIONS");
      if EV'Length /= 0 then
         Set_Rsh_Command (EV.all);
      end if;
      GNAT.OS_Lib.Free (EV);

      EV := GNAT.OS_Lib.Getenv ("LOCAL_LAUNCH");
      if EV'Length /= 0 then
         Set_Local_Launch (Boolean'Value (EV.all));
      end if;
      GNAT.OS_Lib.Free (EV);

      while Index <= Argument_Count loop

         if Argument (Index) = "--boot_server" then

            Next_Argument (Index);
            Set_Boot_Location (Unquote (Argument (Index)));

         elsif Argument (Index) = "--boot_location" then

            Next_Argument (Index);
            Set_Boot_Location (Unquote (Argument (Index)));

         elsif Argument (Index) = "--data_location" then

            Next_Argument (Index);
            Set_Data_Location (Unquote (Argument (Index)));

         elsif Argument (Index) = "--boot_mirror" then

            Set_Boot_Mirror (True);

         elsif Argument (Index) = "--mirror_expected" then

            Set_Mirror_Expected (True);

         elsif Argument (Index) = "--self_location" then

            Next_Argument (Index);
            Set_Self_Location (Unquote (Argument (Index)));

         elsif Argument (Index) = "--connection_hits" then

            Next_Argument (Index);
            Set_Connection_Hits (Natural'Value (Argument (Index)));

         elsif Argument (Index) = "--detach" then

            Set_Detach (True);

         elsif Argument (Index) = "--slave" then

            Set_Slave (True);

         elsif Argument (Index) = "--reconnection" then

            Next_Argument (Index);
            Set_Reconnection (Value (Argument (Index)));

         elsif Argument (Index) = "--termination" then

            Next_Argument (Index);
            Set_Termination (Value (Argument (Index)));

         elsif Argument (Index) = "--rsh-command" then

            Next_Argument (Index);
            Set_Rsh_Command (Argument (Index));

         elsif Argument (Index) = "--rsh-options" then

            Next_Argument (Index);
            Set_Rsh_Options (Argument (Index));

         elsif Argument (Index) = "--nolaunch" then

            Set_Nolaunch (True);

         elsif Argument (Index) = "--trace" then

            Set_Execution_Mode (Trace_Mode);

         elsif Argument (Index) = "--replay" then

            Set_Execution_Mode (Replay_Mode);

         elsif Argument (Index) = "--local-launch" then

            Next_Argument (Index);
            Set_Local_Launch (Boolean'Value (Argument (Index)));

         end if;

         Index := Index + 1;
      end loop;

      if Is_Boot_Server then
         Is_Boot_Mirror := True;
      end if;

      if Is_Boot_Server
        and then Self_Location /= null
      then
         if Boot_Location = null then
            Boot_Location := Copy (Self_Location);

         else
            --  For the boot partition, boot locations are also self
            --  locations. They have to be appended to the current
            --  self locations because they are not the prefered
            --  locations.

            declare
               All_Locations : constant String
                 := Merge_String (Self_Location) & " " &
                    Merge_String (Boot_Location);
            begin
               Set_Self_Location (All_Locations);
            end;
         end if;
      end if;

      if Boot_Location = null then
         if (Is_Boot_Server and then not Nolaunch)
           or else Default_Protocol_Name'Length = 0
         then
            Set_Boot_Location ("tcp");
         else
            Set_Boot_Location (Default_Protocol_Name & "://" &
                               Default_Protocol_Data);
         end if;
      end if;

      if Self_Location = null then
         if Is_Boot_Server then
            Self_Location := Copy (Boot_Location);
         else
            Self_Location
              := new String_Array'(1 .. 1 =>
                                     new String'(Default_Protocol_Name));
         end if;
      end if;

      if Data_Location = null then
         Data_Location
           := new String_Array'(1 .. 1 =>
                                  new String'(Default_Storage_Name & "://" &
                                              Default_Storage_Data));
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

   -----------------------
   -- Set_Boot_Location --
   -----------------------

   procedure Set_Boot_Location (Default : in String) is
   begin
      Destroy (Boot_Location);
      Boot_Location := Split_String (Default);
   end Set_Boot_Location;

   ---------------------
   -- Set_Boot_Mirror --
   ---------------------

   procedure Set_Boot_Mirror (Default : in Boolean) is
   begin
      if Is_Boot_Mirror
        and then not Default
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition is already a boot mirror partition");
      end if;
      if Default
        and then Has_A_Light_PCS
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition with light PCS cannot be a boot mirror");
      end if;
      Is_Boot_Mirror := True;
   end Set_Boot_Mirror;

   -------------------------
   -- Set_Connection_Hits --
   -------------------------

   procedure Set_Connection_Hits (Default : in Natural) is
   begin
      Connection_Hits := Default;
   end Set_Connection_Hits;

   -----------------------
   -- Set_Data_Location --
   -----------------------

   procedure Set_Data_Location (Default : in String) is
   begin
      Destroy (Data_Location);
      Data_Location := Split_String (Default);
   end Set_Data_Location;

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
      Execution_Mode := Default;
   end Set_Execution_Mode;

   -------------------
   -- Set_Light_PCS --
   -------------------

   procedure Set_Light_PCS (Default : in Boolean) is
   begin
      if Has_A_Light_PCS
        and then not Default
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition is configured for a light PCS");
      end if;
      if Default then
         if Termination /= Local_Termination then
            Ada.Exceptions.Raise_Exception
              (Program_Error'Identity,
               "Partition with light PCS must have a local termination");
         elsif Is_Boot_Mirror then
            Ada.Exceptions.Raise_Exception
              (Program_Error'Identity,
               "Partition with light PCS cannot be a boot mirror partition");
         end if;
         Set_Pure_Client (True);
      end if;
      Has_A_Light_PCS := Default;
   end Set_Light_PCS;

   ----------------------
   -- Set_Local_Launch --
   ----------------------

   procedure Set_Local_Launch (Default : in Boolean) is
   begin
      Local_Launch := Default;
   end Set_Local_Launch;

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
         Destroy (Partition_Name);
      end if;
      Partition_Name := new String'(Name);
      Set_Trace_File_Name (Name & ".ptf");
   end Set_Partition_Name;

   ---------------------
   -- Set_Pure_Client --
   ---------------------

   procedure Set_Pure_Client (Default : in Boolean) is
   begin
      if Is_Pure_Client
        and then not Default
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition is configured as a pure client");
      end if;
      if Has_A_Light_PCS
        and then not Default
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition is configured with a light PCS");
      end if;
      Is_Pure_Client := Default;
   end Set_Pure_Client;

   ----------------------
   -- Set_Reconnection --
   ----------------------

   procedure Set_Reconnection (Default : in Reconnection_Type) is
   begin
      Reconnection := Default;
   end Set_Reconnection;

   ---------------------
   -- Set_Rsh_Command --
   ---------------------

   procedure Set_Rsh_Command (Default : in String) is
   begin
      if Rsh_Command /= null then
         Destroy (Rsh_Command);
      end if;
      Rsh_Command := new String'(Default);
   end Set_Rsh_Command;

   ---------------------
   -- Set_Rsh_Options --
   ---------------------

   procedure Set_Rsh_Options (Default : in String) is
   begin
      if Rsh_Options /= null then
         Destroy (Rsh_Options);
      end if;
      Rsh_Options := new String'(Default);
   end Set_Rsh_Options;

   -----------------------
   -- Set_Self_Location --
   -----------------------

   procedure Set_Self_Location (Default : in String) is
   begin
      Destroy (Self_Location);
      Self_Location := Split_String (Default);
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
      if Has_A_Light_PCS
        and then Default /= Local_Termination
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "Partition with light PCS must have a local termination");
      end if;
      Termination := Default;
   end Set_Termination;

   -------------------------
   -- Set_Trace_File_Name --
   -------------------------

   procedure Set_Trace_File_Name (Name : in String) is
   begin
      if Trace_File_Name /= null then
         Destroy (Trace_File_Name);
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
