------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . O P T I O N S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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

with GNAT.Strings;

with System.Garlic.Types;

package System.Garlic.Options is

   pragma Elaborate_Body;

   Def_Task_Pool_Low_Bound  : constant := 1;
   Def_Task_Pool_High_Bound : constant := 5;
   Def_Task_Pool_Max_Bound  : constant := 512;
   Def_Connection_Hits      : constant := 20;

   Task_Pool_Low_Bound  : Natural;
   Task_Pool_High_Bound : Natural;
   Task_Pool_Max_Bound  : Natural;
   --  This one must match the definition of Max_Tasks from s-rpcpoo.adb

   Has_A_Light_PCS  : Boolean;
   Mirror_Expected  : Boolean;
   Self_Location    : GNAT.Strings.String_List_Access;
   Boot_Location    : GNAT.Strings.String_List_Access;
   Data_Location    : GNAT.Strings.String_List_Access;
   Is_Boot_Mirror   : Boolean;
   Is_Boot_Server   : Boolean;
   Is_Pure_Client   : Boolean;
   Local_Launch     : Boolean;
   Connection_Hits  : Natural;
   Detach           : Boolean;
   Nolaunch         : Boolean;
   Reconnection     : Types.Reconnection_Type;
   Termination      : Types.Termination_Type;
   Partition_Name   : GNAT.Strings.String_Access;
   Rsh_Command      : GNAT.Strings.String_Access;
   Rsh_Options      : GNAT.Strings.String_Access;
   Execution_Mode   : Types.Execution_Mode_Type;
   Trace_File_Name  : GNAT.Strings.String_Access;
   Checksum         : Types.Word;

   procedure Initialize_Default_Options;

   procedure Initialize_User_Options;

   procedure Set_Boot_Mirror (Default : Boolean);

   procedure Set_Boot_Location (Default : String);

   procedure Set_Data_Location (Default : String);

   procedure Set_Connection_Hits (Default : Natural);

   procedure Set_Detach   (Default : Boolean);

   procedure Set_Execution_Mode (Default : Types.Execution_Mode_Type);

   procedure Set_Light_PCS (Default : Boolean);

   procedure Set_Local_Launch (Default : Boolean);

   procedure Set_Nolaunch (Default : Boolean);

   procedure Set_Partition_Name (Name : String);

   procedure Set_Pure_Client (Default : Boolean);

   procedure Set_Reconnection (Default : Types.Reconnection_Type);

   procedure Set_Rsh_Command (Default : String);

   procedure Set_Rsh_Options (Default : String);

   procedure Set_Self_Location (Default : String);

   procedure Set_Slave (Default : Boolean);

   procedure Set_Task_Pool_Bounds (Low, High, Max : Positive);

   procedure Set_Termination (Default : Types.Termination_Type);

   procedure Set_Trace_File_Name (Name : String);

   procedure Set_Mirror_Expected (Default : Boolean);

end System.Garlic.Options;
