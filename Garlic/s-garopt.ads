------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . O P T I O N S                 --
--                                                                          --
--                                 S p e c                                  --
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

with System.Garlic.Heart;
with System.Garlic.Types;
with System.Garlic.Utils;

package System.Garlic.Options is

   --  This package needs comments ???

   Task_Pool_Low_Bound  : Positive := 1;
   Task_Pool_High_Bound : Positive := 5;
   Task_Pool_Max_Bound  : Positive range 1 .. 512 := 512;
   --  This one must match the definition of Max_Tasks from s-rpcpoo.adb

   Has_RCI_Pkg_Or_RACW_Var : Boolean := True;

   Boot_Server     : Utils.String_Access;
   Connection_Hits : Natural;
   Detach          : Boolean;
   Is_Slave        : Boolean;
   Nolaunch        : Boolean;
   Termination     : Heart.Termination_Type;
   Partition_Name  : Utils.String_Access;
   Execution_Mode  : Heart.Execution_Mode_Type;
   Trace_File_Name : Utils.String_Access;
   Checksum        : Types.Word;

   procedure Initialize;

   procedure Set_Boot_Server (Default : in String);

   procedure Set_Connection_Hits (Default : in Natural);

   procedure Set_Detach   (Default : in Boolean);

   procedure Set_Execution_Mode (Default : in Heart.Execution_Mode_Type);

   procedure Set_Is_Slave (Default : in Boolean);

   procedure Set_Nolaunch (Default : in Boolean);

   procedure Set_Partition_Name (Name : in String);

   procedure Set_Task_Pool_Bounds (Low, High, Max : in Positive);

   procedure Set_Termination (Default : in Heart.Termination_Type);

   procedure Set_Trace_File_Name (Name : in String);

end System.Garlic.Options;

