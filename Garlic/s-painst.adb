------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--   S Y S T E M . P A R T I T I O N _ I N T E R F A C E . S T A R T U P    --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Exceptions;        use Ada.Exceptions;
with System.Garlic.Heart;   use System.Garlic.Heart;
with System.Garlic.Options; use System.Garlic.Options;
with System.Garlic.Remote;  use System.Garlic.Remote;
with System.Garlic.Types;   use System.Garlic.Types;

package body System.Partition_Interface.Startup is

   -----------
   -- Check --
   -----------

   procedure Check (Unit : in String; Version : in String) is
   begin
      if Version /= Get_Active_Version (Unit) then
         Soft_Shutdown;
         Raise_Exception
           (Program_Error'Identity,
            "Versions differ for RCI unit """ &
            Unit & """");
      end if;
   end Check;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Rsh_Command  : in String;
      Name_Is_Host : in Boolean;
      General_Name : in String;
      Command_Line : in String) is
   begin
      if not System.Garlic.Options.Nolaunch then
         if Name_Is_Host then
            Full_Launch (Rsh_Command, General_Name, Command_Line);
         else
            Full_Launch (Rsh_Command, Get_Host (General_Name), Command_Line);
         end if;
      end if;
   end Launch;

   ---------
   -- Run --
   ---------

   procedure Run (Main : in Main_Subprogram_Type) is
      What    : Exception_Id;
      Message : String_Access;

   begin
      select
         Fatal_Error.Occurred (What, Message);
         Soft_Shutdown;
         Raise_Exception (What, Message.all);
      then abort
         Main.all;
      end select;
      Free (Message);
   end Run;

end System.Partition_Interface.Startup;
