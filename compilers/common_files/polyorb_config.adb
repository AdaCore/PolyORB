------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O L Y O R B _ C O N F I G                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2008, Free Software Foundation, Inc.             --
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

with Ada.Command_Line; use Ada.Command_Line;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Platform;

package body PolyORB_Config is

   function Get_Absolute_Command return String;
   --  Get the absolute path of the command being executed

   --------------------------
   -- Get_Absolute_Command --
   --------------------------

   function Get_Absolute_Command return String is
      Cmd : constant String := Command_Name;
   begin
      for J in Cmd'Range loop
         if Cmd (J) = Dir_Separator then
            return Normalize_Pathname (Cmd);
         end if;
      end loop;

      --  Case of command name containing no directory separator

      declare
         Abs_Command_Access : String_Access := Locate_Exec_On_Path (Cmd);
         Abs_Command : constant String := Abs_Command_Access.all;
      begin
         Free (Abs_Command_Access);
         return Abs_Command;
      end;

   end Get_Absolute_Command;

   Exec_Abs_Name : constant String := Get_Absolute_Command;
   Exec_Abs_Dir  : constant String := Dir_Name (Exec_Abs_Name);

   --  Strip trailing separator and remove last component ("bin")

   Exec_Prefix   : aliased String  :=
                     Dir_Name (Exec_Abs_Dir (Exec_Abs_Dir'First
                                          .. Exec_Abs_Dir'Last - 1));
   Default_Prefix : aliased String := Platform.Prefix;

   Prefix_Var : String_Access;

   function Prefix return String is
   begin
      if Prefix_Var = null then
         if Is_Readable_File (Exec_Prefix
           & Dir_Separator & "include"
           & Dir_Separator & "polyorb"
           & Dir_Separator & "polyorb.ads")
         then
            Prefix_Var := Exec_Prefix'Access;
         else
            Prefix_Var := Default_Prefix'Access;
         end if;
      end if;
      return Prefix_Var.all;
   end Prefix;

end PolyORB_Config;
