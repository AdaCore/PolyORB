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

   function Get_Absolute_Directory (Dir : String) return String;
   --  Return the absolute directory corresponding to possibly relative path
   --  Dir.

   ----------------------------
   -- Get_Absolute_Directory --
   ----------------------------

   function Get_Absolute_Directory (Dir : String) return String is
      Save_Current_Dir : constant String := Get_Current_Dir;
   begin
      Change_Dir (Dir);
      declare
         Absolute_Dir : constant String := Get_Current_Dir;
      begin
         Change_Dir (Save_Current_Dir);
         return Absolute_Dir;
      end;
   end Get_Absolute_Directory;

   Exec_Name    : constant String_Access := Locate_Exec_On_Path (Command_Name);
   Exec_Rel_Dir : constant String := Dir_Name (Exec_Name.all);
   Exec_Abs_Dir : constant String := Get_Absolute_Directory (Exec_Rel_Dir);

   Exec_Prefix    : aliased String := Dir_Name (Exec_Abs_Dir);
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
