------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          M L I B . T A R G E T                           --
--                            (Default Version)                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--              Copyright (C) 1999, Ada Core Technologies, Inc.             --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a set of target dependent routines to build
--  static, dynamic and shared libraries.

with MLib.Files;
with MLib.Tools;
with GNAT.OS_Lib;
with Ada.Text_IO;

package body MLib.Target is

   use Ada;
   use GNAT;
   use MLib;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : in Argument_List;
      Afiles       : in Argument_List;
      Options      : in Argument_List;
      Lib_Filename : in String;
      Lib_Address  : in String  := "";
      Relocatable  : in Boolean := False)
   is
      DLL_File : constant String := MLib.Files.Ext_To (Lib_Filename, DLL_Ext);

      use type OS_Lib.Argument_List;
      use type OS_Lib.String_Access;

   begin
      if Verbose then
         if Relocatable then
            Text_IO.Put_Line ("building relocatable DLL " & DLL_File);
         else
            Text_IO.Put_Line ("building non-relocatable DLL..." & DLL_File);
         end if;
      end if;

      Tools.Gcc (Output_File => DLL_File,
                 Objects     => Ofiles,
                 Options     => Options,
                 Build_Lib   => True);
      Ali_To_Read_Only (Afiles);

      if Backup_Dir = null then
         Remove_Files (Ofiles);
      else
         Move_Files (Ofiles, Backup_Dir.all);
      end if;
   end Build_Dynamic_Library;

end MLib.Target;
