------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           M L I B . T A R G E T                          --
--                            (Version for Win32)                           --
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

with Text_IO;
with MLib.Files;
with GNAT.OS_Lib;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body MLib.Target is

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : in Argument_List;
      Foreign      : in Argument_List;
      Afiles       : in Argument_List;
      Options      : in Argument_List;
      Lib_Filename : in String;
      Lib_Address  : in String  := "";
      Relocatable  : in Boolean := False)
   is
      use type GNAT.OS_Lib.String_Access;

      procedure Free is
         new Ada.Unchecked_Deallocation (GNAT.OS_Lib.Argument_List,
                                         GNAT.OS_Lib.Argument_List_Access);

      Dll_File : constant String := MLib.Files.Ext_To (Lib_Filename, DLL_Ext);

      Gnatdll_Name : constant String := "gnatdll";
      Gnatdll_Exec : GNAT.OS_Lib.String_Access;
      --  finding the 'gnatdll' tool

      procedure Build_Reloc_DLL;
      --  build a relocatable DLL by calling GNATDLL

      procedure Build_Non_Reloc_DLL;
      --  build a non relocatable DLL by calling GNATDLL

      procedure Gnatdll;
      --  execute 'gnatdll' tool

      procedure Build_Reloc_DLL is
      begin
         if not Quiet then
            Text_IO.Put_Line ("building relocatable DLL...");
            Text_IO.Put_Line ("make " & Dll_File);
         end if;

         --  Call GNATDLL
         Gnatdll;
      end Build_Reloc_DLL;

      procedure Build_Non_Reloc_DLL is
      begin
         if not Quiet then
            Text_IO.Put_Line ("building non relocatable DLL...");
            Text_IO.Put_Line ("make " & Dll_File & " using address " &
              Lib_Address);
         end if;

         --  Call GNATDLL
         Gnatdll;
      end Build_Non_Reloc_DLL;

      procedure Gnatdll is

         Arguments : GNAT.OS_Lib.Argument_List_Access;
         Success   : Boolean;
         --  gnatdll execution variables

         Addr  : GNAT.OS_Lib.String_Access := new String'(Lib_Address);
         Name  : GNAT.OS_Lib.String_Access := new
           String'(MLib.Files.Ext_To (Lib_Filename, DLL_Ext));
         Quiet : GNAT.OS_Lib.String_Access;
         Reloc : GNAT.OS_Lib.String_Access := new String'("-d");
         Stats : GNAT.OS_Lib.String_Access := new String'("-a" & Lib_Address);
         --  gnatdll arguments

      begin

         --  check if gnatdll needs to be verbose
         if Verbose then
            Quiet := new String'("-v");
         else
            Quiet := new String'("-q");
         end if;

         --  set the gnatdll arguments
         if Relocatable then

            --  arguments for relocatable DLL
            Arguments := new GNAT.OS_Lib.Argument_List
              (1 .. 3 + Afiles'Length + Foreign'Length);
            Arguments (1) := Quiet;
            Arguments (2) := Reloc;
            Arguments (3) := Name;
            Arguments (4 .. 3 + Afiles'Length) := Afiles;
            Arguments (4 + Afiles'Length .. 3 + Afiles'Length + Foreign'Length)
              := Foreign;

         else

            --  arguments for non-relocatable DLL
            Arguments := new GNAT.OS_Lib.Argument_List
              (1 .. 4 + Afiles'Length + Foreign'Length);
            Arguments (1) := Quiet;
            Arguments (2) := Reloc;
            Arguments (3) := Name;
            Arguments (4) := Stats;
            Arguments (5 .. 4 + Afiles'Length) := Afiles;
            Arguments (5 + Afiles'Length .. 4 + Afiles'Length + Foreign'Length)
              := Foreign;

         end if;

         --  execute gnatdll
         GNAT.OS_Lib.Spawn (Gnatdll_Exec.all, Arguments.all, Success);

         --  check if DLL was build
         if not Success then

            if Continue_On_Errors then
               Report_On_Error (Gnatdll_Name & " execution error.");
            else
               Ada.Exceptions.Raise_Exception
                 (Tools_Error'Identity, Gnatdll_Name & " execution error.");
            end if;

         end if;

         --  free used strings
         GNAT.OS_Lib.Free (Addr);
         GNAT.OS_Lib.Free (Name);
         GNAT.OS_Lib.Free (Quiet);
         GNAT.OS_Lib.Free (Reloc);

         --  free gnatdll argument list
         Free (Arguments);

      end Gnatdll;

   begin

      --  make sure that we can find 'gnatdll'
      Gnatdll_Exec := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatdll_Name);

      if Gnatdll_Exec = null then

         if Continue_On_Errors then
            Report_On_Error (Gnatdll_Name & " not found in path.");
         else
            Ada.Exceptions.Raise_Exception
              (Tools_Error'Identity,
               Gnatdll_Name & " not found in path");
         end if;

      elsif Verbose then
         Text_IO.Put_Line ("found " & Gnatdll_Exec.all);
      end if;

      --  check that we have a export definition file
      declare
         Def_Filename : constant String :=
           MLib.Files.Ext_To (Lib_Filename, Def_Ext);
      begin
         if not GNAT.OS_Lib.Is_Regular_File (Def_Filename) then
            Ada.Exceptions.Raise_Exception
              (Tools_Error'Identity,
               "Export definition file " & Def_Filename & " not found");
         end if;
      end;

      --  build library
      if Relocatable then
         Build_Reloc_DLL;
      else
         Build_Non_Reloc_DLL;
      end if;

      --  make .ali files read-only
      Ali_To_Read_Only (Afiles);

      --  save or delete object files
      if Backup_Dir = null then
         Remove_Files (Ofiles);
      else
         Move_Files (Ofiles, Backup_Dir.all);
      end if;

   end Build_Dynamic_Library;

end MLib.Target;
