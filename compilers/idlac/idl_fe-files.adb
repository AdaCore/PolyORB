------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . F I L E S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Table;

with Idlac_Errors;
with Platform;
with Utils;

package body Idl_Fe.Files is

   use GNAT.Directory_Operations;
   use GNAT.OS_Lib;

   IDL_File_Suffix : constant String := ".idl";

   package Search_Path is
     new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => Natural'First + 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Search paths are stored in table with directory separator at
   --  the end.

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path
     (Path    : String;
      Success : out Boolean)
   is
   begin
      if Is_Directory (Path) then
         if Utils.Is_Dir_Separator (Path (Path'Last)) then
            Search_Path.Append (new String'(Path));
         else
            Search_Path.Append (new String'(Path & Dir_Separator));
         end if;
         Success := True;
      else
         Success := False;
      end if;
   end Add_Search_Path;

   ---------------------
   -- Locate_IDL_File --
   ---------------------

   function Locate_IDL_File (File_Name : String) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      Separator : Natural;

   begin
      --  If file doesn't have IDL file extension then add it.

      if File_Extension (File_Name) /= IDL_File_Suffix then
         return Locate_IDL_File (File_Name & IDL_File_Suffix);
      end if;

      --  If File_Name has directory prefix then check file existence
      --  and return File_Name as result.

      Separator := Index
        (File_Name, To_Set (Dir_Separator & "/"), Inside, Backward);

      if Separator /= 0 then
         --  Directory prefix present: check file existence

         if Is_Regular_File (File_Name) then
            return File_Name;
         else
            return "";
         end if;
      end if;

      --  Check in the current working directory

      if Is_Regular_File (File_Name) then
         return File_Name;
      end if;

      for J in Search_Path.First .. Search_Path.Last loop
         declare
            Full_Path : constant String
              := Search_Path.Table (J).all & File_Name;

         begin
            if Is_Regular_File (Full_Path) then
               return Full_Path;
            end if;
         end;
      end loop;

      return "";
   end Locate_IDL_File;

   ------------------------------
   -- Locate_IDL_Specification --
   ------------------------------

   function Locate_IDL_Specification (Scoped_Name : String) return String is
   begin
      if Scoped_Name = "CORBA" then
         --  CORBA specification actually stored in orb.idl file
         --  for historical reasons.

         return Locate_IDL_File ("orb.idl");

      else
         return Locate_IDL_File (Scoped_Name & IDL_File_Suffix);
      end if;
   end Locate_IDL_Specification;

   ---------------------
   -- Preprocess_File --
   ---------------------

   function Preprocess_File (File_Name : String) return String is

      use Ada.Command_Line;
      use GNAT.Command_Line;

      CPP_Arg_List : constant Argument_List_Access
        := Argument_String_To_List (Platform.IDL_Preprocessor);

      Tmp_File_Name_NUL : Temp_File_Name;
      --  Name of the temporary file to which preprocessor output
      --  is sent (NUL-terminated).

      Tmp_File_Name    : String_Access;

      Args             : Argument_List (1 .. 128);
      Arg_Count        : Natural := Args'First - 1;
      --  Arguments to be passed to the preprocessor

      procedure Add_Argument (Arg : String);
      --  Increment Arg_Count and set Args (Arg_Count) to Arg

      ------------------
      -- Add_Argument --
      ------------------

      procedure Add_Argument (Arg : String) is
      begin
         Arg_Count := Arg_Count + 1;
         Args (Arg_Count) := new String'(Arg);
      end Add_Argument;

   begin
      --  Create temporary file.

      declare
         Fd : File_Descriptor;
      begin
         Create_Temp_File (Fd, Tmp_File_Name_NUL);
         if Fd = Invalid_FD then
            Idlac_Errors.Error
              (Base_Name (Command_Name)
                & ": cannot create temporary file name",
               Idlac_Errors.Fatal,
               Idlac_Errors.No_Location);
            return "";
         end if;

         --  We don't need the file descriptor

         Close (Fd);

         Tmp_File_Name := new String'(
           Tmp_File_Name_NUL (Tmp_File_Name_NUL'First
                           .. Tmp_File_Name_NUL'Last - 1)
           & Platform.IDL_Preprocessor_Suffix);
      end;

      --  Add platform specific C++ preprocessor arguments as well as C++
      --  preprocessor command name.

      for J in CPP_Arg_List'First + 1 .. CPP_Arg_List'Last loop
         Add_Argument (CPP_Arg_List (J).all);
      end loop;

      --  Pass user options to the preprocessor.

      Goto_Section ("cppargs");
      while Getopt ("*") /= ASCII.NUL loop
         Add_Argument (Full_Switch);
      end loop;

      --  Add all search paths. Remove directory separator, because gcc does
      --  not work on Windows when we call it with things like:
      --     -I /some/directory\

      for J in Search_Path.First .. Search_Path.Last loop
         Add_Argument ("-I");
         declare
            Dir : String renames Search_Path.Table (J).all;
            pragma Assert (Dir (Dir'Last) = Dir_Separator);
         begin
            Add_Argument (Dir (Dir'First .. Dir'Last - 1));
         end;
      end loop;

      --  Always add the current directory at the end of the include list

      Add_Argument ("-I");
      Add_Argument (".");

      --  Add output and source file names.

      Add_Argument ("-o");
      Add_Argument (Tmp_File_Name.all);
      Add_Argument (File_Name);

      declare
         Preprocessor_Full_Pathname : constant String_Access
           := Locate_Exec_On_Path (CPP_Arg_List (CPP_Arg_List'First).all);

         Spawn_Result : Boolean;

      begin
         if Preprocessor_Full_Pathname = null then
            Idlac_Errors.Error
              ("Cannot find preprocessor "
               & "'" & CPP_Arg_List (CPP_Arg_List'First).all & "'",
               Idlac_Errors.Fatal,
               Idlac_Errors.No_Location);
            Free (Tmp_File_Name);
            return "";
         end if;

         Spawn (Preprocessor_Full_Pathname.all,
                Args (Args'First .. Arg_Count),
                Spawn_Result);

         if not Spawn_Result then
            Idlac_Errors.Error
              (Base_Name (Command_Name) & ": preprocessor failed",
               Idlac_Errors.Fatal,
               Idlac_Errors.No_Location);
            Free (Tmp_File_Name);
            return "";
         end if;
      end;

      declare
         Result : constant String := Tmp_File_Name.all;
      begin
         Free (Tmp_File_Name);
         return Result;
      end;
   end Preprocess_File;

end Idl_Fe.Files;
