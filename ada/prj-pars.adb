------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . P A R S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2000 Free Software Foundation, Inc.            --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with Errout;                     use Errout;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with Namet;                      use Namet;
with Osint;                      use Osint;
with Output;                     use Output;
with Prj.Com;                    use Prj.Com;
with Prj.Decl;
with Prj.Nmsc;                   use Prj.Nmsc;
with Prj.Str;
with Scans;                      use Scans;
with Scn;                        use Scn;
with Sinput;                     use Sinput;
with Sinput.P;                   use Sinput.P;
with Types;                      use Types;

package body Prj.Pars is

   Project_Error : exception;
   Project_File_Extension : String := ".apr";

   ------------------------------------
   -- Local Packages and Subprograms --
   ------------------------------------

   package Naming_Scheme renames Prj.Nmsc;

   procedure Parse_Single_Project
     (Project         : out Project_Id;
      Modified_By     : Project_Id;
      Path_Name       : String);
   --  Parse a project file.
   --  Recursive procedure: it calls itself for imported and
   --  modified projects.

   function Path_Name_Of
     (File_Name : String;
      Directory : String)
      return      String;
   --  Returns the path name of a (non project) file.
   --  Returns No_Name if file cannot be found.

   function Path_Name_Of
     (Project_File_Name : String;
      Directory         : String;
      Referred_In       : Project_Id)
      return              String;
   --  Returns the path name of a project file.
   --  Raises Project_Error if project file cannot be found.

   function Immediate_Directory_Of (Path_Name : Name_Id) return Name_Id;
   --  Get the directory of the file with the specified path name.
   --  This includes the directory separator as the last character.
   --  Returns "./" if Path_Name contains no directory separator.

   function Simple_File_Name_Of (Path_Name : Name_Id) return Name_Id;
   --  Returns the name of a file with the specified path name
   --  with no directory information.

   ----------------------------
   -- Immediate_Directory_Of --
   ----------------------------

   function Immediate_Directory_Of (Path_Name : Name_Id) return Name_Id is
   begin
      Get_Name_String (Path_Name);

      for Index in reverse 1 .. Name_Len loop
         if Name_Buffer (Index) = '/'
           or else
           Name_Buffer (Index) = Directory_Separator

         then
            Name_Len := Index;
            return Name_Find;
         end if;
      end loop;

      Name_Len := 2;
      Name_Buffer (1 .. 2) := "./";
      return Name_Find;

   end Immediate_Directory_Of;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Project           : out Project_Id;
      Project_File_Name : String;
      Package_Name      : String := "")
   is
      Current_Directory : constant String := Get_Current_Dir;

   begin
      --  Establish the Tool_Name, if any

      if Package_Name /= "" then
         Name_Len := Package_Name'Length;
         Name_Buffer (1 .. Name_Len) := Package_Name;
         Tool_Name := Name_Find;
      end if;

      declare
         Path_Name : constant String := Path_Name_Of
           (Project_File_Name,
            Directory   => Current_Directory,
            Referred_In => No_Project);

      begin
         --  Initialize the project table

         Projects.Set_Last (No_Project);

         Errout.Initialize;

         --  And parse the main project file

         Parse_Single_Project
           (Project         => Project,
            Modified_By     => No_Project,
            Path_Name       => Path_Name);
      end;

   exception
      when Project_Error =>

         --  There has been some error.
         --  Output the errors and return No_Project to the calling tool.

         Project := No_Project;
         Errout.Finalize;

      when X : others =>

         --  Internal error

         Write_Str  ("Exception ");
         Write_Str  (Exception_Name (X));
         Write_Line (" raised, while parsing project file");
         Project := No_Project;
   end Parse;

   --------------------------
   -- Parse_Single_Project --
   --------------------------

   procedure Parse_Single_Project
     (Project         : out Project_Id;
      Modified_By     : Project_Id;
      Path_Name       : String)
   is
      Canonical_Path_Name : Name_Id;
      Data                : Project_Data := Empty_Project;
      Project_Directory   : Name_Id;
      Imported            : Project_List := Empty_Project_List;
      Project_Scan_State  : Saved_Project_Scan_State;
      Source_Index        : Source_File_Index;

   begin
      Name_Len := Path_Name'Length;
      Name_Buffer (1 .. Name_Len) := Path_Name;
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      Canonical_Path_Name := Name_Find;

      --  Check if the project file is allowed to be imported
      --  or if it has already been parsed.

      for Index in Projects.First .. Projects.Last loop
         Data := Projects.Table (Index);

         if Data.Path_Name = Canonical_Path_Name then

            --  A project that is already modified by another project
            --  cannot be imported directly.

            if Data.Modified_By /= No_Project then
               Error_Msg_BC
                 ("Project " &
                  Get_Name_String (Canonical_Path_Name) &
                  " cannot be imported, because it is modified by project " &
                  Get_Name_String (Projects.Table (Data.Modified_By).Name));
               raise Project_Error;

            elsif Modified_By /= No_Project then
               declare
                  Text : constant String :=
                           "Project " &
                           Get_Name_String (Canonical_Path_Name) &
                           " cannot be modified; ";

               begin
                  if Data.First_Referred_By = No_Project then
                     Error_Msg_BC
                       (Text &
                        "it is the compiling project.");

                  else
                     Error_Msg_BC
                       (Text &
                        "it is already imported by project " &
                        Get_Name_String
                          (Projects.Table (Data.First_Referred_By).Path_Name));
                  end if;

                  raise Project_Error;
               end;

            else
               if Current_Verbosity >= Medium then
                  Write_Line ("   Skipped, because already parsed.");

               end if;

               Project := Index;
               return;
            end if;
         end if;
      end loop;

      --  We never encountered this project file.
      --  Save the scan state, load the project file and start to scan it.

      Save_Project_Scan_State (Project_Scan_State);
      Source_Index := Load_Project_File (Path_Name);

      --  We cannot find it, so we stop

      if Source_Index = No_Source_File then
         Project := No_Project;
         return;
      end if;

      Initialize_Scanner (Types.No_Unit, Source_Index);

      if Current_Verbosity >= Medium then
         Write_Str  ("Parsing """);
         Write_Str  (Path_Name);
         Write_Char ('"');
         Write_Eol;
      end if;

      Project_Directory := Immediate_Directory_Of (Canonical_Path_Name);

      --  We put the project's incomplete data in the project table

      Projects.Increment_Last;
      Project := Projects.Last;
      Data := Empty_Project;
      Data.Path_Name := Canonical_Path_Name;
      Data.Directory := Project_Directory;
      Data.File_Name := Simple_File_Name_Of (Canonical_Path_Name);
      Data.Modified_By := Modified_By;

      Projects.Table (Project) := Data;

      --  Is there any imported project?

      while Token = Tok_With loop

         loop
            Scan;

            declare
               Project_Name : constant String :=
                                Prj.Str.Value
                                  (Project => Data,
                                   Pkg     => No_Package);

               Path_Name : constant String :=
                             Path_Name_Of
                               (Project_Name,
                                Directory   => Get_Name_String
                                                 (Project_Directory),
                                Referred_In => Project);

               New_Project : Project_Id;
               New_Data    : Project_Data;

            begin
               --  Parse this imported project

               Parse_Single_Project
                 (Project         => New_Project,
                  Modified_By     => No_Project,
                  Path_Name       => Path_Name);

               New_Data := Projects.Table (New_Project);

               --  If we were the first project to import it,
               --  set First_Referred_By to us.

               if New_Data.First_Referred_By = No_Project then
                  New_Data.First_Referred_By := Project;
                  Projects.Table (New_Project) := New_Data;
               end if;

               --  Add this project to our list of imported projects

               Project_Lists.Increment_Last;
               Project_Lists.Table (Project_Lists.Last) :=
                 (Project => New_Project, Next => Empty_Project_List);

               --  Imported is the id of the last imported project.
               --  If it is No_Name, then this imported project is our first.

               if Imported = Empty_Project_List then
                  Data.Imported_Projects := Project_Lists.Last;

               else
                  Project_Lists.Table (Imported).Next := Project_Lists.Last;
               end if;

               Imported := Project_Lists.Last;
            end;

            exit when Token /= Tok_Comma;
         end loop;

         Expect (Tok_Semicolon, ";");

         --  Scan past the semi-colon

         if Token = Tok_Semicolon then
            Scan;
         end if;

      end loop;

      Expect (Tok_Project, "project");

      --  Scan past "project"

      if Token = Tok_Project then
         Scan;
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Get_Name_String (Token_Name);
         Name_Buffer (1 .. Name_Len) := To_Lower (Name_Buffer (1 .. Name_Len));
         Data.Name := Name_Find;

         --  Scan past the project name

         Scan;

      end if;

      if Token = Tok_Modifying then
         Scan;

         declare
            Project_Name : constant String := Prj.Str.Value
              (Project => Data,
               Pkg     => No_Package);

            Path_Name : String :=
                         Path_Name_Of
                           (Project_Name,
                            Directory   => Get_Name_String (Data.Directory),
                            Referred_In => Project);

            New_Project : Project_Id;

         begin
            if Path_Name = Parse_Single_Project.Path_Name then
               Error_Msg_BC ("A project cannot modify itself.");

            else
               --  Parse the project we are modifying

               Parse_Single_Project
                 (Project         => New_Project,
                  Modified_By     => Project,
                  Path_Name       => Path_Name);
               Data.Modifies := New_Project;

            end if;
         end;
      end if;

      Expect (Tok_Is, "is");

      Prj.Decl.Parse (Project, Data, No_Package, Do_Not_Skip => True);

      Expect (Tok_End, "end");

      --  Scan past "end"

      if Token = Tok_End then
         Scan;
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier
        and then To_Lower (Get_Name_String (Token_Name)) /=
                                              Get_Name_String (Data.Name)
      then
         Error_Msg_BC ("Expected """ & Get_Name_String (Data.Name) & """");
      end if;

      if Token /= Tok_Semicolon then
         Scan;
      end if;

      Expect (Tok_Semicolon, ";");

      --  If there are any error in any project file, we stop parsing.

      if Compilation_Errors then
         raise Project_Error;
      end if;

      --  We save the project data in the project table

      Projects.Table (Project) := Data;

      --  And we check the Naming Scheme, the object directory,
      --  the source directories and the source files of the project

      Check_Naming_Scheme (Project);

      --  If an error is detected during the check, we stop parsing.

      if Compilation_Errors then
         raise Project_Error;
      end if;

      --  Restore the scan state, in case we are not the main project

      Restore_Project_Scan_State (Project_Scan_State);

   end Parse_Single_Project;

   ------------------
   -- Path_Name_Of --
   ------------------

   function Path_Name_Of
     (File_Name : String;
      Directory : String)
      return      String
   is
      Result : String_Access;

   begin
      Result := Locate_Regular_File (File_Name => File_Name,
                                     Path      => Directory);

      if Result = null then
         return "";
      else
         Canonical_Case_File_Name (Result.all);
         return Result.all;
      end if;
   end Path_Name_Of;

   function Path_Name_Of
     (Project_File_Name : String;
      Directory         : String;
      Referred_In       : Project_Id)
      return        String
   is
      Result : String_Access;

   begin

      --  First we try <file_name>

      if Current_Verbosity = High then
         Write_Str  ("Path_Name_Of (""");
         Write_Str  (Project_File_Name);
         Write_Str  (""", """);
         Write_Str  (Directory);
         Write_Line (""");");
         Write_Str  ("   Trying ");
         Write_Line (Project_File_Name);
      end if;

      Result :=
        Locate_Regular_File
          (File_Name => Project_File_Name,
           Path      => "");

      --  Then we try <file_name>.apr

      if Result = null then
         if Current_Verbosity = High then
            Write_Str  ("   Trying ");
            Write_Str  (Project_File_Name);
            Write_Line (Project_File_Extension);
         end if;

         Result :=
           Locate_Regular_File
           (File_Name => Project_File_Name & Project_File_Extension,
            Path      => "");

         --  The we try <directory>/<file_name>

         if Result = null then
            if Current_Verbosity = High then
               Write_Str  ("   Trying ");
               Write_Str  (Directory);
               Write_Line (Project_File_Name);
            end if;

            Result :=
              Locate_Regular_File
              (File_Name => Directory & Project_File_Name,
               Path      => "");

            --  Then we try <directory>/<file_name>.apr

            if Result = null then
               if Current_Verbosity = High then
                  Write_Str  ("   Trying ");
                  Write_Str  (Directory);
                  Write_Str  (Project_File_Name);
                  Write_Line (Project_File_Extension);
               end if;

               Result :=
                 Locate_Regular_File
                 (File_Name => Directory & Project_File_Name &
                  Project_File_Extension,
                  Path      => "");
            end if;
         end if;
      end if;

      --  If we cannot find the project file, we stop the parsing
      --  by raising Project_Error.

      --  But first, we explain what project file cannot be found,
      --  and why we were parsing it.

      if Result = null then
         declare
            Current : Project_Id;
            Data    : Project_Data;

         begin
            Write_Str  ("Project file """);
            Write_Str  (Project_File_Name);
            Write_Line (""" unknown.");

            Current := Referred_In;
            while Current /= No_Project loop
               Data := Projects.Table (Current);
               Write_Str  ("   first refered in """);
               Write_Str  (Get_Name_String (Data.File_Name));
               Write_Str  (""" (directory """);
               Write_Str  (Get_Name_String (Data.Directory));
               Write_Line (""")");
               Current := Data.First_Referred_By;
            end loop;

            raise Project_Error;
         end;
      end if;

      Canonical_Case_File_Name (Result.all);
      return Result.all;
   end Path_Name_Of;

   -------------------
   -- Set_Verbosity --
   -------------------

   procedure Set_Verbosity (To : in Verbosity) is
   begin
      Current_Verbosity := To;
   end Set_Verbosity;

   -------------------------
   -- Simple_File_Name_Of --
   -------------------------

   function Simple_File_Name_Of (Path_Name : Name_Id) return Name_Id is
   begin
      Get_Name_String (Path_Name);

      for Index in reverse 1 .. Name_Len loop
         if Name_Buffer (Index) = '/'
           or else
           Name_Buffer (Index) = Directory_Separator

         then
            exit when Index = Name_Len;
            Name_Buffer (1 .. Name_Len - Index) :=
              Name_Buffer (Index + 1 .. Name_Len);
            Name_Len := Name_Len - Index;
            return Name_Find;
         end if;
      end loop;

      return No_Name;

   end Simple_File_Name_Of;

begin

   Canonical_Case_File_Name (Project_File_Extension);

end Prj.Pars;
