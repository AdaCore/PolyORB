------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . P A R T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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
with Prj.Dect;
with Scans;                      use Scans;
with Scn;                        use Scn;
with Sinfo;                      use Sinfo;
with Sinput;                     use Sinput;
with Sinput.P;                   use Sinput.P;
with Stringt;                    use Stringt;
with Types;                      use Types;

pragma Elaborate_All (GNAT.OS_Lib);

package body Prj.Part is

   Project_File_Extension : String := ".gpr";

   Project_Path : String_Access;
   --  The project path; initialized during package elaboration.

   Ada_Project_Path : constant String := "ADA_PROJECT_PATH";
   Prj_Path : constant String_Access := Getenv (Ada_Project_Path);

   ------------------------------------
   -- Local Packages and Subprograms --
   ------------------------------------

   procedure Parse_Context_Clause
     (Context_Clause    : out Project_Node_Id;
      Project_Directory : Name_Id);
   --  Parse the context clause of a project
   --  Does nothing if there is b\no context clause (if the current
   --  token is not "with").

   procedure Parse_Single_Project
     (Project         : out Project_Node_Id;
      Path_Name       : String;
      Modified        : Boolean);
   --  Parse a project file.
   --  Recursive procedure: it calls itself for imported and
   --  modified projects.

   function Path_Name_Of
     (File_Name : String;
      Directory : String)
      return      String;
   --  Returns the path name of a (non project) file.
   --  Returns an empty string if file cannot be found.

   function Project_Path_Name_Of
     (Project_File_Name : String;
      Directory         : String)
      return              String;
   --  Returns the path name of a project file.
   --  Returns an empty string if project file cannot be found.

   function Immediate_Directory_Of (Path_Name : Name_Id) return Name_Id;
   --  Get the directory of the file with the specified path name.
   --  This includes the directory separator as the last character.
   --  Returns "./" if Path_Name contains no directory separator.

   function Simple_File_Name_Of (Path_Name : Name_Id) return Name_Id;
   --  Returns the name of a file with the specified path name
   --  with no directory information.

   function Project_Name_From (Path_Name : String) return Name_Id;
   --  Returns the name of the project that corresponds to its path name.
   --  Returns No_Name if the path name is invalid, because the corresponding
   --  project name does not have the syntax of an ada identifier.

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
            --  Remove from name all characters after the last
            --  directory separator.
            Name_Len := Index;
            return Name_Find;
         end if;
      end loop;

      --  There is no directory separator in name. Return "./".
      Name_Len := 2;
      Name_Buffer (1 .. 2) := "./";
      return Name_Find;

   end Immediate_Directory_Of;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Project           : out Project_Node_Id;
      Project_File_Name : String)
   is
      Current_Directory : constant String := Get_Current_Dir;

   begin

      --
      Project := Empty_Node;

      if Current_Verbosity >= Medium then
         Write_Str ("ADA_PROJECT_PATH=""");
         Write_Str (Project_Path.all);
         Write_Line ("""");
      end if;

      declare
         Path_Name : constant String :=
           Project_Path_Name_Of (Project_File_Name,
                                 Directory   => Current_Directory);

      begin
         --  Initialize the tables

         Project_Nodes.Set_Last (Empty_Node);
         Projects_Htable.Reset;

         Errout.Initialize;

         --  And parse the main project file

         if Path_Name = "" then
            Fail ("project file """ & Project_File_Name & """ not found");
         end if;

         Parse_Single_Project
           (Project         => Project,
            Path_Name       => Path_Name,
            Modified        => False);

         if Project /= Empty_Node then
            if Errout.Errors_Detected > 0 then
               Project := Empty_Node;
            end if;
            Errout.Finalize;
         end if;

      end;

   exception
      when X : others =>

         --  Internal error
         Write_Line (Exception_Information (X));
         Write_Str  ("Exception ");
         Write_Str  (Exception_Name (X));
         Write_Line (" raised, while processing project file");
         Project := Empty_Node;
   end Parse;

   --------------------------
   -- Parse_Context_Clause --
   --------------------------

   procedure Parse_Context_Clause
     (Context_Clause    : out Project_Node_Id;
      Project_Directory : Name_Id) is
      Project_Directory_Path : constant String :=
        Get_Name_String (Project_Directory);
      Current_With_Clause : Project_Node_Id := Empty_Node;
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_With_Clause);
   begin

      --  Assume no context clause
      Context_Clause := Empty_Node;
      With_Loop :

      --  if Token is not "with", there is no context clause,
      --  or we have exhausted the with clauses.
      while Token = Tok_With loop

         Comma_Loop :
         loop
            --  Scan past "with" or ","
            Scan;
            Expect (Tok_String_Literal, "literal string");

            if Token /= Tok_String_Literal then
               return;
            end if;

            --  new with clause
            Project_Nodes.Increment_Last;

            if Current_With_Clause = Empty_Node then
               --  first with clause of the context clause
               Current_With_Clause := Project_Nodes.Last;
               Context_Clause := Current_With_Clause;

            else
               Data.Field2 := Project_Nodes.Last;
               Project_Nodes.Table (Current_With_Clause) := Data;
               Current_With_Clause := Project_Nodes.Last;
               Data := Default_Project_Node (Of_Kind => N_With_Clause);
            end if;

            Data.Value := Strval (Token_Node);
            Data.Location := Token_Ptr;
            String_To_Name_Buffer (Data.Value);

            declare
               Original_Path : constant String :=
                 Name_Buffer (1 .. Name_Len);
               Imported_Path_Name : constant String :=
                 Project_Path_Name_Of
                 (Original_Path, Project_Directory_Path);

            begin
               if Imported_Path_Name = "" then
                  --  The project file cannot be found
                  Error_Msg ("unknown project file", Token_Ptr);

               else
                  --  Parse the imported project
                  Parse_Single_Project
                    (Project   => Data.Field1,
                     Path_Name => Imported_Path_Name,
                     Modified  => False);

                  if Data.Field1 /= Empty_Node then
                     --  If parsing was successful, record project name
                     --  and path name in with clause
                     Data.Name := Project_Nodes.Table (Data.Field1).Name;
                     Name_Len := Imported_Path_Name'Length;
                     Name_Buffer (1 .. Name_Len) := Imported_Path_Name;
                     Data.Path_Name := Name_Find;
                  end if;
               end if;
            end;

            Scan;
            if Token = Tok_Semicolon then
               --  End of (possibly multiple) with clause;
               --  Scan past the semicolon.
               Scan;
               exit Comma_Loop;

            elsif Token /= Tok_Comma then
               Error_Msg ("expected comma or semi colon", Token_Ptr);
               exit Comma_Loop;
            end if;
         end loop Comma_Loop;
      end loop With_Loop;

      if Current_With_Clause /= Empty_Node then
         Project_Nodes.Table (Current_With_Clause) := Data;
      end if;

   end Parse_Context_Clause;

   --------------------------
   -- Parse_Single_Project --
   --------------------------

   procedure Parse_Single_Project
     (Project         : out Project_Node_Id;
      Path_Name       : String;
      Modified        : Boolean)
   is
      Canonical_Path_Name : Name_Id;
      Data                : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Project);
      Project_Directory   : Name_Id;
      Project_Scan_State  : Saved_Project_Scan_State;
      Source_Index        : Source_File_Index;

      Modified_Project    : Project_Node_Id := Empty_Node;

      A_Project_Name_And_Node : Project_Name_And_Node :=
        Projects_Htable.Get_First;

      Name_From_Path : constant Name_Id := Project_Name_From (Path_Name);

   begin
      Name_Len := Path_Name'Length;
      Name_Buffer (1 .. Name_Len) := Path_Name;
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      Canonical_Path_Name := Name_Find;

      --  Check if the project file has already been parsed.

      while A_Project_Name_And_Node /= No_Project_Name_And_Node loop

         if Project_Nodes.Table (A_Project_Name_And_Node.Node).Path_Name =
           Canonical_Path_Name then

            if Modified then

               if A_Project_Name_And_Node.Modified then
                  Error_Msg
                    ("cannot modify several times the same project file",
                     Token_Ptr);

               else
                  Error_Msg
                    ("cannot modify an imported project file",
                     Token_Ptr);
               end if;

            elsif A_Project_Name_And_Node.Modified then
               Error_Msg
                 ("cannot imported a modified project file",
                  Token_Ptr);
            end if;

            Project := A_Project_Name_And_Node.Node;
            return;
         end if;

         A_Project_Name_And_Node := Projects_Htable.Get_Next;
      end loop;

      --  We never encountered this project file
      --  Save the scan state, load the project file and start to scan it.

      Save_Project_Scan_State (Project_Scan_State);
      Source_Index := Load_Project_File (Path_Name);

      --  if we cannot find it, we stop

      if Source_Index = No_Source_File then
         Project := Empty_Node;
         return;
      end if;

      Initialize_Scanner (Types.No_Unit, Source_Index);

      if Name_From_Path = No_Name then
         --  The project file name is not correct (no or bad extension,
         --  or not following Ada identifier's syntax).
         Error_Msg_Name_1 := Canonical_Path_Name;
         Error_Msg ("?{ is not a valid path name for a project file",
                    Token_Ptr);
      end if;

      if Current_Verbosity >= Medium then
         Write_Str  ("Parsing """);
         Write_Str  (Path_Name);
         Write_Char ('"');
         Write_Eol;
      end if;

      Project_Directory := Immediate_Directory_Of (Canonical_Path_Name);
      Data.Directory    := Project_Directory;

      Project_Nodes.Increment_Last;
      Project        := Project_Nodes.Last;
      Data.Name      := Simple_File_Name_Of (Canonical_Path_Name);
      Data.Path_Name := Canonical_Path_Name;
      Data.Location  := Token_Ptr;

      --  Is there any imported project?

      Parse_Context_Clause (Context_Clause    => Data.Field1,
                            Project_Directory => Project_Directory);

      Expect (Tok_Project, "project");

      --  Scan past "project"

      if Token = Tok_Project then
         Data.Location := Token_Ptr;
         Scan;
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Data.Name := Token_Name;

         Get_Name_String (Data.Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

         declare
            Expected_Name : constant Name_Id := Name_Find;

         begin
            if Name_From_Path /= No_Name
              and then Expected_Name /= Name_From_Path
            then
               --  The project name is not the one that was expected from
               --  the file name. Report a warning.
               Error_Msg_Name_1 := Expected_Name;
               Error_Msg ("?file name does not match unit name, " &
                          "should be `{" & Project_File_Extension & "`",
                          Token_Ptr);
            end if;

         end;

         declare
            Project_Name : Name_Id := Projects_Htable.Get_First.Name;

         begin
            --  Check if we already have a project with this name.
            while Project_Name /= No_Name
              and then
              Project_Name /= Data.Name
            loop
               Project_Name := Projects_Htable.Get_Next.Name;
            end loop;

            if Project_Name /= No_Name then
               Error_Msg ("duplicate project name", Token_Ptr);

            else
               Projects_Htable.Set (K => Data.Name,
                                    E => (Name     => Data.Name,
                                          Node     => Project,
                                          Modified => Modified));
            end if;
         end;

         --  Scan past the project name

         Scan;

      end if;

      if Token = Tok_Modifying then

         --  We are modifying another project

         --  Scan past "modifying"

         Scan;

         Expect (Tok_String_Literal, "literal string");

         if Token = Tok_String_Literal then
            Data.Value := Strval (Token_Node);
            String_To_Name_Buffer (Data.Value);

            declare
               Original_Path_Name : constant String :=
                 Name_Buffer (1 .. Name_Len);
               Modified_Project_Path_Name : constant String :=
                 Project_Path_Name_Of
                  (Original_Path_Name,
                   Get_Name_String (Project_Directory));

            begin
               if Modified_Project_Path_Name = "" then
                  --  We could not find the project file to modify
                  Error_Msg ("unknown project file", Token_Ptr);

               else
                  Parse_Single_Project
                    (Project   => Modified_Project,
                     Path_Name => Modified_Project_Path_Name,
                     Modified  => True);
               end if;
            end;

            --  Scan past the modified project path
            Scan;
         end if;

      end if;

      Expect (Tok_Is, "is");

      --  We save the project data in the project table

      Project_Nodes.Table (Project) := Data;

      --  No need to Scan past "is", Prj.Dect.Parse will do it.
      Prj.Dect.Parse
        (Declarations    => Project_Nodes.Table (Project).Field2,
         Current_Project => Project,
         Modifying       => Modified_Project);

      Expect (Tok_End, "end");

      --  Scan past "end"

      if Token = Tok_End then
         Scan;
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         --  We check if this is the project name
         if To_Lower (Get_Name_String (Token_Name)) /=
                                              Get_Name_String (Data.Name)
         then
            Error_Msg ("Expected """ & Get_Name_String (Data.Name) & """",
                       Token_Ptr);
         end if;
      end if;

      if Token /= Tok_Semicolon then
         Scan;
      end if;

      Expect (Tok_Semicolon, ";");

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

   -----------------------
   -- Project_Name_From --
   -----------------------

   function Project_Name_From (Path_Name : String) return Name_Id is
      Canonical : String (1 .. Path_Name'Length) := Path_Name;
      First : Natural  := Canonical'Last;
      Last  : Positive := First;

   begin
      if First = 0 then
         return No_Name;
      end if;

      Canonical_Case_File_Name (Canonical);

      while First > 0
        and then
        Canonical (First) /= '.'
      loop
         First := First - 1;
      end loop;

      if Canonical (First) = '.' then
         if Canonical (First .. Last) = Project_File_Extension
           and then
           First /= 1
         then
            First := First - 1;
            Last := First;

            while First > 0
              and then
              Canonical (First) /= '/'
            loop
               First := First - 1;
            end loop;

         else
            return No_Name;
         end if;

      else
         return No_Name;
      end if;

      if Canonical (First) = '/' then
         First := First + 1;
      end if;

      Name_Len := Last - First + 1;
      Name_Buffer (1 .. Name_Len) := To_Lower (Canonical (First .. Last));

      if not Is_Letter (Name_Buffer (1)) then
         return No_Name;

      else
         for Index in 2 .. Name_Len - 1 loop
            if Name_Buffer (Index) = '_' then
               if Name_Buffer (Index + 1) = '_' then
                  return No_Name;
               end if;

            elsif not Is_Alphanumeric (Name_Buffer (Index)) then
               return No_Name;
            end if;

         end loop;

         if not Is_Alphanumeric (Name_Buffer (Name_Len)) then
            return No_Name;

         else
            return Name_Find;
         end if;

      end if;

   end Project_Name_From;

   --------------------------
   -- Project_Path_Name_Of --
   --------------------------

   function Project_Path_Name_Of
     (Project_File_Name : String;
      Directory         : String)
      return        String
   is
      Result : String_Access;

   begin

      --  First we try <file_name>.<extension>

      if Current_Verbosity = High then
         Write_Str  ("Project_Path_Name_Of (""");
         Write_Str  (Project_File_Name);
         Write_Str  (""", """);
         Write_Str  (Directory);
         Write_Line (""");");
         Write_Str  ("   Trying ");
         Write_Str (Project_File_Name);
         Write_Line (Project_File_Extension);
      end if;

      Result :=
        Locate_Regular_File
          (File_Name => Project_File_Name & Project_File_Extension,
           Path      => Project_Path.all);

      --  Then we try <file_name>

      if Result = null then
         if Current_Verbosity = High then
            Write_Str  ("   Trying ");
            Write_Line  (Project_File_Name);
         end if;

         Result :=
           Locate_Regular_File
           (File_Name => Project_File_Name,
            Path      => Project_Path.all);

         --  The we try <directory>/<file_name>.<extension>

         if Result = null then
            if Current_Verbosity = High then
               Write_Str  ("   Trying ");
               Write_Str  (Directory);
               Write_Str (Project_File_Name);
               Write_Line (Project_File_Extension);
            end if;

            Result :=
              Locate_Regular_File
              (File_Name => Directory & Project_File_Name &
                            Project_File_Extension,
               Path      => Project_Path.all);

            --  Then we try <directory>/<file_name>

            if Result = null then
               if Current_Verbosity = High then
                  Write_Str  ("   Trying ");
                  Write_Str  (Directory);
                  Write_Line  (Project_File_Name);
               end if;

               Result :=
                 Locate_Regular_File
                 (File_Name => Directory & Project_File_Name,
                  Path      => Project_Path.all);
            end if;
         end if;
      end if;

      --  If we cannot find the project file, we return an empty string

      if Result = null then
         return "";

      else
         Canonical_Case_File_Name (Result.all);
         return Result.all;
      end if;

   end Project_Path_Name_Of;

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

   if Prj_Path.all = "" then
      Project_Path := new String'(".");

   else
      Project_Path := new String'("." & Path_Separator & Prj_Path.all);
   end if;

end Prj.Part;
