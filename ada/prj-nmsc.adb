------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 2000-2001 Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Errout;                  use Errout;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Namet;                   use Namet;
with Osint;                   use Osint;
with Output;                  use Output;
with Prj.Com;                 use Prj.Com;
with Prj.Util;                use Prj.Util;
with Snames;                  use Snames;
with Stringt;                 use Stringt;
with Types;                   use Types;

package body Prj.Nmsc is

   procedure Check_Naming_Scheme (Naming : Naming_Data);
   --  Check that the package Naming is correct.

   procedure Check_Naming_Scheme
     (Name : Name_Id;
      Unit : out Name_Id);
   --  Check that a name is a valid unit name.

   function Get_Name_String (S : String_Id) return String;
   --  Get the string from a String_Id

   procedure Get_Unit
     (File_Name    : Name_Id;
      Naming       : Naming_Data;
      Unit_Name    : out Name_Id;
      Unit_Kind    : out Spec_Or_Body;
      Needs_Pragma : out Boolean);
   --  Find out, from a file name, the unit name, the unit kind
   --  and if a specific SFN pragma is needed.
   --  If the file name corresponds to no unit, then Unit_Name
   --  will be No_Name.

   function Is_Illegal_Append (This : String) return Boolean;
   --  Returns True if the string This cannot be used as
   --  a Specification_Append, a Body_Append or a Separate_Append.

   procedure Record_Source
     (File_Name        : Name_Id;
      Path_Name        : Name_Id;
      Project          : Project_Id;
      Data             : in out Project_Data;
      Error_If_Invalid : Boolean;
      Location         : Source_Ptr;
      Current_Source   : in out String_List_Id);
   --  Put a unit in the list of units of a project, if the file name
   --  corresponds to a valid unit name.
   --  If it does not correspond to a valid unit name, report an error
   --  only if Error_If_Invalid is true.

   procedure Show_Source_Dirs (Project : Project_Id);
   --  List all the source directories of a project.

   function Locate_Directory
     (Name   : Name_Id;
      Parent : Name_Id)
     return   Name_Id;
   --  Locate a directory.
   --  Returns No_Name if directory does not exist.

   function Path_Name_Of
     (File_Name : String_Id;
      Directory : Name_Id)
     return      String;
   --  Returns the path name of a (non project) file.
   --  Returns an empty string if file cannot be found.

   function Path_Name_Of
     (File_Name : String_Id;
      Directory : String_Id)
     return      String;
   --  Same as above except that Directory is a String_Id instead
   --  of a Name_Id.

   -------------------------
   -- Check_Naming_Scheme --
   -------------------------

   procedure Check_Naming_Scheme (Naming : Naming_Data) is
   begin
      --  Only check if we are not using the standard naming scheme

      if Naming /= Standard_Naming_Data then
         declare
            Dot_Replacement      : constant String :=
                                     Get_Name_String
                                       (Naming.Dot_Replacement);
            Specification_Append : constant String :=
                                     Get_Name_String
                                       (Naming.Specification_Append);
            Body_Append          : constant String :=
                                     Get_Name_String
                                       (Naming.Body_Append);
            Separate_Append      : constant String :=
                                     Get_Name_String
                                       (Naming.Separate_Append);

         begin
            --  Dot_Replacement cannot
            --   - be empty
            --   - start or end with an alphanumeric
            --   - be a single '_'
            --   - start with an '_' followed by an alphanumeric
            --   - contain a '.' except if it is "."

            if Dot_Replacement'Length = 0
              or else Is_Alphanumeric
                        (Dot_Replacement (Dot_Replacement'First))
              or else Is_Alphanumeric
                        (Dot_Replacement (Dot_Replacement'Last))
              or else (Dot_Replacement (Dot_Replacement'First) = '_'
                        and then
                        (Dot_Replacement'Length = 1
                          or else
                           Is_Alphanumeric
                             (Dot_Replacement (Dot_Replacement'First + 1))))
              or else (Dot_Replacement'Length > 1
                         and then
                           Index (Source => Dot_Replacement,
                                  Pattern => ".") /= 0)
            then
               Error_Msg
                 ('"' & Dot_Replacement &
                  """ is illegal for Dot_Replacement.",
                  Naming.Dot_Repl_Loc);
            end if;

            --  Appends cannot
            --   - be empty
            --   - start with an alphanumeric
            --   - start with an '_' followed by an alphanumeric

            if Is_Illegal_Append (Specification_Append) then
               Error_Msg
                 ('"' & Specification_Append &
                  """ is illegal for Specification_Append.",
                  Naming.Spec_Append_Loc);
            end if;

            if Is_Illegal_Append (Body_Append) then
               Error_Msg
                 ('"' & Body_Append &
                  """ is illegal for Body_Append.",
                  Naming.Body_Append_Loc);
            end if;

            if Body_Append /= Separate_Append then
               if Is_Illegal_Append (Separate_Append) then
                  Error_Msg
                    ('"' & Separate_Append &
                     """ is illegal for Separate_Append.",
                     Naming.Sep_Append_Loc);
               end if;
            end if;

            --  Specification_Append cannot have the same termination as
            --  Body_Append or Separate_Append

            if Specification_Append'Length >= Body_Append'Length
              and then
                Body_Append (Body_Append'Last -
                             Specification_Append'Length + 1 ..
                             Body_Append'Last) = Specification_Append
            then
               Error_Msg
                 ("Body_Append (""" &
                  Body_Append &
                  """) cannot end with" &
                  " Specification_Append (""" &
                  Specification_Append & """).",
                  Naming.Body_Append_Loc);
            end if;

            if Specification_Append'Length >= Separate_Append'Length
              and then
                Separate_Append
                  (Separate_Append'Last - Specification_Append'Length + 1
                    ..
                   Separate_Append'Last) = Specification_Append
            then
               Error_Msg
                 ("Separate_Append (""" &
                  Separate_Append &
                  """) cannot end with" &
                  " Specification_Append (""" &
                  Specification_Append & """).",
                  Naming.Sep_Append_Loc);
            end if;
         end;
      end if;
   end Check_Naming_Scheme;

   procedure Check_Naming_Scheme
     (Name : Name_Id;
      Unit : out Name_Id)
   is
      The_Name        : String := Get_Name_String (Name);
      Need_Letter     : Boolean := True;
      Last_Underscore : Boolean := False;
      OK              : Boolean := The_Name'Length > 0;

   begin
      for Index in The_Name'Range loop
         if Need_Letter then

            --  We need a letter (at the beginning, and following a dot),
            --  but we don't have one.

            if Is_Letter (The_Name (Index)) then
               Need_Letter := False;

            else
               OK := False;

               if Current_Verbosity = High then
                  Write_Int  (Types.Int (Index));
                  Write_Str  (": '");
                  Write_Char (The_Name (Index));
                  Write_Line ("' is not a letter.");
               end if;

               exit;
            end if;

         elsif Last_Underscore
           and then (The_Name (Index) = '_' or else The_Name (Index) = '.')
         then
            --  Two underscores are illegal, and a dot cannot follow
            --  an underscore.

            OK := False;

            if Current_Verbosity = High then
               Write_Int  (Types.Int (Index));
               Write_Str  (": '");
               Write_Char (The_Name (Index));
               Write_Line ("' is illegal here.");
            end if;

            exit;

         elsif The_Name (Index) = '.' then

            --  We need a letter after a dot

            Need_Letter := True;

         elsif The_Name (Index) = '_' then
            Last_Underscore := True;

         else
            --  We need an letter or a digit

            Last_Underscore := False;

            if not Is_Alphanumeric (The_Name (Index)) then
               OK := False;

               if Current_Verbosity = High then
                  Write_Int  (Types.Int (Index));
                  Write_Str  (": '");
                  Write_Char (The_Name (Index));
                  Write_Line ("' is not alphanumeric.");
               end if;

               exit;
            end if;
         end if;
      end loop;

      --  We cannot end with an underscore or a dot

      OK := OK and then not Need_Letter and then not Last_Underscore;

      if OK then
         Unit := Name;
      else
         --  We signal a problem with No_Name

         Unit := No_Name;
      end if;
   end Check_Naming_Scheme;

   procedure Check_Naming_Scheme (Project : Project_Id) is
      Last_Source_Dir   : String_List_Id  := Nil_String;
      Data              : Project_Data    := Projects.Table (Project);

      procedure Check_Unit_Names (List : Array_Element_Id);
      --  Check that a list of unit names contains only valid names.

      procedure Find_Source_Dirs (From : String_Id; Location : Source_Ptr);
      --  Find one or several source directories, and add them
      --  to the list of source directories of the project.

      procedure Find_Sources;
      --  Find all the sources in all of the source directories
      --  of a project.

      procedure Get_Path_Name_And_Record_Source
        (File_Name        : String;
         Location         : Source_Ptr;
         Current_Source   : in out String_List_Id);
      --  Find the path name of a source in the source directories and
      --  record the source, if found.

      procedure Get_Sources_From_File
        (Path     : String;
         Location : Source_Ptr);
      --  Get the sources of a project from a text file

      ----------------------
      -- Check_Unit_Names --
      ----------------------

      procedure Check_Unit_Names (List : Array_Element_Id) is
         Current   : Array_Element_Id := List;
         Element   : Array_Element;
         Unit_Name : Name_Id;

      begin
         --  Loop through elements of the string list

         while Current /= No_Array_Element loop
            Element := Array_Elements.Table (Current);

            --  Check that it contains a valid unit name

            Check_Naming_Scheme (Element.Index, Unit_Name);

            if Unit_Name = No_Name then
               Error_Msg_Name_1 := Element.Index;
               Error_Msg
                 ("{ is not a valid unit name.",
                  Element.Value.Location);

            else
               Element.Index := Unit_Name;
               Array_Elements.Table (Current) := Element;
            end if;

            Current := Element.Next;
         end loop;
      end Check_Unit_Names;

      ----------------------
      -- Find_Source_Dirs --
      ----------------------

      procedure Find_Source_Dirs (From : String_Id; Location : Source_Ptr) is

         Directory    : String (1 .. Integer (String_Length (From)));
         Directory_Id : Name_Id;
         Element      : String_Element;

         procedure Recursive_Find_Dirs (Path : String_Id);
         --  Find all the subdirectories (recursively) of Path
         --  and add them to the list of source directories
         --  of the project.

         -------------------------
         -- Recursive_Find_Dirs --
         -------------------------

         procedure Recursive_Find_Dirs (Path : String_Id) is
            Dir     : Dir_Type;
            Name    : String (1 .. 250);
            Last    : Natural;
            The_Path : String := Get_Name_String (Path);

         begin
            if Current_Verbosity = High then
               Write_Str  ("   ");
               Write_Line (The_Path);
            end if;

            String_Elements.Increment_Last;
            Element :=
              (Value    => Path,
               Location => No_Location,
               Next     => Nil_String);

            --  Case of first source directory

            if Last_Source_Dir = Nil_String then
               Data.Source_Dirs := String_Elements.Last;

            --  Here we already have source directories.

            else
               --  Link the previous last to the new one

               String_Elements.Table (Last_Source_Dir).Next :=
                 String_Elements.Last;
            end if;

            --  And register this source directory as the new last

            Last_Source_Dir  := String_Elements.Last;
            String_Elements.Table (Last_Source_Dir) := Element;

            --  Now look for subdirectories

            Open (Dir, The_Path);

            loop
               Read (Dir, Name, Last);
               exit when Last = 0;

               if Current_Verbosity = High then
                  Write_Str  ("   Checking ");
                  Write_Line (Name (1 .. Last));
               end if;

               if Name (1 .. Last) /= "."
                 and then Name (1 .. Last) /= ".."
               then
                  --  Avoid . and ..

                  declare
                     Path_Name : constant String :=
                                   The_Path &
                                   Directory_Separator &
                                   Name (1 .. Last);

                  begin
                     if Is_Directory (Path_Name) then

                        --  We have found a new subdirectory,
                        --  register it and find its own subdirectories.

                        Start_String;
                        Store_String_Chars (Path_Name);
                        Recursive_Find_Dirs (End_String);
                     end if;
                  end;
               end if;
            end loop;

            Close (Dir);

         exception
            when Directory_Error =>
               null;
         end Recursive_Find_Dirs;

         --  Start of processing for Find_Source_Dirs

      begin
         if Current_Verbosity = High then
            Write_Str ("Find_Source_Dirs (""");
         end if;

         String_To_Name_Buffer (From);
         Directory    := Name_Buffer (1 .. Name_Len);
         Directory_Id := Name_Find;

         if Current_Verbosity = High then
            Write_Str (Directory);
            Write_Line (""")");
         end if;

         --  First, check if we are looking for a directory tree,
         --  indicated by "/**" at the end.

         if Directory'Length >= 3
           and then Directory (Directory'Last - 1 .. Directory'Last) = "**"
           and then (Directory (Directory'Last - 2) = '/'
                       or else
                     Directory (Directory'Last - 2) = Directory_Separator)
         then
            Name_Len := Directory'Length - 3;

            if Name_Len = 0 then
               --  This is the case of "/**": all directories
               --  in the file system.

               Name_Len := 1;
               Name_Buffer (1) := Directory (Directory'First);

            else
               Name_Buffer (1 .. Name_Len) :=
                 Directory (Directory'First .. Directory'Last - 2);
            end if;

            if Current_Verbosity = High then
               Write_Str ("Looking for all subdirectories of """);
               Write_Str (Name_Buffer (1 .. Name_Len));
               Write_Line ("""");
            end if;

            declare
               Base_Dir : constant Name_Id := Name_Find;
               Root     : constant Name_Id :=
                            Locate_Directory (Base_Dir, Data.Directory);

            begin
               if Root = No_Name then
                  Error_Msg_Name_1 := Base_Dir;
                  if Location = No_Location then
                     Error_Msg_S ("{ is not a valid directory.");
                  else
                     Error_Msg   ("{ is not a valid directory.", Location);
                  end if;

               else
                  --  We have an existing directory,
                  --  we register it and all of its subdirectories.

                  if Current_Verbosity = High then
                     Write_Line ("Looking for source directories:");
                  end if;

                  Start_String;
                  Store_String_Chars (Get_Name_String (Root));
                  Recursive_Find_Dirs (End_String);

                  if Current_Verbosity = High then
                     Write_Line ("End of looking for source directories.");
                  end if;
               end if;
            end;

         --  We have a single directory

         else
            declare
               Path_Name : constant Name_Id :=
                 Locate_Directory (Directory_Id, Data.Directory);

            begin
               if Path_Name = No_Name then
                  Error_Msg_Name_1 := Directory_Id;
                  if Location = No_Location then
                     Error_Msg_S ("{ is not a valid directory");
                  else
                     Error_Msg   ("{ is not a valid directory", Location);
                  end if;
               else

                  --  As it is an existing directory, we add it to
                  --  the list of directories.

                  String_Elements.Increment_Last;
                  Start_String;
                  Store_String_Chars (Get_Name_String (Path_Name));
                  Element.Value := End_String;

                  if Last_Source_Dir = Nil_String then

                     --  This is the first source directory

                     Data.Source_Dirs := String_Elements.Last;

                  else
                     --  We already have source directories,
                     --  link the previous last to the new one.

                     String_Elements.Table (Last_Source_Dir).Next :=
                       String_Elements.Last;
                  end if;

                  --  And register this source directory as the new last

                  Last_Source_Dir := String_Elements.Last;
                  String_Elements.Table (Last_Source_Dir) := Element;
               end if;
            end;
         end if;
      end Find_Source_Dirs;

      ------------------
      -- Find_Sources --
      ------------------

      procedure Find_Sources is
         Source_Dir     : String_List_Id := Data.Source_Dirs;
         Element        : String_Element;
         Dir            : Dir_Type;
         Current_Source : String_List_Id := Nil_String;

      begin
         if Current_Verbosity = High then
            Write_Line ("Looking for sources:");
         end if;

         --  For each subdirectory

         while Source_Dir /= Nil_String loop
            begin
               Element := String_Elements.Table (Source_Dir);
               if Element.Value /= No_String then
                  declare
                     Source_Directory : String
                       (1 .. Integer (String_Length (Element.Value)));
                  begin
                     String_To_Name_Buffer (Element.Value);
                     Source_Directory := Name_Buffer (1 .. Name_Len);
                     if Current_Verbosity = High then
                        Write_Str ("Source_Dir = ");
                        Write_Line (Source_Directory);
                     end if;

                     --  We look to every entry in the source directory

                     Open (Dir, Source_Directory);

                     loop
                        Read (Dir, Name_Buffer, Name_Len);

                        if Current_Verbosity = High then
                           Write_Str  ("   Checking ");
                           Write_Line (Name_Buffer (1 .. Name_Len));
                        end if;

                        exit when Name_Len = 0;

                        declare

                           Path_Access : constant
                             GNAT.OS_Lib.String_Access :=
                             Locate_Regular_File
                             (Name_Buffer (1 .. Name_Len),
                              Source_Directory);
                           File_Name : Name_Id;
                           Path_Name : Name_Id;

                        begin

                           --  If it is a regular file

                           if Path_Access /= null then
                              File_Name := Name_Find;
                              Name_Len := Path_Access'Length;
                              Name_Buffer (1 .. Name_Len) := Path_Access.all;
                              Path_Name := Name_Find;

                              --  We attempt to register it as a source.
                              --  However, there is no error if the file
                              --  does not contain a valid source
                              --  (as indicated by Error_If_Invalid => False).
                              --  But there is an error if we have a duplicate
                              --  unit name.

                              Record_Source
                                (File_Name        => File_Name,
                                 Path_Name        => Path_Name,
                                 Project          => Project,
                                 Data             => Data,
                                 Error_If_Invalid => False,
                                 Location         => No_Location,
                                 Current_Source   => Current_Source);

                           else
                              if Current_Verbosity = High then
                                 Write_Line
                                   ("      Not a regular file.");
                              end if;
                           end if;
                        end;
                     end loop;

                     Close (Dir);
                  end;
               end if;

            exception
               when Directory_Error =>
                  null;
            end;

            Source_Dir := Element.Next;
         end loop;

         if Current_Verbosity = High then
            Write_Line ("end Looking for sources.");
         end if;

         --  If we have looked for sources and found none, then
         --  it is an error. If a project is not supposed to contain
         --  any source, then we never call Find_Sources.

         if Current_Source = Nil_String then
            Error_Msg_S ("there are no sources in this project");
         end if;
      end Find_Sources;

      -------------------------------------
      -- Get_Path_Name_And_Record_Source --
      -------------------------------------

      procedure Get_Path_Name_And_Record_Source
        (File_Name        : String;
         Location         : Source_Ptr;
         Current_Source   : in out String_List_Id) is

         Source_Dir : String_List_Id := Data.Source_Dirs;
         Element    : String_Element;
         Path_Name  : GNAT.OS_Lib.String_Access;
         Found      : Boolean := False;

         File       : Name_Id;
      begin
         if Current_Verbosity = High then
            Write_Str  ("   Checking """);
            Write_Str  (File_Name);
            Write_Line (""".");
         end if;

         --  We look in all source directories for this file name

         while Source_Dir /= Nil_String loop
            Element := String_Elements.Table (Source_Dir);
            if Current_Verbosity = High then
               Write_Str ("      """);
               Write_Str (Get_Name_String (Element.Value));
               Write_Str (""": ");
            end if;

            Path_Name :=
              Locate_Regular_File
              (File_Name,
               Get_Name_String (Element.Value));

            if Path_Name /= null then
               if Current_Verbosity = High then
                  Write_Line ("OK");
               end if;

               Name_Len := File_Name'Length;
               Name_Buffer (1 .. Name_Len) := File_Name;
               File := Name_Find;
               Name_Len := Path_Name'Length;
               Name_Buffer (1 .. Name_Len) := Path_Name.all;

               --  We register the source.
               --  We report an error if the file does not
               --  correspond to a source.

               Record_Source
                 (File_Name        => File,
                  Path_Name        => Name_Find,
                  Project          => Project,
                  Data             => Data,
                  Error_If_Invalid => True,
                  Location         => Location,
                  Current_Source   => Current_Source);
               Found := True;
               exit;

            else
               if Current_Verbosity = High then
                  Write_Line ("No");
               end if;

               Source_Dir := Element.Next;
            end if;
         end loop;

         if not Found then
            Name_Len := File_Name'Length;
            Name_Buffer (1 .. Name_Len) := File_Name;
            Error_Msg_Name_1 := Name_Find;
            Error_Msg
              ("cannot find source {", Location);
         end if;
      end Get_Path_Name_And_Record_Source;

      ---------------------------
      -- Get_Sources_From_File --
      ---------------------------

      procedure Get_Sources_From_File
        (Path     : String;
         Location : Source_Ptr)
      is
         File           : Prj.Util.Text_File;
         Line           : String (1 .. 250);
         Last           : Natural;
         Current_Source : String_List_Id := Nil_String;

         Nmb_Errors : constant Nat := Errors_Detected;

      begin
         if Current_Verbosity = High then
            Write_Str  ("Opening """);
            Write_Str  (Path);
            Write_Line (""".");
         end if;

         --  We open the file

         Prj.Util.Open
           (File, Path);

         if not Prj.Util.Is_Valid (File) then
            Error_Msg ("file does not exist", Location);
         else
            while not Prj.Util.End_Of_File (File) loop
               Prj.Util.Get_Line (File, Line, Last);

               --  If the line is not empty and does not start with "--",
               --  then it must contains a file name.

               if Last /= 0
                 and then
                 (Last = 1 or else Line (1 .. 2) /= "--")
               then
                  Get_Path_Name_And_Record_Source
                    (File_Name => Line (1 .. Last),
                     Location => Location,
                     Current_Source => Current_Source);
                  exit when Nmb_Errors /= Errors_Detected;
               end if;
            end loop;

            Prj.Util.Close (File);

         end if;

         --  We should have found at least one source.
         --  If not, report an error.

         if Current_Source = Nil_String then
            Error_Msg ("this project has no source", Location);
         end if;
      end Get_Sources_From_File;

      --  Start of processing for Check_Naming_Scheme

   begin

      if Current_Verbosity = High then
         Write_Line ("Starting to look for directories");
      end if;

      --  Let's check the object directory

      declare
         Object_Dir : Variable_Value :=
           Util.Value_Of (Name_Object_Dir, Data.Decl.Variables);

      begin
         --  We set the object directory to its default

         if Current_Verbosity = High then
            Write_Line ("Starting to look for object directory");
         end if;

         Data.Object_Directory := Data.Directory;

         case Object_Dir.Kind is
            when Undefined =>

               --  Object_Dir is not specified. Nothing to do,
               --  because the object directory is already defaulted.

               null;

            when List =>

               --  It is an error for Object_Dir to be a string list

               Error_Msg_S ("Object_Dir cannot be a list.");

            when Single =>

               --  We check that the specified object directory
               --  does exist.

               String_To_Name_Buffer (Object_Dir.Value);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               declare
                  Dir_Id : constant Name_Id := Name_Find;
               begin
                  Data.Object_Directory :=
                    Locate_Directory (Dir_Id, Data.Directory);

                  if Data.Object_Directory = No_Name then
                     Error_Msg_Name_1 := Dir_Id;
                     Error_Msg_S
                       ("the object directory { cannot be found");
                  end if;
               end;
         end case;

         if Current_Verbosity = High then
            if Data.Object_Directory = No_Name then
               Write_Line ("No object directory");
            else
               Write_Str ("Object directory: """);
               Write_Str (Get_Name_String (Data.Object_Directory));
               Write_Line ("""");
            end if;
         end if;
      end;

      --  Let's check the source directories

      declare
         Source_Dirs : Variable_Value :=
           Util.Value_Of (Name_Source_Dirs, Data.Decl.Variables);

      begin

         if Current_Verbosity = High then
            Write_Line ("Starting to look for source directories");
         end if;

         case Source_Dirs.Kind is
            when Undefined =>

               --  No Source_Dirs specified: the single source directory
               --  is the one containing the project file

               String_Elements.Increment_Last;
               Data.Source_Dirs := String_Elements.Last;
               Start_String;
               Store_String_Chars (Get_Name_String (Data.Directory));
               String_Elements.Table (Data.Source_Dirs) :=
                 (Value    => End_String,
                  Location => No_Location,
                  Next     => Nil_String);

               if Current_Verbosity = High then
                  Write_Line ("(Undefined) Single object directory:");
                  Write_Str ("    """);
                  Write_Str (Get_Name_String (Data.Directory));
                  Write_Line ("""");
               end if;

            when List =>

               --  Source_Dirs is a list of directories

               declare
                  Source_Dir : String_List_Id := Source_Dirs.Values;
                  Element    : String_Element;

               begin

                  --  We will find the source directories for each
                  --  element of the list

                  while Source_Dir /= Nil_String loop
                     Element := String_Elements.Table (Source_Dir);
                     Find_Source_Dirs (Element.Value, Element.Location);
                     Source_Dir := Element.Next;
                  end loop;
               end;

            when Single =>

               --  If Source_Dirs is an empty string, this means
               --  that this project contains no source.

               if Source_Dirs.Value = No_String
                 or else String_Length (Source_Dirs.Value) = 0
               then
                  if Data.Object_Directory = Data.Directory then
                     Data.Object_Directory := No_Name;
                  end if;
                  Data.Source_Dirs := Nil_String;
               else

                  --  Let's find the source directories

                  Find_Source_Dirs (Source_Dirs.Value, Source_Dirs.Location);
               end if;

         end case;

         if Current_Verbosity = High then
            Write_Line ("Puting source directories in canonical cases");
         end if;

         declare
            Current : String_List_Id := Data.Source_Dirs;
            Element : String_Element;

         begin
            while Current /= Nil_String loop
               Element := String_Elements.Table (Current);
               if Element.Value /= No_String then
                  String_To_Name_Buffer (Element.Value);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                  Start_String;
                  Store_String_Chars (Name_Buffer (1 .. Name_Len));
                  Element.Value := End_String;
                  String_Elements.Table (Current) := Element;
               end if;

               Current := Element.Next;
            end loop;
         end;
      end;

      if Current_Verbosity = High then
         Show_Source_Dirs (Project);
      end if;

      declare
         Naming_Id : constant Package_Id :=
                       Util.Value_Of (Name_Naming, Data.Decl.Packages);
         Naming : Package_Element;

      begin
         --  If there is a package Naming, we will put in Data.Naming
         --  what is in this package Naming.

         if Naming_Id /= No_Package then
            Naming := Packages.Table (Naming_Id);

            if Current_Verbosity = High then
               Write_Line ("Checking ""Naming"".");
            end if;

            declare
               Bodies         : constant Array_Element_Id :=
                                  Util.Value_Of
                                    (Name_Body_Part, Naming.Decl.Arrays);
               Specifications : constant Array_Element_Id :=
                                  Util.Value_Of
                                    (Name_Specification, Naming.Decl.Arrays);

            begin
               if Bodies /= No_Array_Element then

                  --  We have elements in the array Body_Part

                  if Current_Verbosity = High then
                     Write_Line ("Found Bodies.");
                  end if;

                  Data.Naming.Bodies := Bodies;
                  Check_Unit_Names (Bodies);

               else
                  if Current_Verbosity = High then
                     Write_Line ("No Bodies.");
                  end if;
               end if;

               if Specifications /= No_Array_Element then

                  --  We have elements in the array Specification

                  if Current_Verbosity = High then
                     Write_Line ("Found Specifications.");
                  end if;

                  Data.Naming.Specifications := Specifications;
                  Check_Unit_Names (Specifications);

               else
                  if Current_Verbosity = High then
                     Write_Line ("No Specifications.");
                  end if;
               end if;
            end;

            --  We are now checking if variables Dot_Replacement, Casing,
            --  Specification_Append, Body_Append and/or Separate_Append
            --  exist.
            --  It is an error if one of this variable is a string list.
            --  For each variable, if it does not exist, we do nothing,
            --  because we already have the default.

            --  Let's check Dot_Replacement

            declare
               Dot_Replacement : constant Variable_Value :=
                                   Util.Value_Of
                                     (Name_Dot_Replacement,
                                      Naming.Decl.Variables);

            begin
               case Dot_Replacement.Kind is
                  when Undefined =>
                     null;

                  when List =>
                     Error_Msg ("Dot_Replacement cannot be a list.",
                                Dot_Replacement.Location);

                  when Single =>
                     String_To_Name_Buffer (Dot_Replacement.Value);

                     if Name_Len = 0 then
                        Error_Msg
                          ("Dot_Replacement cannot be empty",
                           Dot_Replacement.Location);
                     else
                        Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                        Data.Naming.Dot_Replacement := Name_Find;
                        Data.Naming.Dot_Repl_Loc := Dot_Replacement.Location;
                     end if;
               end case;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Dot_Replacement = """);
               Write_Str  (Get_Name_String (Data.Naming.Dot_Replacement));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Let's check Casing

            declare
               Casing_String : constant Variable_Value :=
                 Util.Value_Of (Name_Casing, Naming.Decl.Variables);

            begin
               case Casing_String.Kind is
                  when Undefined =>
                     null;

                  when List =>
                     Error_Msg ("Casing cannot be a list.",
                                Casing_String.Location);

                  when Single =>
                     declare
                        Casing_Image : constant String
                             := Get_Name_String (Casing_String.Value);
                     begin

                        declare
                           Casing : constant Casing_Type :=
                             Value (Casing_Image);

                        begin
                           Data.Naming.Casing := Casing;
                        end;

                     exception
                        when Constraint_Error =>
                           if Casing_Image'Length = 0 then
                              Error_Msg ("Casing cannot be an empty string",
                                         Casing_String.Location);
                           else
                              Name_Len := Casing_Image'Length;
                              Name_Buffer (1 .. Name_Len) := Casing_Image;
                              Error_Msg_Name_1 := Name_Find;
                              Error_Msg
                                ("{ is not a correct Casing",
                                 Casing_String.Location);
                           end if;
                     end;
               end case;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Casing = ");
               Write_Str  (Image (Data.Naming.Casing));
               Write_Char ('.');
               Write_Eol;
            end if;

            --  Let's check Specification_Append

            declare
               Specification_Append : constant Variable_Value :=
                 Util.Value_Of
                 (Name_Specification_Append,
                  Naming.Decl.Variables);

            begin
               case Specification_Append.Kind is
                  when Undefined =>
                     null;

                  when List =>
                     Error_Msg ("Specification_Append cannot be a list.",
                                Specification_Append.Location);

                  when Single =>
                     String_To_Name_Buffer (Specification_Append.Value);
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Data.Naming.Specification_Append := Name_Find;
                     Data.Naming.Spec_Append_Loc :=
                       Specification_Append.Location;
               end case;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Specification_Append = """);
               Write_Str  (Get_Name_String (Data.Naming.Specification_Append));
               Write_Line (""".");
            end if;

            --  Let's check Body_Append

            declare
               Body_Append : constant Variable_Value :=
                 Util.Value_Of (Name_Body_Append, Naming.Decl.Variables);

            begin
               case Body_Append.Kind is
                  when Undefined =>
                     null;

                  when List =>
                     Error_Msg ("Body_Append cannot be a list.",
                                Body_Append.Location);

                  when Single =>
                     String_To_Name_Buffer (Body_Append.Value);
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Data.Naming.Body_Append := Name_Find;
                     Data.Naming.Body_Append_Loc := Body_Append.Location;

                     --  As we have a new Body_Append, we set Separate_Append
                     --  to the same value.

                     Data.Naming.Separate_Append := Data.Naming.Body_Append;
                     Data.Naming.Sep_Append_Loc := Data.Naming.Body_Append_Loc;
               end case;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Body_Append = """);
               Write_Str  (Get_Name_String (Data.Naming.Body_Append));
               Write_Line (""".");
            end if;

            --  Let's check Separate_Append

            declare
               Separate_Append : constant Variable_Value :=
                 Util.Value_Of (Name_Separate_Append, Naming.Decl.Variables);

            begin
               case Separate_Append.Kind is
                  when Undefined =>
                     Data.Naming.Separate_Append := Data.Naming.Body_Append;

                  when List =>
                     Error_Msg ("Separate_Append cannot be a list.",
                                Separate_Append.Location);

                  when Single =>
                     String_To_Name_Buffer (Separate_Append.Value);
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Data.Naming.Separate_Append := Name_Find;
                     Data.Naming.Sep_Append_Loc := Separate_Append.Location;
               end case;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Separate_Append = """);
               Write_Str  (Get_Name_String (Data.Naming.Separate_Append));
               Write_Line (""".");
               Write_Line ("end Naming.");
            end if;

            --  Now, we check if Data.Naming is valid

            Check_Naming_Scheme (Data.Naming);
         end if;
      end;

      --  If we have source directories, then let's find the sources.

      if Data.Source_Dirs /= Nil_String then
         declare
            Sources : constant Variable_Value :=
              Util.Value_Of (Name_Source_Files, Data.Decl.Variables);
            Source_List_File : constant Variable_Value :=
              Util.Value_Of (Name_Source_List_File, Data.Decl.Variables);

         begin

            if Sources.Kind /= Undefined
              and then
              Source_List_File.Kind /= Undefined
            then
               Error_Msg
                 ("?both variables source_files and " &
                  "source_list_file are present",
                  Source_List_File.Location);
            end if;

            case  Sources.Kind is
               when Undefined =>

                  --  No source_files specified.
                  --  We check Source_List_File
                  --  Find all the files that satisfy the naming scheme
                  --  in all the source directories.

                  case Source_List_File.Kind is

                     when Undefined =>
                        --  Find all the files that satisfy
                        --  the naming scheme in all the source directories.

                        Find_Sources;

                     when List =>

                        Error_Msg
                          ("source_list_file cannot be a string list",
                           Source_List_File.Location);

                     when Single =>

                        --  Source_List_File is the name of the file
                        --  that contains the source file names

                        declare
                           Source_File_Path_Name : constant String :=
                             Path_Name_Of
                               (Source_List_File.Value, Data.Directory);

                        begin
                           if Source_File_Path_Name'Length = 0 then
                              String_To_Name_Buffer (Source_List_File.Value);
                              Error_Msg_Name_1 := Name_Find;
                              Error_Msg
                                ("file with sources { does not exist",
                                 Source_List_File.Location);

                           else
                              Get_Sources_From_File
                                (Source_File_Path_Name,
                                 Source_List_File.Location);
                           end if;

                        end;

                  end case;

               when Single =>

                  --  Sources is the name of the unique the source file

                  if Sources.Value /= No_String then
                     String_To_Name_Buffer (Sources.Value);
                     declare
                        File_Name : constant String :=
                          Name_Buffer (1 .. Name_Len);
                        Current_Source : String_List_Id := Nil_String;

                     begin
                        Get_Path_Name_And_Record_Source
                          (File_Name        => File_Name,
                           Location         => Sources.Location,
                           Current_Source   => Current_Source);

                     end;

                  end if;

               when List =>

                  --  Sources is a list of file names

                  declare
                     Current_Source : String_List_Id := Nil_String;
                     Current        : String_List_Id := Sources.Values;
                     Element        : String_Element;

                  begin
                     while Current /= Nil_String loop
                        Element := String_Elements.Table (Current);
                        String_To_Name_Buffer (Element.Value);

                        declare
                           File_Name : constant String :=
                             Name_Buffer (1 .. Name_Len);

                        begin
                           Get_Path_Name_And_Record_Source
                             (File_Name        => File_Name,
                              Location         => Element.Location,
                              Current_Source   => Current_Source);
                           Current := Element.Next;

                        end;

                     end loop;

                  end;

            end case;

         end;

      end if;

      Projects.Table (Project) := Data;

   end Check_Naming_Scheme;

   ---------------------
   -- Get_Name_String --
   ---------------------

   function Get_Name_String (S : String_Id) return String is
   begin
      if S = No_String then
         return "";
      else
         String_To_Name_Buffer (S);
         return Name_Buffer (1 .. Name_Len);
      end if;
   end Get_Name_String;

   --------------
   -- Get_Unit --
   --------------

   procedure Get_Unit
     (File_Name    : Name_Id;
      Naming       : Naming_Data;
      Unit_Name    : out Name_Id;
      Unit_Kind    : out Spec_Or_Body;
      Needs_Pragma : out Boolean)
   is
      Canonical_Case_Name : Name_Id;

   begin
      Needs_Pragma := False;
      Get_Name_String (File_Name);
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      Canonical_Case_Name := Name_Find;

      if Naming.Bodies /= No_Array_Element then

         --  There are some specified file names for some bodies
         --  of this project. Find out if File_Name is one of these bodies.

         declare
            Current : Array_Element_Id := Naming.Bodies;
            Element : Array_Element;

         begin
            while Current /= No_Array_Element loop
               Element := Array_Elements.Table (Current);

               if Element.Index /= No_Name then
                  String_To_Name_Buffer (Element.Value.Value);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

                  if Canonical_Case_Name = Name_Find then

                     --  File_Name corresponds to one body.
                     --  So, we know it is a body, and we know the unit name.

                     Unit_Kind := Body_Part;
                     Unit_Name := Element.Index;
                     Needs_Pragma := True;
                     return;
                  end if;
               end if;

               Current := Element.Next;
            end loop;
         end;
      end if;

      if Naming.Specifications /= No_Array_Element then

         --  There are some specified file names for some bodiesspecifications
         --  of this project. Find out if File_Name is one of these
         --  specifications.

         declare
            Current : Array_Element_Id := Naming.Specifications;
            Element : Array_Element;

         begin
            while Current /= No_Array_Element loop
               Element := Array_Elements.Table (Current);

               if Element.Index /= No_Name then
                  String_To_Name_Buffer (Element.Value.Value);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

                  if Canonical_Case_Name = Name_Find then

                     --  File_Name corresponds to one specification.
                     --  So, we know it is a spec, and we know the unit name.

                     Unit_Kind := Specification;
                     Unit_Name := Element.Index;
                     Needs_Pragma := True;
                     return;
                  end if;

               end if;

               Current := Element.Next;
            end loop;
         end;
      end if;

      declare
         File  : String   := Get_Name_String (Canonical_Case_Name);
         First : Positive := File'First;
         Last  : Natural  := File'Last;

      begin
         --  Check if the end of the file name is Specification_Append

         Get_Name_String (Naming.Specification_Append);
         if File'Length > Name_Len
           and then File (Last - Name_Len + 1 .. Last) =
                                                Name_Buffer (1 .. Name_Len)
         then
            --  We have a spec

            Unit_Kind := Specification;
            Last := Last - Name_Len;

            if Current_Verbosity = High then
               Write_Str  ("   Specification: ");
               Write_Line (File (First .. Last));
            end if;

         else
            Get_Name_String (Naming.Body_Append);

            --  Check if the end of the file name is Body_Append

            if File'Length > Name_Len
              and then File (Last - Name_Len + 1 .. Last) =
                                                Name_Buffer (1 .. Name_Len)
            then
               --  We have a body

               Unit_Kind := Body_Part;
               Last := Last - Name_Len;

               if Current_Verbosity = High then
                  Write_Str  ("   Body: ");
                  Write_Line (File (First .. Last));
               end if;

            elsif Naming.Separate_Append /= Naming.Body_Append then
               Get_Name_String (Naming.Separate_Append);

               --  Check if the end of the file name is Separate_Append

               if File'Length > Name_Len
                 and then File (Last - Name_Len + 1 .. Last) =
                                                Name_Buffer (1 .. Name_Len)
               then
                  --  We have a separate (a body)

                  Unit_Kind := Body_Part;
                  Last := Last - Name_Len;

                  if Current_Verbosity = High then
                     Write_Str  ("   Separate: ");
                     Write_Line (File (First .. Last));
                  end if;

               else
                  Last := 0;
               end if;

            else
               Last := 0;
            end if;
         end if;

         if Last = 0 then

            --  This is not a source file

            Unit_Name := No_Name;
            Unit_Kind := Specification;

            if Current_Verbosity = High then
               Write_Line ("   Not a valid file name.");
            end if;

            return;
         end if;

         Get_Name_String (Naming.Dot_Replacement);

         if Name_Buffer (1 .. Name_Len) /= "." then

            --  If Dot_Replacement is not a single dot,
            --  then there should not be any dot in the name.

            for Index in First .. Last loop

               if File (Index) = '.' then

                  if Current_Verbosity = High then
                     Write_Line
                       ("   Not a valid file name (some dot not replaced).");
                  end if;

                  Unit_Name := No_Name;
                  return;

               end if;
            end loop;

            --  Replace the substring Dot_Replacement with dots

            declare
               Index : Positive := First;

            begin
               while Index <= Last - Name_Len + 1 loop

                  if File (Index .. Index + Name_Len - 1) =
                    Name_Buffer (1 .. Name_Len)
                  then
                     File (Index) := '.';

                     if Name_Len > 1 and then Index < Last then
                        File (Index + 1 .. Last - Name_Len + 1) :=
                          File (Index + Name_Len .. Last);
                     end if;

                     Last := Last - Name_Len + 1;
                  end if;

                  Index := Index + 1;
               end loop;
            end;
         end if;

         --  Check if the casing is right

         declare
            Src : String := File (First .. Last);

         begin
            case Naming.Casing is
               when All_Lower_Case =>
                  Fixed.Translate
                    (Source  => Src,
                     Mapping => Lower_Case_Map);

               when All_Upper_Case =>
                  Fixed.Translate
                    (Source  => Src,
                     Mapping => Upper_Case_Map);

               when Mixed_Case | Unknown =>
                  null;
            end case;

            if Src /= File (First .. Last) then
               if Current_Verbosity = High then
                  Write_Line ("   Not a valid file name (casing).");
               end if;

               Unit_Name := No_Name;
               return;
            end if;

            --  We put the name in lower case

            Fixed.Translate
              (Source  => Src,
               Mapping => Lower_Case_Map);

            if Current_Verbosity = High then
               Write_Str  ("      ");
               Write_Line (Src);
            end if;

            Name_Len := Src'Length;
            Name_Buffer (1 .. Name_Len) := Src;

            --  Now, we check if this name is a valid unit name

            Check_Naming_Scheme (Name => Name_Find, Unit => Unit_Name);
         end;

      end;

   end Get_Unit;

   -----------------------
   -- Is_Illegal_Append --
   -----------------------

   function Is_Illegal_Append (This : String) return Boolean is
   begin
      return This'Length = 0
        or else Is_Alphanumeric (This (This'First))
        or else (This'Length >= 2
                 and then This (This'First) = '_'
                 and then Is_Alphanumeric (This (This'First + 1)));
   end Is_Illegal_Append;

   ----------------------
   -- Locate_Directory --
   ----------------------

   function Locate_Directory
     (Name   : Name_Id;
      Parent : Name_Id)
     return   Name_Id
   is
      The_Name : constant String := Get_Name_String (Name);

   begin

      if Current_Verbosity = High then
         Write_Str ("Locate_Directory (""");
         Write_Str (The_Name);
         Write_Str (""", """);
         Write_Str (Get_Name_String (Parent));
         Write_Line (""")");
      end if;

      if The_Name (The_Name'First) = '/' then
         if Current_Verbosity = High then
            Write_Str ("Checking """);
            Write_Str (The_Name);
            Write_Line ("""");
         end if;

         if Is_Directory (The_Name) then
            if Current_Verbosity = High then
               Write_Line ("    OK");
            end if;

            return Name;
         end if;

      else
         declare
            The_Full_Name : constant String :=
              Get_Name_String (Parent) & The_Name;

         begin
            if Current_Verbosity = High then
               Write_Str ("Checking """);
               Write_Str (The_Full_Name);
               Write_Line ("""");
            end if;

            if Is_Directory (The_Full_Name) then
               if Current_Verbosity = High then
                  Write_Line ("    OK");
               end if;

               Name_Len := The_Full_Name'Length;
               Name_Buffer (1 .. Name_Len) := The_Full_Name;
               return Name_Find;
            end if;
         end;
      end if;

      if Current_Verbosity = High then
         Write_Line ("     directory does not exist");
      end if;

      return No_Name;
   end Locate_Directory;

   ------------------
   -- Path_Name_Of --
   ------------------

   function Path_Name_Of
     (File_Name : String_Id;
      Directory : String_Id)
     return      String
   is
      Result : String_Access;

   begin
      String_To_Name_Buffer (File_Name);

      declare
         The_File_Name : constant String := Name_Buffer (1 .. Name_Len);

      begin
         String_To_Name_Buffer (Directory);
         Result := Locate_Regular_File
           (File_Name => The_File_Name,
            Path      => Name_Buffer (1 .. Name_Len));
      end;

      if Result = null then
         return "";
      else
         Canonical_Case_File_Name (Result.all);
         return Result.all;
      end if;
   end Path_Name_Of;

   function Path_Name_Of
     (File_Name : String_Id;
      Directory : Name_Id)
     return      String
   is
      Result : String_Access;
      The_Directory : constant String := Get_Name_String (Directory);

   begin
      String_To_Name_Buffer (File_Name);
      Result := Locate_Regular_File
        (File_Name => Name_Buffer (1 .. Name_Len),
         Path      => The_Directory);

      if Result = null then
         return "";
      else
         Canonical_Case_File_Name (Result.all);
         return Result.all;
      end if;
   end Path_Name_Of;

   -------------------
   -- Record_Source --
   -------------------

   procedure Record_Source
     (File_Name        : Name_Id;
      Path_Name        : Name_Id;
      Project          : Project_Id;
      Data             : in out Project_Data;
      Error_If_Invalid : Boolean;
      Location         : Source_Ptr;
      Current_Source   : in out String_List_Id)
   is
      Unit_Name    : Name_Id;
      Unit_Kind    : Spec_Or_Body;
      Needs_Pragma : Boolean;

   begin
      --  Find out the unit name, the unit kind and if it needs
      --  a specific SFN pragma.

      Get_Unit
        (File_Name    => File_Name,
         Naming       => Data.Naming,
         Unit_Name    => Unit_Name,
         Unit_Kind    => Unit_Kind,
         Needs_Pragma => Needs_Pragma);

      --  If it is not a source file, report an error only if
      --  Error_If_Invalid is true.

      if Unit_Name = No_Name then
         if Error_If_Invalid then
            Error_Msg_Name_1 := File_Name;
            Error_Msg
              ("{ is not a valid source file name",
               Location);

         else
            if Current_Verbosity = High then
               Write_Str  ("   """);
               Write_Str  (Get_Name_String (File_Name));
               Write_Line (""" is not a valid source file name (ignored).");
            end if;
         end if;

      else
         --  Put the file name in the list of sources of the project.

         String_Elements.Increment_Last;
         Get_Name_String (File_Name);
         Start_String;
         Store_String_Chars (Name_Buffer (1 .. Name_Len));
         String_Elements.Table (String_Elements.Last) :=
           (Value    => End_String,
            Location => No_Location,
            Next     => Nil_String);

         if Current_Source = Nil_String then
            Data.Sources := String_Elements.Last;

         else
            String_Elements.Table (Current_Source).Next :=
              String_Elements.Last;
         end if;

         Current_Source := String_Elements.Last;

         --  Put the unit in unit list

         declare
            The_Unit      : Unit_Id := Units_Htable.Get (Unit_Name);
            The_Unit_Data : Unit_Data;

         begin
            if Current_Verbosity = High then
               Write_Str  ("Putting ");
               Write_Str  (Get_Name_String (Unit_Name));
               Write_Line (" in the unit list.");
            end if;

            --  The unit is already in the list, but may be it is
            --  only the other unit kind (spec or body), or what is
            --  in the unit list is a unit of a project we are modifying.

            if The_Unit /= Prj.Com.No_Unit then
               The_Unit_Data := Units.Table (The_Unit);

               if The_Unit_Data.File_Names (Unit_Kind).Name = No_Name
                 or else (Data.Modifies /= No_Project
                            and then
                          The_Unit_Data.File_Names (Unit_Kind).Project =
                                                            Data.Modifies)
               then
                  The_Unit_Data.File_Names (Unit_Kind) :=
                    (Name         => File_Name,
                     Path         => Path_Name,
                     Project      => Project,
                     Needs_Pragma => Needs_Pragma);
                  Units.Table (The_Unit) := The_Unit_Data;

               else
                  --  It is an error to have two units with the same name
                  --  and the same kind (spec or body).

                  Error_Msg_Name_1 := Unit_Name;
                  if Location /= No_Location then
                     Error_Msg
                       ("duplicate source {",
                        Location);
                  else

                     Error_Msg_S
                       ("duplicate source {");
                  end if;
               end if;

            --  It is a new unit, create a new record

            else
               Units.Increment_Last;
               The_Unit := Units.Last;
               Units_Htable.Set (Unit_Name, The_Unit);
               The_Unit_Data.Name := Unit_Name;
               The_Unit_Data.File_Names (Unit_Kind) :=
                 (Name         => File_Name,
                  Path         => Path_Name,
                  Project      => Project,
                  Needs_Pragma => Needs_Pragma);
               Units.Table (The_Unit) := The_Unit_Data;
            end if;
         end;
      end if;
   end Record_Source;

   ----------------------
   -- Show_Source_Dirs --
   ----------------------

   procedure Show_Source_Dirs (Project : Project_Id) is
      Current : String_List_Id := Projects.Table (Project).Source_Dirs;
      Element : String_Element;

   begin
      Write_Line ("Source_Dirs:");

      while Current /= Nil_String loop
         Element := String_Elements.Table (Current);
         Write_Str  ("   ");
         Write_Line (Get_Name_String (Element.Value));
         Current := Element.Next;
      end loop;

      Write_Line ("end Source_Dirs.");
   end Show_Source_Dirs;

end Prj.Nmsc;
