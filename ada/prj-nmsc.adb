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
--          Copyright (C) 2000-2002 Free Software Foundation, Inc.          --
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

with Errout;
with Hostparm;
with MLib.Tgt;
with Namet;    use Namet;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Com;  use Prj.Com;
with Prj.Env;  use Prj.Env;
with Prj.Util; use Prj.Util;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Types;    use Types;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;

package body Prj.Nmsc is

   Dir_Sep : Character renames GNAT.OS_Lib.Directory_Separator;

   Error_Report    : Put_Line_Access := null;
   Current_Project : Project_Id := No_Project;

   procedure Check_Ada_Naming_Scheme (Naming : Naming_Data);
   --  Check that the package Naming is correct.

   procedure Check_Ada_Name
     (Name : Name_Id;
      Unit : out Name_Id);
   --  Check that a name is a valid Ada unit name.

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr);
   --  Output an error message. If Error_Report is null, simply call
   --  Errout.Error_Msg. Otherwise, disregard Flag_Location and use
   --  Error_Report.

   function Get_Name_String (S : String_Id) return String;
   --  Get the string from a String_Id

   procedure Get_Unit
     (File_Name    : Name_Id;
      Naming       : Naming_Data;
      Unit_Name    : out Name_Id;
      Unit_Kind    : out Spec_Or_Body;
      Needs_Pragma : out Boolean);
   --  Find out, from a file name, the unit name, the unit kind and if a
   --  specific SFN pragma is needed. If the file name corresponds to no
   --  unit, then Unit_Name will be No_Name.

   function Is_Illegal_Suffix
     (Suffix                          : String;
      Dot_Replacement_Is_A_Single_Dot : Boolean)
      return                            Boolean;
   --  Returns True if the string Suffix cannot be used as
   --  a spec suffix, a body suffix or a separate suffix.

   procedure Record_Source
     (File_Name          : Name_Id;
      Path_Name          : Name_Id;
      Project            : Project_Id;
      Data               : in out Project_Data;
      Location           : Source_Ptr;
      Current_Source     : in out String_List_Id);
   --  Put a unit in the list of units of a project, if the file name
   --  corresponds to a valid unit name.

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

   ---------------
   -- Ada_Check --
   ---------------

   procedure Ada_Check
     (Project      : Project_Id;
      Report_Error : Put_Line_Access)
   is
      Data         : Project_Data;
      Languages    : Variable_Value := Nil_Variable_Value;

      procedure Check_Unit_Names (List : Array_Element_Id);
      --  Check that a list of unit names contains only valid names.

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

            Check_Ada_Name (Element.Index, Unit_Name);

            if Unit_Name = No_Name then
               Errout.Error_Msg_Name_1 := Element.Index;
               Error_Msg
                 ("{ is not a valid unit name.",
                  Element.Value.Location);

            else
               if Current_Verbosity = High then
                  Write_Str ("   Body_Part (""");
                  Write_Str (Get_Name_String (Unit_Name));
                  Write_Line (""")");
               end if;

               Element.Index := Unit_Name;
               Array_Elements.Table (Current) := Element;
            end if;

            Current := Element.Next;
         end loop;
      end Check_Unit_Names;

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
                           Path_Access : constant GNAT.OS_Lib.String_Access :=
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
                              --  does not contain a valid source.
                              --  But there is an error if we have a
                              --  duplicate unit name.

                              Record_Source
                                (File_Name          => File_Name,
                                 Path_Name          => Path_Name,
                                 Project            => Project,
                                 Data               => Data,
                                 Location           => No_Location,
                                 Current_Source     => Current_Source);

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
            Error_Msg ("there are no sources in this project",
                       Data.Location);
         end if;
      end Find_Sources;

      -------------------------------------
      -- Get_Path_Name_And_Record_Source --
      -------------------------------------

      procedure Get_Path_Name_And_Record_Source
        (File_Name        : String;
         Location         : Source_Ptr;
         Current_Source   : in out String_List_Id)
      is
         Source_Dir : String_List_Id := Data.Source_Dirs;
         Element    : String_Element;
         Path_Name  : GNAT.OS_Lib.String_Access;
         File       : Name_Id;
         Path       : Name_Id;

         Found      : Boolean := False;
         Fname      : String  := File_Name;

      begin
         Canonical_Case_File_Name (Fname);
         Name_Len := Fname'Length;
         Name_Buffer (1 .. Name_Len) := Fname;
         File := Name_Find;

         if Current_Verbosity = High then
            Write_Str  ("   Checking """);
            Write_Str  (Fname);
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
              (Fname,
               Get_Name_String (Element.Value));

            if Path_Name /= null then
               if Current_Verbosity = High then
                  Write_Line ("OK");
               end if;

               Name_Len := Path_Name'Length;
               Name_Buffer (1 .. Name_Len) := Path_Name.all;
               Path := Name_Find;

               --  Register the source if it is an Ada compilation unit..

               Record_Source
                 (File_Name          => File,
                  Path_Name          => Path,
                  Project            => Project,
                  Data               => Data,
                  Location           => Location,
                  Current_Source     => Current_Source);
               Found := True;
               exit;

            else
               if Current_Verbosity = High then
                  Write_Line ("No");
               end if;

               Source_Dir := Element.Next;
            end if;
         end loop;

         --  It is an error if a source file names in a source list or
         --  in a source list file is not found.

         if not Found then
            Errout.Error_Msg_Name_1 := File;
            Error_Msg ("source file { cannot be found", Location);
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

      begin
         if Current_Verbosity = High then
            Write_Str  ("Opening """);
            Write_Str  (Path);
            Write_Line (""".");
         end if;

         --  We open the file

         Prj.Util.Open (File, Path);

         if not Prj.Util.Is_Valid (File) then
            Error_Msg ("file does not exist", Location);
         else
            while not Prj.Util.End_Of_File (File) loop
               Prj.Util.Get_Line (File, Line, Last);

               --  If the line is not empty and does not start with "--",
               --  then it should contain a file name. However, if the
               --  file name does not exist, it may be for another language
               --  and we don't fail.

               if Last /= 0
                 and then (Last = 1 or else Line (1 .. 2) /= "--")
               then
                  Get_Path_Name_And_Record_Source
                    (File_Name => Line (1 .. Last),
                     Location => Location,
                     Current_Source => Current_Source);
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

      --  Start of processing for Ada_Check

   begin
      Language_Independent_Check (Project, Report_Error);

      Error_Report    := Report_Error;
      Current_Project := Project;

      Data      := Projects.Table (Project);
      Languages := Prj.Util.Value_Of (Name_Languages, Data.Decl.Attributes);

      Data.Naming.Current_Language := Name_Ada;
      Data.Sources_Present         := Data.Source_Dirs /= Nil_String;

      if not Languages.Default then
         declare
            Current   : String_List_Id := Languages.Values;
            Element   : String_Element;
            Ada_Found : Boolean := False;

         begin
            Look_For_Ada : while Current /= Nil_String loop
               Element := String_Elements.Table (Current);
               String_To_Name_Buffer (Element.Value);
               To_Lower (Name_Buffer (1 .. Name_Len));

               if Name_Buffer (1 .. Name_Len) = "ada" then
                  Ada_Found := True;
                  exit Look_For_Ada;
               end if;

               Current := Element.Next;
            end loop Look_For_Ada;

            if not Ada_Found then

               --  Mark the project file as having no sources for Ada

               Data.Sources_Present := False;
            end if;
         end;
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
               Write_Line ("Checking ""Naming"" for Ada.");
            end if;

            declare
               Bodies : constant Array_Element_Id :=
                                  Util.Value_Of
                                    (Name_Implementation, Naming.Decl.Arrays);

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

            --  For each variable, if it does not exist, we do nothing,
            --  because we already have the default.

            --  Check Dot_Replacement

            declare
               Dot_Replacement : constant Variable_Value :=
                                   Util.Value_Of
                                     (Name_Dot_Replacement,
                                      Naming.Decl.Attributes);

            begin
               pragma Assert (Dot_Replacement.Kind = Single,
                              "Dot_Replacement is not a single string");

               if not Dot_Replacement.Default then

                  String_To_Name_Buffer (Dot_Replacement.Value);

                  if Name_Len = 0 then
                     Error_Msg ("Dot_Replacement cannot be empty",
                                Dot_Replacement.Location);

                  else
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Data.Naming.Dot_Replacement := Name_Find;
                     Data.Naming.Dot_Repl_Loc := Dot_Replacement.Location;
                  end if;

               end if;

            end;

            if Current_Verbosity = High then
               Write_Str  ("  Dot_Replacement = """);
               Write_Str  (Get_Name_String (Data.Naming.Dot_Replacement));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Check Casing

            declare
               Casing_String : constant Variable_Value :=
                 Util.Value_Of (Name_Casing, Naming.Decl.Attributes);

            begin
               pragma Assert (Casing_String.Kind = Single,
                              "Casing is not a single string");

               if not Casing_String.Default then
                  declare
                     Casing_Image : constant String :=
                                      Get_Name_String (Casing_String.Value);

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
                           Errout.Error_Msg_Name_1 := Name_Find;
                           Error_Msg
                             ("{ is not a correct Casing",
                              Casing_String.Location);
                        end if;
                  end;
               end if;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Casing = ");
               Write_Str  (Image (Data.Naming.Casing));
               Write_Char ('.');
               Write_Eol;
            end if;

            --  Check Specification_Suffix

            declare
               Ada_Spec_Suffix : constant Variable_Value :=
                 Prj.Util.Value_Of
                   (Index => Name_Ada,
                    In_Array => Data.Naming.Specification_Suffix);

            begin
               if Ada_Spec_Suffix.Kind = Single
                 and then String_Length (Ada_Spec_Suffix.Value) /= 0
               then
                  String_To_Name_Buffer (Ada_Spec_Suffix.Value);
                  Data.Naming.Current_Spec_Suffix := Name_Find;
                  Data.Naming.Spec_Suffix_Loc := Ada_Spec_Suffix.Location;

               else
                  Data.Naming.Current_Spec_Suffix := Default_Ada_Spec_Suffix;
               end if;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Specification_Suffix = """);
               Write_Str  (Get_Name_String (Data.Naming.Current_Spec_Suffix));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Check Implementation_Suffix

            declare
               Ada_Impl_Suffix : constant Variable_Value :=
                 Prj.Util.Value_Of
                   (Index => Name_Ada,
                    In_Array => Data.Naming.Implementation_Suffix);

            begin
               if Ada_Impl_Suffix.Kind = Single
                 and then String_Length (Ada_Impl_Suffix.Value) /= 0
               then
                  String_To_Name_Buffer (Ada_Impl_Suffix.Value);
                  Data.Naming.Current_Impl_Suffix := Name_Find;
                  Data.Naming.Impl_Suffix_Loc := Ada_Impl_Suffix.Location;

               else
                  Data.Naming.Current_Impl_Suffix := Default_Ada_Impl_Suffix;
               end if;
            end;

            if Current_Verbosity = High then
               Write_Str  ("  Implementation_Suffix = """);
               Write_Str  (Get_Name_String (Data.Naming.Current_Impl_Suffix));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Check Separate_Suffix

            declare
               Ada_Sep_Suffix : constant Variable_Value :=
                 Prj.Util.Value_Of
                 (Variable_Name => Name_Separate_Suffix,
                  In_Variables  => Naming.Decl.Attributes);
            begin
               if Ada_Sep_Suffix.Default then
                  Data.Naming.Separate_Suffix :=
                    Data.Naming.Current_Impl_Suffix;

               else
                  String_To_Name_Buffer (Ada_Sep_Suffix.Value);

                  if Name_Len = 0 then
                     Error_Msg ("Separate_Suffix cannot be empty",
                                Ada_Sep_Suffix.Location);

                  else
                     Data.Naming.Separate_Suffix := Name_Find;
                     Data.Naming.Sep_Suffix_Loc  := Ada_Sep_Suffix.Location;
                  end if;

               end if;

            end;

            if Current_Verbosity = High then
               Write_Str  ("  Separate_Suffix = """);
               Write_Str  (Get_Name_String (Data.Naming.Separate_Suffix));
               Write_Char ('"');
               Write_Eol;
            end if;

            --  Check if Data.Naming is valid

            Check_Ada_Naming_Scheme (Data.Naming);

         else
            Data.Naming.Current_Spec_Suffix := Default_Ada_Spec_Suffix;
            Data.Naming.Current_Impl_Suffix := Default_Ada_Impl_Suffix;
            Data.Naming.Separate_Suffix     := Default_Ada_Impl_Suffix;
         end if;
      end;

      --  If we have source directories, then find the sources

      if Data.Sources_Present then
         if Data.Source_Dirs = Nil_String then
            Data.Sources_Present := False;

         else
            declare
               Sources : constant Variable_Value :=
                 Util.Value_Of
                 (Name_Source_Files,
                  Data.Decl.Attributes);

               Source_List_File : constant Variable_Value :=
                 Util.Value_Of
                 (Name_Source_List_File,
                  Data.Decl.Attributes);

            begin
               pragma Assert
                 (Sources.Kind = List,
                    "Source_Files is not a list");
               pragma Assert
                 (Source_List_File.Kind = Single,
                    "Source_List_File is not a single string");

               if not Sources.Default then
                  if not Source_List_File.Default then
                     Error_Msg
                       ("?both variables source_files and " &
                        "source_list_file are present",
                        Source_List_File.Location);
                  end if;

                  --  Sources is a list of file names

                  declare
                     Current_Source : String_List_Id := Nil_String;
                     Current        : String_List_Id := Sources.Values;
                     Element        : String_Element;

                  begin
                     Data.Sources_Present := Current /= Nil_String;

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

                  --  No source_files specified.
                  --  We check Source_List_File has been specified.

               elsif not Source_List_File.Default then

                  --  Source_List_File is the name of the file
                  --  that contains the source file names

                  declare
                     Source_File_Path_Name : constant String :=
                       Path_Name_Of
                       (Source_List_File.Value,
                        Data.Directory);

                  begin
                     if Source_File_Path_Name'Length = 0 then
                        String_To_Name_Buffer (Source_List_File.Value);
                        Errout.Error_Msg_Name_1 := Name_Find;
                        Error_Msg
                          ("file with sources { does not exist",
                           Source_List_File.Location);

                     else
                        Get_Sources_From_File
                          (Source_File_Path_Name,
                           Source_List_File.Location);
                     end if;
                  end;

               else
                  --  Neither Source_Files nor Source_List_File has been
                  --  specified.
                  --  Find all the files that satisfy
                  --  the naming scheme in all the source directories.

                  Find_Sources;
               end if;
            end;
         end if;
      end if;

      Projects.Table (Project) := Data;
   end Ada_Check;

   --------------------
   -- Check_Ada_Name --
   --------------------

   procedure Check_Ada_Name
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

      --  Cannot end with an underscore or a dot

      OK := OK and then not Need_Letter and then not Last_Underscore;

      if OK then
         Unit := Name;
      else
         --  Signal a problem with No_Name

         Unit := No_Name;
      end if;
   end Check_Ada_Name;

   -----------------------------
   -- Check_Ada_Naming_Scheme --
   -----------------------------

   procedure Check_Ada_Naming_Scheme (Naming : Naming_Data) is
   begin
      --  Only check if we are not using the standard naming scheme

      if Naming /= Standard_Naming_Data then
         declare
            Dot_Replacement       : constant String :=
                                     Get_Name_String
                                       (Naming.Dot_Replacement);

            Specification_Suffix : constant String :=
                                     Get_Name_String
                                       (Naming.Current_Spec_Suffix);

            Implementation_Suffix : constant String :=
                                     Get_Name_String
                                       (Naming.Current_Impl_Suffix);

            Separate_Suffix       : constant String :=
                                     Get_Name_String
                                       (Naming.Separate_Suffix);

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

            --  Suffixes cannot
            --   - be empty
            --   - start with an alphanumeric
            --   - start with an '_' followed by an alphanumeric

            if Is_Illegal_Suffix
                 (Specification_Suffix, Dot_Replacement = ".")
            then
               Errout.Error_Msg_Name_1 := Naming.Current_Spec_Suffix;
               Error_Msg
                 ("{ is illegal for Specification_Suffix",
                  Naming.Spec_Suffix_Loc);
            end if;

            if Is_Illegal_Suffix
                 (Implementation_Suffix, Dot_Replacement = ".")
            then
               Errout.Error_Msg_Name_1 := Naming.Current_Impl_Suffix;
               Error_Msg
                 ("{ is illegal for Implementation_Suffix",
                  Naming.Impl_Suffix_Loc);
            end if;

            if Implementation_Suffix /= Separate_Suffix then
               if Is_Illegal_Suffix
                    (Separate_Suffix, Dot_Replacement = ".")
               then
                  Errout.Error_Msg_Name_1 := Naming.Separate_Suffix;
                  Error_Msg
                    ("{ is illegal for Separate_Suffix",
                     Naming.Sep_Suffix_Loc);
               end if;
            end if;

            --  Specification_Suffix cannot have the same termination as
            --  Implementation_Suffix or Separate_Suffix

            if Specification_Suffix'Length <= Implementation_Suffix'Length
              and then
                Implementation_Suffix (Implementation_Suffix'Last -
                             Specification_Suffix'Length + 1 ..
                             Implementation_Suffix'Last) = Specification_Suffix
            then
               Error_Msg
                 ("Implementation_Suffix (""" &
                  Implementation_Suffix &
                  """) cannot end with" &
                  "Specification_Suffix  (""" &
                   Specification_Suffix & """).",
                  Naming.Impl_Suffix_Loc);
            end if;

            if Specification_Suffix'Length <= Separate_Suffix'Length
              and then
                Separate_Suffix
                  (Separate_Suffix'Last - Specification_Suffix'Length + 1
                    ..
                   Separate_Suffix'Last) = Specification_Suffix
            then
               Error_Msg
                 ("Separate_Suffix (""" &
                  Separate_Suffix &
                  """) cannot end with" &
                  " Specification_Suffix (""" &
                  Specification_Suffix & """).",
                  Naming.Sep_Suffix_Loc);
            end if;
         end;
      end if;

   end Check_Ada_Naming_Scheme;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr) is

      Error_Buffer : String (1 .. 5_000);
      Error_Last   : Natural := 0;
      Msg_Name     : Natural := 0;
      First        : Positive := Msg'First;

      procedure Add (C : Character);
      --  Add a character to the buffer

      procedure Add (S : String);
      --  Add a string to the buffer

      procedure Add (Id : Name_Id);
      --  Add a name to the buffer

      ---------
      -- Add --
      ---------

      procedure Add (C : Character) is
      begin
         Error_Last := Error_Last + 1;
         Error_Buffer (Error_Last) := C;
      end Add;

      procedure Add (S : String) is
      begin
         Error_Buffer (Error_Last + 1 .. Error_Last + S'Length) := S;
         Error_Last := Error_Last + S'Length;
      end Add;

      procedure Add (Id : Name_Id) is
      begin
         Get_Name_String (Id);
         Add (Name_Buffer (1 .. Name_Len));
      end Add;

   --  Start of processing for Error_Msg

   begin
      if Error_Report = null then
         Errout.Error_Msg (Msg, Flag_Location);
         return;
      end if;

      if Msg (First) = '\' then

         --  Continuation character, ignore.

         First := First + 1;

      elsif Msg (First) = '?' then

         --  Warning character. It is always the first one,
         --  in this package.

         First := First + 1;
         Add ("Warning: ");
      end if;

      for Index in First .. Msg'Last loop
         if Msg (Index) = '{' or else Msg (Index) = '%' then

            --  Include a name between double quotes.

            Msg_Name := Msg_Name + 1;
            Add ('"');

            case Msg_Name is
               when 1 => Add (Errout.Error_Msg_Name_1);
               when 2 => Add (Errout.Error_Msg_Name_2);
               when 3 => Add (Errout.Error_Msg_Name_3);

               when others => null;
            end case;

            Add ('"');

         else
            Add (Msg (Index));
         end if;

      end loop;

      Error_Report (Error_Buffer (1 .. Error_Last), Current_Project);
   end Error_Msg;

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

         Standard_GNAT : Boolean :=
                           Naming.Current_Spec_Suffix =
                                         Default_Ada_Spec_Suffix
                             and then
                           Naming.Current_Impl_Suffix =
                                         Default_Ada_Impl_Suffix;

      begin
         --  Check if the end of the file name is Specification_Append

         Get_Name_String (Naming.Current_Spec_Suffix);

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
            Get_Name_String (Naming.Current_Impl_Suffix);

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

            elsif Naming.Separate_Suffix /= Naming.Current_Spec_Suffix then
               Get_Name_String (Naming.Separate_Suffix);

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
         Standard_GNAT :=
           Standard_GNAT and then Name_Buffer (1 .. Name_Len) = "-";

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

            --  In the standard GNAT naming scheme, check for special cases:
            --  children or separates of A, G, I or S, and run time sources.

            if Standard_GNAT and then Src'Length >= 3 then
               declare
                  S1 : constant Character := Src (Src'First);
                  S2 : constant Character := Src (Src'First + 1);

               begin
                  if S1 = 'a' or else S1 = 'g'
                    or else S1 = 'i' or else S1 = 's'
                  then
                     --  Children or separates of packages A, G, I or S

                     if (Hostparm.OpenVMS and then S2 = '$')
                       or else (not Hostparm.OpenVMS and then S2 = '~')
                     then
                        Src (Src'First + 1) := '.';

                     --  If it is potentially a run time source, disable
                     --  filling of the mapping file to avoid warnings.

                     elsif S2 = '.' then
                        Set_Mapping_File_Initial_State_To_Empty;
                     end if;

                  end if;
               end;
            end if;

            if Current_Verbosity = High then
               Write_Str  ("      ");
               Write_Line (Src);
            end if;

            Name_Len := Src'Length;
            Name_Buffer (1 .. Name_Len) := Src;

            --  Now, we check if this name is a valid unit name

            Check_Ada_Name (Name => Name_Find, Unit => Unit_Name);
         end;

      end;

   end Get_Unit;

   -----------------------
   -- Is_Illegal_Suffix --
   -----------------------

   function Is_Illegal_Suffix
     (Suffix                          : String;
      Dot_Replacement_Is_A_Single_Dot : Boolean)
      return                            Boolean
   is
   begin
      if Suffix'Length = 0
        or else Is_Alphanumeric (Suffix (Suffix'First))
        or else Index (Suffix, ".") = 0
        or else (Suffix'Length >= 2
                 and then Suffix (Suffix'First) = '_'
                 and then Is_Alphanumeric (Suffix (Suffix'First + 1)))
      then
         return True;
      end if;

      --  If dot replacement is a single dot, and first character of
      --  suffix is also a dot

      if Dot_Replacement_Is_A_Single_Dot
        and then Suffix (Suffix'First) = '.'
      then
         for Index in Suffix'First + 1 .. Suffix'Last loop

            --  If there is another dot

            if Suffix (Index) = '.' then

               --  It is illegal to have a letter following the initial dot

               return Is_Letter (Suffix (Suffix'First + 1));
            end if;
         end loop;
      end if;

      --  Everything is OK

      return False;
   end Is_Illegal_Suffix;

   --------------------------------
   -- Language_Independent_Check --
   --------------------------------

   procedure Language_Independent_Check
     (Project      : Project_Id;
      Report_Error : Put_Line_Access)
   is
      Last_Source_Dir   : String_List_Id  := Nil_String;
      Data              : Project_Data    := Projects.Table (Project);

      procedure Find_Source_Dirs (From : String_Id; Location : Source_Ptr);
      --  Find one or several source directories, and add them
      --  to the list of source directories of the project.

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
            Dir      : Dir_Type;
            Name     : String (1 .. 250);
            Last     : Natural;
            The_Path : String := Get_Name_String (Path) & Dir_Sep;

            The_Path_Last : Positive := The_Path'Last;

         begin
            if The_Path'Length > 1
              and then
                (The_Path (The_Path_Last - 1) = Dir_Sep
                   or else The_Path (The_Path_Last - 1) = '/')
            then
               The_Path_Last := The_Path_Last - 1;
            end if;

            Canonical_Case_File_Name (The_Path);

            if Current_Verbosity = High then
               Write_Str  ("   ");
               Write_Line (The_Path (The_Path'First .. The_Path_Last));
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

            Open (Dir, The_Path (The_Path'First .. The_Path_Last));

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
                     Path_Name : String :=
                                   The_Path (The_Path'First .. The_Path_Last) &
                                   Name (1 .. Last);

                  begin
                     Canonical_Case_File_Name (Path_Name);

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
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
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
                     Directory (Directory'Last - 2) = Dir_Sep)
         then
            Name_Len := Directory'Length - 3;

            if Name_Len = 0 then
               --  This is the case of "/**": all directories
               --  in the file system.

               Name_Len := 1;
               Name_Buffer (1) := Directory (Directory'First);

            else
               Name_Buffer (1 .. Name_Len) :=
                 Directory (Directory'First .. Directory'Last - 3);
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
                  Errout.Error_Msg_Name_1 := Base_Dir;
                  if Location = No_Location then
                     Error_Msg ("{ is not a valid directory.", Data.Location);
                  else
                     Error_Msg ("{ is not a valid directory.", Location);
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
                  Errout.Error_Msg_Name_1 := Directory_Id;
                  if Location = No_Location then
                     Error_Msg ("{ is not a valid directory", Data.Location);
                  else
                     Error_Msg ("{ is not a valid directory", Location);
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

      --  Start of processing for Language_Independent_Check

   begin

      if Data.Language_Independent_Checked then
         return;
      end if;

      Data.Language_Independent_Checked := True;

      Error_Report := Report_Error;

      if Current_Verbosity = High then
         Write_Line ("Starting to look for directories");
      end if;

      --  Check the object directory

      declare
         Object_Dir : Variable_Value :=
                        Util.Value_Of (Name_Object_Dir, Data.Decl.Attributes);

      begin
         pragma Assert (Object_Dir.Kind = Single,
                        "Object_Dir is not a single string");

         --  We set the object directory to its default

         Data.Object_Directory := Data.Directory;

         if not String_Equal (Object_Dir.Value, Empty_String) then

            String_To_Name_Buffer (Object_Dir.Value);

            if Name_Len = 0 then
               Error_Msg ("Object_Dir cannot be empty",
                          Object_Dir.Location);

            else
               --  We check that the specified object directory
               --  does exist.

               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

               declare
                  Dir_Id : constant Name_Id := Name_Find;

               begin
                  Data.Object_Directory :=
                    Locate_Directory (Dir_Id, Data.Directory);

                  if Data.Object_Directory = No_Name then
                     Errout.Error_Msg_Name_1 := Dir_Id;
                     Error_Msg
                       ("the object directory { cannot be found",
                        Data.Location);
                  end if;
               end;
            end if;
         end if;
      end;

      if Current_Verbosity = High then
         if Data.Object_Directory = No_Name then
            Write_Line ("No object directory");
         else
            Write_Str ("Object directory: """);
            Write_Str (Get_Name_String (Data.Object_Directory));
            Write_Line ("""");
         end if;
      end if;

      --  Check the exec directory

      declare
         Exec_Dir : Variable_Value :=
                      Util.Value_Of (Name_Exec_Dir, Data.Decl.Attributes);

      begin
         pragma Assert (Exec_Dir.Kind = Single,
                        "Exec_Dir is not a single string");

         --  We set the object directory to its default

         Data.Exec_Directory := Data.Object_Directory;

         if not String_Equal (Exec_Dir.Value, Empty_String) then

            String_To_Name_Buffer (Exec_Dir.Value);

            if Name_Len = 0 then
               Error_Msg ("Exec_Dir cannot be empty",
                          Exec_Dir.Location);

            else
               --  We check that the specified object directory
               --  does exist.

               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

               declare
                  Dir_Id : constant Name_Id := Name_Find;

               begin
                  Data.Exec_Directory :=
                    Locate_Directory (Dir_Id, Data.Directory);

                  if Data.Exec_Directory = No_Name then
                     Errout.Error_Msg_Name_1 := Dir_Id;
                     Error_Msg
                       ("the exec directory { cannot be found",
                        Data.Location);
                  end if;
               end;
            end if;
         end if;
      end;

      if Current_Verbosity = High then
         if Data.Exec_Directory = No_Name then
            Write_Line ("No exec directory");
         else
            Write_Str ("Exec directory: """);
            Write_Str (Get_Name_String (Data.Exec_Directory));
            Write_Line ("""");
         end if;
      end if;

      --  Look for the source directories

      declare
         Source_Dirs : Variable_Value :=
           Util.Value_Of (Name_Source_Dirs, Data.Decl.Attributes);

      begin

         if Current_Verbosity = High then
            Write_Line ("Starting to look for source directories");
         end if;

         pragma Assert (Source_Dirs.Kind = List,
                          "Source_Dirs is not a list");

         if Source_Dirs.Default then

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

         elsif Source_Dirs.Values = Nil_String then

            --  If Source_Dirs is an empty string list, this means
            --  that this project contains no source.

            if Data.Object_Directory = Data.Directory then
               Data.Object_Directory := No_Name;
            end if;

            Data.Source_Dirs     := Nil_String;
            Data.Sources_Present := False;

         else
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
         end if;

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

      --  Library Dir, Name, Version and Kind

      declare
         Attributes : constant Prj.Variable_Id := Data.Decl.Attributes;

         Lib_Dir : Prj.Variable_Value :=
                     Prj.Util.Value_Of (Snames.Name_Library_Dir, Attributes);

         Lib_Name : Prj.Variable_Value :=
                      Prj.Util.Value_Of (Snames.Name_Library_Name, Attributes);

         Lib_Version : Prj.Variable_Value :=
                         Prj.Util.Value_Of
                           (Snames.Name_Library_Version, Attributes);

         The_Lib_Kind : Prj.Variable_Value :=
                          Prj.Util.Value_Of
                            (Snames.Name_Library_Kind, Attributes);

      begin
         pragma Assert (Lib_Dir.Kind = Single);

         if Lib_Dir.Value = Empty_String then

            if Current_Verbosity = High then
               Write_Line ("No library directory");
            end if;

         else
            --  Find path name, check that it is a directory

            Stringt.String_To_Name_Buffer (Lib_Dir.Value);

            declare
               Dir_Id : constant Name_Id := Name_Find;

            begin
               Data.Library_Dir :=
                 Locate_Directory (Dir_Id, Data.Directory);

               if Data.Library_Dir = No_Name then
                  Error_Msg ("not an existing directory",
                             Lib_Dir.Location);

               elsif Data.Library_Dir = Data.Object_Directory then
                  Error_Msg
                    ("library directory cannot be the same " &
                     "as object directory",
                     Lib_Dir.Location);
                  Data.Library_Dir := No_Name;

               else
                  if Current_Verbosity = High then
                     Write_Str ("Library directory =""");
                     Write_Str (Get_Name_String (Data.Library_Dir));
                     Write_Line ("""");
                  end if;
               end if;
            end;
         end if;

         pragma Assert (Lib_Name.Kind = Single);

         if Lib_Name.Value = Empty_String then
            if Current_Verbosity = High then
               Write_Line ("No library name");
            end if;

         else
            Stringt.String_To_Name_Buffer (Lib_Name.Value);

            if not Is_Letter (Name_Buffer (1)) then
               Error_Msg ("must start with a letter",
                          Lib_Name.Location);

            else
               Data.Library_Name := Name_Find;

               for Index in 2 .. Name_Len loop
                  if not Is_Alphanumeric (Name_Buffer (Index)) then
                     Data.Library_Name := No_Name;
                     Error_Msg ("only letters and digits are allowed",
                                Lib_Name.Location);
                     exit;
                  end if;
               end loop;

               if Data.Library_Name /= No_Name
                 and then Current_Verbosity = High then
                  Write_Str ("Library name = """);
                  Write_Str (Get_Name_String (Data.Library_Name));
                  Write_Line ("""");
               end if;
            end if;
         end if;

         Data.Library :=
           Data.Library_Dir /= No_Name
             and then
           Data.Library_Name /= No_Name;

         if Data.Library then

            if not MLib.Tgt.Libraries_Are_Supported then
               Error_Msg ("?libraries are not supported on this platform",
                          Lib_Name.Location);
               Data.Library := False;

            else
               if Current_Verbosity = High then
                  Write_Line ("This is a library project file");
               end if;

               pragma Assert (Lib_Version.Kind = Single);

               if Lib_Version.Value = Empty_String then
                  if Current_Verbosity = High then
                     Write_Line ("No library version specified");
                  end if;

               else
                  Stringt.String_To_Name_Buffer (Lib_Version.Value);
                  Data.Lib_Internal_Name := Name_Find;
               end if;

               pragma Assert (The_Lib_Kind.Kind = Single);

               if The_Lib_Kind.Value = Empty_String then
                  if Current_Verbosity = High then
                     Write_Line ("No library kind specified");
                  end if;

               else
                  Stringt.String_To_Name_Buffer (The_Lib_Kind.Value);

                  declare
                     Kind_Name : constant String :=
                                   To_Lower (Name_Buffer (1 .. Name_Len));

                     OK : Boolean := True;

                  begin
                     if Kind_Name = "static" then
                        Data.Library_Kind := Static;

                     elsif Kind_Name = "dynamic" then
                        Data.Library_Kind := Dynamic;

                     elsif Kind_Name = "relocatable" then
                        Data.Library_Kind := Relocatable;

                     else
                        Error_Msg
                          ("illegal value for Library_Kind",
                           The_Lib_Kind.Location);
                        OK := False;
                     end if;

                     if Current_Verbosity = High and then OK then
                        Write_Str ("Library kind = ");
                        Write_Line (Kind_Name);
                     end if;
                  end;
               end if;
            end if;
         end if;
      end;

      if Current_Verbosity = High then
         Show_Source_Dirs (Project);
      end if;

      declare
         Naming_Id : constant Package_Id :=
                       Util.Value_Of (Name_Naming, Data.Decl.Packages);

         Naming    : Package_Element;

      begin
         --  If there is a package Naming, we will put in Data.Naming
         --  what is in this package Naming.

         if Naming_Id /= No_Package then
            Naming := Packages.Table (Naming_Id);

            if Current_Verbosity = High then
               Write_Line ("Checking ""Naming"".");
            end if;

            --  Check Specification_Suffix

            declare
               Spec_Suffixs : Array_Element_Id :=
                                Util.Value_Of
                                  (Name_Specification_Suffix,
                                   Naming.Decl.Arrays);
               Suffix  : Array_Element_Id;
               Element : Array_Element;
               Suffix2 : Array_Element_Id;

            begin
               --  If some suffixs have been specified, we make sure that
               --  for each language for which a default suffix has been
               --  specified, there is a suffix specified, either the one
               --  in the project file or if there were noe, the default.

               if Spec_Suffixs /= No_Array_Element then
                  Suffix := Data.Naming.Specification_Suffix;

                  while Suffix /= No_Array_Element loop
                     Element := Array_Elements.Table (Suffix);
                     Suffix2 := Spec_Suffixs;

                     while Suffix2 /= No_Array_Element loop
                        exit when Array_Elements.Table (Suffix2).Index =
                          Element.Index;
                        Suffix2 := Array_Elements.Table (Suffix2).Next;
                     end loop;

                     --  There is a registered default suffix, but no
                     --  suffix specified in the project file.
                     --  Add the default to the array.

                     if Suffix2 = No_Array_Element then
                        Array_Elements.Increment_Last;
                        Array_Elements.Table (Array_Elements.Last) :=
                          (Index => Element.Index,
                           Value => Element.Value,
                           Next  => Spec_Suffixs);
                        Spec_Suffixs := Array_Elements.Last;
                     end if;

                     Suffix := Element.Next;
                  end loop;

                  --  Put the resulting array as the specification suffixs

                  Data.Naming.Specification_Suffix := Spec_Suffixs;
               end if;
            end;

            declare
               Current : Array_Element_Id := Data.Naming.Specification_Suffix;
               Element : Array_Element;

            begin
               while Current /= No_Array_Element loop
                  Element := Array_Elements.Table (Current);
                  String_To_Name_Buffer (Element.Value.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       ("Specification_Suffix cannot be empty",
                        Element.Value.Location);
                  end if;

                  Array_Elements.Table (Current) := Element;
                  Current := Element.Next;
               end loop;
            end;

            --  Check Implementation_Suffix

            declare
               Impl_Suffixs : Array_Element_Id :=
                 Util.Value_Of
                   (Name_Implementation_Suffix,
                    Naming.Decl.Arrays);
               Suffix  : Array_Element_Id;
               Element : Array_Element;
               Suffix2 : Array_Element_Id;
            begin
               --  If some suffixs have been specified, we make sure that
               --  for each language for which a default suffix has been
               --  specified, there is a suffix specified, either the one
               --  in the project file or if there were noe, the default.

               if Impl_Suffixs /= No_Array_Element then
                  Suffix := Data.Naming.Implementation_Suffix;

                  while Suffix /= No_Array_Element loop
                     Element := Array_Elements.Table (Suffix);
                     Suffix2 := Impl_Suffixs;

                     while Suffix2 /= No_Array_Element loop
                        exit when Array_Elements.Table (Suffix2).Index =
                          Element.Index;
                        Suffix2 := Array_Elements.Table (Suffix2).Next;
                     end loop;

                     --  There is a registered default suffix, but no
                     --  suffix specified in the project file.
                     --  Add the default to the array.

                     if Suffix2 = No_Array_Element then
                        Array_Elements.Increment_Last;
                        Array_Elements.Table (Array_Elements.Last) :=
                          (Index => Element.Index,
                           Value => Element.Value,
                           Next  => Impl_Suffixs);
                        Impl_Suffixs := Array_Elements.Last;
                     end if;

                     Suffix := Element.Next;
                  end loop;

                  --  Put the resulting array as the implementation suffixs

                  Data.Naming.Implementation_Suffix := Impl_Suffixs;
               end if;
            end;

            declare
               Current : Array_Element_Id := Data.Naming.Implementation_Suffix;
               Element : Array_Element;

            begin
               while Current /= No_Array_Element loop
                  Element := Array_Elements.Table (Current);
                  String_To_Name_Buffer (Element.Value.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       ("Implementation_Suffix cannot be empty",
                        Element.Value.Location);
                  end if;

                  Array_Elements.Table (Current) := Element;
                  Current := Element.Next;
               end loop;
            end;

            --  Get the exceptions, if any

            Data.Naming.Specification_Exceptions :=
              Util.Value_Of
                (Name_Specification_Exceptions,
                 In_Arrays => Naming.Decl.Arrays);

            Data.Naming.Implementation_Exceptions :=
              Util.Value_Of
                (Name_Implementation_Exceptions,
                 In_Arrays => Naming.Decl.Arrays);
         end if;
      end;

      Projects.Table (Project) := Data;
   end Language_Independent_Check;

   ----------------------
   -- Locate_Directory --
   ----------------------

   function Locate_Directory
     (Name   : Name_Id;
      Parent : Name_Id)
      return   Name_Id
   is
      The_Name   : constant String := Get_Name_String (Name);
      The_Parent : constant String :=
                     Get_Name_String (Parent) & Dir_Sep;

      The_Parent_Last : Positive := The_Parent'Last;

   begin
      if The_Parent'Length > 1
        and then (The_Parent (The_Parent_Last - 1) = Dir_Sep
                    or else The_Parent (The_Parent_Last - 1) = '/')
      then
         The_Parent_Last := The_Parent_Last - 1;
      end if;

      if Current_Verbosity = High then
         Write_Str ("Locate_Directory (""");
         Write_Str (The_Name);
         Write_Str (""", """);
         Write_Str (The_Parent);
         Write_Line (""")");
      end if;

      if Is_Absolute_Path (The_Name) then
         if Is_Directory (The_Name) then
            return Name;
         end if;

      else
         declare
            Full_Path : constant String :=
                          The_Parent (The_Parent'First .. The_Parent_Last) &
                                                                     The_Name;

         begin
            if Is_Directory (Full_Path) then
               Name_Len := Full_Path'Length;
               Name_Buffer (1 .. Name_Len) := Full_Path;
               return Name_Find;
            end if;
         end;

      end if;

      return No_Name;
   end Locate_Directory;

   ------------------
   -- Path_Name_Of --
   ------------------

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
     (File_Name          : Name_Id;
      Path_Name          : Name_Id;
      Project            : Project_Id;
      Data               : in out Project_Data;
      Location           : Source_Ptr;
      Current_Source     : in out String_List_Id)
   is
      Unit_Name    : Name_Id;
      Unit_Kind    : Spec_Or_Body;
      Needs_Pragma : Boolean;
      The_Location : Source_Ptr := Location;

   begin
      --  Find out the unit name, the unit kind and if it needs
      --  a specific SFN pragma.

      Get_Unit
        (File_Name    => File_Name,
         Naming       => Data.Naming,
         Unit_Name    => Unit_Name,
         Unit_Kind    => Unit_Kind,
         Needs_Pragma => Needs_Pragma);

      if Unit_Name = No_Name then
         if Current_Verbosity = High then
            Write_Str  ("   """);
            Write_Str  (Get_Name_String (File_Name));
            Write_Line (""" is not a valid source file name (ignored).");
         end if;

      else
         --  Put the file name in the list of sources of the project

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
            --  in the unit list is a unit of a project we are extending.

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

                  if The_Location = No_Location then
                     The_Location := Projects.Table (Project).Location;
                  end if;

                  Errout.Error_Msg_Name_1 := Unit_Name;
                  Error_Msg ("duplicate source {", The_Location);

                  Errout.Error_Msg_Name_1 :=
                    Projects.Table
                      (The_Unit_Data.File_Names (Unit_Kind).Project).Name;
                  Errout.Error_Msg_Name_2 :=
                    The_Unit_Data.File_Names (Unit_Kind).Path;
                  Error_Msg ("\   project file {, {", The_Location);

                  Errout.Error_Msg_Name_1 := Projects.Table (Project).Name;
                  Errout.Error_Msg_Name_2 := Path_Name;
                  Error_Msg ("\   project file {, {", The_Location);

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
