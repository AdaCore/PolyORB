------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . P A R S                             --
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
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with Osint;                      use Osint;
with Output;                     use Output;
with Prj.Com;                    use Prj.Com;
with Prj.Ext;
with Prj.Util;
with Types;

package body Prj.Pars is

   Current_Verbosity : Verbosity := Default;

   Project_Error : exception;
   Project_File_Extension : constant String_Access :=
                              new String'(".apr");

   Package_Ref : String_Access;
   --  Contains the name of the package of the tool in use.

   type Token_Type is
     (Tok_Project,
      Tok_Is,
      Tok_Modifying,
      Tok_With,
      Tok_External,
      Tok_Package,
      Tok_Renames,
      Tok_End,
      Tok_Case,
      Tok_When,
      Tok_Others,

      Tok_Identifier,
      Tok_Comma,
      Tok_Semi_Colon,
      Tok_Assign,
      Tok_Literal_String,
      Tok_Left_Par,
      Tok_Right_Par,
      Tok_Ampersand,
      Tok_Vertical_Bar,
      Tok_Arrow,
      Tok_Dot,

      Tok_EOF,

      Tok_Invalid);

   ------------------------------------
   -- Local Packages and Subprograms --
   ------------------------------------

   package Scan is

      type Source_Id is limited private;
      --  Identifies a project file.

      Nil_Source_Id : constant Source_Id;

      procedure Initialize
        (Source              : in out Source_Id;
         Path_Name           : String;
         Canonical_Path_Name : out String_Access);
      --  Open a project file

      procedure Finalize (Source : in out Source_Id);
      --  Close the project file

      procedure Get
        (Token  : out Token_Type;
         Source : in Source_Id);
      --  Get the next token

      function Current_Token_Of (Source : Source_Id) return Token_Type;
      --  Returns the last token returned by Get

      function Value_Of (Source : in Source_Id) return String;
      --  Return the name of an identifier or the value of a literal string.
      --  The current token must be an identifier or a literal string,
      --  otherwise Program_Error is raised.

      procedure Error
        (Source : in Source_Id;
         Text   : String;
         Fatal  : Boolean := True);
      --  Display an error and raise Project_Error, if Fatal

   private
      type Source_Data is record
         Path_Name           : String_Access;
         File                : Prj.Util.Text_File;
         Buffer              : String (1 .. 1_000);
         Buffer_Len          : Natural := 0;
         Buffer_Cursor       : Natural := 0;
         End_Of_File_Reached : Boolean := False;
         Line_Number         : Natural := 0;
         Line                : String (1 .. 250);
         Last                : Natural;
         Cursor              : Natural;
         Current             : Token_Type;
         Value               : String (1 .. 250);
         Last_Value          : Natural;
      end record;

      type Source_Id is access Source_Data;

      Nil_Source_Id : constant Source_Id := null;
   end Scan;

   package String_Expression is

      function Value
        (Ref    : Reference;
         Pkg    : Package_List;
         Source : Scan.Source_Id)
         return   String;
      --  Parse a String Expression:
      --
      --   string_expression ::=
      --     string_term {& string_term}
      --
      --   string_term ::=
      --     literal_string |
      --     <single_variable_>name |
      --     external_value
      --
      --   external_value ::=
      --     _external_ (string_expression [,string_expression])
      --
      --  Before: the current token is the first of the expression.
      --  After:  the current token is the first one after the expression.

   end String_Expression;

   package Declarations is

      procedure Parse
        (Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean;
         Level       : Natural := 0);
      --  Parse a list of declarative items:
      --    {declarative_item}

   end Declarations;

   package Naming_Scheme is

      procedure Check
        (Source : Scan.Source_Id;
         Ref    : Reference);
      --  Check that the Naming Scheme of a project is legal. Find the
      --  object directory, the source directories, and the source files.
      --  Check the source files against the Naming Scheme.

   end Naming_Scheme;

   function Locate_Directory
     (Name   : String_Access;
      Parent : String_Access)
      return   String_Access;
   --  Locate a directory.
   --  Returns null if directory does not exist.

   procedure Parse_Single_Project
     (Ref             : in out Reference;
      Modified_By     : Reference;
      Path_Name       : String;
      Original_Source : Scan.Source_Id);
   --  Parse a project file.
   --  Recursive procedure: it calls itself for imported and
   --  modified projects.

   function Path_Name_Of
     (File_Name : String;
      Directory : String)
      return      String;
   --  Returns the path name of a (non project) file.
   --  Returns an empty string if file cannot be found.

   function Path_Name_Of
     (Project_File_Name : String;
      Directory         : String;
      Referred_In       : Reference)
      return              String;
   --  Returns the path name of a project file.
   --  Raises Project_Error if project file cannot be found.

   function Immediate_Directory_Of
     (Path_Name : String)
      return      String;
   --  Get the directory of the file with the specified path name.
   --  This includes the directory separator as the last character.
   --  Returns "./" if Path_Name contains no directory separator.

   function Simple_File_Name_Of
     (Path_Name : String)
      return      String;
   --  Returns the name of a file with the specified path name
   --  with no directory information.

   package body Declarations is

      --  Comments are required for subprograms below ???

      procedure Case_Construction
        (Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean;
         Level       : Natural);

      procedure Destroy (Value : in out Variable_Value);

      procedure Dump_Single_Variables
        (Name      : String;
         Variables : Variable_List);

      procedure Package_Declaration
        (Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean;
         Level       : Natural);

      procedure Parse_Array_Component_Assignment
        (Name        : String;
         Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean);

      procedure Parse_Variable_Assignment
        (Name        : String;
         Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean);

      procedure Parse_Variable_Value
        (Result      : in out Variable_Value;
         Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean);

      -----------------------
      -- Case_Construction --
      -----------------------

      procedure Case_Construction
        (Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean;
         Level       : Natural)
      is
         Token                 : Token_Type;
         Follow_Current_Branch : Boolean := False;
         One_Branch_Followed   : Boolean := False;

      begin
         Scan.Get (Token, Source);

         declare
            Case_Switch : constant String :=
                            String_Expression.Value (Ref, Pkg, Source);
         begin
            Token := Scan.Current_Token_Of (Source);

            if Token /= Tok_Is then
               Scan.Error (Source,
                           Text => "Expected ""is"".");
            end if;

            Scan.Get (Token, Source);

            loop
               Token := Scan.Current_Token_Of (Source);
               exit when Token /= Tok_When;
               Scan.Get (Token, Source);

               if Token = Tok_Others then
                  Follow_Current_Branch := Do_Not_Skip and then
                    (not One_Branch_Followed);
                  Scan.Get (Token, Source);

                  if Token /= Tok_Arrow then
                     Scan.Error (Source,
                                 Text => "Expected ""=>"".");
                  end if;

                  Parse (Ref, Pkg, Source, Follow_Current_Branch, Level);
                  exit;

               else
                  loop
                     declare
                        Label : constant String :=
                                  String_Expression.Value (Ref, Pkg, Source);

                     begin
                        Follow_Current_Branch :=
                          Do_Not_Skip
                            and then
                              (Follow_Current_Branch
                                 or else ((not One_Branch_Followed)
                                            and then
                                          (Label = Case_Switch)));
                     end;

                     Token := Scan.Current_Token_Of (Source);
                     exit when Token /= Tok_Vertical_Bar;
                     Scan.Get (Token, Source);
                  end loop;

                  if Token /= Tok_Arrow then
                     Scan.Error (Source,
                                 Text => "Expected ""=>"".");
                  end if;

                  Parse (Ref, Pkg, Source, Follow_Current_Branch, Level);

                  if Follow_Current_Branch then
                     One_Branch_Followed := True;
                     Follow_Current_Branch := False;
                  end if;
               end if;
            end loop;

            Token := Scan.Current_Token_Of (Source);

            if Token /= Tok_End then
               Scan.Error (Source,
                           Text => "Expected ""end case;"".");
            end if;

            Scan.Get (Token, Source);

            if Token /= Tok_Case then
               Scan.Error (Source,
                           Text => "Expected ""case;"".");
            end if;

            Scan.Get (Token, Source);

            if Token /= Tok_Semi_Colon then
               Scan.Error (Source,
                           Text => "Expected ';'.");
            end if;
         end;
      end Case_Construction;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (Value : in out Variable_Value) is
      begin
         case Value.Kind is
            when Undefined =>
               null;

            when List =>
               declare
                  Current : String_List := Value.Values;
                  Next : String_List;

               begin
                  while Current /= null loop
                     Next := Current.Next;
                     Free (Current.Value);
                     Free (Current);
                     Current := Next;
                  end loop;
               end;

            when Single =>
               Free (Value.Value);
         end case;

         Value := Nil_Variable_Value;
      end Destroy;

      ---------------------------
      -- Dump_Single_Variables --
      ---------------------------

      procedure Dump_Single_Variables
        (Name      : String;
         Variables : Variable_List)
      is
         Current : Variable_List := Variables;

      begin
         Write_Str (Name);
         Write_Line (":");
         while Current /= null loop
            if Current.Value.Kind = Single then
               Write_Str ("  ");
               Write_Str (Current.Name.all);
               Write_Str (" = """);
               Write_Str (Current.Value.Value.all);
               Write_Line (""";");
            end if;

            Current := Current.Next;
         end loop;

         Write_Str ("end ");
         Write_Str (Name);
         Write_Line (";");
      end Dump_Single_Variables;

      -------------------------
      -- Package_Declaration --
      -------------------------

      procedure Package_Declaration
        (Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean;
         Level       : Natural)
      is
         Token           : Token_Type;
         New_Pkg         : Package_List;
         New_Do_Not_Skip : Boolean;

      begin
         Scan.Get (Token, Source);

         if Token /= Tok_Identifier then
            Scan.Error (Source,
                        Text => "Expected identifier.");
         end if;

         declare
            Name : constant String := To_Lower (Scan.Value_Of (Source));
            List : Package_List;

         begin
            --  We skip packages that are at the ground level and that are not
            --  Naming or the package associated with the tool.
            --
            New_Do_Not_Skip := Do_Not_Skip and then
              (Level /= 0 or else Name = "naming"
               or else Name = Package_Ref.all);

            if New_Do_Not_Skip then

               --  Check for package name duplication

               if Pkg = null then
                  List := Ref.Decl.Packages;
               else
                  List := Pkg.Decl.Packages;
               end if;

               if List = null then
                  List := new Package_Element;
                  List.Name := new String'(Name);

                  if Pkg = null then
                     Ref.Decl.Packages := List;
                  else
                     List.Parent := Pkg;
                     Pkg.Decl.Packages := List;
                  end if;

                  New_Pkg := List;

               else
                  loop
                     if List.Name.all = Name then
                        Scan.Error (Source,
                                    Text => "Duplicate package name.");
                     end if;

                     exit when List.Next = null;
                     List := List.Next;
                  end loop;

                  List.Next := new Package_Element;
                  New_Pkg := List.Next;
                  New_Pkg.Name := new String'(Name);

                  if Pkg /= null then
                     New_Pkg.Parent := Pkg;
                  end if;
               end if;
            end if;

            Scan.Get (Token, Source);

            if Token = Tok_Renames then
               declare
                  Imported : Reference;
                  Ren_Pkg : Package_List;

               begin
                  Scan.Get (Token, Source);

                  if Token /= Tok_Identifier then
                     Scan.Error (Source,
                                 Text =>
                                   "Expected Identifier.");
                  end if;

                  declare
                     Current : Reference_List := Ref.Imported_Projects;
                     Name : constant String :=
                              To_Lower (Scan.Value_Of (Source));
                  begin
                     while Current /= null loop
                        if Current.Ref.Name.all = Name then
                           if Current_Verbosity = High then
                              Write_Str ("Found ");
                              Write_Line (Name);
                           end if;

                           Imported := Current.Ref;

                           if Current_Verbosity = High then
                              Dump_Single_Variables
                                (Name => Imported.Name.all,
                                 Variables => Imported.Decl.Variables);
                           end if;

                           exit;

                        else
                           Current := Current.Next;
                        end if;
                     end loop;

                     if Imported = null then
                        Scan.Error
                          (Source,
                           Text =>
                             "Not a project imported by this project.");
                     end if;
                  end;

                  Scan.Get (Token, Source);

                  if Token /= Tok_Dot then
                     Scan.Error (Source,
                                 Text => "Expected '.'.");
                  end if;

                  loop
                     Scan.Get (Token, Source);

                     if Token /= Tok_Identifier then
                        Scan.Error (Source,
                                    Text =>
                                      "Expected Identifier.");
                     end if;

                     declare
                        Name    : constant String :=
                                    To_Lower (Scan.Value_Of (Source));
                        Current : Package_List;

                     begin
                        if Ren_Pkg = null then
                           Current := Imported.Decl.Packages;
                        else
                           Current := Ren_Pkg.Decl.Packages;
                        end if;

                        loop
                           if Current = null then
                              Scan.Error (Source,
                                          Text =>
                                            "Unknown package.");
                              exit;
                           end if;

                           if Current.Name.all = Name then
                              if Current_Verbosity = High then
                                 Write_Str ("Found ");
                                 Write_Line (Name);
                              end if;

                              Ren_Pkg := Current;

                              if Current_Verbosity = High then
                                 Dump_Single_Variables
                                   (Name => Ren_Pkg.Name.all,
                                    Variables => Ren_Pkg.Decl.Variables);
                              end if;

                              exit;

                           else
                              Current := Current.Next;
                           end if;
                        end loop;
                     end;

                     Scan.Get (Token, Source);
                     exit when Token /= Tok_Dot;
                  end loop;

                  New_Pkg.Decl := Ren_Pkg.Decl;

                  if Current_Verbosity = High then
                     Dump_Single_Variables
                       (Name => New_Pkg.Name.all,
                        Variables => New_Pkg.Decl.Variables);
                  end if;
               end;

            elsif Token = Tok_Is then

               --  We look for declarative items, increasing the level by one.

               Parse (Ref, New_Pkg, Source, New_Do_Not_Skip, Level + 1);
               Token := Scan.Current_Token_Of (Source);

               if Token /= Tok_End then
                  Scan.Error (Source,
                              Text => "Expected ""end"".");
               end if;

               Scan.Get (Token, Source);

               if Token /= Tok_Identifier then
                  Scan.Error (Source,
                              Text => "Expected identifier.");
               end if;

               declare
                  End_Name : constant String :=
                               To_Lower (Scan.Value_Of (Source));

               begin
                  if Name /= End_Name then
                     Scan.Error (Source,
                                 Text => "Expected """ & Name & """.");
                  end if;
               end;

               Scan.Get (Token, Source);

            else
               Scan.Error (Source,
                           Text => "Expected ""is"" or ""renames"".");
            end if;

            if Token /= Tok_Semi_Colon then
               Scan.Error (Source,
                           Text => "Expected ';'.");
            end if;
         end;
      end Package_Declaration;

      -----------
      -- Parse --
      -----------

      procedure Parse
        (Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean;
         Level       : Natural := 0)
      is
         Token : Token_Type;

      begin
         loop
            --  We are always positioned at the token that precedes
            --  the first token of the declarative element.

            Scan.Get (Token, Source);

            case Token is
               when Tok_Identifier =>
                  declare
                     Name : constant String :=
                              To_Lower (Scan.Value_Of (Source));

                  begin
                     if Current_Verbosity = High then
                        Write_Line ("Identifier");
                     end if;

                     Scan.Get (Token, Source);

                     --  We must distinguished between variable assignment
                     --  (Tok_Assign) and array component assignment
                     --  (Tok_Left_Par).

                     case Token is
                        when Tok_Assign =>
                           if Current_Verbosity = High then
                              Write_Line ("Variable Assignment");
                           end if;

                           Parse_Variable_Assignment
                             (Name, Ref, Pkg, Source, Do_Not_Skip);

                        when Tok_Left_Par =>
                           if Current_Verbosity = High then
                              Write_Line ("Array Component Assignment");
                           end if;

                           Parse_Array_Component_Assignment
                             (Name, Ref, Pkg, Source, Do_Not_Skip);

                        when others =>
                           Scan.Error (Source,
                                       Text => "Expected "":="" or '('.");
                     end case;
                  end;

               when Tok_Package =>
                  Package_Declaration (Ref, Pkg, Source, Do_Not_Skip, Level);

               when Tok_Case =>
                  Case_Construction (Ref, Pkg, Source, Do_Not_Skip, Level);

               when others =>
                  exit;

                  --  We are leaving Parse with the positionned at the first
                  --  token after the list of declarative items.
                  --  It could be "end" or "when".

            end case;
         end loop;

      end Parse;

      --------------------------------------
      -- Parse_Array_Component_Assignment --
      --------------------------------------

      procedure Parse_Array_Component_Assignment
        (Name        : String;
         Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean)
      is
         Token     : Token_Type;
         List      : Array_List;
         Component : Array_Component_Reference;

      begin
         if Do_Not_Skip then
            if Pkg = null then
               List := Ref.Decl.Arrays;
            else
               List := Pkg.Decl.Arrays;
            end if;

            if List = null then
               List := new Array_Element;
               List.Name := new String'(Name);

               if Pkg = null then
                  Ref.Decl.Arrays := List;
               else
                  Pkg.Decl.Arrays := List;
               end if;

            else
               loop
                  exit when List.Name.all = Name;

                  if List.Next = null then
                     List.Next := new Array_Element;
                     List := List.Next;
                     List.Name := new String'(Name);
                     exit;
                  else
                     List := List.Next;
                  end if;
               end loop;
            end if;
         end if;

         Scan.Get (Token, Source);

         declare
            Index : constant String :=
                      String_Expression.Value (Ref, Pkg, Source);

         begin
            Token := Scan.Current_Token_Of (Source);

            if Token /= Tok_Right_Par then
               Scan.Error (Source,
                           Text => "Expected ')'.");
            end if;

            Scan.Get (Token, Source);

            if Token /= Tok_Assign then
               Scan.Error (Source,
                           Text => "Expected "":="".");
            end if;

            if Do_Not_Skip then
               if List.Value = null then
                  List.Value := new Array_Component'
                    (Index => new String'(Index),
                     Value => Nil_Variable_Value,
                     Next => null);
                  Component := List.Value;

               else
                  Component := List.Value;
                  loop
                     exit when Component.Index.all = Index;

                     if Component.Next = null then
                        Component.Next := new Array_Component'
                          (Index => new String'(Index),
                           Value => Nil_Variable_Value,
                           Next => null);
                        Component := Component.Next;
                        exit;

                     else
                        Component := Component.Next;
                     end if;
                  end loop;
               end if;

               Parse_Variable_Value
                (Result      => Component.Value,
                 Ref         => Ref,
                 Pkg         => Pkg,
                 Source      => Source,
                 Do_Not_Skip => Do_Not_Skip);

            else
               declare
                  Dummy_Value : Variable_Value;

               begin
                  Parse_Variable_Value
                    (Result      => Dummy_Value,
                     Ref         => Ref,
                     Pkg         => Pkg,
                     Source      => Source,
                     Do_Not_Skip => Do_Not_Skip);
               end;
            end if;
         end;
      end Parse_Array_Component_Assignment;

      -------------------------------
      -- Parse_Variable_Assignment --
      -------------------------------

      procedure Parse_Variable_Assignment
        (Name        : String;
         Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean)
      is
         Variable : Variable_List;

      begin
         if Do_Not_Skip then
            if Pkg = null then
               Variable := Ref.Decl.Variables;
            else
               Variable := Pkg.Decl.Variables;
            end if;

            if Variable = null then
               Variable := new Variable_Element;

               if Pkg = null then
                  Ref.Decl.Variables := Variable;
               else
                  Pkg.Decl.Variables := Variable;
               end if;

            else
               loop
                  exit when Variable.Name.all = Name;

                  if Variable.Next = null then
                     Variable.Next := new Variable_Element;
                     Variable := Variable.Next;
                     exit;
                  else
                     Variable := Variable.Next;
                  end if;
               end loop;
            end if;

            if Variable.Name = null then
               Variable.Name := new String'(Name);
            end if;

            Parse_Variable_Value
              (Result      => Variable.Value,
               Ref         => Ref,
               Pkg         => Pkg,
               Source      => Source,
               Do_Not_Skip => Do_Not_Skip);

         else
            declare
               Dummy_Value : Variable_Value;

            begin
               Parse_Variable_Value
                 (Result      => Dummy_Value,
                  Ref         => Ref,
                  Pkg         => Pkg,
                  Source      => Source,
                  Do_Not_Skip => Do_Not_Skip);
            end;
         end if;

      end Parse_Variable_Assignment;

      --------------------------
      -- Parse_Variable_Value --
      --------------------------

      procedure Parse_Variable_Value
        (Result      : in out Variable_Value;
         Ref         : Reference;
         Pkg         : Package_List;
         Source      : Scan.Source_Id;
         Do_Not_Skip : Boolean)
      is
         Token : Token_Type;

      begin
         Scan.Get (Token, Source);

         if Token = Tok_Left_Par then

            --  Result is a list

            declare
               New_Value : Variable_Value :=
                             (Kind => List, Values => new String_Element);
               Current   : String_List := New_Value.Values;

            begin
               Scan.Get (Token, Source);

               declare
                  Value : constant String :=
                            String_Expression.Value (Ref, Pkg, Source);

               begin
                  Current.Value := new String'(Value);
               end;

               loop
                  Token := Scan.Current_Token_Of (Source);

                  case Token is
                     when Tok_Right_Par =>
                        Scan.Get (Token, Source);
                        exit;

                     when Tok_Comma =>
                        Scan.Get (Token, Source);

                        declare
                           Value : constant String :=
                                    String_Expression.Value (Ref, Pkg, Source);

                        begin
                           Current.Next := new String_Element'
                             (Value => new String'(Value),
                              Next  => null);
                           Current := Current.Next;
                        end;

                     when others =>
                        Scan.Error
                          (Source, Text => "Expected ',' or ')'.");
                  end case;
               end loop;

               if Do_Not_Skip then
                  Destroy (Result);
                  Result := New_Value;
               else
                  Destroy (New_Value);
               end if;
            end;

         --  Result is a single string

         else
            declare
               Value : constant String := String_Expression.Value
                                            (Ref, Pkg, Source);

            begin
               if Do_Not_Skip then
                  Destroy (Result);
                  Result :=
                    (Kind => Single,
                     Value => new String'(Value));
               end if;
            end;
         end if;

         Token := Scan.Current_Token_Of (Source);

         if Token /= Tok_Semi_Colon then
            Scan.Error (Source,
                        Text => "Expected ';'.");
         end if;
      end Parse_Variable_Value;

   end Declarations;

   ----------------------------
   -- Immediate_Directory_Of --
   ----------------------------

   function Immediate_Directory_Of (Path_Name : String) return String is
      Slash_Position : constant Natural :=
                         Index (Source  => Path_Name,
                                Pattern => "/",
                                Going   => Ada.Strings.Backward);

      Separator_Position : constant Natural :=
                             Index (Source  => Path_Name,
                                    Pattern => "" & Directory_Separator,
                                    Going   => Ada.Strings.Backward);

      End_Of_Directory : constant Natural :=
                           Integer'Max (Slash_Position, Separator_Position);

   begin
      if End_Of_Directory /= 0 then
         return Path_Name (Path_Name'First .. End_Of_Directory);
      else
         return "./";
      end if;
   end Immediate_Directory_Of;

   ----------------------
   -- Locate_Directory --
   ----------------------

   function Locate_Directory
     (Name   : String_Access;
      Parent : String_Access)
      return   String_Access
   is
   begin
      if Name (Name'First) = '/' then
         if Is_Directory (Name.all) then
            return Name;
         end if;

      else
         if Is_Directory (Parent.all & Name.all) then
            return new String'(Parent.all & Name.all);
         end if;
      end if;

      return null;
   end Locate_Directory;

   -------------------
   -- Naming_Scheme --
   -------------------

   package body Naming_Scheme is

      use Ada.Strings;

      --  Comments needed for procedures below ???

      procedure Check
        (Naming : Naming_Data;
         Source : Scan.Source_Id;
         OK     : out Boolean);

      procedure Check
        (Name : String;
         Unit : out String_Access);

      procedure Get_Unit
        (File_Name    : String;
         Naming       : Naming_Data;
         Source       : Scan.Source_Id;
         Unit_Name    : out String_Access;
         Unit_Kind    : out Spec_Or_Body;
         Needs_Pragma : out Boolean);

      function Is_Illegal_Append (This : String) return Boolean;

      procedure Record_Source
        (File_Name        : String_Access;
         Path_Name        : String_Access;
         Ref              : Reference;
         Source           : Scan.Source_Id;
         Error_If_Invalid : Boolean;
         Current_Source   : in out String_List);

      procedure Show_Source_Dirs (Ref : in Reference);

      -----------
      -- Check --
      -----------

      procedure Check
        (Naming : Naming_Data;
         Source : Scan.Source_Id;
         OK     : out Boolean)
      is
      begin
         OK := True;

         if Naming /= Standard_Naming_Data then
            declare
               Dot_Replacement      : constant String :=
                                        Naming.Dot_Replacement.all;
               Specification_Append : constant String :=
                                        Naming.Specification_Append.all;
               Body_Append          : constant String :=
                                        Naming.Body_Append.all;
               Separate_Append      : constant String :=
                                        Naming.Separate_Append.all;

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
                                    (Dot_Replacement
                                      (Dot_Replacement'First + 1))))
                 or else (Dot_Replacement'Length > 1
                            and then
                              Index (Source => Dot_Replacement,
                                     Pattern => ".") /= 0)
               then
                  Scan.Error (Source,
                              Text =>
                                '"' & Dot_Replacement &
                              """ is illegal for Dot_Replacement.");
                  OK := False;
               end if;

               --  Appends cannot
               --   - be empty
               --   - start with an alphanumeric
               --   - start with an '_' followed by an alphanumeric

               if Is_Illegal_Append (Specification_Append) then
                  Scan.Error (Source,
                              Text =>
                                '"' & Specification_Append &
                              """ is illegal for Specification_Append.");
                  OK := False;
               end if;

               if Is_Illegal_Append (Body_Append) then
                  Scan.Error (Source,
                              Text =>
                                '"' & Body_Append &
                              """ is illegal for Body_Append.");
                  OK := False;
               end if;

               if Body_Append /= Separate_Append then
                  if Is_Illegal_Append (Separate_Append) then
                     Scan.Error (Source,
                                 Text =>
                                   '"' & Separate_Append &
                                 """ is illegal for Separate_Append.");
                     OK := False;
                  end if;
               end if;

               --  Specification_Append cannot have the same termination as
               --  Body_Append or Separate_Append

               if Specification_Append'Length >= Body_Append'Length
                 and then
                   Body_Append (Body_Append'Last -
                                  Specification_Append'Length + 1 ..
                                                          Body_Append'Last) =
                   Specification_Append
               then
                  Scan.Error
                    (Source,
                     Text => "Body_Append (""" &
                              Body_Append &
                              """) cannot end with" &
                              " Specification_Append (""" &
                              Specification_Append & """).");
                  OK := False;
               end if;

               if Specification_Append'Length >=
                    Separate_Append'Length
               and then
                 Separate_Append
                   (Separate_Append'Last - Specification_Append'Length + 1 ..
                     Separate_Append'Last) = Specification_Append
               then
                  Scan.Error
                    (Source,
                     Text => "Separate_Append (""" &
                              Separate_Append &
                              """) cannot end with" &
                              " Specification_Append (""" &
                              Specification_Append & """).");
                  OK := False;
               end if;
            end;
         end if;
      end Check;

      procedure Check
        (Name : String;
         Unit : out String_Access)
      is
         Need_Letter     : Boolean := True;
         Last_Underscore : Boolean := False;
         OK              : Boolean := Name'Length > 0;

      begin
         for Index in Name'Range loop
            if Need_Letter then
               if Is_Letter (Name (Index)) then
                  Need_Letter := False;
               else
                  OK := False;
                  if Current_Verbosity = High then
                     Write_Int  (Types.Int (Index));
                     Write_Str  (": '");
                     Write_Char (Name (Index));
                     Write_Line ("' is not a letter.");
                  end if;

                  exit;
               end if;

            elsif Last_Underscore
              and then (Name (Index) = '_' or else Name (Index) = '.')
            then
               OK := False;

               if Current_Verbosity = High then
                  Write_Int  (Types.Int (Index));
                  Write_Str  (": '");
                  Write_Char (Name (Index));
                  Write_Line ("' is illegal here.");
               end if;

               exit;

            elsif Name (Index) = '.' then
               Need_Letter := True;

            elsif Name (Index) = '_' then
               Last_Underscore := True;

            else
               Last_Underscore := False;

               if not Is_Alphanumeric (Name (Index)) then
                  OK := False;

                  if Current_Verbosity = High then
                     Write_Int  (Types.Int (Index));
                     Write_Str  (": '");
                     Write_Char (Name (Index));
                     Write_Line ("' is not alphanumeric.");
                  end if;

                  exit;
               end if;
            end if;
         end loop;

         OK := OK and then not Need_Letter and then not Last_Underscore;

         if OK then
            Unit := new String'(Name);
         else
            Unit := null;
         end if;
      end Check;

      procedure Check
        (Source : Scan.Source_Id;
         Ref    : Reference)
      is
         Last_Source_Dir : String_List;

         procedure Check_Unit_Names (List : in Array_Component_Reference);

         procedure Find_Source_Dirs (From : in String_Access);
         --  Find all the directories that are Path and
         --  its subdirectories (recursively).

         ----------------------
         -- Check_Unit_Names --
         ----------------------

         procedure Check_Unit_Names (List : Array_Component_Reference) is
            Current   : Array_Component_Reference;
            Unit_Name : String_Access;

         begin
            Current := List;
            while Current /= null loop
               Check (Current.Index.all, Unit_Name);

               if Unit_Name = null then
                  Scan.Error
                    (Source,
                     Text => Current.Index.all &
                             " is not a valid unit name.");

               else
                  Current.Index := Unit_Name;
               end if;

               Current := Current.Next;
            end loop;
         end Check_Unit_Names;

         ----------------------
         -- Find_Source_Dirs --
         ----------------------

         procedure Find_Source_Dirs (From : String_Access) is

            procedure Recursive_Find_Dirs (Path : in String);

            -------------------------
            -- Recursive_Find_Dirs --
            -------------------------

            procedure Recursive_Find_Dirs (Path : in String) is
               Dir  : Dir_Type;
               Name : String (1 .. 250);
               Last : Natural;

            begin
               if Current_Verbosity = High then
                  Write_Str  ("   ");
                  Write_Line (Path);
               end if;

               if Last_Source_Dir = null then
                  Ref.Source_Dirs := new String_Element'
                    (Value => new String'(Path),
                     Next => null);
                  Last_Source_Dir := Ref.Source_Dirs;

               else
                  Last_Source_Dir.Next := new String_Element'
                    (Value => new String'(Path),
                     Next => null);
                  Last_Source_Dir := Last_Source_Dir.Next;
               end if;

               Open (Dir, Path);

               loop
                  Read (Dir, Name, Last);
                  exit when Last = 0;

                  if Current_Verbosity = High then
                     Write_Str  ("   Checking ");
                     Write_Line (Name (1 .. Last));
                  end if;

                  if Name (1) /= '.' then
                     declare
                        Path_Name : constant String_Access :=
                                      new String'
                                        (Path &
                                         Directory_Separator &
                                         Name (1 .. Last));

                     begin
                        if Is_Directory (Path_Name.all) then
                           Recursive_Find_Dirs (Path_Name.all);
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
            if From'Length >= 3
              and then From (From'Last - 2 .. From'Last) = "/**"
            then
               declare
                  Root : constant String_Access :=
                    Locate_Directory
                      (new String'(From (From'First .. From'Last - 3)),
                       Ref.Directory);

               begin
                  if Root = null then
                     Scan.Error
                       (Source,
                        Text => From.all & " is not a valid directory.");

                  else
                     if Current_Verbosity = High then
                        Write_Line ("Looking for source directories:");
                     end if;

                     Recursive_Find_Dirs (Root.all);

                     if Current_Verbosity = High then
                        Write_Line ("End of looking for source directories.");
                     end if;
                  end if;
               end;

            else
               declare
                  Path_Name : constant String_Access :=
                                Locate_Directory
                                 (From, Ref.Directory);

               begin
                  if Path_Name = null then
                     Scan.Error
                       (Source,
                        Text => From.all & " is not a valid directory.");
                  end if;

                  if Last_Source_Dir = null then
                     Ref.Source_Dirs := new String_Element'
                       (Value => Path_Name,
                        Next => null);
                     Last_Source_Dir := Ref.Source_Dirs;

                  else
                     Last_Source_Dir.Next := new String_Element'
                       (Value => Path_Name,
                        Next => null);
                     Last_Source_Dir := Last_Source_Dir.Next;
                  end if;
               end;
            end if;

         end Find_Source_Dirs;

      --  Start of processig for Check

      begin
         declare
            Object_Dir : Variable_Value :=
                           Util.Value_Of ("object_dir", Ref.Decl.Variables);

         begin
            case Object_Dir.Kind is
               when Undefined =>
                  Ref.Object_Directory := Ref.Directory;

               when List =>
                  Scan.Error (Source,
                              Text => "Object_Dir cannot be a list.");

               when Single =>
                  Ref.Object_Directory :=
                    Locate_Directory (Object_Dir.Value, Ref.Directory);

                  if Ref.Object_Directory = null then
                     Scan.Error
                       (Source,
                        Text => "The object directory """ &
                                 Object_Dir.Value.all &
                                 """ cannot be found.");
                  end if;
            end case;

            Canonical_Case_File_Name (Ref.Directory.all);
         end;

         declare
            Source_Dirs : Variable_Value :=
                            Util.Value_Of ("source_dirs", Ref.Decl.Variables);

         begin

            case Source_Dirs.Kind is
               when Undefined =>

                  --  No Source_Dirs specified: the single source directory
                  --  is the one containing the project file

                  Ref.Source_Dirs :=
                    new String_Element'(Value => Ref.Directory,
                                        Next => null);

               when List =>

                  --  Source_Dirs is a list of files.

                  declare
                     Source_Dir : String_List := Source_Dirs.Values;

                  begin
                     while Source_Dir /= null loop
                        Find_Source_Dirs (Source_Dir.Value);
                        Source_Dir := Source_Dir.Next;
                     end loop;
                  end;

               when Single =>

                  if Source_Dirs.Value.all = "" then
                     if Ref.Object_Directory = Ref.Directory then
                        Ref.Object_Directory := null;
                     end if;
                  else
                     Find_Source_Dirs (Source_Dirs.Value);
                  end if;

            end case;

            declare
               Current : String_List := Ref.Source_Dirs;

            begin
               while Current /= null loop
                  if Current.Value /= null then
                     Canonical_Case_File_Name (Current.Value.all);
                  end if;

                  Current := Current.Next;
               end loop;
            end;
         end;

         if Current_Verbosity = High then
            Show_Source_Dirs (Ref);
         end if;

         declare
            Naming : constant Package_List :=
                       Util.Value_Of ("naming", Ref.Decl.Packages);

         begin
            if Naming /= null then
               if Current_Verbosity = High then
                  Write_Line ("Checking ""Naming"".");
               end if;

               declare
                  Bodies         : constant Array_Component_Reference :=
                                     Util.Value_Of ("body",
                                                    Naming.Decl.Arrays);
                  Specifications : constant Array_Component_Reference :=
                                     Util.Value_Of ("specification",
                                                    Naming.Decl.Arrays);

               begin
                  if Bodies /= null then
                     if Current_Verbosity = High then
                        Write_Line ("Found Bodies.");
                     end if;

                     Ref.Naming.Bodies := Bodies;
                     Check_Unit_Names (Bodies);

                  else
                     if Current_Verbosity = High then
                        Write_Line ("No Bodies.");
                     end if;
                  end if;

                  if Specifications /= null then
                     if Current_Verbosity = High then
                        Write_Line ("Found Specifications.");
                     end if;

                     Ref.Naming.Specifications := Specifications;
                     Check_Unit_Names (Specifications);

                  else
                     if Current_Verbosity = High then
                        Write_Line ("No Specifications.");
                     end if;
                  end if;
               end;

               declare
                  Dot_Replacement : constant Variable_Value :=
                                      Util.Value_Of ("dot_replacement",
                                                     Naming.Decl.Variables);

               begin
                  case Dot_Replacement.Kind is
                     when Undefined =>
                        null;

                     when List =>
                        Scan.Error
                          (Source,
                           "Dot_Replacement cannot be a list.");

                     when Single =>
                        Ref.Naming.Dot_Replacement :=
                          Dot_Replacement.Value;
                        Canonical_Case_File_Name
                          (Ref.Naming.Dot_Replacement.all);
                  end case;
               end;

               if Current_Verbosity = High then
                  Write_Str  ("  Dot_Replacement = """);
                  Write_Str  (Ref.Naming.Dot_Replacement.all);
                  Write_Char ('"');
                  Write_Eol;
               end if;

               declare
                  Casing_String : constant Variable_Value :=
                                    Util.Value_Of ("casing",
                                                   Naming.Decl.Variables);

               begin
                  case Casing_String.Kind is
                     when Undefined =>
                        null;

                     when List =>
                        Scan.Error (Source,
                                    "Casing cannot be a list.");

                     when Single =>
                        begin
                           declare
                              Casing : constant Casing_Type :=
                                         Value (Casing_String.Value.all);

                           begin
                              Ref.Naming.Casing := Casing;
                           end;

                        exception
                           when Constraint_Error =>
                              Scan.Error
                                (Source,
                                 '"' &
                                 Casing_String.Value.all &
                                 """ is not a correct Casing.");
                        end;
                  end case;
               end;

               if Current_Verbosity = High then
                  Write_Str  ("  Casing = ");
                  Write_Str  (Image (Ref.Naming.Casing));
                  Write_Char ('.');
                  Write_Eol;
               end if;

               declare
                  Specification_Append : constant Variable_Value :=
                                           Util.Value_Of
                                             ("specification_append",
                                              Naming.Decl.Variables);

               begin
                  case Specification_Append.Kind is
                     when Undefined =>
                        null;

                     when List =>
                        Scan.Error
                          (Source,
                           "Specification_Append cannot be a list.");

                     when Single =>
                        Ref.Naming.Specification_Append :=
                          Specification_Append.Value;
                        Canonical_Case_File_Name
                          (Ref.Naming.Specification_Append.all);
                  end case;
               end;

               if Current_Verbosity = High then
                  Write_Str  ("  Specification_Append = """);
                  Write_Str  (Ref.Naming.Specification_Append.all);
                  Write_Line (""".");
               end if;

               declare
                  Body_Append : constant Variable_Value :=
                                  Util.Value_Of ("body_append",
                                                 Naming.Decl.Variables);

               begin
                  case Body_Append.Kind is
                     when Undefined =>
                        null;

                     when List =>
                        Scan.Error
                          (Source, "Body_Append cannot be a list.");

                     when Single =>
                        Ref.Naming.Body_Append :=
                          Body_Append.Value;
                        Canonical_Case_File_Name (Ref.Naming.Body_Append.all);
                  end case;
               end;

               if Current_Verbosity = High then
                  Write_Str  ("  Body_Append = """);
                  Write_Str  (Ref.Naming.Body_Append.all);
                  Write_Line (""".");
               end if;

               declare
                  Separate_Append : constant Variable_Value :=
                                      Util.Value_Of ("separate_append",
                                                     Naming.Decl.Variables);

               begin
                  case Separate_Append.Kind is
                     when Undefined =>
                        Ref.Naming.Separate_Append := Ref.Naming.Body_Append;

                     when List =>
                        Scan.Error
                          (Source,
                           "Separate_Append cannot be a list.");

                     when Single =>
                        Ref.Naming.Separate_Append :=
                          Separate_Append.Value;
                        Canonical_Case_File_Name
                          (Ref.Naming.Separate_Append.all);
                  end case;
               end;

               if Current_Verbosity = High then
                  Write_Str  ("  Separate_Append = """);
                  Write_Str  (Ref.Naming.Separate_Append.all);
                  Write_Line (""".");
                  Write_Line ("end Naming.");
               end if;
            end if;
         end;

         if Ref.Source_Dirs /= null then
            declare
               Sources : constant Variable_Value :=
                           Util.Value_Of ("source_files",
                                          Ref.Decl.Variables);

            begin
               case  Sources.Kind is
                  when Undefined =>

                     --  No source files specified.
                     --  Find all the files that satisfy the naming scheme
                     --  in all the source directories.

                     declare
                        Source_Dir     : String_List := Ref.Source_Dirs;
                        Dir            : Dir_Type;
                        Name           : String (1 .. 250);
                        Last           : Natural;
                        Current_Source : String_List;

                     begin
                        if Current_Verbosity = High then
                           Write_Line ("Looking for sources:");
                        end if;

                        while Source_Dir /= null loop
                           begin
                              if Source_Dir.Value /= null then
                                 if Current_Verbosity = High then
                                    Write_Str ("Source_Dir = ");
                                    Write_Line (Source_Dir.Value.all);
                                 end if;

                                 Open (Dir, Source_Dir.Value.all);

                                 loop
                                    Read (Dir, Name, Last);

                                    if Current_Verbosity = High then
                                       Write_Str  ("   Checking ");
                                       Write_Line (Name (1 .. Last));
                                    end if;

                                    exit when Last = 0;

                                    declare
                                       Path_Name : constant
                                         GNAT.OS_Lib.String_Access :=
                                           Locate_Regular_File
                                             (Name (1 .. Last),
                                              Source_Dir.Value.all);

                                    begin
                                       if Path_Name /= null then
                                          Record_Source
                                            (File_Name =>
                                               new String'(Name (1 .. Last)),
                                             Path_Name =>
                                               new String'(Path_Name.all),
                                             Ref => Ref,
                                             Source => Source,
                                             Error_If_Invalid => False,
                                             Current_Source => Current_Source);

                                       --  Awfully deep nesting here???
                                       --  Make a procedure ???

                                       else
                                          if Current_Verbosity = High then
                                             Write_Line
                                               ("      Not a regular file.");
                                          end if;
                                       end if;
                                    end;
                                 end loop;

                                 Close (Dir);
                              end if;

                           exception
                              when Directory_Error =>
                                 null;
                           end;

                           Source_Dir := Source_Dir.Next;
                        end loop;

                        if Current_Verbosity = High then
                           Write_Line ("end Looking for sources.");
                        end if;

                        if Current_Source = null then
                           Scan.Error
                             (Source,
                              Text =>
                                "There are no sources in this project.");
                        end if;
                     end;

                  when Single =>

                     --  Sources is a file containing the source file names.

                     declare
                        Source_File_Path_Name : constant String :=
                                                  Path_Name_Of
                                                    (Sources.Value.all,
                                                     Ref.Directory.all);

                     begin
                        if Source_File_Path_Name'Length = 0 then
                           Scan.Error
                             (Source,
                              Text => "File with sources """
                                       & Sources.Value.all &
                                       """ does not exist.");

                        else
                           declare
                              File           : Prj.Util.Text_File;
                              Line           : String (1 .. 250);
                              Last           : Natural;
                              Current_Source : String_List;

                           begin
                              if Current_Verbosity = High then
                                 Write_Str  ("Opening """);
                                 Write_Str  (Source_File_Path_Name);
                                 Write_Line (""".");
                              end if;

                              Prj.Util.Open
                                (File, Source_File_Path_Name);

                              while not Prj.Util.End_Of_File (File) loop
                                 Prj.Util.Get_Line (File, Line, Last);

                                 if Last /= 0
                                   and then
                                     (Last = 1 or else Line (1 .. 2) /= "--")
                                 then
                                    declare
                                       Source_Dir : String_List;
                                       Path_Name  : GNAT.OS_Lib.String_Access;
                                       Found      : Boolean := False;

                                    begin
                                       if Current_Verbosity = High then
                                          Write_Str  ("   Checking """);
                                          Write_Str  (Line (1 .. Last));
                                          Write_Line (""".");
                                       end if;

                                       Source_Dir := Ref.Source_Dirs;
                                       Found := False;

                                       while Source_Dir /= null loop
                                          if Current_Verbosity = High then
                                             Write_Str ("      """);
                                             Write_Str (Source_Dir.Value.all);
                                             Write_Str (""": ");
                                          end if;

                                          Path_Name :=
                                            Locate_Regular_File
                                              (Line (1 .. Last),
                                               Source_Dir.Value.all);

                                          if Path_Name /= null then
                                             if Current_Verbosity = High then
                                                Write_Line ("OK");
                                             end if;

                                             Record_Source
                                               (File_Name =>
                                                  new String'
                                                (Line (1 .. Last)),
                                                Path_Name =>
                                                  new String'(Path_Name.all),
                                                Ref => Ref,
                                                Source => Source,
                                                Error_If_Invalid => True,
                                                Current_Source =>
                                                  Current_Source);
                                             Found := True;
                                             exit;

                                          else
                                             if Current_Verbosity = High then
                                                Write_Line ("No");
                                             end if;

                                             Source_Dir := Source_Dir.Next;
                                          end if;
                                       end loop;

                                       if not Found then
                                          Scan.Error
                                            (Source,
                                             Text => "Cannot find source """ &
                                                     Line (1 .. Last) &
                                                     """.");
                                       end if;
                                    end;
                                 end if;
                              end loop;

                              Prj.Util.Close (File);

                              if Current_Source = null then
                                 Scan.Error
                                   (Source,
                                    Text => "This project has no source.");
                              end if;
                           end;
                        end if;
                     end;

                  when List =>

                     --  Sources is a list of file names

                     declare
                        Current_Source : String_List;
                        Current        : String_List := Sources.Values;

                     begin
                        while Current /= null loop
                           Record_Source
                             (File_Name        => Current.Value,
                              Path_Name        => null,
                              Ref              => Ref,
                              Source           => Source,
                              Error_If_Invalid => True,
                              Current_Source   => Current_Source);
                           Current := Current.Next;
                        end loop;
                     end;
               end case;
            end;
         end if;

      end Check;

      --------------
      -- Get_Unit --
      --------------

      procedure Get_Unit
        (File_Name    : String;
         Naming       : Naming_Data;
         Source       : Scan.Source_Id;
         Unit_Name    : out String_Access;
         Unit_Kind    : out Spec_Or_Body;
         Needs_Pragma : out Boolean)
      is
         Canonical_Case_Name : String := File_Name;
         Result              : Unbounded.Unbounded_String;
         Position            : Natural;

      begin
         Needs_Pragma := False;
         Canonical_Case_File_Name (Canonical_Case_Name);

         if Naming.Bodies /= null then
            declare
               Current : Array_Component_Reference := Naming.Bodies;

            begin
               while Current /= null loop
                  if Current.Index /= null
                    and then Canonical_Case_Name = Current.Value.Value.all
                  then
                     Unit_Kind := Body_Part;
                     Unit_Name := new String' (Current.Index.all);
                     Needs_Pragma := True;
                     return;

                  else
                     Current := Current.Next;
                  end if;
               end loop;
            end;
         end if;

         if Naming.Specifications /= null then
            declare
               Current : Array_Component_Reference := Naming.Specifications;

            begin
               while Current /= null loop
                  if Current.Index /= null
                    and then Canonical_Case_Name = Current.Value.Value.all
                  then
                     Unit_Kind := Specification;
                     Unit_Name := new String'
                       (Current.Index.all);
                     Needs_Pragma := True;
                     return;
                  else
                     Current := Current.Next;
                  end if;
               end loop;
            end;
         end if;

         Result := Unbounded.To_Unbounded_String (Canonical_Case_Name);
         Unit_Kind := Specification;
         Position :=
           Unbounded.Index
             (Source  => Result,
              Pattern => Naming.Specification_Append.all,
              Going   => Backward);

         if Position /= 0
           and then Position + Naming.Specification_Append'Length =
                                               File_Name'Length + 1
         then
            Unbounded.Head (Source => Result, Count => Position - 1);

            if Current_Verbosity = High then
               Write_Str  ("   Specification: ");
               Write_Line (Unbounded.To_String (Result));
            end if;

         else
            Position :=
              Unbounded.Index
                (Source  => Result,
                 Pattern => Naming.Body_Append.all,
                 Going   => Backward);

            if Position /= 0
              and then Position + Naming.Body_Append'Length =
                                                  File_Name'Length + 1
            then
               Unbounded.Head
                 (Source => Result,
                  Count  => Position - 1);
               Unit_Kind := Body_Part;

               if Current_Verbosity = High then
                  Write_Str  ("   Body: ");
                  Write_Line (Unbounded.To_String (Result));
               end if;

            else
               Position := Unbounded.Index
                 (Source  => Result,
                  Pattern => Naming.Separate_Append.all,
                  Going   => Backward);

               if Position /= 0
                 and then Position + Naming.Separate_Append'Length =
                                                 File_Name'Length + 1
               then
                  Unbounded.Head
                    (Source => Result,
                     Count  => Position - 1);
                  Unit_Kind := Body_Part;

                  if Current_Verbosity = High then
                     Write_Str  ("   Separate: ");
                     Write_Line (Unbounded.To_String (Result));
                  end if;

               else
                  Unit_Name := null;

                  if Current_Verbosity = High then
                     Write_Line ("   Not a valid file name.");
                  end if;

                  return;
               end if;
            end if;
         end if;

         if Naming.Dot_Replacement.all /= "." then
            if Unbounded.Index (Source => Result,
                                Pattern => ".") /= 0
            then
               if Current_Verbosity = High then
                  Write_Line
                    ("   Not a valid file name (some dot not replaced).");
               end if;
               Unit_Name := null;
               return;
            end if;

            loop
               Position :=
                 Unbounded.Index
                   (Source  => Result,
                    Pattern => Naming.Dot_Replacement.all);
               exit when Position = 0;
               Unbounded.Replace_Slice
                 (Source => Result,
                  Low    => Position,
                  High   => Position + Naming.Dot_Replacement'Length - 1,
                  By     => ".");
            end loop;
         end if;

         declare
            Src : String := Unbounded.To_String (Result);
            use Unbounded;

         begin
            case Naming.Casing is
               when Lowercase =>
                  Fixed.Translate
                    (Source  => Src,
                     Mapping => Lower_Case_Map);

               when Uppercase =>
                  Fixed.Translate
                    (Source  => Src,
                     Mapping => Upper_Case_Map);

               when Mixedcase =>
                  null;
            end case;

            if Src /= Result then
               if Current_Verbosity = High then
                  Write_Line ("   Not a valid file name (casing).");
               end if;

               Unit_Name := null;
               return;
            end if;
         end;

         Unbounded.Translate
           (Source  => Result,
            Mapping => Lower_Case_Map);

         if Current_Verbosity = High then
            Write_Str  ("      ");
            Write_Line (Unbounded.To_String (Result));
         end if;

         Check
           (Name => Unbounded.To_String (Result),
            Unit => Unit_Name);
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

      -------------------
      -- Record_Source --
      -------------------

      procedure Record_Source
        (File_Name        : String_Access;
         Path_Name        : String_Access;
         Ref              : Reference;
         Source           : Scan.Source_Id;
         Error_If_Invalid : Boolean;
         Current_Source   : in out String_List)
     is
         Unit_Name    : String_Access;
         Unit_Kind    : Spec_Or_Body;
         Needs_Pragma : Boolean;

      begin
         Get_Unit
           (File_Name    => File_Name.all,
            Naming       => Ref.Naming,
            Source       => Source,
            Unit_Name    => Unit_Name,
            Unit_Kind    => Unit_Kind,
            Needs_Pragma => Needs_Pragma);

         if Unit_Name = null then
            if Error_If_Invalid then
               Scan.Error
                 (Source,
                  Text => '"' &
                          File_Name.all &
                           """ is not a valid source file name.");

            else
               if Current_Verbosity = High then
                  Write_Str  ("   """);
                  Write_Str  (File_Name.all);
                  Write_Line (""" is not a valid source file name (ignored).");
               end if;
            end if;

         else
            if Current_Source = null then
               Current_Source := new String_Element'
                 (Value => File_Name,
                  Next  => null);
               Ref.Sources := Current_Source;

            else
               Current_Source.Next :=
                 new String_Element'
                   (Value => File_Name,
                    Next  => null);
               Current_Source := Current_Source.Next;
            end if;

            --  Put in unit list

            declare
               Current_Unit    : Unit_List := First_Unit;
               Found : Boolean := False;

            begin
               if Current_Verbosity = High then
                  Write_Str  ("Putting ");
                  Write_Str  (Unit_Name.all);
                  Write_Line (" in the unit list.");
               end if;

               while Current_Unit /= null loop
                  if Current_Unit.Name.all = Unit_Name.all then
                     Found := True;

                     if Current_Unit.File_Names (Unit_Kind).Name = null
                       or else Current_Unit.Ref = Ref.Modifies
                     then
                        Current_Unit.Ref := Ref;
                        Current_Unit.File_Names (Unit_Kind).Name := File_Name;
                        Current_Unit.File_Names (Unit_Kind).Path := Path_Name;
                        Current_Unit.File_Names (Unit_Kind).Needs_Pragma :=
                          Needs_Pragma;

                     else
                        Scan.Error
                          (Source,
                           Text => Current_Unit.Name.all &
                                   " already exists. (" &
                                   Unit_Name.all & ").");
                     end if;
                     exit;

                  else
                     Current_Unit := Current_Unit.Next;
                  end if;

               end loop;

               if not Found then
                  if First_Unit = null then
                     First_Unit := new Unit_Data;
                     Last_Unit := First_Unit;
                  else
                     Last_Unit.Next := new Unit_Data;
                     Last_Unit := Last_Unit.Next;
                  end if;

                  Last_Unit.Name := Unit_Name;
                  Last_Unit.File_Names (Unit_Kind).Name := File_Name;
                  Last_Unit.File_Names (Unit_Kind).Path := Path_Name;
                  Last_Unit.Ref := Ref;
                  Last_Unit.File_Names (Unit_Kind).Needs_Pragma :=
                    Needs_Pragma;
               end if;
            end;
         end if;
      end Record_Source;

      ----------------------
      -- Show_Source_Dirs --
      ----------------------

      procedure Show_Source_Dirs (Ref : Reference) is
         Current : String_List := Ref.Source_Dirs;

      begin
         Write_Line ("Source_Dirs:");

         while Current /= null loop
            Write_Str  ("   ");
            Write_Line (Current.Value.all);
            Current := Current.Next;
         end loop;

         Write_Line ("end Source_Dirs.");
      end Show_Source_Dirs;

   --  Start of processing for Naming_Scheme

   begin
      Canonical_Case_File_Name (Standard_Body_Append.all);
      Canonical_Case_File_Name (Standard_Naming_Data.Specification_Append.all);
   end Naming_Scheme;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Ref               : in out Reference;
      Project_File_Name : String;
      Package_Name      : String := "")
   is
   begin
      declare
         Path_Name : constant String := Path_Name_Of
                                          (Project_File_Name,
                                           Directory   => Get_Current_Dir,
                                           Referred_In => null);

      begin
         Package_Ref := new String'(Package_Name);
         First_Project := null;
         Last_Project := First_Project;
         First_Unit := null;
         Last_Unit := null;

         Parse_Single_Project
           (Ref             => Ref,
            Modified_By     => null,
            Path_Name       => Path_Name,
            Original_Source => Scan.Nil_Source_Id);
      end;

   exception
      when Project_Error =>
         Ref := null;

      when X : others =>
         Write_Str  ("Exception ");
         Write_Str  (Exception_Name (X));
         Write_Line (" raised.");
         Ref := null;
   end Parse;

   --------------------------
   -- Parse_Single_Project --
   --------------------------

   procedure Parse_Single_Project
     (Ref             : in out Reference;
      Modified_By     : Reference;
      Path_Name       : String;
      Original_Source : Scan.Source_Id)
   is
      Current             : Project_Ref := First_Project;
      Source              : Scan.Source_Id;
      Token               : Token_Type;
      Canonical_Path_Name : String_Access;
      Imported            : Reference_List;

   begin
      if Current_Verbosity >= Medium then
         Write_Str  ("Parsing """);
         Write_Str  (Path_Name);
         Write_Char ('"');
         Write_Eol;
      end if;

      Scan.Initialize (Source, Path_Name, Canonical_Path_Name);

      while Current /= null and then Current.Project /= null loop
         if Current.Project.Path_Name.all = Canonical_Path_Name.all then
            if Current.Project.Modified_By /= null then
               declare
                  Text : constant String :=
                           "Project " & Canonical_Path_Name.all &
                           " cannot be imported, because it is modified by " &
                           Current.Project.Modified_By.Path_Name.all;

               begin
                  Scan.Error (Source => Original_Source, Text => Text);
               end;
            end if;

            if Modified_By /= null then
               declare
                  Text : constant String :=
                           "Project " &
                           Canonical_Path_Name.all &
                           " cannot be modified; ";

               begin
                  if Current.Project.First_Referred_By = null then
                     Scan.Error
                       (Source => Original_Source,
                        Text   => Text &
                                  "it is the compiling project.");

                  else
                     Scan.Error
                       (Source => Original_Source,
                        Text => Text &
                                "it is already imported by " &
                                 Current.Project.First_Referred_By.
                                 Path_Name.all & '.');
                  end if;
               end;
            end if;

            if Current_Verbosity >= Medium then
               Write_Line ("   Skipped, because already parsed.");
            end if;

            Scan.Finalize (Source);
            Ref := Current.Project;
            return;

         else
            Current := Current.Next;
         end if;
      end loop;

      Ref := new Structure;
      Ref.Path_Name := Canonical_Path_Name;
      Ref.Directory :=
        new String' (Immediate_Directory_Of (Canonical_Path_Name.all));
      Ref.File_Name :=
        new String' (Simple_File_Name_Of (Canonical_Path_Name.all));
      Ref.Modified_By := Modified_By;

      if First_Project = null then
         First_Project := new Project_Data'
           (Project => Ref,
            Next    => null);
         Last_Project := First_Project;

      else
         Last_Project.Next := new Project_Data'
           (Project => Ref,
            Next    => null);
         Last_Project := Last_Project.Next;
      end if;

      loop
         Scan.Get (Token, Source);
         exit when Token /= Tok_With;

         loop
            Scan.Get (Token, Source);

            declare
               Project_Name : constant String :=
                                String_Expression.Value
                                  (Ref    => Ref,
                                   Pkg    => null,
                                   Source => Source);

               Path_Name : constant String :=
                             Path_Name_Of
                               (Project_Name,
                                Directory   => Ref.Directory.all,
                                Referred_In => Ref);

               New_Ref : Reference;

            begin
               Parse_Single_Project
                 (Ref             => New_Ref,
                  Modified_By     => null,
                  Path_Name       => Path_Name,
                  Original_Source => Source);

               if New_Ref.First_Referred_By = null then
                  New_Ref.First_Referred_By := Ref;
               end if;

               if Imported = null then
                  Ref.Imported_Projects := new Reference_Data;
                  Imported := Ref.Imported_Projects;
               else
                  Imported.Next := new Reference_Data;
                  Imported := Imported.Next;
               end if;

               Imported.Ref := New_Ref;
            end;

            Token := Scan.Current_Token_Of (Source);
            exit when Token /= Tok_Comma;
         end loop;

         if Token /= Tok_Semi_Colon then
            Scan.Error (Source, Text => "Expected ';'.");
         end if;
      end loop;

      if Token /= Tok_Project then
         Scan.Error (Source, Text => "Expected ""project"".");
      end if;

      Scan.Get (Token, Source);

      if Token /= Tok_Identifier then
         Scan.Error (Source, Text => "Expected identifier.");
      end if;

      Ref.Name := new String'(To_Lower (Scan.Value_Of (Source)));
      Scan.Get (Token, Source);

      if Token = Tok_Modifying then
         Scan.Get (Token, Source);

         declare
            Project_Name : constant String := String_Expression.Value
              (Ref    => Ref,
               Pkg    => null,
               Source => Source);

            Path_Name : constant String :=
                          Path_Name_Of
                            (Project_Name,
                             Directory   => Ref.Directory.all,
                             Referred_In => Ref);

            New_Ref : Reference;

         begin
            if Path_Name = Parse_Single_Project.Path_Name then
               Scan.Error (Source, Text => "A project cannot modify itself.");
            end if;

            Parse_Single_Project
              (Ref             => New_Ref,
               Modified_By     => Ref,
               Path_Name       => Path_Name,
               Original_Source => Source);
            Ref.Modifies := New_Ref;
         end;

         Token := Scan.Current_Token_Of (Source);
      end if;

      if Token /= Tok_Is then
         Scan.Error (Source, Text => "Expected ""is"".");
      end if;

      Declarations.Parse (Ref, null, Source, Do_Not_Skip => True);
      Token := Scan.Current_Token_Of (Source);

      --  ??? seems like this very common operation could be abstracted
      --  into Expect (Tok_End) (you could get the error message text
      --  from the image of the token.

      if Token /= Tok_End then
         Scan.Error (Source, Text => "Expected ""end"".");
      end if;

      Scan.Get (Token, Source);

      if Token /= Tok_Identifier then
         Scan.Error (Source, Text => "Expected identifier.");
      end if;

      if To_Lower (Scan.Value_Of (Source)) /= Ref.Name.all then
         Scan.Error (Source, Text => "Expected """ & Ref.Name.all & """.");
      end if;

      Scan.Get (Token, Source);

      if Token /= Tok_Semi_Colon then
         Scan.Error (Source,
                     Text => "Expected ';'.");
      end if;

      Naming_Scheme.Check (Source, Ref);

      Scan.Finalize (Source);

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
                                     Path => Directory);

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
      Referred_In       : Reference)
      return        String
   is
      Result : String_Access;

   begin
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

      if Result = null then
         if Current_Verbosity = High then
            Write_Str  ("   Trying ");
            Write_Str  (Project_File_Name);
            Write_Line (Project_File_Extension.all);
         end if;

         Result :=
           Locate_Regular_File
             (File_Name => Project_File_Name & Project_File_Extension.all,
              Path      => "");
      end if;

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
      end if;

      if Result = null then
         if Current_Verbosity = High then
            Write_Str  ("   Trying ");
            Write_Str  (Directory);
            Write_Str  (Project_File_Name);
            Write_Line (Project_File_Extension.all);
         end if;

         Result :=
           Locate_Regular_File
             (File_Name => Directory & Project_File_Name &
                                             Project_File_Extension.all,
              Path      => "");
      end if;

      if Result = null then
         declare
            Current : Reference;

         begin
            Write_Str  ("Project file """);
            Write_Str  (Project_File_Name);
            Write_Line (""" unknown.");

            Current := Referred_In;
            while Current /= null loop
               Write_Str  ("   first refered in """);
               Write_Str  (Current.File_Name.all);
               Write_Str  (""" (directory """);
               Write_Str  (Current.Directory.all);
               Write_Line (""")");
               Current := Current.First_Referred_By;
            end loop;

            raise Project_Error;
         end;
      end if;

      Canonical_Case_File_Name (Result.all);
      return Result.all;
   end Path_Name_Of;

   ----------
   -- Scan --
   ----------

   package body Scan is

      Blanks : constant String (1 .. 250) := (others => ' ');

      procedure Free is new Ada.Unchecked_Deallocation
        (Source_Data, Source_Id);

      procedure Check_Reserved (Source : in Source_Id);

      procedure Get_Next_Line (Source : in Source_Id);

      procedure Get_Next_Character (Source : in Source_Id);

      --------------------
      -- Check_Reserved --
      --------------------

      procedure Check_Reserved (Source : Source_Id) is
      begin
         case Source.Value (1) is
            when 'c' =>
               if Source.Last_Value = 4
                 and then Source.Value (1 .. 4) = "case"
               then
                  Source.Current := Tok_Case;
               end if;

            when 'e' =>
               if Source.Last_Value = 3
                 and then Source.Value (1 .. 3) = "end"
               then
                  Source.Current := Tok_End;

               elsif Source.Last_Value = 8
                 and then Source.Value (1 .. 8) = "external"
               then
                  Source.Current := Tok_External;
               end if;

            when 'i' =>
               if Source.Last_Value = 2
                 and then Source.Value (1 .. 2) = "is"
               then
                  Source.Current := Tok_Is;
               end if;

            when 'm' =>
               if Source.Last_Value = 9
                 and then Source.Value (1 .. 9) = "modifying"
               then
                  Source.Current := Tok_Modifying;
               end if;

            when 'o' =>
               if Source.Last_Value = 6
                 and then Source.Value (1 .. 6) = "others"
               then
                  Source.Current := Tok_Others;
               end if;

            when 'p' =>
               if Source.Last_Value = 7 then
                  if Source.Value (1 .. 7) = "package" then
                     Source.Current := Tok_Package;
                  elsif Source.Value (1 .. 7) = "project" then
                     Source.Current := Tok_Project;
                  end if;
               end if;

            when 'r' =>
               if Source.Last_Value = 7 then
                  if Source.Value (1 .. 7) = "renames" then
                     Source.Current := Tok_Renames;
                  end if;
               end if;

            when 'w' =>
               if Source.Last_Value = 4 then
                  if Source.Value (1 .. 4) = "when" then
                     Source.Current := Tok_When;
                  elsif Source.Value (1 .. 4) = "with" then
                     Source.Current := Tok_With;
                  end if;
               end if;

            when others =>
               null;
         end case;
      end Check_Reserved;

      ----------------------
      -- Current_Token_Of --
      ----------------------

      function Current_Token_Of (Source : Source_Id) return Token_Type is
      begin
         if Source /= null then
            return Source.Current;
         else
            return Tok_Invalid;
         end if;
      end Current_Token_Of;

      -----------
      -- Error --
      -----------

      procedure Error
        (Source : Source_Id;
         Text   : String;
         Fatal  : Boolean := True)
      is
      begin
         if Source /= null then
            Write_Str (Source.Path_Name.all);
            Write_Char (':');
            Write_Eol;

            declare
               Line_Number : constant String :=
                               Integer'Image (Source.Line_Number);

            begin
               for J in Line_Number'Length .. 4 loop
                  Write_Char (' ');
               end loop;

               Write_Str (Line_Number);
            end;

            Write_Str  (": ");
            Write_Line (Source.Line (1 .. Source.Last));
            Write_Str (Blanks (1 .. Source.Cursor + 6));
            Write_Line ("|");
         end if;

         if Fatal then
            Write_Str ("***Fatal ERROR: ");
         else
            Write_Str ("***ERROR: ");
         end if;

         Write_Line (Text);

         if Fatal then
            raise Project_Error;
         end if;
      end Error;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Source : in out Source_Id) is
      begin
         if Source /= null and then Source.Path_Name /= null then
            if Current_Verbosity >= Medium then
               Write_Str  ("Closing """);
               Write_Str  (Source.Path_Name.all);
               Write_Line (""".");
            end if;
            Prj.Util.Close (Source.File);
         end if;

         Free (Source);
      end Finalize;

      ---------
      -- Get --
      ---------

      procedure Get
        (Token  : out Token_Type;
         Source : Source_Id)
      is
      begin
         loop
            Get_Next_Character (Source => Source);
            exit when Source.Current = Tok_EOF;

            case Source.Line (Source.Cursor) is
               when ' ' | ASCII.HT =>
                  null;

               when ',' =>
                  Source.Current := Tok_Comma;
                  exit;

               when ';' =>
                  Source.Current := Tok_Semi_Colon;
                  exit;

               when '(' =>
                  Source.Current := Tok_Left_Par;
                  exit;

               when ')' =>
                  Source.Current := Tok_Right_Par;
                  exit;

               when '&' =>
                  Source.Current := Tok_Ampersand;
                  exit;

               when '|' =>
                  Source.Current := Tok_Vertical_Bar;
                  exit;

               when '.' =>
                  Source.Current := Tok_Dot;
                  exit;

               when '=' =>
                  if Source.Cursor < Source.Last
                    and then Source.Line (Source.Cursor + 1) = '>'
                  then
                     Source.Current := Tok_Arrow;
                     Source.Cursor := Source.Cursor + 1;
                  else
                     Source.Current := Tok_Invalid;
                  end if;

                  exit;

               when ':' =>
                  if Source.Cursor < Source.Last
                    and then Source.Line (Source.Cursor + 1) = '='
                  then
                     Source.Current := Tok_Assign;
                     Source.Cursor := Source.Cursor + 1;
                  else
                     Source.Current := Tok_Invalid;
                  end if;

                  exit;

               when '"' =>
                  Source.Last_Value := 0;
                  loop
                     Source.Cursor := Source.Cursor + 1;

                     if Source.Cursor > Source.Last then
                        Error
                          (Source => Source,
                           Text =>
                             "Literal string must end on the same line.");
                     end if;

                     if Source.Line (Source.Cursor) = '"' then
                        if Source.Cursor = Source.Last
                          or else Source.Line (Source.Cursor + 1) /= '"'
                        then
                           exit;
                        else
                           Source.Cursor := Source.Cursor + 1;
                        end if;
                     end if;

                     Source.Last_Value := Source.Last_Value + 1;

                     Source.Value (Source.Last_Value) :=
                       Source.Line (Source.Cursor);
                  end loop;

                  Source.Current := Tok_Literal_String;
                  exit;

               when '-' =>
                  if Source.Cursor = Source.Last
                    or else Source.Line (Source.Cursor + 1) /= '-'
                  then
                     Error (Source => Source, Text => "Illegal character");
                  else
                     Source.Cursor := Source.Last;
                  end if;

               when 'a' .. 'z' | 'A' .. 'Z' =>
                  declare
                     Cursor  : Positive := Source.Cursor;
                     Current : Character;

                  begin
                     loop
                        Cursor := Cursor + 1;
                        exit when Cursor > Source.Last;
                        Current := Source.Line (Cursor);

                        if Current = '_' then
                           if Cursor = Source.Last
                             or else not Is_Alphanumeric
                                           (Source.Line (Cursor + 1))
                           then
                              Error
                                (Source => Source,
                                 Text   => "'_' cannot end or be " &
                                           "doubled in an identifier");
                           end if;

                        elsif not Is_Alphanumeric (Current) then
                           exit;
                        end if;
                     end loop;

                     Source.Last_Value := Cursor - Source.Cursor;
                     Source.Value (1 .. Source.Last_Value) :=
                       To_Lower (Source.Line (Source.Cursor .. Cursor - 1));
                     Source.Current := Tok_Identifier;
                     Source.Cursor := Cursor - 1;
                  end;

                  Check_Reserved (Source => Source);
                  exit;

               when others =>
                  Error (Source => Source, Text => "Illegal character");
            end case;
         end loop;

         Token := Source.Current;
      end Get;

      ------------------------
      -- Get_Next_Character --
      ------------------------

      procedure Get_Next_Character (Source : Source_Id) is
      begin
         if Source.Cursor >= Source.Last then
            Get_Next_Line (Source);
         else
            Source.Cursor := Source.Cursor + 1;
         end if;
      end Get_Next_Character;

      -------------------
      -- Get_Next_Line --
      -------------------

      procedure Get_Next_Line (Source : Source_Id) is
      begin
         Source.Last := 0;

         while not Source.End_Of_File_Reached loop
            if Prj.Util.End_Of_File (Source.File) then
               Source.End_Of_File_Reached := True;
               Source.Current := Tok_EOF;
               return;

            else
               Prj.Util.Get_Line
                 (File => Source.File,
                  Line => Source.Line,
                  Last => Source.Last);
               Source.Line_Number := Source.Line_Number + 1;
               Source.Cursor := 1;
            end if;

            exit when Source.Last > 0;
         end loop;
      end Get_Next_Line;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Source              : in out Source_Id;
         Path_Name           : String;
         Canonical_Path_Name : out String_Access) is

      begin
         Source := new Source_Data;
         Prj.Util.Open (Source.File, Path_Name);

         if not Prj.Util.Is_Valid (Source.File) then
            Write_Str  ("Unable to open """);
            Write_Str  (Path_Name);
            Write_Char ('"');
            Write_Eol;
            raise Project_Error;
         end if;

         Source.Path_Name := new String'(Path_Name);
         Osint.Canonical_Case_File_Name (Source.Path_Name.all);
         Canonical_Path_Name := Source.Path_Name;
         Source.End_Of_File_Reached := Prj.Util.End_Of_File (Source.File);
         Source.Line_Number := 0;
         Source.Last := 0;
         Source.Cursor := 1;
         Source.Current := Tok_Invalid;
      end Initialize;

      --------------
      -- Value_Of --
      --------------

      function Value_Of (Source : in Source_Id) return String is
      begin
         if Source.Current /= Tok_Identifier
           and then Source.Current /= Tok_Literal_String
         then
            Write_Line ("Illegal call to Identifier_Value_Of");
            raise Program_Error;

         else
            return Source.Value (1 .. Source.Last_Value);
         end if;
      end Value_Of;

   end Scan;

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

   function Simple_File_Name_Of (Path_Name : String) return String is
      Slash_Position : constant Natural :=
                         Index (Source  => Path_Name,
                                Pattern => "/",
                                Going   => Ada.Strings.Backward);

      Separator_Position : constant Natural :=
                             Index (Source  => Path_Name,
                                    Pattern => "" & Directory_Separator,
                                    Going   => Ada.Strings.Backward);

      End_Of_Directory : constant Natural :=
                           Integer'Max (Slash_Position, Separator_Position);

   begin
      if End_Of_Directory /= 0 then
         return Path_Name (End_Of_Directory + 1 .. Path_Name'Last);
      else
         return Path_Name;
      end if;
   end Simple_File_Name_Of;

   -----------------------
   -- String_Expression --
   -----------------------

   package body String_Expression is
      type Expression;
      type Expression_Ref is access Expression;

      type Expression is record
         Value  : String_Access := new String (1 .. 50);
         Length : Natural := 0;
         Next   : Expression_Ref;
      end record;

      Free_Expression : Expression_Ref := null;

      procedure Add (To_Exp : in Expression_Ref; Str : in String);

      function External_Value
        (Ref    : Reference;
         Pkg    : Package_List;
         Source : Scan.Source_Id)
         return   String;

      function New_Expression return Expression_Ref;

      procedure Recycle (Exp : Expression_Ref);

      function Term
        (Ref    : Reference;
         Pkg    : Package_List;
         Source : Scan.Source_Id)
         return   String;

      function Variable_Value
        (Ref    : Reference;
         Pkg    : Package_List;
         Source : Scan.Source_Id)
         return   String;

      ---------
      -- Add --
      ---------

      procedure Add (To_Exp : in Expression_Ref; Str : in String) is
         New_Length : constant Natural := To_Exp.Length + Str'Length;

      begin
         if New_Length > To_Exp.Value'Length then
            declare
               New_Value : constant String_Access :=
                             new String (1 .. 2*To_Exp.Value'Length);

            begin
               New_Value (1 .. To_Exp.Length) :=
                 To_Exp.Value (1 .. To_Exp.Length);
               Free (To_Exp.Value);
               To_Exp.Value := New_Value;
            end;
         end if;

         To_Exp.Value (To_Exp.Length + 1 .. New_Length) := Str;
         To_Exp.Length := New_Length;
      end Add;

      --------------------
      -- External_Value --
      --------------------

      function External_Value
        (Ref    : Reference;
         Pkg    : Package_List;
         Source : Scan.Source_Id)
         return   String
      is
         Token : Token_Type;

      begin
         Scan.Get (Token, Source);

         if Token /= Tok_Left_Par then
            Scan.Error (Source, Text => "Expected '('.");
         end if;

         Scan.Get (Token, Source);

         declare
            Name : constant String := Value (Ref, Pkg, Source);

         begin
            Token := Scan.Current_Token_Of (Source);

            case Token is
               when Tok_Right_Par =>
                  return Prj.Ext.Value_Of (Name);

               when Tok_Comma =>

                  Scan.Get (Token, Source);

                  declare
                     Default : constant String := Value (Ref, Pkg, Source);

                  begin
                     Token := Scan.Current_Token_Of (Source);

                     if Token /= Tok_Right_Par then
                        Scan.Error (Source,
                                    Text => "Expected ')'.");
                     else
                        return Prj.Ext.Value_Of (Name, Default);
                     end if;
                  end;

               when others =>
                  Scan.Error (Source, Text => "Expected ',' or ')'.");
            end case;
         end;

         return ""; -- to avoid warning
      end External_Value;

      --------------------
      -- New_Expression --
      --------------------

      function New_Expression return Expression_Ref is
      begin
         if Free_Expression /= null then
            declare
               Result : constant Expression_Ref := Free_Expression;

            begin
               Free_Expression := Free_Expression.Next;
               Result.Length := 0;
               return Result;
            end;

         else
            return new Expression;
         end if;
      end New_Expression;

      -------------
      -- Recycle --
      -------------

      procedure Recycle (Exp : Expression_Ref) is
      begin
         Exp.Next := Free_Expression;
         Free_Expression := Exp;
      end Recycle;

      ----------
      -- Term --
      ----------

      function Term
        (Ref    : Reference;
         Pkg    : Package_List;
         Source : Scan.Source_Id)
         return   String
      is
         Token : constant Token_Type := Scan.Current_Token_Of (Source);

      begin
         case Token is
            when Tok_Literal_String =>
               return Scan.Value_Of (Source);

            when Tok_Identifier =>
               return Variable_Value (Ref, Pkg, Source);

            when Tok_External =>
               return External_Value (Ref, Pkg, Source);

            when others =>
               Scan.Error
                 (Source,
                  Text => "Expected literal string, " &
                          "variable identifier or external value.");
         end case;

         return ""; -- to avoid warning
      end Term;

      -----------
      -- Value --
      -----------

      function Value
        (Ref    : Reference;
         Pkg    : Package_List;
         Source : Scan.Source_Id)
         return   String
      is
         Result : Expression_Ref := New_Expression;
         Token  : Token_Type;

      begin
         loop
            Add (Result, Term (Ref, Pkg, Source));
            Scan.Get (Token, Source);
            exit when Token /= Tok_Ampersand;
            Scan.Get (Token, Source);
         end loop;

         Recycle (Result);
         return Result.Value (1 .. Result.Length);
      end Value;

      --------------------
      -- Variable_Value --
      --------------------

      function Variable_Value
        (Ref    : Reference;
         Pkg    : Package_List;
         Source : Scan.Source_Id)
         return   String
      is
         Name : constant String := To_Lower (Scan.Value_Of (Source));

         Current_Package : Package_List := Pkg;

         function Value (Var : Variable_List) return String_Access;

         function Value (Var : Variable_List) return String_Access is
            Current : Variable_List := Var;

         begin
            while Current /= null loop
               if Current.Name.all = Name then
                  if Current.Value.Kind = List then
                     Scan.Error
                       (Source, Text => ("Variable cannot be list."));
                  else
                     return Current.Value.Value;
                  end if;
               end if;

               Current := Current.Next;
            end loop;
            return null;
         end Value;

      --   Start of processing for Variable_Value

      begin
         while Current_Package /= null loop
            declare
               Result : String_Access :=
                          Value (Current_Package.Decl.Variables);

            begin
               if Result /= null then
                  return Result.all;
               end if;
            end;

            Current_Package := Current_Package.Parent;
         end loop;

         if Ref /= null then
            declare
               Result : constant String_Access := Value (Ref.Decl.Variables);

            begin
               if Result /= null then
                  return Result.all;
               end if;
            end;
         end if;

         Scan.Error (Source, Text => "Unknown variable");
         return ""; -- to avoid warning
      end Variable_Value;

   end String_Expression;

begin

   Canonical_Case_File_Name (Project_File_Extension.all);

end Prj.Pars;
