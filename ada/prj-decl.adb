------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . D E C L                             --
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

with Errout;   use Errout;
with Namet;    use Namet;
with Output;   use Output;
with Prj.Com;  use Prj.Com;
with Prj.Str;
with Scans;    use Scans;
with Scn;      use Scn;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Types;    use Types;

package body Prj.Decl is

   --  Comments are required for subprograms below ???

   procedure Case_Construction
     (Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean;
      Level       : Natural);

   procedure Dump_Single_Variables
     (Name      : Name_Id;
      Variables : Variable_Id);

   procedure Dump_Packages
     (Name      : Name_Id;
      Packages  : Package_Id);

   procedure Package_Declaration
     (Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean;
      Level       : Natural);

   procedure Parse_Array_Component_Assignment
     (Name        : Name_Id;
      Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean);

   procedure Parse_Variable_Assignment
     (Name        : Name_Id;
      Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean);

   procedure Parse_Variable_Value
     (Result      : out Variable_Value;
      Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean);

   -----------------------
   -- Case_Construction --
   -----------------------

   procedure Case_Construction
     (Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean;
      Level       : Natural)
   is
      Follow_Current_Branch : Boolean := False;
      One_Branch_Followed   : Boolean := False;

   begin
      Scan;

      declare
         Case_Switch : constant Name_Id := Prj.Str.Value (Data, Pkg);
      begin

         Expect (Tok_Is, "is");

         if Token = Tok_Is then
            Scan;
         end if;

         loop
            exit when Token /= Tok_When;
            Scan;

            if Token = Tok_Others then
               Follow_Current_Branch := Do_Not_Skip and then
                 (not One_Branch_Followed);
               Scan;

               Expect (Tok_Arrow, "=>");

               Parse (Project, Data, Pkg, Follow_Current_Branch, Level);
               exit;

            else
               loop
                  declare
                     Label : constant Name_Id :=
                       Prj.Str.Value (Data, Pkg);

                  begin
                     Follow_Current_Branch :=
                       Do_Not_Skip
                       and then
                       (Follow_Current_Branch
                        or else ((not One_Branch_Followed)
                                 and then
                                 (Label = Case_Switch)));
                  end;

                  exit when Token /= Tok_Vertical_Bar;
                  Scan;
               end loop;

               Expect (Tok_Arrow, "=>");

               Parse (Project, Data, Pkg, Follow_Current_Branch, Level);

               if Follow_Current_Branch then
                  One_Branch_Followed := True;
                  Follow_Current_Branch := False;
               end if;
            end if;
         end loop;

         Expect (Tok_End, "end case");

         if Token = Tok_End then

            Scan;
            Expect (Tok_Case, "case");
         end if;

         Scan;

         Expect (Tok_Semicolon, ";");
      end;
   end Case_Construction;

   -------------------
   -- Dump_Packages --
   -------------------

   procedure Dump_Packages
     (Name      : Name_Id;
      Packages  : Package_Id)
   is
      Current      : Package_Id := Packages;
      The_Package  : Package_Element;

   begin
      Write_Str ("Packages in " & Get_Name_String (Name));
      Write_Line (":");
      while Current /= No_Package loop
         The_Package := Prj.Packages.Table (Current);
         Write_Str ("  ");
         Write_Str (Get_Name_String (The_Package.Name));
         Current := The_Package.Next;
      end loop;

      Write_Str ("end ");
      Write_Str (Get_Name_String (Name));
      Write_Line (";");
   end Dump_Packages;

   ---------------------------
   -- Dump_Single_Variables --
   ---------------------------

   procedure Dump_Single_Variables
     (Name      : Name_Id;
      Variables : Variable_Id)
   is
      Current      : Variable_Id := Variables;
      The_Variable : Variable;

   begin
      Write_Str ("Variables in " & Get_Name_String (Name));
      Write_Line (":");
      while Current /= No_Variable loop
         The_Variable := Variable_Elements.Table (Current);
         if The_Variable.Value.Kind = Single then
            Write_Str ("  ");
            Write_Str (Get_Name_String (The_Variable.Name));
            Write_Str (" = """);
            String_To_Name_Buffer (The_Variable.Value.Value);
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Line (""";");
         end if;

         Current := The_Variable.Next;
      end loop;

      Write_Str ("end ");
      Write_Str (Get_Name_String (Name));
      Write_Line (";");
   end Dump_Single_Variables;

   -------------------------
   -- Package_Declaration --
   -------------------------

   procedure Package_Declaration
     (Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean;
      Level       : Natural)
   is
      The_New_Package : Package_Element;
      New_Pkg         : Package_Id;
      New_Do_Not_Skip : Boolean;
      Name            : Name_Id;
      List            : Package_Id;

   begin
      Scan;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then

         Name := Token_Name;

         --  We skip packages that are at the ground level and that are not
         --  Naming or the package associated with the tool.
         --
         New_Do_Not_Skip := Do_Not_Skip and then
           (Level /= 0 or else Name = Name_Naming
            or else Name = Tool_Name);

         if New_Do_Not_Skip then

            --  Check for package name duplication

            if Pkg = No_Package then
               List := Data.Decl.Packages;
            else
               List := Packages.Table (Pkg).Decl.Packages;
            end if;

            Packages.Increment_Last;
            New_Pkg                := Packages.Last;
            The_New_Package.Name   := Name;
            The_New_Package.Parent := Pkg;
            Packages.Table (New_Pkg) := The_New_Package;

            if List = No_Package then

               if Pkg = No_Package then
                  Data.Decl.Packages := New_Pkg;
               else
                  The_New_Package.Parent := Pkg;
                  Packages.Table (Pkg).Decl.Packages := New_Pkg;
               end if;

            else
               declare
                  Current_Package : Package_Element;
               begin
                  loop
                     Current_Package := Packages.Table (List);
                     if Current_Package.Name = Name then
                        Error_Msg_BC ("Duplicate package name.");
                        exit;
                     end if;

                     exit when Current_Package.Next = No_Package;
                     List := Current_Package.Next;
                  end loop;

                  Current_Package.Next := New_Pkg;
                  Packages.Table (List) := Current_Package;
               end;

            end if;
         end if;

         Scan;

         if Token = Tok_Renames then
            declare
               Imported : Project_Id := No_Project;
               Ren_Pkg  : Package_Id := No_Package;

            begin
               Scan;

               Expect (Tok_Identifier, "identifier");

               if Token = Tok_Identifier then
                  declare
                     Current : Project_List := Data.Imported_Projects;
                     Element : Project_Element;
                     Name : constant Name_Id := Token_Name;
                  begin
                     while Current /= Empty_Project_List loop
                        Element := Project_Lists.Table (Current);
                        if Projects.Table (Element.Project).Name = Name then
                           if Current_Verbosity = High then
                              Write_Str ("Found ");
                              Write_Line (Get_Name_String (Name));
                           end if;

                           Imported := Element.Project;

                           if Current_Verbosity = High then
                              Dump_Single_Variables
                                (Name => Projects.Table (Imported).Name,
                                 Variables =>
                                   Projects.Table (Imported).Decl.Variables);
                              Dump_Packages
                                (Name => Projects.Table (Imported).Name,
                                 Packages =>
                                   Projects.Table (Imported).Decl.Packages);
                           end if;

                           exit;

                        else
                           Current := Element.Next;
                        end if;
                     end loop;

                     if Imported = No_Project then
                        Error_Msg_BC
                          ("Not a project imported by this project.");
                     end if;
                  end;
               end if;

               Scan;

               Expect (Tok_Dot, ".");

               loop
                  Scan;

                  Expect (Tok_Identifier, "identifier");

                  if Token = Tok_Identifier then

                     declare
                        Name        : constant Name_Id := Token_Name;
                        Current     : Package_Id;
                        The_Package : Package_Element;

                     begin
                        if Ren_Pkg = No_Package then
                           Current := Projects.Table (Imported).Decl.Packages;
                        else
                           Current := Packages.Table (Ren_Pkg).Decl.Packages;
                        end if;

                        loop
                           if Current = No_Package then
                              Error_Msg_BC ("Unknown package.");
                              exit;
                           end if;

                           The_Package := Packages.Table (Current);

                           if The_Package.Name = Name then
                              if Current_Verbosity = High then
                                 Write_Str ("Found ");
                                 Write_Line (Get_Name_String (Name));
                              end if;

                              Ren_Pkg := Current;

                              if Current_Verbosity = High then
                                 Dump_Single_Variables
                                   (Name => Packages.Table (Ren_Pkg).Name,
                                    Variables =>
                                      Packages.Table (Ren_Pkg).Decl.Variables);
                              end if;

                              exit;

                           else
                              Current := The_Package.Next;
                           end if;
                        end loop;
                     end;
                  end if;

                  Scan;
                  exit when Token /= Tok_Dot;
               end loop;

               The_New_Package.Decl := Packages.Table (Ren_Pkg).Decl;

               if New_Do_Not_Skip then
                  Packages.Table (New_Pkg) := The_New_Package;

                  if Current_Verbosity = High then
                     Write_Line ("Dumping the single variables:");
                     Dump_Single_Variables
                       (Name => The_New_Package.Name,
                        Variables => The_New_Package.Decl.Variables);
                  end if;

               end if;

            end;

            Expect (Tok_Semicolon, ";");

         elsif Token = Tok_Is then

            --  We look for declarative items, increasing the level by one.

            Parse (Project, Data, New_Pkg, New_Do_Not_Skip, Level + 1);

            Expect (Tok_End, "end");

            if Token = Tok_End then
               Scan;
            end if;

            Expect (Tok_Identifier, "identifier");

            if
              Token = Tok_Identifier
              and then
              Token_Name /= Name
            then
               Error_Msg_BC ("Expected """ & Get_Name_String (Name) & """");
            end if;

            if Token /= Tok_Semicolon then
               Scan;
            end if;

            if New_Do_Not_Skip then

               if Current_Verbosity = High then
                  The_New_Package := Packages.Table (New_Pkg);
                  Dump_Single_Variables
                    (The_New_Package.Name,
                     The_New_Package.Decl.Variables);
               end if;

            end if;

            Expect (Tok_Semicolon, ";");

         else
            Error_Msg_BC ("Expected ""is"" or ""renames"".");
         end if;

      end if;

   end Package_Declaration;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean;
      Level       : Natural := 0)
   is
   begin
      loop
         --  We are always positioned at the token that precedes
         --  the first token of the declarative element.

         Scan;

         case Token is
            when Tok_Identifier =>
               declare
                  Name : constant Name_Id := Token_Name;

               begin
                  Scan;

                  --  We must distinguished between variable assignment
                  --  (Tok_Assign) and array component assignment
                  --  (Tok_Left_Par).

                  case Token is
                     when Tok_Colon_Equal =>
                        if Current_Verbosity = High then
                           Write_Line ("Variable Assignment");
                        end if;

                        Parse_Variable_Assignment
                          (Name, Project, Data, Pkg, Do_Not_Skip);

                     when Tok_Left_Paren =>
                        if Current_Verbosity = High then
                           Write_Line ("Array Component Assignment");
                        end if;

                        Parse_Array_Component_Assignment
                          (Name, Project, Data, Pkg, Do_Not_Skip);

                     when others =>
                        Error_Msg_BC ("Expected "":="" or '('.");
                  end case;
               end;

            when Tok_Package =>
               Package_Declaration (Project, Data, Pkg, Do_Not_Skip, Level);

            when Tok_Case =>
               Case_Construction (Project, Data, Pkg, Do_Not_Skip, Level);

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
     (Name        : Name_Id;
      Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean)
   is
      Id          : Array_Id;
      The_Array   : Array_Data;
      Elem_Id     : Array_Element_Id;
      The_Element : Array_Element;

   begin
      if Do_Not_Skip then
         if Pkg = No_Package then
            Id := Data.Decl.Arrays;
         else
            Id := Packages.Table (Pkg).Decl.Arrays;
         end if;

         if Id = No_Array then
            Arrays.Increment_Last;
            Id := Arrays.Last;
            The_Array.Name := Name;

            if Pkg = No_Package then
               Data.Decl.Arrays := Id;
            else
               Packages.Table (Pkg).Decl.Arrays := Id;
            end if;

         else
            loop
               The_Array := Arrays.Table (Id);
               exit when The_Array.Name = Name;

               if The_Array.Next = No_Array then
                  Arrays.Increment_Last;
                  Id := Arrays.Last;
                  The_Array.Next := Id;
                  The_Array :=
                    (Name  => Name,
                     Value => No_Array_Element,
                     Next  => No_Array);
                  exit;
               else
                  Id := The_Array.Next;
               end if;
            end loop;
         end if;
      end if;

      Scan;

      declare
         Index : constant Name_Id := Prj.Str.Value (Data, Pkg);

      begin

         Expect (Tok_Right_Paren, ")");

         Scan;

         Expect (Tok_Colon_Equal, ":=");

         if Do_Not_Skip then
            if The_Array.Value = No_Array_Element then
               Array_Elements.Increment_Last;
               Elem_Id := Array_Elements.Last;
               The_Element.Index := Index;
               The_Array.Value := Elem_Id;
               Arrays.Table (Id) := The_Array;

            else
               Elem_Id := The_Array.Value;
               loop
                  The_Element := Array_Elements.Table (Elem_Id);
                  exit when The_Element.Index = Index;

                  if The_Element.Next = No_Array_Element then
                     Array_Elements.Increment_Last;
                     Array_Elements.Table (Elem_Id).Next :=
                       Array_Elements.Last;
                     Elem_Id := Array_Elements.Last;
                     The_Element :=
                       (Index    => Index,
                        Value    => Nil_Variable_Value,
                        Next     => No_Array_Element);
                     exit;

                  else
                     Elem_Id := The_Element.Next;
                  end if;
               end loop;
            end if;

            Parse_Variable_Value
              (Result      => The_Element.Value,
               Project     => Project,
               Data        => Data,
               Pkg         => Pkg,
               Do_Not_Skip => Do_Not_Skip);

            Array_Elements.Table (Elem_Id) := The_Element;

         else
            declare
               Dummy_Value    : Variable_Value;

            begin
               Parse_Variable_Value
                 (Result      => Dummy_Value,
                  Project     => Project,
                  Data        => Data,
                  Pkg         => Pkg,
                  Do_Not_Skip => Do_Not_Skip);
            end;
         end if;
      end;

   end Parse_Array_Component_Assignment;

   -------------------------------
   -- Parse_Variable_Assignment --
   -------------------------------

   procedure Parse_Variable_Assignment
     (Name        : Name_Id;
      Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean)
   is
      Var          : Variable_Id;
      The_Variable : Variable;

   begin
      if Do_Not_Skip then
         if Pkg = No_Package then
            Var := Data.Decl.Variables;
         else
            Var := Packages.Table (Pkg).Decl.Variables;
         end if;

         if Var = No_Variable then
            Variable_Elements.Increment_Last;
            Var := Variable_Elements.Last;
            The_Variable.Name := Name;

            if Pkg = No_Package then
               Data.Decl.Variables := Var;
            else
               Packages.Table (Pkg).Decl.Variables := Var;
            end if;

         else
            loop
               The_Variable := Variable_Elements.Table (Var);
               exit when The_Variable.Name = Name;

               if The_Variable.Next = No_Variable then
                  Variable_Elements.Increment_Last;
                  Variable_Elements.Table (Var).Next := Variable_Elements.Last;
                  Var := Variable_Elements.Last;
                  The_Variable :=
                    (Next     => No_Variable,
                     Name     => Name,
                     Value    => Nil_Variable_Value);
                  exit;
               else
                  Var := The_Variable.Next;
               end if;
            end loop;
         end if;
      end if;

      Parse_Variable_Value
        (Result      => The_Variable.Value,
         Project     => Project,
         Data        => Data,
         Pkg         => Pkg,
         Do_Not_Skip => Do_Not_Skip);

      if Do_Not_Skip then
         Variable_Elements.Table (Var) := The_Variable;
      end if;

   end Parse_Variable_Assignment;

   --------------------------
   -- Parse_Variable_Value --
   --------------------------

   procedure Parse_Variable_Value
     (Result      : out Variable_Value;
      Project     : Project_Id;
      Data        : in out Project_Data;
      Pkg         : Package_Id;
      Do_Not_Skip : Boolean)
   is
      Location : constant Source_Ptr := Token_Ptr;
   begin
      Scan;

      if Token = Tok_Left_Paren then

         --  Result is a list

         declare
            Element    : String_Element;
            Current    : String_List_Id;
            The_String : String_Id;
            Location   : Source_Ptr;

         begin

            Result := (Kind     => List,
                       Location => Location,
                       Values   => Nil_String);
            Scan;

            if Token = Tok_Right_Paren then
               Scan;
            else

               Location := Scan_Ptr;
               The_String := Prj.Str.Value (Data, Pkg);

               if Do_Not_Skip then
                  String_Elements.Increment_Last;
                  Current := String_Elements.Last;
                  Result.Values := Current;
                  Element := (Value    => The_String,
                              Location => Location,
                              Next     => Nil_String);
               end if;

               loop

                  case Token is
                     when Tok_Right_Paren =>
                        Scan;
                        exit;

                     when Tok_Comma =>
                        Scan;

                        Location   := Scan_Ptr;
                        The_String := Prj.Str.Value (Data, Pkg);

                        if Do_Not_Skip then
                           String_Elements.Increment_Last;
                           Element.Next := String_Elements.Last;
                           String_Elements.Table (Current) := Element;
                           Current := String_Elements.Last;
                           Element :=
                             (Value    => The_String,
                              Location => Location,
                              Next     => Nil_String);
                        end if;

                     when others =>
                        Error_Msg_BC ("Expected ',' or ')'.");
                  end case;
               end loop;

               if Do_Not_Skip then
                  String_Elements.Table (Current) := Element;
               end if;

            end if;
         end;

         --  Result is a single string

      else
         Result := (Kind     => Single,
                    Location => Location,
                    Value    => Prj.Str.Value (Data, Pkg));

      end if;

      Expect (Tok_Semicolon, ";");

   end Parse_Variable_Value;

end Prj.Decl;
