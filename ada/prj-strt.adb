------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . S T R T                              --
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

with Errout;    use Errout;
with Prj.Attr;  use Prj.Attr;
with Prj.Tree;  use Prj.Tree;
with Scans;     use Scans;
with Sinfo;     use Sinfo;
with Stringt;   use Stringt;
with Table;
with Types;     use Types;

package body Prj.Strt is

   Initial_Size : constant := 8;

   type Name_Location is record
      Name     : Name_Id := No_Name;
      Location : Source_Ptr := No_Location;
   end record;

   type Name_Range is range 0 .. 3;
   subtype Name_Index is Name_Range range 1 .. Name_Range'Last;

   type Names is array (Name_Index) of Name_Location;
   --  Used to store 1 to 3 simple_names. 2 simple names are for
   --  <project>.<package>, <project>.<variable> or <package>.<variable>.
   --  3 simple names are for <project>.<package>.<variable>.

   type Choice_String is record
      The_String : String_Id;
      Already_Used : Boolean := False;
   end record;

   Choices_Initial   : constant := 10;
   Choices_Increment : constant := 10;

   Choice_Node_Low_Bound  : constant := 0;
   Choice_Node_High_Bound : constant := 099_999_999;

   type Choice_Node_Id is
     range Choice_Node_Low_Bound .. Choice_Node_High_Bound;

   First_Choice_Node_Id : constant Choice_Node_Id :=
     Choice_Node_Low_Bound;

   Empty_Choice : constant Choice_Node_Id :=
     Choice_Node_Low_Bound;

   Choice_First : constant Choice_Node_Id := First_Choice_Node_Id + 1;

   package Choices is
      new Table.Table (Table_Component_Type => Choice_String,
                       Table_Index_Type     => Choice_Node_Id,
                       Table_Low_Bound      => First_Choice_Node_Id,
                       Table_Initial        => Choices_Initial,
                       Table_Increment      => Choices_Increment,
                       Table_Name           => "Prj.Strt.Choices");
   --  Used to store the case labels and check that there is no duplicate.

   procedure Add (This_String : String_Id);
   --  Add a string to the case label list, indicating that it has not
   --  yet been used.

   procedure External_Reference (External_Value : out Project_Node_Id);
   --  Parse an external reference. Current token is "external".

   procedure Attribute_Reference
     (Reference       : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id);
   --  Parse an attribute reference. Current token is an apostrophe.

   procedure Terms (Term            : out Project_Node_Id;
                    Expr_Kind       : in out Variable_Kind;
                    Current_Project : Project_Node_Id;
                    Current_Package : Project_Node_Id);
   --  Recursive procedure to parse one term or several terms concatenated
   --  using "&".

   ---------
   -- Add --
   ---------

   procedure Add (This_String : String_Id) is
   begin
      Choices.Increment_Last;
      Choices.Table (Choices.Last) :=
        (The_String   => This_String,
         Already_Used => False);
   end Add;

   -------------------------
   -- Attribute_Reference --
   -------------------------

   procedure Attribute_Reference
     (Reference       : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id) is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Atribute_Reference);
      Current_Attribute : Attribute_Node_Id := First_Attribute;
   begin
      Reference := Empty_Node;

      Data.Location := Token_Ptr;

      --  Scan past apostrophe
      Scan;

      Expect (Tok_Identifier, "Identifier");

      if Token = Tok_Identifier then
         Data.Name := Token_Name;
         while Current_Attribute /= Empty_Attribute
           and then
           Attributes.Table (Current_Attribute).Name /= Data.Name
         loop
            Current_Attribute := Attributes.Table (Current_Attribute).Next;
         end loop;
         if Current_Attribute = Empty_Attribute then
            Error_Msg ("unknown attribute", Token_Ptr);
         elsif
           Attributes.Table (Current_Attribute).Kind_2 = Associative_Array
         then
            Error_Msg
              ("associative array attribute cannot be referenced",
               Token_Ptr);
         else
            Project_Nodes.Increment_Last;
            Reference := Project_Nodes.Last;
            Data.Field1 := Current_Project;
            Data.Field2 := Current_Package;
            Data.Expr_Kind := Attributes.Table (Current_Attribute).Kind_1;
            Project_Nodes.Table (Reference) := Data;
            Scan;
         end if;
      end if;
   end Attribute_Reference;

   ------------------------
   -- External_Reference --
   ------------------------

   procedure External_Reference (External_Value : out Project_Node_Id) is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind       => N_External_Value,
                              And_Expr_Kind => Single);

      Field_Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind       => N_Literal_String,
                              And_Expr_Kind => Single);

   begin

      Project_Nodes.Increment_Last;
      External_Value := Project_Nodes.Last;
      Data.Location := Token_Ptr;

      --  The current token is External

      --  Get the left parenthesis

      Scan;
      Expect (Tok_Left_Paren, "(");

      --  Scan past the left parenthesis

      if Token = Tok_Left_Paren then
         Scan;
      end if;

      --  Get the name of the external reference

      Expect (Tok_String_Literal, "literal string");

      if Token = Tok_String_Literal then
         Project_Nodes.Increment_Last;
         Data.Field1 := Project_Nodes.Last;
         Field_Data.Value := Strval (Token_Node);
         Project_Nodes.Table (Data.Field1) := Field_Data;

         --  Scan past the first argument
         Scan;

         case Token is

            when Tok_Right_Paren =>
               Project_Nodes.Table (External_Value) := Data;

               --  Scan past the right parenthesis
               Scan;

            when Tok_Comma =>

               --  Scan past the comma

               Scan;

               Expect (Tok_String_Literal, "literal string");

               --  Get the default

               if Token = Tok_String_Literal then
                  Project_Nodes.Increment_Last;
                  Data.Field2 := Project_Nodes.Last;
                  Field_Data.Value := Strval (Token_Node);
                  Project_Nodes.Table (Data.Field2) := Field_Data;
                  Project_Nodes.Table (External_Value) := Data;
                  Scan;
                  Expect (Tok_Right_Paren, ")");
               end if;

               --  Scan past the right parenthesis
               if Token = Tok_Right_Paren then
                  Scan;
               end if;

            when others =>
               Error_Msg ("',' or ')' expected", Token_Ptr);
               Project_Nodes.Table (External_Value) := Data;
         end case;
      end if;
   end External_Reference;

   -----------------------
   -- Parse_Choice_List --
   -----------------------

   procedure Parse_Choice_List (First_Choice : out Project_Node_Id) is
      Current_Choice : Project_Node_Id := Empty_Node;
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind       => N_Literal_String,
                              And_Expr_Kind => Single);
      Found : Boolean := False;
   begin
      Project_Nodes.Increment_Last;
      First_Choice   := Project_Nodes.Last;
      Current_Choice := Project_Nodes.Last;
      loop
         Expect (Tok_String_Literal, "literal string");
         exit when Token /= Tok_String_Literal;
         Data.Location := Token_Ptr;
         Data.Value := Strval (Token_Node);
         Found := False;
         for Choice in Choice_First .. Choices.Last loop
            if String_Equal (Choices.Table (Choice).The_String,
                             Data.Value)
            then
               Found := True;
               if Choices.Table (Choice).Already_Used then
                  Error_Msg ("duplicate case label", Token_Ptr);
               else
                  Choices.Table (Choice).Already_Used := True;
               end if;
               exit;
            end if;
         end loop;
         if not Found then
            Error_Msg ("illegal case label", Token_Ptr);
         end if;
         Scan;
         if Token = Tok_Vertical_Bar then
            Project_Nodes.Increment_Last;
            Data.Field1 := Project_Nodes.Last;
            Project_Nodes.Table (Current_Choice) := Data;
            Current_Choice := Data.Field1;
            Scan;
         else
            Data.Field1 := Empty_Node;
            Project_Nodes.Table (Current_Choice) := Data;
            exit;
         end if;
      end loop;
   end Parse_Choice_List;

   ----------------------
   -- Parse_Expression --
   ----------------------

   procedure Parse_Expression
     (Expression      : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id) is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Expression);
   begin
      Data.Location := Token_Ptr;
      Project_Nodes.Increment_Last;
      Expression := Project_Nodes.Last;
      Terms (Term            => Data.Field1,
             Expr_Kind       => Data.Expr_Kind,
             Current_Project => Current_Project,
             Current_Package => Current_Package);
      Project_Nodes.Table (Expression) := Data;
   end Parse_Expression;

   ----------------------------
   -- Parse_String_Type_List --
   ----------------------------

   procedure Parse_String_Type_List (First_String : out Project_Node_Id) is
      Last_String : Project_Node_Id := Empty_Node;
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind       => N_Literal_String,
                              And_Expr_Kind => Single);
   begin
      Project_Nodes.Increment_Last;
      First_String := Project_Nodes.Last;
      Last_String := Project_Nodes.Last;
      loop
         Expect (Tok_String_Literal, "literal string");
         exit when Token /= Tok_String_Literal;
         Data.Value := Strval (Token_Node);
         Data.Location := Token_Ptr;
         declare
            Current : Project_Node_Id := First_String;
         begin
            while Current /= Last_String loop
               if String_Equal (Project_Nodes.Table (Current).Value,
                                Data.Value)
               then
                  Error_Msg ("duplicate value in type", Token_Ptr);
                  exit;
               end if;
               Current := Project_Nodes.Table (Current).Field1;
            end loop;
         end;
         Scan;
         if Token /= Tok_Comma then
            Data.Field1 := Empty_Node;
            Project_Nodes.Table (Last_String) := Data;
            exit;
         else
            Project_Nodes.Increment_Last;
            Data.Field1 := Project_Nodes.Last;
            Project_Nodes.Table (Last_String) := Data;
            Last_String := Data.Field1;
            Scan;
         end if;
      end loop;
   end Parse_String_Type_List;

   ------------------------------
   -- Parse_Variable_Reference --
   ------------------------------

   procedure Parse_Variable_Reference
     (Variable        : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id) is
      The_Names       : Names;
      Last_Name       : Name_Range := 0;
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Variable_Reference);
      Current_Variable : Project_Node_Id := Empty_Node;
      The_Package : Project_Node_Id := Current_Package;
      The_Project : Project_Node_Id := Current_Project;
      Specified_Project : Project_Node_Id := Empty_Node;
      Specified_Package : Project_Node_Id := Empty_Node;
      Look_For_Variable : Boolean := True;
      First_Attribute   : Attribute_Node_Id := Empty_Attribute;
   begin
      Data.Location := Token_Ptr;

      for Index in The_Names'Range loop
         Expect (Tok_Identifier, "identifier");
         if Token /= Tok_Identifier then
            Look_For_Variable := False;
            exit;
         end if;
         Last_Name := Last_Name + 1;
         The_Names (Last_Name) :=
           (Name     => Token_Name,
            Location => Token_Ptr);
         Scan;
         exit when Token /= Tok_Dot;
         Scan;
      end loop;

      if Look_For_Variable then
         if Token = Tok_Apostrophe then
            --  Attribute reference
            case Last_Name is
               when 0 =>
                  --  Cannot happen
                  null;
               when 1 =>
                  for Index in Package_First .. Package_Attributes.Last loop
                     if Package_Attributes.Table (Index).Name =
                       The_Names (1).Name
                     then
                        First_Attribute :=
                          Package_Attributes.Table (Index).First_Attribute;
                        exit;
                     end if;
                  end loop;
                  if First_Attribute /= Empty_Attribute then
                     The_Package :=
                          Project_Nodes.Table (Current_Project).Packages;
                     while The_Package /= Empty_Node
                       and then
                       Project_Nodes.Table (The_Package).Name /=
                       The_Names (1).Name
                     loop
                        The_Package :=
                          Project_Nodes.Table (The_Package).Field3;
                     end loop;
                     if The_Package = Empty_Node then
                        Error_Msg ("package not yet defined",
                                   The_Names (1).Location);
                     end if;
                  else
                     First_Attribute := Attribute_First;
                     The_Package     := Empty_Node;
                     declare
                        The_Project_Name_And_Node :
                          constant Project_Name_And_Node :=
                          Projects_Htable.Get (The_Names (1).Name);
                     begin
                        if
                          The_Project_Name_And_Node = No_Project_Name_And_Node
                        then
                           Error_Msg ("unknown project",
                                      The_Names (1).Location);
                        else
                           The_Project := The_Project_Name_And_Node.Node;
                        end if;
                     end;
                  end if;

               when 2 =>
                  declare
                     With_Clause : Project_Node_Id :=
                       Project_Nodes.Table (Current_Project).Field1;
                  begin
                     while With_Clause /= Empty_Node loop
                        The_Project :=
                          Project_Nodes.Table (With_Clause).Field1;
                        exit when Project_Nodes.Table (The_Project).Name =
                          The_Names (1).Name;
                        With_Clause :=
                          Project_Nodes.Table (With_Clause).Field2;
                     end loop;
                     if With_Clause = Empty_Node then
                        Error_Msg ("unknown project",
                                   The_Names (1).Location);
                        The_Project := Empty_Node;
                        The_Package := Empty_Node;
                        First_Attribute := Attribute_First;
                     else
                        The_Package :=
                          Project_Nodes.Table (The_Project).Packages;
                        while
                          The_Package /= Empty_Node
                          and then
                          Project_Nodes.Table (The_Package).Name /=
                          The_Names (2).Name
                        loop
                           The_Package :=
                             Project_Nodes.Table (The_Package).Field3;
                        end loop;
                        if The_Package = Empty_Node then
                           Error_Msg ("package not declared in project",
                                      The_Names (2).Location);
                           First_Attribute := Attribute_First;
                        else
                           First_Attribute :=
                             Package_Attributes.Table
                               (Project_Nodes.Table (The_Package).Pkg_Id).
                                   First_Attribute;
                        end if;
                     end if;
                  end;

               when 3 =>
                  Error_Msg
                    ("too many single names for an attribute reference",
                     The_Names (1).Location);
                  Scan;
                  return;
            end case;

            Attribute_Reference
              (Variable,
               Current_Project => The_Project,
               Current_Package => The_Package,
               First_Attribute => First_Attribute);
            return;
         end if;
      end if;

      Project_Nodes.Increment_Last;
      Variable := Project_Nodes.Last;

      if Look_For_Variable then
         case Last_Name is
            when 0 =>
               --  Cannot happen
               null;
            when 1 =>
               Data.Name := The_Names (1).Name;
            when 2 =>
               Data.Name := The_Names (2).Name;
               The_Package := Project_Nodes.Table (Current_Project).Packages;
               while The_Package /= Empty_Node
                 and then
                 Project_Nodes.Table (The_Package).Name /= The_Names (1).Name
               loop
                  The_Package := Project_Nodes.Table (The_Package).Field3;
               end loop;
               if The_Package /= Empty_Node then
                  Specified_Package := The_Package;
                  The_Project := Empty_Node;
               else
                  declare
                     With_Clause : Project_Node_Id :=
                       Project_Nodes.Table (Current_Project).Field1;
                  begin
                     while With_Clause /= Empty_Node loop
                        The_Project :=
                          Project_Nodes.Table (With_Clause).Field1;
                        exit when Project_Nodes.Table (The_Project).Name =
                          The_Names (1).Name;
                        With_Clause :=
                          Project_Nodes.Table (With_Clause).Field2;
                     end loop;
                     if With_Clause = Empty_Node then
                        The_Project :=
                          Project_Nodes.Table
                            (Project_Nodes.Table (Current_Project).Field2).
                              Field2;
                        if The_Project /= Empty_Node
                          and then
                          Project_Nodes.Table (The_Project).Name /=
                          The_Names (1).Name then
                           The_Project := Empty_Node;
                        end if;
                     end if;
                     if The_Project = Empty_Node then
                        Error_Msg ("unknown package or project",
                                   The_Names (1).Location);
                        Look_For_Variable := False;
                     else
                        Specified_Project := The_Project;
                     end if;
                  end;
               end if;
            when 3 =>
               Data.Name := The_Names (3).Name;
               declare
                  With_Clause : Project_Node_Id :=
                    Project_Nodes.Table (Current_Project).Field1;
               begin
                  while With_Clause /= Empty_Node loop
                     The_Project :=
                       Project_Nodes.Table (With_Clause).Field1;
                     exit when Project_Nodes.Table (The_Project).Name =
                       The_Names (1).Name;
                     With_Clause :=
                       Project_Nodes.Table (With_Clause).Field2;
                  end loop;
                  if With_Clause = Empty_Node then
                     The_Project :=
                       Project_Nodes.Table
                       (Project_Nodes.Table (Current_Project).Field2).
                       Field2;
                     if The_Project /= Empty_Node
                       and then
                       Project_Nodes.Table (The_Project).Name /=
                       The_Names (1).Name then
                        The_Project := Empty_Node;
                     end if;
                  end if;
                  if The_Project = Empty_Node then
                     Error_Msg ("unknown package or project",
                                The_Names (1).Location);
                     Look_For_Variable := False;
                  else
                     Specified_Project := The_Project;
                     The_Package := Project_Nodes.Table (The_Project).Packages;
                     while The_Package /= Empty_Node
                       and then
                       Project_Nodes.Table (The_Package).Name /=
                          The_Names (2).Name
                     loop
                        The_Package :=
                          Project_Nodes.Table (The_Package).Field3;
                     end loop;
                     if The_Package = Empty_Node then
                        Error_Msg ("unknown package",
                                   The_Names (2).Location);
                        Look_For_Variable := False;
                     else
                        Specified_Package := The_Package;
                        The_Project := Empty_Node;
                     end if;
                  end if;
               end;

         end case;
      end if;

      if Look_For_Variable then
         Data.Field1 := Specified_Project;
         Data.Field2 := Specified_Package;
         if The_Package /= Empty_Node then
            Current_Variable :=
              Project_Nodes.Table (The_Package).Variables;

            while Current_Variable /= Empty_Node
              and then
              Project_Nodes.Table (Current_Variable).Name /= Data.Name
            loop
               Current_Variable :=
                 Project_Nodes.Table (Current_Variable).Field3;
            end loop;
         end if;

         if Current_Variable = Empty_Node
           and then The_Project /= Empty_Node
         then
            Current_Variable :=
              Project_Nodes.Table (The_Project).Variables;
            while Current_Variable /= Empty_Node
              and then
              Project_Nodes.Table (Current_Variable).Name /= Data.Name
            loop
               Current_Variable :=
                 Project_Nodes.Table (Current_Variable).Field3;
            end loop;
         end if;

         if Current_Variable = Empty_Node then
            Error_Msg ("unknown variable", The_Names (Last_Name).Location);
         end if;
      end if;

      if Current_Variable /= Empty_Node then
         Data.Expr_Kind := Project_Nodes.Table (Current_Variable).Expr_Kind;
         Data.Field3 := Project_Nodes.Table (Current_Variable).Field2;
      end if;

      Project_Nodes.Table (Variable) := Data;

   end Parse_Variable_Reference;

   ---------------------------------
   -- Start_New_Case_Construction --
   ---------------------------------

   procedure Start_New_Case_Construction (String_Type  : Project_Node_Id) is
      Current_String : Project_Node_Id := String_Type;
   begin
      Choices.Set_Last (First_Choice_Node_Id);
      while Current_String /= Empty_Node loop
         Add (This_String => Project_Nodes.Table (Current_String).Value);
         Current_String := Project_Nodes.Table (Current_String).Field1;
      end loop;
   end Start_New_Case_Construction;

   -----------
   -- Terms --
   -----------

   procedure Terms (Term            : out Project_Node_Id;
                    Expr_Kind       : in out Variable_Kind;
                    Current_Project : Project_Node_Id;
                    Current_Package : Project_Node_Id) is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Term);
      Term_Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Literal_String);
      Current_Expression : Project_Node_Id := Empty_Node;
      Next_Expression    : Project_Node_Id := Empty_Node;
      Current_Location : Source_Ptr;
   begin
      Data.Location := Token_Ptr;
      Project_Nodes.Increment_Last;
      Term := Project_Nodes.Last;

      case Token is

         when Tok_Left_Paren =>
            case Expr_Kind is
               when Undefined =>
                  Expr_Kind := List;
               when List =>
                  null;
               when Single =>
                  Expr_Kind := List;
                  Error_Msg
                    ("literal string list cannot appear in a string",
                     Token_Ptr);
            end case;

            Project_Nodes.Increment_Last;
            Data.Field1 := Project_Nodes.Last;
            Data.Location := Token_Ptr;
            Term_Data.Kind := N_Literal_String_List;
            Term_Data.Expr_Kind := List;

            Scan;
            if Token = Tok_Right_Paren then
               Scan;
            else
               loop
                  Current_Location := Token_Ptr;
                  Parse_Expression (Expression      => Next_Expression,
                                    Current_Project => Current_Project,
                                    Current_Package => Current_Package);
                  if Project_Nodes.Table (Current_Expression).Expr_Kind
                    = List
                  then
                     Error_Msg ("single expression expected",
                                Current_Location);
                  end if;
                  if Current_Expression = Empty_Node then
                     Term_Data.Field1 := Next_Expression;
                  else
                     Project_Nodes.Table (Current_Expression).Field2 :=
                       Next_Expression;
                  end if;
                  Current_Expression := Next_Expression;
                  exit when Token /= Tok_Comma;
                  Scan; -- past the comma
               end loop;
               Expect (Tok_Right_Paren, "(");
               if Token = Tok_Right_Paren then
                  Scan;
               end if;
            end if;

            Project_Nodes.Table (Data.Field1) := Term_Data;

         when Tok_String_Literal =>
            if Expr_Kind = Undefined then
               Expr_Kind := Single;
            end if;
            Project_Nodes.Increment_Last;
            Data.Field1 := Project_Nodes.Last;
            Project_Nodes.Increment_Last;
            Term_Data.Kind  := N_Literal_String;
            Term_Data.Value := Strval (Token_Node);
            Project_Nodes.Table (Data.Field1) := Term_Data;

            Scan;

         when Tok_Identifier =>

            Current_Location := Token_Ptr;
            Parse_Variable_Reference
              (Variable        => Data.Field1,
               Current_Project => Current_Project,
               Current_Package => Current_Package);

            if Expr_Kind = Undefined then
               Expr_Kind := Project_Nodes.Table (Data.Field1).Expr_Kind;
            elsif Expr_Kind = Single
              and then
              Project_Nodes.Table (Data.Field1).Expr_Kind = List
            then
               Expr_Kind := List;
               Error_Msg
                 ("list variable cannot appear in single string expression",
                  Current_Location);
            end if;

         when Tok_Project =>

            Current_Location := Token_Ptr;
            Scan;
            Expect (Tok_Apostrophe, "'");
            if Token = Tok_Apostrophe then
               Attribute_Reference
                 (Reference       => Data.Field1,
                  First_Attribute => Prj.Attr.Attribute_First,
                  Current_Project => Current_Project,
                  Current_Package => Empty_Node);
            end if;
            if Data.Field1 /= Empty_Node then
               if Expr_Kind = Undefined then
                  Expr_Kind := Project_Nodes.Table (Data.Field1).Expr_Kind;
               elsif Expr_Kind = Single
                 and then
                 Project_Nodes.Table (Data.Field1).Expr_Kind = List
               then
                  Error_Msg
                    ("lists cannot appear in single string expression",
                     Current_Location);
               end if;
            end if;

         when Tok_External =>
            if Expr_Kind = Undefined then
               Expr_Kind := Single;
            end if;
            External_Reference (External_Value => Data.Field1);

         when others =>
            Error_Msg ("cannot be part of an expression", Token_Ptr);
            Term := Empty_Node;
            return;
      end case;

      if Token = Tok_Ampersand then

         Scan;
         Terms (Term            => Data.Field2,
                Expr_Kind       => Expr_Kind,
                Current_Project => Current_Project,
                Current_Package => Current_Package);

      end if;

      Project_Nodes.Table (Term) := Data;

   end Terms;

end Prj.Strt;
