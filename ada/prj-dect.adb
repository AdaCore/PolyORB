------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . D E C T                             --
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

with Errout;     use Errout;
with Prj.Strt;
with Prj.Tree;   use Prj.Tree;
with Scans;      use Scans;
with Sinfo;      use Sinfo;
with Types;      use Types;
with Prj.Attr;   use Prj.Attr;

package body Prj.Dect is

   type Zone is (In_Project, In_Package, In_Case_Construction);

   procedure Parse_Attribute_Declaration
     (Attribute         : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id);
   --  Parse an attribute declaration.

   procedure Parse_Case_Construction
     (Case_Construction : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id);
   --  Parse a case construction

   procedure Parse_Declarative_Items
     (Declarations      : out Project_Node_Id;
      In_Zone           : Zone;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id);
   --  Parse declarative items. Depending on In_Zone, some declarative
   --  items may be forbiden.

   procedure Parse_Package_Declaration
     (Package_Declaration : out Project_Node_Id;
      Current_Project     : Project_Node_Id);
   --  Parse a package declaration

   procedure Parse_String_Type_Declaration
     (String_Type       : out Project_Node_Id;
      Current_Project   : Project_Node_Id;
      First_Attribute   : Attribute_Node_Id);
   --  type <name> is ( <literal_string> { , <literal_string> } ) ;

   procedure Parse_Variable_Declaration
     (Variable          : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id);
   --  Parse a variable assignment
   --  <variable_Name> := <expression>; OR
   --  <variable_Name> : <string_type_Name> := <string_expression>;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Declarations      : out Project_Node_Id;
      Current_Project   : Project_Node_Id;
      Modifying         : Project_Node_Id) is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Project_Declaration);
   begin

      Data.Field2   := Modifying;
      Data.Location := Token_Ptr;

      Project_Nodes.Increment_Last;
      Declarations := Project_Nodes.Last;
      Project_Nodes.Table (Declarations) := Data;
      Parse_Declarative_Items
        (Declarations    => Project_Nodes.Table (Declarations).Field1,
         In_Zone         => In_Project,
         First_Attribute => Prj.Attr.Attribute_First,
         Current_Project => Current_Project,
         Current_Package => Empty_Node);
   end Parse;

   ---------------------------------
   -- Parse_Attribute_Declaration --
   ---------------------------------

   procedure Parse_Attribute_Declaration
     (Attribute       : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id) is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Attribute_Declaration);
      Current_Attribute : Attribute_Node_Id := First_Attribute;
   begin
      Project_Nodes.Increment_Last;
      Attribute := Project_Nodes.Last;

      Data.Location := Token_Ptr;
      --  Scan past "for"
      Scan;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Data.Name := Token_Name;
         Data.Location := Token_Ptr;
         while Current_Attribute /= Empty_Attribute
           and then
           Attributes.Table (Current_Attribute).Name /= Data.Name
         loop
            Current_Attribute := Attributes.Table (Current_Attribute).Next;
         end loop;
         if Current_Attribute = Empty_Attribute then
            Error_Msg ("undefined attribute", Token_Ptr);
         end if;

         Scan;
      end if;

      if Token = Tok_Left_Paren then
         if Current_Attribute /= Empty_Attribute
           and then
           Attributes.Table (Current_Attribute).Kind_2 = Single
         then
            Error_Msg ("this attribute cannot be an associative array",
                       Data.Location);
         end if;
         Scan;
         Expect (Tok_String_Literal, "literal string");
         if Token = Tok_String_Literal then
            Data.Value := Strval (Token_Node);
            Scan;
         end if;
         Expect (Tok_Right_Paren, ")");
         if Token = Tok_Right_Paren then
            Scan;
         end if;
      else
         if Current_Attribute /= Empty_Attribute
           and then
           Attributes.Table (Current_Attribute).Kind_2
           = Associative_Array
         then
            Error_Msg ("this attribute need to be an associative array",
                       Data.Location);
         end if;
      end if;

      if Current_Attribute /= Empty_Attribute then
         Data.Expr_Kind := Attributes.Table (Current_Attribute).Kind_1;
      end if;

      Expect (Tok_Use, "use");

      if Token = Tok_Use then
         Scan;
         declare
            Expression_Location : constant Source_Ptr := Token_Ptr;
         begin
            Prj.Strt.Parse_Expression
              (Expression      => Data.Field1,
               Current_Project => Current_Project,
               Current_Package => Current_Package);
            if Current_Attribute /= Empty_Attribute
              and then
              Data.Field1 /= Empty_Node
              and then
              Attributes.Table (Current_Attribute).Kind_1 /=
              Project_Nodes.Table (Data.Field1).Expr_Kind
            then
               Error_Msg ("wrong expression kind for the attribute",
                          Expression_Location);
            end if;
         end;
      end if;

      Project_Nodes.Table (Attribute) := Data;

   end Parse_Attribute_Declaration;

   -----------------------------
   -- Parse_Case_Construction --
   -----------------------------

   procedure Parse_Case_Construction
     (Case_Construction : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id)
   is
      Case_Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Case_Construction);

      Case_Item_Data  : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Case_Item);
      Current_Item    : Project_Node_Id := Empty_Node;
      First_Case_Item : Boolean := True;

      Variable_Location : Source_Ptr := No_Location;

      String_Type     : Project_Node_Id := Empty_Node;
   begin

      Project_Nodes.Increment_Last;
      Case_Construction  := Project_Nodes.Last;
      Case_Data.Location := Token_Ptr;

      --  Scan past "case"

      Scan;

      --  Get the switch variable

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Variable_Location := Token_Ptr;
         Prj.Strt.Parse_Variable_Reference
           (Variable        => Case_Data.Field1,
            Current_Project => Current_Project,
            Current_Package => Current_Package);
      else
         if Token /= Tok_Is then
            Scan;
         end if;
      end if;

      if Case_Data.Field1 /= Empty_Node then
         String_Type := Project_Nodes.Table (Case_Data.Field1).Field3;
         if String_Type = Empty_Node then
            Error_Msg ("this variable is not typed", Variable_Location);
         end if;
      end if;

      Expect (Tok_Is, "is");

      if Token = Tok_Is then

         --  Scan past "is"

         Scan;
      end if;

      Prj.Strt.Start_New_Case_Construction (String_Type);

      When_Loop :

      while Token = Tok_When loop

         Project_Nodes.Increment_Last;
         if First_Case_Item then
            Case_Data.Field2 := Project_Nodes.Last;
            First_Case_Item := False;
         else
            Case_Item_Data.Field3 := Project_Nodes.Last;
            Project_Nodes.Table (Current_Item) := Case_Item_Data;
         end if;
         Current_Item := Project_Nodes.Last;
         Case_Item_Data := Default_Project_Node (Of_Kind => N_Case_Item);
         Case_Item_Data.Location := Token_Ptr;

         --  Scan past "when"

         Scan;

         if Token = Tok_Others then

            --  Scan past "others"

            Scan;

            Expect (Tok_Arrow, "=>");

            --  Empty_Node in Field1 of a Case_Item indicates
            --  the "when others =>" branch.

            Case_Item_Data.Field1 := Empty_Node;

            Parse_Declarative_Items
              (Declarations    => Case_Item_Data.Field2,
               In_Zone         => In_Case_Construction,
               First_Attribute => First_Attribute,
               Current_Project => Current_Project,
               Current_Package => Current_Package);

            --  "when others =>" must be the last branch, so save the
            --  Case_Item and exit

            Project_Nodes.Table (Current_Item) := Case_Item_Data;
            exit When_Loop;

         else

            Prj.Strt.Parse_Choice_List (First_Choice => Case_Item_Data.Field1);

            Expect (Tok_Arrow, "=>");

            Parse_Declarative_Items
              (Declarations    => Case_Item_Data.Field2,
               In_Zone         => In_Case_Construction,
               First_Attribute => First_Attribute,
               Current_Project => Current_Project,
               Current_Package => Current_Package);

            Project_Nodes.Table (Current_Item) := Case_Item_Data;

         end if;

      end loop When_Loop;

      Expect (Tok_End, "end case");

      if Token = Tok_End then

         --  Scan past "end"

         Scan;

         Expect (Tok_Case, "case");

      end if;

      --  Scan past "case"

      Scan;

      Expect (Tok_Semicolon, ";");

      Project_Nodes.Table (Case_Construction) := Case_Data;

   end Parse_Case_Construction;

   -----------------------------
   -- Parse_Declarative_Items --
   -----------------------------

   procedure Parse_Declarative_Items
     (Declarations    : out Project_Node_Id;
      In_Zone         : Zone;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Declarative_Item);
      Current_Declarative_Item : Project_Node_Id := Empty_Node;
      Current_Declaration      : Project_Node_Id := Empty_Node;
      Item_Location            : Source_Ptr      := No_Location;
   begin

      Declarations := Empty_Node;

      loop

         --  We are always positioned at the token that precedes
         --  the first token of the declarative element.
         --  Scan past it

         Scan;

         Item_Location := Token_Ptr;

         case Token is
            when Tok_Identifier =>

               if In_Zone = In_Case_Construction then
                  Error_Msg ("a variable cannot be declared here",
                             Token_Ptr);
               end if;

               Parse_Variable_Declaration
                 (Current_Declaration,
                  First_Attribute => First_Attribute,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package);

            when Tok_For =>

               Parse_Attribute_Declaration
                 (Attribute       => Current_Declaration,
                  First_Attribute => First_Attribute,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package);

            when Tok_Package =>

               --  Package declaration

               if In_Zone /= In_Project then
                  Error_Msg ("a package cannot be declared here",
                             Token_Ptr);
               end if;

               Parse_Package_Declaration
                 (Package_Declaration => Current_Declaration,
                  Current_Project     => Current_Project);

            when Tok_Type =>

               --  Type String Declaration

               if In_Zone /= In_Project then
                  Error_Msg ("a string type cannot be declared here",
                             Token_Ptr);
               end if;

               Parse_String_Type_Declaration
                 (String_Type     => Current_Declaration,
                  Current_Project => Current_Project,
                  First_Attribute => First_Attribute);

            when Tok_Case =>

               --  Case construction

               Parse_Case_Construction
                 (Case_Construction => Current_Declaration,
                  First_Attribute   => First_Attribute,
                  Current_Project   => Current_Project,
                  Current_Package   => Current_Package);

            when others =>
               exit;

               --  We are leaving Parse_Declarative_Items positionned
               --  at the first token after the list of declarative items.
               --  It could be "end" (for a project, a package declaration or
               --  a case construction) or "when" (for a case construction)

         end case;

         Expect (Tok_Semicolon, "; after declarative items");

         Project_Nodes.Increment_Last;
         if Current_Declarative_Item = Empty_Node then
            Current_Declarative_Item := Project_Nodes.Last;
            Declarations  := Current_Declarative_Item;
            Data.Field1   := Current_Declaration;
            Data.Location := Item_Location;
         else
            Data.Field2 := Project_Nodes.Last;
            Project_Nodes.Table (Current_Declarative_Item) := Data;
            Data := Default_Project_Node (Of_Kind => N_Declarative_Item);
            Data.Field1              := Current_Declaration;
            Data.Location            := Item_Location;
            Current_Declarative_Item := Project_Nodes.Last;
         end if;
         Project_Nodes.Table (Current_Declarative_Item) := Data;

      end loop;

   end Parse_Declarative_Items;

   -------------------------------
   -- Parse_Package_Declaration --
   -------------------------------

   procedure Parse_Package_Declaration
     (Package_Declaration : out Project_Node_Id;
      Current_Project     : Project_Node_Id)
   is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Package_Declaration);
      First_Attribute : Attribute_Node_Id := Empty_Attribute;
      Current_Package : Package_Node_Id   := Empty_Package;
   begin

      Data.Location := Token_Ptr;

      --  Scan past "package"

      Scan;

      Project_Nodes.Increment_Last;
      Package_Declaration := Project_Nodes.Last;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then

         Data.Name := Token_Name;

         for Index in Package_Attributes.First .. Package_Attributes.Last loop
            if Data.Name = Package_Attributes.Table (Index).Name then
               First_Attribute :=
                 Package_Attributes.Table (Index).First_Attribute;
               Current_Package := Index;
               exit;
            end if;
         end loop;
         if Current_Package  = Empty_Package then
            Error_Msg ("not an allowed package name",
                       Token_Ptr);
         else
            Data.Pkg_Id := Current_Package;
            declare
               Current : Project_Node_Id :=
                 Project_Nodes.Table (Current_Project).Packages;
            begin
               while Current /= Empty_Node
                 and then
                 Project_Nodes.Table (Current).Name /= Data.Name loop
                  Current := Project_Nodes.Table (Current).Field3;
               end loop;

               if Current /= Empty_Node then
                  Error_Msg ("package declared twice in the same project",
                             Token_Ptr);
               else

                  --  Add the package to the project list

                  Data.Field3 :=
                    Project_Nodes.Table (Current_Project).Packages;
                  Project_Nodes.Table (Current_Project).Packages :=
                    Package_Declaration;
               end if;
            end;
         end if;

         --  Scan past the package name

         Scan;

      end if;

      Data.Field3 := Project_Nodes.Table (Current_Project).Field3;
      Project_Nodes.Table (Current_Project).Field3 := Package_Declaration;

      if Token = Tok_Renames then
         --  Scan past "renames"
         Scan;

         Expect (Tok_Identifier, "identifier");

         if Token = Tok_Identifier then
            declare
               Project_Name : Name_Id := Token_Name;
               Clause : Project_Node_Id :=
                 Project_Nodes.Table (Current_Project).Field1;
               The_Project : Project_Node_Id := Empty_Node;
            begin
               while Clause /= Empty_Node loop
                  The_Project := Project_Nodes.Table (Clause).Field1;
                  exit when Project_Nodes.Table (The_Project).Name =
                    Project_Name;
                  Clause := Project_Nodes.Table (Clause).Field2;
               end loop;
               if Clause = Empty_Node then
                  Error_Msg ("not an imported project", Token_Ptr);
               else
                  Data.Field1 := The_Project;
               end if;
            end;
            Scan;
            Expect (Tok_Dot, ".");
            if Token = Tok_Dot then
               Scan;
               Expect (Tok_Identifier, "identifier");
               if Token = Tok_Identifier then
                  if Data.Name /= Token_Name then
                     Error_Msg ("not the same package name", Token_Ptr);
                  elsif Data.Field1 /= Empty_Node then
                     declare
                        Current : Project_Node_Id :=
                          Project_Nodes.Table (Data.Field1).Packages;
                     begin
                        while Current /= Empty_Node
                          and then
                          Project_Nodes.Table (Current).Name /= Token_Name
                        loop
                           Current :=
                             Project_Nodes.Table (Current).Field3;
                        end loop;
                        if Current = Empty_Node then
                           Error_Msg
                             ("not a package declared by the project",
                              Token_Ptr);
                        end if;
                     end;
                  end if;
                  Scan;
               end if;
            end if;
         end if;

         Expect (Tok_Semicolon, ";");

         Project_Nodes.Table (Package_Declaration) := Data;

      elsif Token = Tok_Is then

         Project_Nodes.Table (Package_Declaration) := Data;

         Parse_Declarative_Items
           (Declarations    =>
              Project_Nodes.Table (Package_Declaration).Field2,
            In_Zone         => In_Package,
            First_Attribute => First_Attribute,
            Current_Project => Current_Project,
            Current_Package => Package_Declaration);

         Expect (Tok_End, "end");

         if Token = Tok_End then

            --  Scan past "end"

            Scan;
         end if;

         --  We should have the name of the package after "end"

         Expect (Tok_Identifier, "identifier");

         if
           Token = Tok_Identifier
           and then
           Data.Name /= No_Name
           and then
           Token_Name /= Data.Name
         then
            Error_Msg_Name_1 := Data.Name;
            Error_Msg ("expected {", Token_Ptr);
         end if;

         if Token /= Tok_Semicolon then

            --  Scan past the package name

            Scan;
         end if;

         Expect (Tok_Semicolon, ";");

      else
         Error_Msg ("expected ""is"" or ""renames""", Token_Ptr);
      end if;

   end Parse_Package_Declaration;

   -----------------------------------
   -- Parse_String_Type_Declaration --
   -----------------------------------

   procedure Parse_String_Type_Declaration
     (String_Type     : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      First_Attribute : Attribute_Node_Id) is

      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_String_Type_Declaration);
      Current : Project_Node_Id := Empty_Node;
   begin
      Project_Nodes.Increment_Last;
      String_Type := Project_Nodes.Last;

      Data.Location := Token_Ptr;

      --  Scan past "type"
      Scan;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Data.Name := Token_Name;

         Current := Project_Nodes.Table (Current_Project).Field3;
         while Current /= Empty_Node
           and then
           Project_Nodes.Table (Current).Name /= Data.Name
         loop
            Current := Project_Nodes.Table (Current).Field2;
         end loop;
         if Current /= Empty_Node then
            Error_Msg ("duplicate string type name", Token_Ptr);
         else
            Current := Project_Nodes.Table (Current_Project).Variables;
            while Current /= Empty_Node
              and then
              Project_Nodes.Table (Current).Name /= Data.Name
            loop
               Current := Project_Nodes.Table (Current).Field3;
            end loop;
            if Current /= Empty_Node then
               Error_Msg ("already a variable name", Token_Ptr);
            else
               Data.Field2 := Project_Nodes.Table (Current_Project).Field3;
               Project_Nodes.Table (Current_Project).Field3 := String_Type;
            end if;
         end if;

         --  Scan past the name
         Scan;
      end if;

      Expect (Tok_Is, "is");

      if Token = Tok_Is then
         Scan;
      end if;

      Expect (Tok_Left_Paren, "(");

      if Token = Tok_Left_Paren then
         Scan;
      end if;

      Prj.Strt.Parse_String_Type_List (First_String => Data.Field1);

      Expect (Tok_Right_Paren, ")");

      if Token = Tok_Right_Paren then
         Scan;
      end if;

      Project_Nodes.Table (String_Type) := Data;

   end Parse_String_Type_Declaration;

   --------------------------------
   -- Parse_Variable_Declaration --
   --------------------------------

   procedure Parse_Variable_Declaration
     (Variable        : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      Data : Project_Node_Record :=
        Default_Project_Node (Of_Kind => N_Variable_Declaration);
      Expression_Location : Source_Ptr;
      String_Type_Name : Name_Id := No_Name;
      Project_String_Type_Name : Name_Id := No_Name;
      --  Variable_Location : constant Source_Ptr := Token_Ptr;
      Type_Location     : Source_Ptr := No_Location;
      Project_Location  : Source_Ptr := No_Location;
   begin

      Data.Name     := Token_Name;
      Data.Location := Token_Ptr;

      Project_Nodes.Increment_Last;
      Variable := Project_Nodes.Last;

      --  Scan past the variable name

      Scan;

      if Token = Tok_Colon then
         --  Typed string variable declaration
         Scan;
         Expect (Tok_Identifier, "identifier");
         if Token = Tok_Identifier then
            String_Type_Name := Token_Name;
            Type_Location := Token_Ptr;
            Scan;
            if Token = Tok_Dot then
               Project_String_Type_Name := String_Type_Name;
               Project_Location := Type_Location;

               --  Scan past the dot
               Scan;
               Expect (Tok_Identifier, "identifier");
               if Token = Tok_Identifier then
                  String_Type_Name := Token_Name;
                  Type_Location := Token_Ptr;
                  Scan;
               else
                  String_Type_Name := No_Name;
               end if;
            end if;
            if String_Type_Name /= No_Name then
               declare
                  Current : Project_Node_Id :=
                    Project_Nodes.Table (Current_Project).Field3;
               begin
                  if Project_String_Type_Name /= No_Name then
                     declare
                        The_Project_Name_And_Node : constant
                          Project_Name_And_Node :=
                          Projects_Htable.Get (Project_String_Type_Name);
                     begin
                        if The_Project_Name_And_Node =
                          No_Project_Name_And_Node
                        then
                           Error_Msg ("unknown project", Project_Location);
                           Current := Empty_Node;
                        else
                           Current := Project_Nodes.Table
                             (The_Project_Name_And_Node.Node).Field3;
                        end if;
                     end;
                  end if;
                  while Current /= Empty_Node
                    and then
                    Project_Nodes.Table (Current).Name /= String_Type_Name
                  loop
                     Current := Project_Nodes.Table (Current).Field2;
                  end loop;
                  if Current = Empty_Node then
                     Error_Msg ("unknown string type", Type_Location);
                  else
                     Data.Field2 := Project_Nodes.Table (Current).Field1;
                  end if;
               end;
            end if;
         end if;
      end if;

      Expect (Tok_Colon_Equal, ":=");

      if Token = Tok_Colon_Equal then
         Scan;
      end if;

      --  Get the single string or string list value

      Expression_Location := Token_Ptr;

      Prj.Strt.Parse_Expression
        (Expression      => Data.Field1,
         Current_Project => Current_Project,
         Current_Package => Current_Package);

      if Data.Field1 /= Empty_Node then
         Data.Expr_Kind := Project_Nodes.Table (Data.Field1).Expr_Kind;
      end if;

      declare
         The_Variable : Project_Node_Id := Empty_Node;
      begin
         if Current_Package /= Empty_Node then
            The_Variable :=  Project_Nodes.Table (Current_Package).Variables;
         elsif Current_Project /= Empty_Node then
            The_Variable :=  Project_Nodes.Table (Current_Project).Variables;
         end if;
         while The_Variable /= Empty_Node
           and then
           Project_Nodes.Table (The_Variable).Name /= Data.Name
         loop
            The_Variable := Project_Nodes.Table (The_Variable).Field3;
         end loop;
         if The_Variable = Empty_Node then
            if Current_Package /= Empty_Node then
               Data.Field3 := Project_Nodes.Table (Current_Package).Variables;
               Project_Nodes.Table (Current_Package).Variables := Variable;
            elsif Current_Project /= Empty_Node then
               Data.Field3 := Project_Nodes.Table (Current_Project).Variables;
               Project_Nodes.Table (Current_Project).Variables := Variable;
            end if;
         else
            if Data.Expr_Kind /= Undefined then
               if Project_Nodes.Table (The_Variable).Expr_Kind = Undefined then
                  Project_Nodes.Table (The_Variable).Expr_Kind :=
                    Data.Expr_Kind;
               else
                  if Project_Nodes.Table (The_Variable).Expr_Kind /=
                    Data.Expr_Kind
                  then
                     Error_Msg ("wrong expression kind for the variable",
                                Expression_Location);
                  end if;
               end if;
            end if;
         end if;
      end;

      Project_Nodes.Table (Variable) := Data;

   end Parse_Variable_Declaration;

end Prj.Dect;
