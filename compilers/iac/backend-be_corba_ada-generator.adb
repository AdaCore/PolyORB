------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       B A C K E N D . B E _ C O R B A _ A D A . G E N E R A T O R        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2011, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Backend.BE_CORBA_Ada;         use Backend.BE_CORBA_Ada;
with Backend.BE_CORBA_Ada.Nodes;   use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;  use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime; use Backend.BE_CORBA_Ada.Runtime;

with Charset;   use Charset;
with Namet;     use Namet;
with Output;    use Output;
with Outfiles;  use Outfiles;
with Values;    use Values;

package body Backend.BE_CORBA_Ada.Generator is

   procedure Generate_Access_Type_Definition (N : Node_Id);
   procedure Generate_Ada_Comment (N : Node_Id);
   procedure Generate_Array_Aggregate (N : Node_Id);
   procedure Generate_Array_Type_Definition (N : Node_Id);
   procedure Generate_String_Type_Definition (N : Node_Id);
   procedure Generate_Assignment_Statement (N : Node_Id);
   procedure Generate_Attribute_Reference (N : Node_Id);
   procedure Generate_Block_Statement (N : Node_Id);
   procedure Generate_Case_Statement (N : Node_Id);
   procedure Generate_Case_Statement_Alternative (N : Node_Id);
   procedure Generate_Component_Association (N : Node_Id);
   procedure Generate_Component_Declaration (N : Node_Id);
   procedure Generate_Decimal_Type_Definition (N : Node_Id);
   procedure Generate_Derived_Type_Definition (N : Node_Id);
   procedure Generate_Elsif_Statement (N : Node_Id);
   procedure Generate_Element_Association (N : Node_Id);
   procedure Generate_Enumeration_Type_Definition (N : Node_Id);
   procedure Generate_Exception_Declaration (N : Node_Id);
   procedure Generate_Explicit_Dereference (N : Node_Id);
   procedure Generate_Expression (N : Node_Id);
   procedure Generate_For_Statement (N : Node_Id);
   procedure Generate_Full_Type_Declaration (N : Node_Id);
   procedure Generate_Identifier (N : Node_Id);
   procedure Generate_IDL_Unit_Packages (N : Node_Id);
   procedure Generate_If_Statement (N : Node_Id);
   procedure Generate_Indexed_Component (N : Node_Id);
   procedure Generate_Instantiated_Subprogram (N : Node_Id);
   procedure Generate_Literal (N : Node_Id);
   procedure Generate_Null_Statement;
   procedure Generate_Object_Declaration (N : Node_Id);
   procedure Generate_Object_Instantiation (N : Node_Id);
   procedure Generate_Package_Declaration (N : Node_Id);
   procedure Generate_Package_Body (N : Node_Id);
   procedure Generate_Package_Instantiation (N : Node_Id);
   procedure Generate_Package_Specification (N : Node_Id);
   procedure Generate_Parameter (N : Node_Id);
   procedure Generate_Parameter_Association (N : Node_Id);
   procedure Generate_Parameter_List (L : List_Id);
   procedure Generate_Pragma (N : Node_Id);
   procedure Generate_Qualified_Expression (N : Node_Id);
   procedure Generate_Raise_Statement (N : Node_Id);
   procedure Generate_Range (N : Node_Id);
   procedure Generate_Record_Aggregate (N : Node_Id);
   procedure Generate_Record_Definition (N : Node_Id);
   procedure Generate_Record_Type_Definition (N : Node_Id);
   procedure Generate_Return_Statement (N : Node_Id);
   procedure Generate_Selected_Component (N : Node_Id);
   procedure Generate_Slice (N : Node_Id);
   procedure Generate_Subprogram_Call (N : Node_Id);
   procedure Generate_Subprogram_Body (N : Node_Id);
   procedure Generate_Subprogram_Specification (N : Node_Id);
   procedure Generate_Type_Conversion (N : Node_Id);
   procedure Generate_Used_Type (N : Node_Id);
   procedure Generate_Used_Package (N : Node_Id);
   procedure Generate_Variant_Part (N : Node_Id);
   procedure Generate_Withed_Package (N : Node_Id);

   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);

   procedure Generate_Statement_Delimiter (N : Node_Id);
   procedure Generate_Comment_Box (M : Name_Id);

   --  The entities declared below are related to the package
   --  generation in different files

   function Get_File_Name (N : Node_Id) return Name_Id;
   --  Generate an Ada file name from the package node given as
   --  parameter

   function Needs_Begin_End (B : Node_Id) return Boolean;
   --  Return True if the statement block B needs to be surrounded by
   --  begin..end.

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (N : Node_Id) return Name_Id is
      pragma Assert (Kind (N) = K_Package_Specification
                     or else Kind (N) = K_Package_Body);
      Package_Spec_Suffix : constant String := ".ads";
      Package_Body_Suffix : constant String := ".adb";
   begin
      --  The File name corresponding to a package is the lowered filly
      --  qualified name of the package. All '.' separators are
      --  replaced by '-'.

      Get_Name_String
        (Fully_Qualified_Name
         (Defining_Identifier
          (Package_Declaration
           (N))));

      --  Lower and replace all '.' by '-'

      for Index in 1 .. Name_Len loop
         if Name_Buffer (Index) = '.' then
            Name_Buffer (Index) := '-';
         else
            Name_Buffer (Index) := To_Lower (Name_Buffer (Index));
         end if;
      end loop;

      --  Adding file suffix

      if Kind (N) = K_Package_Specification then
         Add_Str_To_Name_Buffer (Package_Spec_Suffix);
      else
         Add_Str_To_Name_Buffer (Package_Body_Suffix);
      end if;

      return Name_Find;

   end Get_File_Name;

   ---------------------
   -- Needs_Begin_End --
   ---------------------

   function Needs_Begin_End (B : Node_Id) return Boolean is
   begin
      pragma Assert (Kind (B) = K_Block_Statement);

      return not (Is_Empty (Declarative_Part (B)) and then
                  Is_Empty (Exception_Handler (B)));
   end Needs_Begin_End;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Access_Type_Definition =>
            Generate_Access_Type_Definition (N);

         when K_Ada_Comment =>
            Generate_Ada_Comment (N);

         when K_Array_Aggregate =>
            Generate_Array_Aggregate (N);

         when K_Array_Type_Definition =>
            Generate_Array_Type_Definition (N);

         when K_String_Type_Definition =>
            Generate_String_Type_Definition (N);

         when K_Assignment_Statement =>
            Generate_Assignment_Statement (N);

         when K_Attribute_Reference =>
            Generate_Attribute_Reference (N);

         when K_Block_Statement =>
            Generate_Block_Statement (N);

         when K_Case_Statement =>
            Generate_Case_Statement (N);

         when K_Case_Statement_Alternative =>
            Generate_Case_Statement_Alternative (N);

         when K_Component_Association =>
            Generate_Component_Association (N);

         when K_Component_Declaration =>
            Generate_Component_Declaration (N);

         when K_Decimal_Type_Definition =>
            Generate_Decimal_Type_Definition (N);

         when K_Defining_Identifier =>
            Generate_Identifier (N);

         when K_Derived_Type_Definition =>
            Generate_Derived_Type_Definition (N);

         when K_Element_Association =>
            Generate_Element_Association (N);

         when K_Elsif_Statement =>
            Generate_Elsif_Statement (N);

         when K_Enumeration_Type_Definition =>
            Generate_Enumeration_Type_Definition (N);

         when K_Exception_Declaration =>
            Generate_Exception_Declaration (N);

         when K_Explicit_Dereference =>
            Generate_Explicit_Dereference (N);

         when K_Expression =>
            Generate_Expression (N);

         when K_For_Statement =>
            Generate_For_Statement (N);

         when K_Full_Type_Declaration =>
            Generate_Full_Type_Declaration (N);

         when K_Identifier =>
            Generate_Identifier (N);

         when K_IDL_Unit =>
            Generate_IDL_Unit_Packages (N);

         when K_If_Statement =>
            Generate_If_Statement (N);

         when K_Indexed_Component =>
            Generate_Indexed_Component (N);

         when K_Instantiated_Subprogram =>
            Generate_Instantiated_Subprogram (N);

         when K_Literal =>
            Generate_Literal (N);

         when K_Null_Statement =>
            Generate_Null_Statement;

         when K_Object_Declaration =>
            Generate_Object_Declaration (N);

         when K_Object_Instantiation =>
            Generate_Object_Instantiation (N);

         when K_Package_Declaration =>
            Generate_Package_Declaration (N);

         when K_Package_Body =>
            Generate_Package_Body (N);

         when K_Package_Instantiation =>
            Generate_Package_Instantiation (N);

         when K_Package_Specification =>
            Generate_Package_Specification (N);

         when K_Parameter_Association =>
            Generate_Parameter_Association (N);

         when K_Pragma =>
            Generate_Pragma (N);

         when K_Qualified_Expression =>
            Generate_Qualified_Expression (N);

         when K_Raise_Statement =>
            Generate_Raise_Statement (N);

         when K_Range =>
            Generate_Range (N);

         when K_Record_Aggregate =>
            Generate_Record_Aggregate (N);

         when K_Record_Definition =>
            Generate_Record_Definition (N);

         when K_Record_Type_Definition =>
            Generate_Record_Type_Definition (N);

         when K_Return_Statement =>
            Generate_Return_Statement (N);

         when K_Selected_Component =>
            Generate_Selected_Component (N);

         when K_Slice =>
            Generate_Slice (N);

         when K_Subprogram_Call =>
            Generate_Subprogram_Call (N);

         when K_Subprogram_Specification =>
            Generate_Subprogram_Specification (N);

         when K_Subprogram_Body =>
            Generate_Subprogram_Body (N);

         when K_Type_Conversion =>
            Generate_Type_Conversion (N);

         when K_Used_Type =>
            Generate_Used_Type (N);

         when K_Used_Package =>
            Generate_Used_Package (N);

         when K_Variant_Part =>
            Generate_Variant_Part (N);

         when K_Withed_Package =>
            Generate_Withed_Package (N);

         when K_Float .. K_Octet =>
            Write_Name (Image (Base_Type (N)));

         when others =>
            raise Program_Error with
              "no code generation defined for " & Kind (N)'Img;
      end case;
   end Generate;

   -------------------------------------
   -- Generate_Access_Type_Definition --
   -------------------------------------

   procedure Generate_Access_Type_Definition (N : Node_Id) is
   begin
      if Is_Not_Null (N) then
         Write (Tok_Not);
         Write_Space;
         Write (Tok_Null);
         Write_Space;
      end if;

      Write (Tok_Access);
      Write_Space;

      if Is_All (N) then
         Write (Tok_All);
         Write_Space;
      end if;

      if Is_Constant (N) then
         Write (Tok_Constant);
         Write_Space;
      end if;

      Generate (Subtype_Indication (N));
   end Generate_Access_Type_Definition;

   --------------------------
   -- Generate_Ada_Comment --
   --------------------------

   procedure Generate_Ada_Comment (N : Node_Id) is
      --  This procedure does the following :

      --  * It generates an Ada comment basing on the name of node N

      --  * If the name it too long, and depending on the location of
      --    the comment in the source code, the procedure splits the
      --    comment into more than a line.

      --  The comment is assumed to be a sequence of characters,
      --  beginning and ending with a NON-SPACE character.

      --  A word is a space character, or else a sequence of non space
      --  characters located between two spaces.

      Max_Line_Length : constant Natural := 78;
      --  The maximum length of a line, in columns

      function Buffer_Empty return Boolean;
      --  True when the Name_Buffer is empty

      function Next_Word_Length return Natural;
      --  Length of next word to be retrieved, or 0 if buffer is empty

      function Get_Next_Word return String;
      --  Extract the next word from the buffer

      ------------------
      -- Buffer_Empty --
      ------------------

      function Buffer_Empty return Boolean is
      begin
         return Name_Len = 0;
      end Buffer_Empty;

      ----------------------
      -- Next_Word_Length --
      ----------------------

      function Next_Word_Length return Natural is
         L : Natural;
      begin
         if Buffer_Empty then
            L := 0;

         elsif Name_Buffer (1) = ' ' then
            L := 1;

         else
            L := 0;
            while L + 1 <= Name_Len and then Name_Buffer (L + 1) /= ' ' loop
               L := L + 1;
            end loop;
         end if;
         return L;
      end Next_Word_Length;

      -------------------
      -- Get_Next_Word --
      -------------------

      function Get_Next_Word return String is
         L : constant Natural := Next_Word_Length;
      begin
         if L = 0 then
            return "";
         else
            declare
               Next_Word : constant String := Name_Buffer (1 .. L);
            begin
               if Name_Len = L then
                  Name_Len := 0;
               else
                  Set_Str_To_Name_Buffer (Name_Buffer (L + 1 .. Name_Len));
               end if;
               return Next_Word;
            end;
         end if;
      end Get_Next_Word;

      First_Line : Boolean := True;
      Used_Columns : Natural;
   begin
      Get_Name_String (Message (N));
      while not Buffer_Empty loop
         Used_Columns := N_Space;
         if First_Line then
            First_Line := False;
         else
            Write_Indentation;
         end if;

         --  We consume 4 clumsy

         Used_Columns := Used_Columns + 2;
         Write_Str ("--");

         if Has_Header_Spaces (N) then
            Used_Columns := Used_Columns + 2;
            Write_Str ("  ");
         end if;

         Used_Columns := Used_Columns + Next_Word_Length;
         Write_Str (Get_Next_Word);

         while not Buffer_Empty
           and then (Used_Columns + Next_Word_Length < Max_Line_Length)
         loop
            Used_Columns := Used_Columns + Next_Word_Length;
            Write_Str (Get_Next_Word);
         end loop;

         if not Buffer_Empty then
            Write_Eol;
         end if;
      end loop;
   end Generate_Ada_Comment;

   ------------------------------
   -- Generate_Array_Aggregate --
   ------------------------------

   procedure Generate_Array_Aggregate (N : Node_Id) is
      E : Node_Id;
   begin
      Write (Tok_Left_Paren);

      E := First_Node (Elements (N));
      loop
         Generate (E);
         E := Next_Node (E);
         exit when No (E);
         Write (Tok_Comma);
         Write_Space;
      end loop;

      Write (Tok_Right_Paren);
   end Generate_Array_Aggregate;

   ------------------------------------
   -- Generate_Array_Type_Definition --
   ------------------------------------

   procedure Generate_Array_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      Write (Tok_Array);
      Write_Space;
      Write (Tok_Left_Paren);
      if Present (Index_Definition (N)) then
         Generate (Index_Definition (N));
         if not Index_Def_Constrained (N) then
            Write_Space;
            Write (Tok_Range);
            Write_Space;
            Write (Tok_Less);
            Write (Tok_Greater);
         end if;
      else
         R := First_Node (Range_Constraints (N));
         loop
            Generate (First (R));
            Write_Space;
            Write (Tok_Dot);
            Write (Tok_Dot);
            Write_Space;
            Generate (Last (R));
            R := Next_Node (R);
            exit when No (R);
            Write (Tok_Comma);
            Write_Space;
         end loop;
      end if;
      Write (Tok_Right_Paren);
      Write_Space;
      Write (Tok_Of);
      Write_Space;
      Generate (Component_Definition (N));
   end Generate_Array_Type_Definition;

   -------------------------------------
   -- Generate_String_Type_Definition --
   -------------------------------------

   procedure Generate_String_Type_Definition (N : Node_Id) is
      R : Node_Id;
   begin
      Generate (Defining_Identifier (N));

      Write_Space;
      Write (Tok_Left_Paren);

      R := Range_Constraint (N);

      Generate (First (R));
      Write_Space;
      Write (Tok_Dot);
      Write (Tok_Dot);
      Write_Space;
      Generate (Last (R));
      Write (Tok_Right_Paren);
   end Generate_String_Type_Definition;

   -----------------------------------
   -- Generate_Assignment_Statement --
   -----------------------------------

   procedure Generate_Assignment_Statement (N : Node_Id) is
   begin
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Colon_Equal);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Generate (Expression (N));
      Decrement_Indentation;
   end Generate_Assignment_Statement;

   ----------------------------------
   -- Generate_Attribute_Reference --
   ----------------------------------

   procedure Generate_Attribute_Reference (N : Node_Id) is
   begin
      Generate (Prefix (N));
      Write (Tok_Apostrophe);
      Write_Name (Name (N));
   end Generate_Attribute_Reference;

   ------------------------------
   -- Generate_Block_Statement --
   ------------------------------

   procedure Generate_Block_Statement (N : Node_Id) is
      D           : Node_Id;
      N_Begin_End :  Boolean renames Needs_Begin_End (N);
   begin
      if Present (Defining_Identifier (N)) then
         Write_Eol;
         Decrement_Indentation;
         Write_Indentation (-1);
         Increment_Indentation;
         Generate (Defining_Identifier (N));
         Write_Line (Tok_Colon);
         Write_Indentation;
      end if;

      if not Is_Empty (Declarative_Part (N)) then
         Write (Tok_Declare);
         Write_Eol;
         Increment_Indentation;
         D := First_Node (Declarative_Part (N));
         loop
            Write_Indentation;
            Generate (D);
            Generate_Statement_Delimiter (D);
            D := Next_Node (D);
            exit when No (D);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
      end if;

      if N_Begin_End then
         Write (Tok_Begin);
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
      end if;

      D := First_Node (Statements (N));
      loop
         Generate (D);
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
         exit when No (D);
         Write_Indentation;
      end loop;

      if N_Begin_End then
         Decrement_Indentation;
         Write_Indentation;
      end if;

      if not Is_Empty (Exception_Handler (N)) then
         declare
            Excp_Handler_Alternative : Node_Id;
         begin
            Write (Tok_Exception);
            Write_Eol;
            Increment_Indentation;

            --  Generation of the exception handler

            Excp_Handler_Alternative := First_Node (Exception_Handler (N));
            while Present (Excp_Handler_Alternative) loop
               Generate (Excp_Handler_Alternative);

               Excp_Handler_Alternative :=
                 Next_Node (Excp_Handler_Alternative);
            end loop;
            Decrement_Indentation;
            Write_Indentation;
         end;
      end if;

      if N_Begin_End then
         Write (Tok_End);
      end if;
   end Generate_Block_Statement;

   -----------------------------
   -- Generate_Case_Statement --
   -----------------------------

   procedure Generate_Case_Statement (N : Node_Id) is
      D : Node_Id;

      O : Node_Id := No_Node;
      --  To ensure the `when others' is generated at the end of the
      --  `case' statement.
   begin
      Write (Tok_Case);
      Write_Space;
      Generate (Expression (N));
      Write_Space;
      Write_Line (Tok_Is);

      D := First_Node (Case_Statement_Alternatives (N));
      Increment_Indentation;

      while Present (D) loop
         if Is_Empty (Discret_Choice_List (D)) then
            --  Postpone the generation of the `when others' to the
            --  end of the case statement.

            O := D;
         else
            Generate (D);
         end if;

         D := Next_Node (D);
      end loop;

      --  Generate the `when others' clause

      if Present (O) then
         Generate (O);
      end if;

      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_Case);
   end Generate_Case_Statement;

   -----------------------------------------
   -- Generate_Case_Statement_Alternative --
   -----------------------------------------

   procedure Generate_Case_Statement_Alternative (N : Node_Id) is
      M : Node_Id;
      P : Node_Id;
   begin
      --  For an OTHERS choice, generate a pragma Warnings (Off), because
      --  all choices might be covered by the explicit alternatives.

      if Is_Empty (Discret_Choice_List (N)) then
         Write_Indentation;
         P := Make_Pragma (Pragma_Warnings, New_List (RE (RE_Off)));
         Generate (P);
         Generate_Statement_Delimiter (P);
      end if;

      --  Generate the choices

      Write_Indentation;
      Write (Tok_When);
      Write_Space;

      if Is_Empty (Discret_Choice_List (N)) then
         Write (Tok_Others);

      else
         M := First_Node (Discret_Choice_List (N));
         loop
            Generate (M);
            M := Next_Node (M);
            exit when No (M);
            Write_Space;
            Write (Tok_Vertical_Bar);
            Write_Space;
         end loop;
      end if;

      Write_Space;
      Write_Line (Tok_Arrow);

      --  Generate the statements

      Increment_Indentation;

      if Is_Empty (Statements (N)) then
         Write_Indentation;
         Write (Tok_Null);
         Write_Line (Tok_Semicolon);
      else
         M := First_Node (Statements (N));

         while Present (M) loop
            Write_Indentation;
            Generate (M);
            Generate_Statement_Delimiter (M);
            M := Next_Node (M);
         end loop;
      end if;

      Decrement_Indentation;

      --  Re-enable warnings

      if Is_Empty (Discret_Choice_List (N)) then
         Write_Indentation;
         P := Make_Pragma (Pragma_Warnings, New_List (RE (RE_On)));
         Generate (P);
         Generate_Statement_Delimiter (P);
      end if;
   end Generate_Case_Statement_Alternative;

   ------------------------------------
   -- Generate_Component_Association --
   ------------------------------------

   procedure Generate_Component_Association (N : Node_Id) is
   begin
      --  If the developer gives a defining identifier, we generate
      --  it, else we assume that the developer wants to generate a
      --  "others => XXXX" statement.

      if Present (Defining_Identifier (N)) then
         Generate (Defining_Identifier (N));
      else
         Write (Tok_Others);
      end if;

      Write_Space;
      Write (Tok_Arrow);
      Write_Space;
      Generate (Expression (N));
   end Generate_Component_Association;

   ------------------------------------
   -- Generate_Component_Declaration --
   ------------------------------------

   procedure Generate_Component_Declaration (N : Node_Id) is
      E : constant Node_Id := Expression (N);

   begin
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Colon);
      Write_Space;

      if Aliased_Present (N) then
         Write (Tok_Aliased);
         Write_Space;
      end if;

      Generate (Subtype_Indication (N));

      if Present (E) then
         Write_Space;
         Write (Tok_Colon_Equal);
         Write_Space;
         Generate (E);
      end if;
   end Generate_Component_Declaration;

   --------------------------------------
   -- Generate_Decimal_Type_Definition --
   --------------------------------------

   procedure Generate_Decimal_Type_Definition (N : Node_Id) is
   begin
      Write (Tok_Delta);
      Write_Space;

      Generate (Scale (N));
      Write_Space;

      Write (Tok_Digits);
      Write_Space;

      Write_Str (Values.Image_Ada (Total (N)));
   end Generate_Decimal_Type_Definition;

   --------------------------------------
   -- Generate_Derived_Type_Definition --
   --------------------------------------

   procedure Generate_Derived_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      if Is_Abstract_Type (N) then
         Write (Tok_Abstract);
         Write_Space;
      end if;

      if not Is_Subtype (N) then
         Write (Tok_New);
         Write_Space;
      end if;
      Generate (Subtype_Indication (N));

      if Is_Private_Extension (N) then
         Write_Space;
         Write (Tok_With);
         Write_Space;
         Write (Tok_Private);
      else
         R := Record_Extension_Part (N);

         if Present (R) then
            Write_Space;
            Write (Tok_With);
            Write_Space;
            Generate (Record_Extension_Part (N));
         else
            R := Range_Opt (N);
            if Present (R) then
               Write_Space;
               Write (Tok_Range);
               Write_Space;
               if Kind (R) = K_Literal then
                  declare
                     V : constant Value_Id :=
                       Backend.BE_CORBA_Ada.Nodes.Value (R);
                     Nm : constant Name_Id := Values.Value (V).SVal;
                  begin
                     Namet.Get_Name_String (Nm);
                     Write_Str (Name_Buffer (1 .. Name_Len));
                  end;
               else
                  Generate (R);
               end if;
            end if;
         end if;
      end if;
   end Generate_Derived_Type_Definition;

   ----------------------------------
   -- Generate_Element_Association --
   ----------------------------------

   procedure Generate_Element_Association (N : Node_Id) is
   begin
      if Present (Index (N)) then
         Generate (Index (N));
      else
         Write (Tok_Others);
      end if;

      Write_Space;
      Write (Tok_Arrow);
      Write_Space;

      Generate (Expression (N));
   end Generate_Element_Association;

   ------------------------------
   -- Generate_Elsif_Statement --
   ------------------------------

   procedure Generate_Elsif_Statement (N : Node_Id) is
      D : Node_Id;
   begin
      Write (Tok_Elsif);
      Write_Space;
      Generate (Condition (N));
      Write_Eol;
      Write_Indentation;
      Write_Line (Tok_Then);
      Increment_Indentation;
      D := First_Node (Then_Statements (N));
      while Present (D) loop
         Write_Indentation;
         Generate (D);
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
      end loop;
      Decrement_Indentation;
   end Generate_Elsif_Statement;

   ------------------------------------------
   -- Generate_Enumeration_Type_Definition --
   ------------------------------------------

   procedure Generate_Enumeration_Type_Definition (N : Node_Id) is
      E : Node_Id;

   begin
      Write (Tok_Left_Paren);
      E := First_Node (Enumeration_Literals (N));

      loop
         Generate (E);
         E := Next_Node (E);
         exit when No (E);
         Write_Line (Tok_Comma);
         Write_Indentation;
      end loop;

      Write (Tok_Right_Paren);
   end Generate_Enumeration_Type_Definition;

   ------------------------------------
   -- Generate_Exception_Declaration --
   ------------------------------------

   procedure Generate_Exception_Declaration (N : Node_Id) is
   begin
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;
      Write (Tok_Colon);
      Write_Space;
      Write (Tok_Exception);

      if Present (Renamed_Entity (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Renames);
         Write_Space;
         Generate (Renamed_Entity (N));
         Decrement_Indentation;
      end if;
   end Generate_Exception_Declaration;

   -----------------------------------
   -- Generate_Explicit_Dereference --
   -----------------------------------

   procedure Generate_Explicit_Dereference (N : Node_Id) is
   begin
      Generate (Prefix (N));
      Write (Tok_Dot);
      Write (Tok_All);
   end Generate_Explicit_Dereference;

   -------------------------
   -- Generate_Expression --
   -------------------------

   procedure Generate_Expression (N : Node_Id) is
      L_Expr  : constant Node_Id     := Left_Expr (N);
      Op      : constant Operator_Id := Operator (N);
      R_Expr  : constant Node_Id     := Right_Expr (N);
   begin
      --  Each expression having a right part and a left part is
      --  systematically put between two parentheses.

      if No (R_Expr) then
         if Op = Operator_Type'Pos (Op_Not) then
            Write (Tok_Not);
            Write_Space;
         elsif Op /= Operator_Type'Pos (Op_None) then
            Write_Name (Operator_Image (Standard.Integer (Op)));

            --  Do not generate space after a unary operator
         end if;
      else
         --  Expressions having "|" as operator (case switches
         --  alternatives) and expressions having "&" as operator
         --  (array concatenation) do not require parentheses.

         if Op /= Operator_Type'Pos (Op_Vertical_Bar)
           and then Op /= Operator_Type'Pos (Op_And_Symbol)
         then
            Write (Tok_Left_Paren);
         end if;
      end if;

      Generate (L_Expr);

      if Present (R_Expr) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;

         Write_Name (Operator_Image (Standard.Integer (Op)));
         Write_Space;
         Generate (R_Expr);

         if Op /= Operator_Type'Pos (Op_Vertical_Bar)
           and then Op /= Operator_Type'Pos (Op_And_Symbol)
         then
            Write (Tok_Right_Paren);
         end if;

         Decrement_Indentation;
      end if;
   end Generate_Expression;

   ----------------------------
   -- Generate_For_Statement --
   ----------------------------

   procedure Generate_For_Statement (N : Node_Id) is
      D : Node_Id := First_Node (Statements (N));
   begin
      Write (Tok_For);
      Write_Space;
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;
      Write (Tok_In);
      Write_Space;
      Generate (First (Range_Constraint (N)));
      Write_Space;
      Write (Tok_Dot);
      Write (Tok_Dot);
      Write_Space;
      Generate (Last (Range_Constraint (N)));
      Write_Space;
      Write (Tok_Loop);
      Write_Eol;
      Increment_Indentation;

      while Present (D) loop
         Write_Indentation;
         Generate (D);
         Generate_Statement_Delimiter (D);
         D := Next_Node (D);
      end loop;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_Loop);
   end Generate_For_Statement;

   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------

   procedure Generate_Full_Type_Declaration (N : Node_Id) is
      D : constant List_Id := Discriminant_Spec (N);
      M : Node_Id;
   begin
      if Is_Subtype (N) then
         Write (Tok_Subtype);
      else
         Write (Tok_Type);
      end if;

      Write_Space;
      Write_Name (Name (Defining_Identifier (N)));

      if not Is_Empty (D) then
         M := First_Node (D);

         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);

         while Present (M) loop
            Generate (M);

            if Present (Next_Node (M)) then
               Generate_Statement_Delimiter (M);
               Write_Space;
            end if;

            M := Next_Node (M);
         end loop;

         Write (Tok_Right_Paren);
         Decrement_Indentation;
         Write_Eol;
         Write_Indentation;
      else
         Write_Space;
      end if;

      Write (Tok_Is);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Generate (Type_Definition (N));
      Decrement_Indentation;
   end Generate_Full_Type_Declaration;

   -------------------------
   -- Generate_Identifier --
   -------------------------

   procedure Generate_Identifier (N : Node_Id) is
   begin
      Write_Name (Name (N));
   end Generate_Identifier;

   --------------------------------
   -- Generate_IDL_Unit_Packages --
   --------------------------------

   procedure Generate_IDL_Unit_Packages (N : Node_Id) is
      P : Node_Id := First_Node (Packages (N));
   begin
      if not Generate_Imported
        and then not Generate_Code (N)
      then
         return;
      end if;

      while Present (P) loop
         Generate (P);
         P := Next_Node (P);
      end loop;
   end Generate_IDL_Unit_Packages;

   ---------------------------
   -- Generate_If_Statement --
   ---------------------------

   procedure Generate_If_Statement (N : Node_Id) is
      T : constant List_Id := Then_Statements (N);
      E : constant List_Id := Else_Statements (N);
      I : Node_Id;

   begin
      --  Enter If_Statement

      Write (Tok_If);
      Write_Space;
      Generate (Condition (N));
      Write_Eol;
      Write_Indentation;
      Write (Tok_Then);
      Write_Eol;

      --  If_Statement cannot be empty. A null statement is always
      --  there if needed.

      Increment_Indentation;
      I := First_Node (T);

      while Present (I) loop
         Write_Indentation;
         Generate (I);
         Generate_Statement_Delimiter (I);
         I := Next_Node (I);
      end loop;

      Decrement_Indentation;

      --  Elsif_Statements

      if not Is_Empty (Elsif_Statements (N)) then
         I := First_Node (Elsif_Statements (N));

         loop
            Write_Indentation;
            Generate (I);
            I := Next_Node (I);
            exit when No (I);
         end loop;
      end if;

      --  Else_Statement can be empty

      if not Is_Empty (E) then
         Write_Indentation;
         Write (Tok_Else);
         Write_Eol;
         Increment_Indentation;
         I := First_Node (E);

         while Present (I) loop
            Write_Indentation;
            Generate (I);
            Generate_Statement_Delimiter (I);
            I := Next_Node (I);
         end loop;
         Decrement_Indentation;
      end if;

      --  Leave If_Statement

      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_If);
   end Generate_If_Statement;

   --------------------------------
   -- Generate_Indexed_Component --
   --------------------------------

   procedure Generate_Indexed_Component (N : Node_Id) is
      Exp : constant List_Id := Expressions (N);
      E   : Node_Id;

   begin
      Generate (Prefix (N));

      pragma Assert (not Is_Empty (Exp));

      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_Left_Paren);
      E := First_Node (Exp);

      loop
         Generate (E);
         E := Next_Node (E);
         exit when No (E);
         Write_Line (Tok_Comma);
         Write_Indentation;
      end loop;

      Write (Tok_Right_Paren);
      Decrement_Indentation;
   end Generate_Indexed_Component;

   --------------------------------------
   -- Generate_Instantiated_Subprogram --
   --------------------------------------

   procedure Generate_Instantiated_Subprogram (N : Node_Id) is
      L : constant List_Id := Parameter_List (N);
      P : Node_Id;

   begin
      Generate (Defining_Identifier (N));

      if not Is_Empty (L) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);
         P := First_Node (L);

         loop
            Generate (P);
            P := Next_Node (P);
            exit when No (P);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;

         Write (Tok_Right_Paren);
         Decrement_Indentation;
      end if;

   end Generate_Instantiated_Subprogram;

   ----------------------
   -- Generate_Literal --
   ----------------------

   procedure Generate_Literal (N : Node_Id) is
   begin
      Write_Str (Values.Image_Ada (Value (N)));
   end Generate_Literal;

   -----------------------------
   -- Generate_Null_Statement --
   -----------------------------

   procedure Generate_Null_Statement is
   begin
      Write (Tok_Null);
   end Generate_Null_Statement;

   ---------------------------------
   -- Generate_Object_Declaration --
   ---------------------------------

   procedure Generate_Object_Declaration (N : Node_Id) is
   begin
      Name_Buffer (1 .. Var_Name_Len) := (others => ' ');
      Get_Name_String (Name (Defining_Identifier (N)));

      if Var_Name_Len > Name_Len then
         Name_Len := Var_Name_Len;
      end if;

      Write_Str (Name_Buffer (1 .. Name_Len));
      Write_Space;
      Write (Tok_Colon);

      if Constant_Present (N) then
         Write_Space;
         Write (Tok_Constant);
      end if;

      if Aliased_Present (N) then
         Write_Space;
         Write (Tok_Aliased);
      end if;

      Write_Space;
      if Present (Object_Definition (N)) then
         Generate (Object_Definition (N));
      else
         --  This workaround doesn't affect the classic object
         --  declaration because we must give a type. However it makes
         --  the generation of case statement and exception handlers
         --  simpler.

         Write (Tok_Others);
      end if;

      if Present (Renamed_Entity (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Renames);
         Write_Space;
         Generate (Renamed_Entity (N));
         Decrement_Indentation;

         --  If an object renames another object, it cannot be
         --  initialized,
      else
         if Present (Expression (N)) then
            Write_Space;
            Write (Tok_Colon_Equal);
            Write_Eol;
            Increment_Indentation;
            Write_Indentation (-1);
            Generate (Expression (N));
            Decrement_Indentation;
         end if;
      end if;
   end Generate_Object_Declaration;

   -----------------------------------
   -- Generate_Object_Instantiation --
   -----------------------------------

   procedure Generate_Object_Instantiation (N : Node_Id) is
   begin
      Write (Tok_New);
      Write_Space;
      Generate (Qualified_Expression (N));
   end Generate_Object_Instantiation;

   ---------------------------
   -- Generate_Package_Body --
   ---------------------------

   procedure Generate_Package_Body (N : Node_Id) is
      P  : Node_Id;
      Fd : File_Descriptor;
      Dcl  : constant Node_Id := Package_Declaration (N);
      IDLU : constant Node_Id := IDL_Unit (Dcl);
      Impl : constant Boolean := Dcl = Implementation_Package (IDLU);
   begin
      --  If the user wants to generates only the spec, or if the
      --  package body is empty, we don't generate it.

      --  For Helper's packages, they always contain the package
      --  implementation of their corresponding 'Internals'
      --  sub-package. We do not generate a file for these Helper's if
      --  they contain only the 'Internals' package body and if this
      --  body is empty.

      if Disable_Pkg_Body_Gen
        or else (Is_Empty (Statements (N)) and then not Impl)
        or else (Length (Statements (N)) = 1
                 and then Kind (First_Node (Statements (N))) =
                 K_Package_Body
                 and then Is_Empty (Statements (First_Node (Statements (N)))))
      then
         return;
      end if;

      if not Is_Nested_Package
        (Package_Specification (Package_Declaration (N)))
      then
         Fd := Set_Output (Get_File_Name (N));

         P := First_Node (Context_Clause (N));

         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
         end loop;
      end if;

      Write_Eol;

      Write_Indentation;
      Write (Tok_Package);
      Write_Space;
      Write (Tok_Body);
      Write_Space;

      if Kind (N) = K_Package_Body
        and then Is_Nested_Package
        (Package_Specification (Package_Declaration (N)))
      then
         Write_Name (Get_Name (Get_Base_Identifier (Package_Declaration (N))));
      else
         Generate (Defining_Identifier (Package_Declaration (N)));
      end if;

      Write_Space;
      Write (Tok_Is);
      Write_Eol (2);

      Increment_Indentation;
      P := First_Node (Statements (N));

      while Present (P) loop
         Write_Indentation;
         Generate (P);

         if not (Kind (P) = K_Package_Body
                 and then Is_Nested_Package
                 (Package_Specification (Package_Declaration (P))))
         then
            Generate_Statement_Delimiter (P);
         end if;
         Write_Eol;
         P := Next_Node (P);
      end loop;

      Decrement_Indentation;
      Write_Indentation;

      if not Is_Empty (Package_Initialization (N)) then
         Write_Line (Tok_Begin);
         Increment_Indentation;
         P := First_Node (Package_Initialization (N));

         loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
            exit when No (P);
         end loop;

         Decrement_Indentation;
         Write_Indentation;
      end if;

      Write  (Tok_End);
      Write_Space;

      if Kind (N) = K_Package_Body
        and then Is_Nested_Package
        (Package_Specification (Package_Declaration (N)))
      then
         Write_Name (Get_Name (Get_Base_Identifier (Package_Declaration (N))));
      else
         Generate (Defining_Identifier (Package_Declaration (N)));
      end if;

      Generate_Statement_Delimiter
        (Defining_Identifier
         (Package_Declaration (N)));

      if not Is_Nested_Package
               (Package_Specification (Package_Declaration (N)))
      then
         Release_Output (Fd);
         Fd := Invalid_FD;
      end if;
   end Generate_Package_Body;

   ----------------------------------
   -- Generate_Package_Declaration --
   ----------------------------------

   procedure Generate_Package_Declaration (N : Node_Id) is
   begin
      Generate (Package_Specification (N));
      Generate (Package_Body (N));
   end Generate_Package_Declaration;

   ------------------------------------
   -- Generate_Package_Instantiation --
   ------------------------------------

   procedure Generate_Package_Instantiation (N : Node_Id) is
      Param : Node_Id;
   begin
      Write (Tok_Package);
      Write_Space;
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;
      Write (Tok_Is);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_New);
      Write_Space;
      Generate (Generic_Package (N));

      if not Is_Empty (Parameter_List (N)) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);
         Param := First_Node (Parameter_List (N));

         loop
            Generate (Param);
            Param := Next_Node (Param);
            exit when No (Param);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;

         Write (Tok_Right_Paren);
         Decrement_Indentation;
      end if;

      Decrement_Indentation;
   end Generate_Package_Instantiation;

   ------------------------------------
   -- Generate_Package_Specification --
   ------------------------------------

   procedure Generate_Package_Specification (N : Node_Id) is
      P  : Node_Id;
      Fd : File_Descriptor;
   begin
      --  If the user wants to generates only the body, or if the
      --  package spec is empty, we don't generate it.

      if Disable_Pkg_Spec_Gen
        or else (Is_Empty (Visible_Part (N))
                 and then Is_Empty (Private_Part (N)))
      then
         return;
      end if;

      if not Is_Nested_Package (N) then
         Fd := Set_Output (Get_File_Name (N));
         P := First_Node (Context_Clause (N));

         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            P := Next_Node (P);
         end loop;
      end if;

      Write_Eol;

      Write_Indentation;
      Write (Tok_Package);
      Write_Space;

      if Is_Nested_Package (N) then
         Write_Name (Get_Name (Get_Base_Identifier (Package_Declaration (N))));
      else
         Generate (Defining_Identifier (Package_Declaration (N)));
      end if;

      Write_Space;
      Write (Tok_Is);
      Write_Eol (2);

      Increment_Indentation;
      P := First_Node (Visible_Part (N));

      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Generate_Statement_Delimiter (P);
         Write_Eol;
         P := Next_Node (P);
      end loop;

      P := First_Node (Nested_Packages (N));

      while Present (P) loop
         Write_Indentation;
         Generate (P);
         if not (Kind (P) = K_Package_Specification
                 and then Is_Nested_Package (P))
         then
            Generate_Statement_Delimiter (P);
         end if;
         Write_Eol;
         P := Next_Node (P);
      end loop;

      Decrement_Indentation;

      if not Is_Empty (Private_Part (N)) then
         Write_Indentation;
         Write (Tok_Private);
         Write_Eol;
         Increment_Indentation;
         P := First_Node (Private_Part (N));

         while Present (P) loop
            Write_Indentation;
            Generate (P);
            Generate_Statement_Delimiter (P);
            Write_Eol;
            P := Next_Node (P);
         end loop;

         Decrement_Indentation;
      end if;

      Write_Indentation;
      Write (Tok_End);
      Write_Space;

      if Is_Nested_Package (N) then
         Write_Name (Get_Name (Get_Base_Identifier (Package_Declaration (N))));
      else
         Generate (Defining_Identifier (Package_Declaration (N)));
      end if;

      Generate_Statement_Delimiter
        (Defining_Identifier
         (Package_Declaration (N)));

      if not Is_Nested_Package (N) then
         Release_Output (Fd);
         Fd := Invalid_FD;
      end if;
   end Generate_Package_Specification;

   ------------------------
   -- Generate_Parameter --
   ------------------------

   procedure Generate_Parameter (N : Node_Id) is
   begin
      Name_Buffer (1 .. Var_Name_Len) := (others => ' ');
      Get_Name_String (Name (Defining_Identifier (N)));

      if Var_Name_Len > Name_Len then
         Name_Len := Var_Name_Len;
      end if;

      Write_Str (Name_Buffer (1 .. Name_Len));
      Write_Space;
      Write  (Tok_Colon);

      if Kind (Parameter_Type (N)) /= K_Access_Type_Definition then
         case Parameter_Mode (N) is
            when Mode_In =>
               null;

            when Mode_Out =>
               Write_Space;
               Write (Tok_Out);

            when Mode_Inout =>
               Write_Space;
               Write (Tok_In);
               Write_Space;
               Write (Tok_Out);
         end case;
      end if;

      Write_Space;
      Generate (Parameter_Type (N));

      if Present (Expression (N)) then
         Write_Space;
         Write (Tok_Colon_Equal);
         Write_Space;
         Generate (Expression (N));
      end if;
   end Generate_Parameter;

   ------------------------------------
   -- Generate_Parameter_Association --
   ------------------------------------

   procedure Generate_Parameter_Association (N : Node_Id) is
   begin
      Generate (Selector_Name (N));
      Write_Space;
      Write (Tok_Arrow);
      Write_Space;
      Generate (Actual_Parameter (N));
   end Generate_Parameter_Association;

   -----------------------------
   -- Generate_Parameter_List --
   -----------------------------

   procedure Generate_Parameter_List (L : List_Id) is
      N : Node_Id;

   begin
      --  If we got there, then L is not empty.

      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_Left_Paren);
      N := First_Node (L);

      loop
         Generate_Parameter (N);
         exit when No (Next_Node (N));
         Generate_Statement_Delimiter (N);
         Write_Indentation;
         N := Next_Node (N);
      end loop;

      Write (Tok_Right_Paren);
      Decrement_Indentation;
   end Generate_Parameter_List;

   ---------------------
   -- Generate_Pragma --
   ---------------------

   procedure Generate_Pragma (N : Node_Id) is
      Args : constant List_Id := Nodes.Argument_List (N);
      Arg  : Node_Id;
   begin
      Write (Tok_Pragma);
      Write_Space;
      Generate (Defining_Identifier (N));

      if not Is_Empty (Args) then
         Write_Space;
         Write (Tok_Left_Paren);
         Arg := First_Node (Args);

         loop
            Generate (Arg);
            Arg := Next_Node (Arg);
            exit when No (Arg);
            Write (Tok_Comma);
            Write_Space;
         end loop;

         Write (Tok_Right_Paren);
      end if;
   end Generate_Pragma;

   -----------------------------------
   -- Generate_Qualified_Expression --
   -----------------------------------

   procedure Generate_Qualified_Expression (N : Node_Id) is
   begin
      Generate (Subtype_Mark (N));
      Write_Line (Tok_Apostrophe);
      Increment_Indentation;
      Write_Indentation (-1);

      --  We generate parentheses only in case the Operand is not an
      --  aggregate

      if Kind (Operand (N)) = K_Record_Aggregate or else
        Kind (Operand (N)) = K_Array_Aggregate
      then
         Generate (Operand (N));
      else
         Write (Tok_Left_Paren);
         Generate (Operand (N));
         Write (Tok_Right_Paren);
      end if;

      Decrement_Indentation;
   end Generate_Qualified_Expression;

   ------------------------------
   -- Generate_Raise_Statement --
   ------------------------------

   procedure Generate_Raise_Statement (N : Node_Id) is
      E : constant Node_Id := Raised_Error (N);
   begin
      Write (Tok_Raise);

      if Present (E) then
         Write_Space;
         Generate (E);
      end if;
   end Generate_Raise_Statement;

   --------------------
   -- Generate_Range --
   --------------------

   procedure Generate_Range (N : Node_Id) is
   begin
      Generate (Low_Bound (N));
      Write_Str (" .. ");
      Generate (High_Bound (N));
   end Generate_Range;

   -------------------------------
   -- Generate_Record_Aggregate --
   -------------------------------

   procedure Generate_Record_Aggregate (N : Node_Id) is
      L : List_Id;
      M : Node_Id;
   begin
      L := Component_Association_List (N);
      Write (Tok_Left_Paren);

      if Present (Ancestor_Part (N)) then
         Generate (Ancestor_Part (N));
         Write_Space;
         Write (Tok_With);
         Write_Eol;
         Write_Indentation;
      end if;

      if not Is_Empty (L) then
         M := First_Node (L);

         loop
            Generate (M);
            M := Next_Node (M);
            exit when No (M);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;
      else
         Write (Tok_Null);
         Write_Space;
         Write (Tok_Record);
      end if;

      Write (Tok_Right_Paren);
   end Generate_Record_Aggregate;

   --------------------------------
   -- Generate_Record_Definition --
   --------------------------------

   procedure Generate_Record_Definition (N : Node_Id) is
      L : constant List_Id := Component_List (N);
      C : Node_Id;

   begin
      if Is_Empty (L) then
         Write (Tok_Null);
         Write_Space;
         Write (Tok_Record);
      else
         Write (Tok_Record);
         Write_Eol;
         Increment_Indentation;
         C := First_Node (L);

         while Present (C) loop
            Write_Indentation;
            Generate (C);
            Generate_Statement_Delimiter (C);
            C := Next_Node (C);
         end loop;

         Decrement_Indentation;
         Write_Indentation;
         Write (Tok_End);
         Write_Space;
         Write (Tok_Record);
      end if;
   end Generate_Record_Definition;

   -------------------------------------
   -- Generate_Record_Type_Definition --
   -------------------------------------

   procedure Generate_Record_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      if Is_Abstract_Type (N) then
         Write (Tok_Abstract);
         Write_Space;
      end if;

      if Is_Tagged_Type (N) then
         Write (Tok_Tagged);
         Write_Space;
      end if;

      if Is_Limited_Type (N) then
         Write (Tok_Limited);
         Write_Space;
      end if;

      R := Record_Definition (N);

      if Present (R) then
         Generate (R);
      end if;
   end Generate_Record_Type_Definition;

   -------------------------------
   -- Generate_Return_Statement --
   -------------------------------

   procedure Generate_Return_Statement (N : Node_Id) is
      E : constant Node_Id := Expression (N);
   begin
      Write (Tok_Return);

      if Present (E) then
         Write_Space;
         Generate (E);
      end if;
   end Generate_Return_Statement;

   ---------------------------------
   -- Generate_Selected_Component --
   ---------------------------------

   procedure Generate_Selected_Component (N : Node_Id) is
   begin
      Generate (Prefix (N));
      Write (Tok_Dot);
      Generate (Selector_Name (N));
   end Generate_Selected_Component;

   --------------------
   -- Generate_Slice --
   --------------------

   procedure Generate_Slice (N : Node_Id) is
   begin
      Generate (Prefix (N));
      Write_Space;
      Write (Tok_Left_Paren);
      Generate (Discrete_Range (N));
      Write (Tok_Right_Paren);
   end Generate_Slice;

   ------------------------------
   -- Generate_Subprogram_Call --
   ------------------------------

   procedure Generate_Subprogram_Call (N : Node_Id) is
      L : constant List_Id := Actual_Parameter_Part (N);
      P : Node_Id;

   begin
      Generate (Defining_Identifier (N));

      if not Is_Empty (L) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Left_Paren);
         P := First_Node (L);

         loop
            Generate (P);
            P := Next_Node (P);
            exit when No (P);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;

         Write (Tok_Right_Paren);
         Decrement_Indentation;
      end if;
   end Generate_Subprogram_Call;

   ------------------------------
   -- Generate_Subprogram_Body --
   ------------------------------

   procedure Generate_Subprogram_Body (N : Node_Id) is
      D : constant List_Id := Declarations (N);
      S : constant List_Id := Statements (N);
      P : constant Node_Id := Specification (N);
      M : Node_Id;

   begin
      Generate_Comment_Box (Name (Defining_Identifier (P)));
      Write_Eol;

      Write_Indentation;
      Generate (P);

      if not Is_Empty (Parameter_Profile (P)) then
         Write_Eol;
         Write_Indentation;
      else
         Write_Space;
      end if;

      Write (Tok_Is);
      Write_Eol;

      if not Is_Empty (D)  then
         Increment_Indentation;
         M := First_Node (D);

         while Present (M) loop
            Write_Indentation;
            Generate (M);
            Generate_Statement_Delimiter (M);

            --  If this is a nested subprogram spec or body, or the next thing
            --  is one of those, leave an extra blank line.

            if Kind (M) = K_Subprogram_Specification
              or else Kind (M) = K_Subprogram_Body
            then
               Write_Eol;

            elsif Present (Next_Node (M))
              and then (Kind (Next_Node (M)) = K_Subprogram_Specification
                          or else Kind (Next_Node (M)) = K_Subprogram_Body)
            then
               Write_Eol;

            end if;

            M := Next_Node (M);
         end loop;

         Decrement_Indentation;
      end if;

      Write_Indentation;
      Write (Tok_Begin);
      Write_Eol;
      Increment_Indentation;

      if not Is_Empty (S) then
         M := First_Node (S);

         while Present (M) loop
            Write_Indentation;
            Generate (M);
            Generate_Statement_Delimiter (M);
            M := Next_Node (M);
         end loop;
      else
         Write_Indentation;
         Write (Tok_Null);
         Write_Line (Tok_Semicolon);
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write_Name (Name (Defining_Identifier (P)));
   end Generate_Subprogram_Body;

   ---------------------------------------
   -- Generate_Subprogram_Specification --
   ---------------------------------------

   procedure Generate_Subprogram_Specification (N : Node_Id) is
      P : constant List_Id := Parameter_Profile (N);
      T : constant Node_Id := Return_Type (N);
      R : constant Node_Id := Renamed_Entity (N);
      I : constant Node_Id := Instantiated_Subprogram (N);
   begin
      if Present (T) then
         Write (Tok_Function);
      else
         Write (Tok_Procedure);
      end if;

      --  This work around is used to define access subprogram types

      if Present (Defining_Identifier (N)) then
         Write_Space;
         Write_Name (Name (Defining_Identifier (N)));
      end if;

      if not Is_Empty (P) then
         Write_Eol;
         Generate_Parameter_List (P);
      end if;

      --  Note that for an instance of a generic function, we set the return
      --  type in order to generate the proper FUNCTION keyword, but we never
      --  actually output the type.

      if Present (T) and then No (I) then
         if not Is_Empty (P) then
            Write_Eol;
            Increment_Indentation;
            Write_Indentation (-1);
         else
            Write_Space;
         end if;

         Write (Tok_Return);
         Write_Space;
         Generate (T);

         if not Is_Empty (P) then
            Decrement_Indentation;
         end if;
      end if;

      if Present (R) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Renames);
         Write_Space;
         Generate (R);
         Decrement_Indentation;
      end if;

      if Present (I) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Is);
         Write_Space;
         Write (Tok_New);
         Write_Space;
         Generate (I);
         Decrement_Indentation;
      end if;
   end Generate_Subprogram_Specification;

   ------------------------------
   -- Generate_Type_Conversion --
   ------------------------------

   procedure Generate_Type_Conversion (N : Node_Id) is
   begin
      Generate (Subtype_Mark (N));
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Write (Tok_Left_Paren);
      Generate (Expression (N));
      Write (Tok_Right_Paren);
      Decrement_Indentation;
   end Generate_Type_Conversion;

   ------------------------
   -- Generate_Used_Type --
   ------------------------

   procedure Generate_Used_Type (N : Node_Id) is
   begin
      Write (Tok_Use);
      Write_Space;
      Write (Tok_Type);
      Write_Space;
      Generate (The_Used_Entity (N));
   end Generate_Used_Type;

   procedure Generate_Used_Package (N : Node_Id) is
   begin
      Write (Tok_Use);
      Write_Space;
      Generate (The_Used_Entity (N));
   end Generate_Used_Package;

   ---------------------------
   -- Generate_Variant_Part --
   ---------------------------

   procedure Generate_Variant_Part (N : Node_Id) is
      V : Node_Id;
      C : Node_Id;
      O : Node_Id := No_Node;
      P : Node_Id;

   begin
      Write (Tok_Case);
      Write_Space;
      Generate (Discriminant (N));
      Write_Space;
      Write (Tok_Is);
      Write_Eol;
      V := First_Node (Variants (N));
      Increment_Indentation;

      while Present (V) loop
         C := First_Node (Discrete_Choices (V));

         if (Kind (C) = K_Literal
             and then Value (C) /= No_Value)
           or else Kind (C) /= K_Literal
         then
            --  If we have a valued or a casted (for alignment) literal

            Write_Indentation;
            Write (Tok_When);
            Write_Space;
            Increment_Indentation;
            loop
               Generate (C);
               C := Next_Node (C);

               if No (C) then
                  Write_Space;
                  Write (Tok_Arrow);
                  Write_Eol;
                  exit;
               end if;

               Write_Eol;
               Write_Indentation (-1);
               Write (Tok_Vertical_Bar);
               Write_Space;
            end loop;

            Write_Indentation;
            Generate (Component (V));
            Generate_Statement_Delimiter (Component (V));
            Decrement_Indentation;
         else
            --  An empty switch

            O := V;
         end if;

         V := Next_Node (V);
      end loop;

      --  Add a "when others" clause either based on the "default"
      --  label or a null one. In case of null statement, add two
      --  pragmas to disable warnings and enable them after the
      --  addition of the null statement

      if No (O) then
         Write_Indentation;
         P := Make_Pragma
           (Pragma_Warnings, New_List (RE (RE_Off)));
         Generate (P);
         Generate_Statement_Delimiter (P);
      end if;

      Write_Indentation;
      Write (Tok_When);
      Write_Space;
      Write (Tok_Others);
      Write_Space;
      Write (Tok_Arrow);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;

      if Present (O) then
         Generate (Component (O));
         Generate_Statement_Delimiter (Component (O));
      else
         Write (Tok_Null);
         Generate_Statement_Delimiter (O);
      end if;

      Decrement_Indentation;

      if No (O) then
         Write_Indentation;
         P := Make_Pragma
           (Pragma_Warnings, New_List (RE (RE_On)));
         Generate (P);
         Generate_Statement_Delimiter (P);
      end if;

      Decrement_Indentation;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_Case);
   end Generate_Variant_Part;

   -----------------------------
   -- Generate_Withed_Package --
   -----------------------------

   procedure Generate_Withed_Package (N : Node_Id) is
      procedure Add_Pragma (Pragma_Name : String);
      --  Add a pragma with the given name applying to the WITH'd unit

      ----------------
      -- Add_Pragma --
      ----------------

      procedure Add_Pragma (Pragma_Name : String) is
      begin
         Write (Tok_Semicolon);
         Write_Eol;
         Write_Indentation;
         Write (Tok_Pragma);
         Write_Space;
         Write_Str (Pragma_Name);
         Write_Space;
         Write (Tok_Left_Paren);
         Generate (Defining_Identifier (N));
         Write (Tok_Right_Paren);
      end Add_Pragma;

   begin
      Write (Tok_With);
      Write_Space;
      Generate (Defining_Identifier (N));

      if Used (N) then
         Write (Tok_Semicolon);
         Write_Eol;
         Write_Indentation;
         Write (Tok_Use);
         Write_Space;
         Generate (Defining_Identifier (N));
      end if;

      if Elaborated (N) then
         Add_Pragma ("Elaborate_All");
      end if;

      if Unreferenced (N) then
         Add_Pragma ("Unreferenced");
      end if;
   end Generate_Withed_Package;

   -----------
   -- Write --
   -----------

   procedure Write (T : Token_Type) is
   begin
      Write_Name (Token_Image (T));
   end Write;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (T : Token_Type) is
   begin
      Write (T);
      Write_Eol;
   end Write_Line;

   ----------------------------------
   -- Generate_Statement_Delimiter --
   ----------------------------------

   procedure Generate_Statement_Delimiter (N : Node_Id) is
   begin
      if No (N)
        or else (Kind (N) = K_Block_Statement and then Needs_Begin_End (N))
        or else (Kind (N) /= K_Block_Statement
                   and then Kind (N) /= K_Ada_Comment)
      then
         Write_Line (Tok_Semicolon);
      else
         Write_Eol;
      end if;
   end Generate_Statement_Delimiter;

   --------------------------
   -- Generate_Comment_Box --
   --------------------------

   procedure Generate_Comment_Box (M : Name_Id) is
   begin
      Get_Name_String (M);

      for I in 1 .. Name_Len + 6 loop
         Write_Char ('-');
      end loop;

      Write_Eol;
      Write_Indentation;

      Write_Str ("-- ");
      Write_Name (M);
      Write_Str (" --");
      Write_Eol;
      Write_Indentation;

      for I in 1 .. Name_Len + 6 loop
         Write_Char ('-');
      end loop;

      Write_Eol;
   end Generate_Comment_Box;

end Backend.BE_CORBA_Ada.Generator;
