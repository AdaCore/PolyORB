with Backend.BE_Ada;        use Backend.BE_Ada;
with Backend.BE_Ada.Nodes;  use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;

with Namet;   use Namet;
with Output;  use Output;
with Types;   use Types;
with Values;  use Values;

package body Backend.BE_Ada.Generator is

   procedure Generate_Access_Type_Definition (N : Node_Id);
   procedure Generate_Array_Type_Definition (N : Node_Id);
   procedure Generate_Assignment_Statement (N : Node_Id);
   procedure Generate_Case_Statement (N : Node_Id);
   procedure Generate_Component_Association (N : Node_Id);
   procedure Generate_Component_Declaration (N : Node_Id);
   procedure Generate_Defining_Identifier (N : Node_Id);
   procedure Generate_Derived_Type_Definition (N : Node_Id);
   procedure Generate_Designator (N : Node_Id);
   procedure Generate_Enumeration_Type_Definition (N : Node_Id);
   procedure Generate_Exception_Declaration (N : Node_Id);
   procedure Generate_Expression (N : Node_Id);
   procedure Generate_For_Statement (N : Node_Id);
   procedure Generate_Full_Type_Declaration (N : Node_Id);
   procedure Generate_IDL_Unit_Packages (N : Node_Id);
   procedure Generate_If_Statement (N : Node_Id);
   procedure Generate_Literal (N : Node_Id);
   procedure Generate_Null_Statement;
   procedure Generate_Object_Declaration (N : Node_Id);
   procedure Generate_Package_Declaration (N : Node_Id);
   procedure Generate_Package_Implementation (N : Node_Id);
   procedure Generate_Package_Specification (N : Node_Id);
   procedure Generate_Parameter (N : Node_Id);
   procedure Generate_Parameter_List (L : List_Id);
   procedure Generate_Pragma_Statement (N : Node_Id);
   procedure Generate_Record_Aggregate (N : Node_Id);
   procedure Generate_Record_Definition (N : Node_Id);
   procedure Generate_Record_Type_Definition (N : Node_Id);
   procedure Generate_Return_Statement (N : Node_Id);
   procedure Generate_Subprogram_Call (N : Node_Id);
   procedure Generate_Subprogram_Implementation (N : Node_Id);
   procedure Generate_Subprogram_Specification (N : Node_Id);
   procedure Generate_Variant_Part (N : Node_Id);
   procedure Generate_Withed_Package (N : Node_Id);


   procedure Write (T : Token_Type);
   procedure Write_Line (T : Token_Type);

   --------------
   -- Generate --
   --------------

   procedure Generate (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Access_Type_Definition =>
            Generate_Access_Type_Definition (N);

         when K_Array_Type_Definition =>
            Generate_Array_Type_Definition (N);

         when K_Assignment_Statement =>
            Generate_Assignment_Statement (N);

         when K_Case_Statement =>
            Generate_Case_Statement (N);

         when K_Component_Association =>
            Generate_Component_Association (N);

         when K_Component_Declaration =>
            Generate_Component_Declaration (N);

         when K_Defining_Identifier =>
            Generate_Defining_Identifier (N);

         when K_Derived_Type_Definition =>
            Generate_Derived_Type_Definition (N);

         when K_Designator =>
            Generate_Designator (N);

         when K_Enumeration_Type_Definition =>
            Generate_Enumeration_Type_Definition (N);

         when K_Exception_Declaration =>
            Generate_Exception_Declaration (N);

         when K_Expression =>
            Generate_Expression (N);

         when K_For_Statement =>
            Generate_For_Statement (N);

         when K_Full_Type_Declaration =>
            Generate_Full_Type_Declaration (N);

         when K_IDL_Unit =>
            Generate_IDL_Unit_Packages (N);

         when K_If_Statement =>
            Generate_If_Statement (N);

         when K_Literal =>
            Generate_Literal (N);

         when K_Null_Statement =>
            Generate_Null_Statement;

         when K_Object_Declaration =>
            Generate_Object_Declaration (N);

         when K_Package_Declaration =>
            Generate_Package_Declaration (N);

         when K_Package_Implementation =>
            Generate_Package_Implementation (N);

         when K_Package_Specification =>
            Generate_Package_Specification (N);

         when K_Pragma_Statement =>
            Generate_Pragma_Statement (N);

         when K_Record_Aggregate =>
            Generate_Record_Aggregate (N);

         when K_Record_Definition =>
            Generate_Record_Definition (N);

         when K_Record_Type_Definition =>
            Generate_Record_Type_Definition (N);

         when K_Return_Statement =>
            Generate_Return_Statement (N);

         when K_Subprogram_Call =>
            Generate_Subprogram_Call (N);

         when K_Subprogram_Specification =>
            Generate_Subprogram_Specification (N);

         when K_Subprogram_Implementation =>
            Generate_Subprogram_Implementation (N);

         when K_Variant_Part =>
            Generate_Variant_Part (N);

         when K_Withed_Package =>
            Generate_Withed_Package (N);

         when K_Float .. K_Octet =>
            Write_Name (Image (Base_Type (N)));

         when others =>
            null;
      end case;
   end Generate;

   -------------------------------------
   -- Generate_Access_Type_Definition --
   -------------------------------------

   procedure Generate_Access_Type_Definition (N : Node_Id) is
   begin
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

   ------------------------------------
   -- Generate_Array_Type_Definition --
   ------------------------------------

   procedure Generate_Array_Type_Definition (N : Node_Id) is
      R : Node_Id;

   begin
      Write (Tok_Array);
      Write_Space;
      Write (Tok_Left_Paren);
      R := First_Node (Range_Constraints (N));
      loop
         Write_Str (Values.Image (First (R)));
         Write_Space;
         Write (Tok_Dot);
         Write (Tok_Dot);
         Write_Space;
         Write_Str (Values.Image (Last (R)));
         R := Next_Node (R);
         exit when No (R);
         Write (Tok_Comma);
         Write_Space;
      end loop;
      Write (Tok_Right_Paren);
      Write_Space;
      Write (Tok_Of);
      Write_Space;
      Generate (Component_Definition (N));
   end Generate_Array_Type_Definition;

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

   -----------------------------
   -- Generate_Case_Statement --
   -----------------------------

   procedure Generate_Case_Statement (N : Node_Id) is
      D : Node_Id;
      M : Node_Id;
   begin
      Write (Tok_Case);
      Write_Space;
      Generate (Expression (N));
      D := First_Node (Case_Statement_Alternatives (N));
      Increment_Indentation;
      while Present (D) loop
         M := First_Node (Discret_Choice_List (D));
         Write_Eol;
         Write_Indentation;
         Write (Tok_When);
         Write_Space;
         loop
            Write_Str (Values.Image (Value (M)));
            M := Next_Node (M);
            exit when No (M);
            Write_Space;
            Write (Tok_Vertical_Bar);
            Write_Space;
         end loop;
         Write_Space;
         Write (Tok_Arrow);
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         M := First_Node (Statements (D));
         while Present (M) loop
            Generate (M);
            M := Next_Node (M);
         end loop;
         Decrement_Indentation;
         D := Next_Node (D);
      end loop;
      Decrement_Indentation;
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_Case);
   end Generate_Case_Statement;

   ------------------------------------
   -- Generate_Component_Association --
   ------------------------------------

   procedure Generate_Component_Association (N : Node_Id) is
   begin
      Generate (Defining_Identifier (N));
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
      Generate (Subtype_Indication (N));

      if Present (E) then
         Write_Space;
         Write (Tok_Colon_Equal);
         Write_Space;
         Generate (E);
      end if;
   end Generate_Component_Declaration;

   ----------------------------------
   -- Generate_Defining_Identifier --
   ----------------------------------

   procedure Generate_Defining_Identifier (N : Node_Id) is
      P : Node_Id;

   begin
      P := Parent_Unit_Name (N);

      if Present (P) then
         Generate (P);
         Write (Tok_Dot);
      end if;

      Write_Name (Name (N));
   end Generate_Defining_Identifier;

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

      Write (Tok_New);
      Write_Space;
      Generate (Subtype_Indication (N));

      if Is_Private_Extention (N) then
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
         end if;
      end if;
   end Generate_Derived_Type_Definition;

   -------------------------
   -- Generate_Designator --
   -------------------------

   procedure Generate_Designator (N : Node_Id) is
      P : Node_Id;

   begin
      P := Parent_Unit_Name (N);

      if Present (P) then
         Generate (P);
         Write (Tok_Dot);
      end if;

      Write_Name (Name (Defining_Identifier (N)));
   end Generate_Designator;

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
      Generate (Defining_Identifier (N));
      Write_Space;
      Write (Tok_Colon);
      Write_Space;
      Write (Tok_Exception);
   end Generate_Exception_Declaration;

   -------------------------
   -- Generate_Expression --
   -------------------------

   procedure Generate_Expression (N : Node_Id) is
      L_Expr  : constant Node_Id     := Left_Expr (N);
      Op      : constant Operator_Id := Operator (N);
      R_Expr  : constant Node_Id     := Right_Expr (N);
   begin
      if No (R_Expr) then
         if Op = Operator_Type'Pos (Op_Not) then
            Write (Tok_Not);
         else
            Write_Name (Operator_Image (Standard.Integer (Op)));
         end if;

         Write_Space;
      end if;

      Generate (L_Expr);

      if Present (R_Expr) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation;
         Write_Name (Operator_Image (Standard.Integer (Op)));
         Write_Space;
         Generate (R_Expr);
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
      Write_Str (Values.Image (First (Range_Constraint (N))));
      Write_Space;
      Write (Tok_Dot);
      Write (Tok_Dot);
      Write_Space;
      Write_Str (Values.Image (Last (Range_Constraint (N))));
      Write_Space;
      Write (Tok_Loop);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation;
      while Present (D) loop
         Generate (D);
         D := Next_Node (D);
      end loop;
      Decrement_Indentation;
      Write (Tok_Semicolon);
      Write_Eol;
      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Write (Tok_Loop);
   end Generate_For_Statement;

   ------------------------------------
   -- Generate_Full_Type_Declaration --
   ------------------------------------

   procedure Generate_Full_Type_Declaration (N : Node_Id) is
      D : constant Node_Id := Discriminant_Spec (N);

   begin
      Write (Tok_Type);
      Write_Space;
      Write_Name (Name (Defining_Identifier (N)));
      Write_Space;

      if Present (D) then
         Write (Tok_Left_Paren);
         Generate (D);
         Write (Tok_Right_Paren);
         Write_Space;
      end if;

      Write (Tok_Is);
      Write_Eol;
      Increment_Indentation;
      Write_Indentation (-1);
      Generate (Type_Definition (N));
      Decrement_Indentation;
   end Generate_Full_Type_Declaration;

   --------------------------------
   -- Generate_IDL_Unit_Packages --
   --------------------------------

   procedure Generate_IDL_Unit_Packages (N : Node_Id) is
      P : Node_Id := First_Node (Packages (N));

   begin
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
         Write_Line (Tok_Semicolon);
         I := Next_Node (I);
      end loop;
      Decrement_Indentation;

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
            Write_Line (Tok_Semicolon);
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

   ----------------------
   -- Generate_Literal --
   ----------------------

   procedure Generate_Literal (N : Node_Id) is
   begin
      Write_Str (Values.Image (Value (N)));
   end Generate_Literal;

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

      Write_Space;
      Generate (Object_Definition (N));

      if Present (Expression (N)) then
         Write_Space;
         Write (Tok_Colon_Equal);
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Generate (Expression (N));
         Decrement_Indentation;
      end if;
   end Generate_Object_Declaration;

   ----------------------------------
   -- Generate_Package_Declaration --
   ----------------------------------

   procedure Generate_Package_Declaration (N : Node_Id) is
   begin
      if Generate_Specs then
         Generate (Package_Specification (N));
      end if;

      if Generate_Bodies then
         Generate (Package_Implementation (N));
      end if;
   end Generate_Package_Declaration;

   -------------------------------------
   -- Generate_Package_Implementation --
   -------------------------------------

   procedure Generate_Package_Implementation (N : Node_Id) is
      P : Node_Id;
   begin
      if not Is_Generated (N) then
         return;
      end if;

      P := First_Node (Withed_Packages (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Write_Line (Tok_Semicolon);
         P := Next_Node (P);
      end loop;
      Write_Eol;
      Write_Indentation;
      Write (Tok_Package);
      Write_Space;
      Write (Tok_Body);
      Write_Space;
      Generate (Defining_Identifier (Package_Declaration (N)));
      Write_Space;
      Write (Tok_Is);
      Write_Eol (2);
      Increment_Indentation;
      P := First_Node (Statements (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Write (Tok_Semicolon);
         Write_Eol (2);
         P := Next_Node (P);
      end loop;
      Decrement_Indentation;
      Write_Indentation;
      Write  (Tok_End);
      Write_Space;
      Generate (Defining_Identifier (Package_Declaration (N)));
      Write (Tok_Semicolon);
      Write_Eol;
   end Generate_Package_Implementation;

   -----------------------------
   -- Generate_Null_Statement --
   -----------------------------

   procedure Generate_Null_Statement is
   begin
      Write (Tok_Null);
   end Generate_Null_Statement;

   ------------------------------------
   -- Generate_Package_Specification --
   ------------------------------------

   procedure Generate_Package_Specification (N : Node_Id) is
      P : Node_Id;
   begin
      if not Is_Generated (N) then
         return;
      end if;

      P := First_Node (Withed_Packages (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Write_Line (Tok_Semicolon);
         P := Next_Node (P);
      end loop;
      Write_Eol;
      Write_Indentation;
      Write (Tok_Package);
      Write_Space;
      Generate (Defining_Identifier (Package_Declaration (N)));
      Write_Space;
      Write (Tok_Is);
      Write_Eol (2);
      Increment_Indentation;
      P := First_Node (Visible_Part (N));
      while Present (P) loop
         Write_Indentation;
         Generate (P);
         Write (Tok_Semicolon);
         Write_Eol (2);
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
            Write (Tok_Semicolon);
            Write_Eol (2);
            P := Next_Node (P);
         end loop;
         Decrement_Indentation;
      end if;

      Write_Indentation;
      Write (Tok_End);
      Write_Space;
      Generate (Defining_Identifier (Package_Declaration (N)));
      Write_Line (Tok_Semicolon);
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
         Write_Space;
         case Parameter_Mode (N) is
            when Mode_In =>
               Write (Tok_In);

            when Mode_Out =>
               Write (Tok_Out);

            when Mode_Inout =>
               Write (Tok_In);
               Write_Space;
               Write (Tok_Out);
         end case;
      end if;

      Write_Space;
      Generate (Parameter_Type (N));
   end Generate_Parameter;

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
         N := Next_Node (N);
         exit when No (N);
         Write_Line (Tok_Semicolon);
         Write_Indentation;
      end loop;
      Write (Tok_Right_Paren);
      Decrement_Indentation;
   end Generate_Parameter_List;

   -------------------------------
   -- Generate_Pragma_Statement --
   -------------------------------

   procedure Generate_Pragma_Statement (N : Node_Id) is
   begin
      Write (Tok_Pragma);
      Write_Space;
      Generate (Expression (N));
   end Generate_Pragma_Statement;

   -------------------------------
   -- Generate_Record_Aggregate --
   -------------------------------

   procedure Generate_Record_Aggregate (N : Node_Id) is
      L : List_Id;
      M : Node_Id;
   begin
      L := Component_Association_List (N);
      Write (Tok_Left_Paren);

      if not Is_Empty (L) then
         M := First_Node (L);
         loop
            Generate (M);
            M := Next_Node (M);
            exit when No (M);
            Write_Line (Tok_Comma);
            Write_Indentation;
         end loop;
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
         Write_Space;
         Write (Tok_Record);
         Write_Eol;
         Increment_Indentation;
         C := First_Node (L);
         while Present (C) loop
            Write_Indentation;
            Generate (C);
            C := Next_Node (C);
            Write_Line (Tok_Semicolon);
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
      Write_Space;
      Generate (E);
   end Generate_Return_Statement;
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

   ----------------------------------------
   -- Generate_Subprogram_Implementation --
   ----------------------------------------

   procedure Generate_Subprogram_Implementation (N : Node_Id) is
      D : constant List_Id := Declarations (N);
      S : constant List_Id := Statements (N);
      P : constant Node_Id := Specification (N);
      M : Node_Id;

   begin
      Generate (P);
      Write_Eol;
      Write_Indentation;
      Write (Tok_Is);
      Write_Eol;

      if not Is_Empty (D)  then
         Increment_Indentation;
         M := First_Node (D);
         while Present (M) loop
            Write_Indentation;
            Generate (M);
            M := Next_Node (M);
            Write_Line (Tok_Semicolon);
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
            M := Next_Node (M);
            Write_Line (Tok_Semicolon);
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
      Generate (Defining_Identifier (P));
   end Generate_Subprogram_Implementation;

   ---------------------------------------
   -- Generate_Subprogram_Specification --
   ---------------------------------------

   procedure Generate_Subprogram_Specification (N : Node_Id) is
      P : constant List_Id := Parameter_Profile (N);
      T : constant Node_Id := Return_Type (N);

   begin
      if Present (T) then
         Write (Tok_Function);
      else
         Write (Tok_Procedure);
      end if;

      Write_Space;
      Write_Name (Name (Defining_Identifier (N)));
      Write_Eol;

      if not Is_Empty (P) then
         Generate_Parameter_List (P);
      end if;

      if Present (T) then
         Write_Eol;
         Increment_Indentation;
         Write_Indentation (-1);
         Write (Tok_Return);
         Write_Space;
         Generate (T);
         Decrement_Indentation;
      end if;
   end Generate_Subprogram_Specification;

   ---------------------------
   -- Generate_Variant_Part --
   ---------------------------

   procedure Generate_Variant_Part (N : Node_Id) is
      V : Node_Id;
      C : Node_Id;
      O : Node_Id := No_Node;

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

         if Value (C) = No_Value then
            O := V;
         else
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
            Write (Tok_Semicolon);
            Write_Eol;
            Decrement_Indentation;
         end if;

         V := Next_Node (V);
      end loop;

      --  Add a "when others" clause either based on the "default"
      --  label or a null one.

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
      else
         Write (Tok_Null);
      end if;

      Write (Tok_Semicolon);
      Write_Eol;
      Decrement_Indentation;
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
   begin
      Write (Tok_With);
      Write_Space;
      Generate (Defining_Identifier (N));
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
      Write_Name (Token_Image (T));
      Write_Eol;
   end Write_Line;

end Backend.BE_Ada.Generator;
