with Lexer;     use Lexer;
with Namet;     use Namet;
with Nodes;     use Nodes;
with Nutils;    use Nutils;
with Output;    use Output;
with Types;     use Types;
with Values;    use Values;

package body Backend.BE_IDL is

   Space_Increment : constant := 2;
   N_Space         : Natural  := 0;

   procedure Decrement_Indentation;
   procedure Increment_Indentation;

   procedure Write      (T : Token_Type);
   procedure Write_Indentation;
   procedure Write_Line (T : Token_Type);
   procedure Write_Space;

   procedure Generate_Abstract_Value_Declaration (E : Node_Id);
   procedure Generate_Attribute_Declaration (E : Node_Id);
   procedure Generate_Base_Type (E : Node_Id);
   procedure Generate_Complex_Declarator (E : Node_Id);
   procedure Generate_Constant_Declaration (E : Node_Id);
   procedure Generate_Enumeration_Type (E : Node_Id);
   procedure Generate_Enumerator (E : Node_Id);
   procedure Generate_Exception_Declaration (E : Node_Id);
   procedure Generate_Expression (E : Node_Id);
   procedure Generate_Fixed_Point_Type (E : Node_Id);
   procedure Generate_Forward_Interface_Declaration (E : Node_Id);
   procedure Generate_Forward_Structure_Type (E : Node_Id);
   procedure Generate_Forward_Union_Type (E : Node_Id);
   procedure Generate_Identifier (E : Node_Id);
   procedure Generate_Initializer_Declaration (E : Node_Id);
   procedure Generate_Interface_Declaration (E : Node_Id);
   procedure Generate_Literal (E : Node_Id);
   procedure Generate_Member (E : Node_Id);
   procedure Generate_Module (E : Node_Id);
   procedure Generate_Operation_Declaration (E : Node_Id);
   procedure Generate_Native_Type (E : Node_Id);
   procedure Generate_Parameter_Declaration (E : Node_Id);
   procedure Generate_Scoped_Name (E : Node_Id);
   procedure Generate_Simple_Declarator (E : Node_Id);
   procedure Generate_Sequence_Type (E : Node_Id);
   procedure Generate_State_Member (E : Node_Id);
   procedure Generate_String (E : Node_Id);
   procedure Generate_Structure_Type (E : Node_Id);
   procedure Generate_Type_Declaration (E : Node_Id);
   procedure Generate_Union_Type (E : Node_Id);
   procedure Generate_Value_Declaration (E : Node_Id);
   procedure Generate_Value_Box_Declaration (E : Node_Id);
   procedure Generate_Value_Forward_Declaration (E : Node_Id);

   ---------------------------
   -- Decrement_Indentation --
   ---------------------------

   procedure Decrement_Indentation is
   begin
      N_Space := N_Space - Space_Increment;
   end Decrement_Indentation;

   ---------------------------
   -- Increment_Indentation --
   ---------------------------

   procedure Increment_Indentation is
   begin
      N_Space := N_Space + Space_Increment;
   end Increment_Indentation;

   -------------
   -- Generate --
   -------------

   procedure Generate (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Abstract_Value_Declaration =>
            Generate_Abstract_Value_Declaration (E);

         when K_Attribute_Declaration =>
            Generate_Attribute_Declaration (E);

         when K_Complex_Declarator =>
            Generate_Complex_Declarator (E);

         when K_Constant_Declaration =>
            Generate_Constant_Declaration (E);

         when K_Enumerator =>
            Generate_Enumerator (E);

         when K_Enumeration_Type =>
            Generate_Enumeration_Type (E);

         when K_Exception_Declaration =>
            Generate_Exception_Declaration (E);

         when K_Expression =>
            Generate_Expression (E);

         when K_Fixed_Point_Type =>
            Generate_Fixed_Point_Type (E);

         when K_Forward_Interface_Declaration =>
            Generate_Forward_Interface_Declaration (E);

         when K_Forward_Structure_Type =>
            Generate_Forward_Structure_Type (E);

         when K_Forward_Union_Type =>
            Generate_Forward_Union_Type (E);

         when K_Initializer_Declaration =>
            Generate_Initializer_Declaration (E);

         when K_Interface_Declaration =>
            Generate_Interface_Declaration (E);

         when K_Literal .. K_Wide_String_Literal =>
            Generate_Literal (E);

         when K_Member =>
            Generate_Member (E);

         when K_Module =>
            Generate_Module (E);

         when K_Operation_Declaration =>
            Generate_Operation_Declaration (E);

         when K_Native_Type =>
            Generate_Native_Type (E);

         when K_Parameter_Declaration =>
            Generate_Parameter_Declaration (E);

         when K_Scoped_Name =>
            Generate_Scoped_Name (E);

         when K_Simple_Declarator =>
            Generate_Simple_Declarator (E);

         when K_Sequence_Type =>
            Generate_Sequence_Type (E);

         when K_Specification =>
            Generate_Module (E);

         when K_State_Member =>
            Generate_State_Member (E);

         when K_String_Type | K_Wide_String_Type =>
            Generate_String (E);

         when K_Structure_Type =>
            Generate_Structure_Type (E);

         when K_Type_Declaration =>
            Generate_Type_Declaration (E);

         when K_Union_Type =>
            Generate_Union_Type (E);

         when K_Value_Declaration =>
            Generate_Value_Declaration (E);

         when K_Value_Box_Declaration =>
            Generate_Value_Box_Declaration (E);

         when K_Value_Forward_Declaration =>
            Generate_Value_Forward_Declaration (E);

         when K_Float .. K_Value_Base =>
            Generate_Base_Type (E);

         when K_Identifier =>
            Generate_Identifier (E);

         when others =>
            Dummy (E);
      end case;
   end Generate;

   ----------------------------------------
   -- Generate_Abstract_Value_Declaration --
   ----------------------------------------

   procedure Generate_Abstract_Value_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Generate_Abstract_Value_Declaration;

   -----------------------------------
   -- Generate_Attribute_Declaration --
   -----------------------------------

   procedure Generate_Attribute_Declaration (E : Node_Id)
   is
      D : Node_Id := First_Node (Declarators (E));
   begin
      Write_Indentation;
      Generate (Type_Spec (E));
      Write_Space;
      loop
         Generate (Identifier (D));
         D := Next_Node (D);
         exit when No (D);
         Write (T_Comma);
         Write_Space;
      end loop;
   end Generate_Attribute_Declaration;

   ---------------------------------
   -- Generate_Complex_Declarator --
   ---------------------------------

   procedure Generate_Complex_Declarator (E : Node_Id)
   is
      C : Node_Id;
   begin
      Generate (Identifier (E));

      --  The array sizes attribute is never empty

      Write_Space;
      Write (T_Left_Bracket);
      C := First_Node (Array_Sizes (E));
      loop
         Generate (C);
         C := Next_Node (C);
         exit when No (C);
         Write (T_Comma);
         Write_Space;
      end loop;
      Write (T_Right_Bracket);
   end Generate_Complex_Declarator;

   ----------------------------------
   -- Generate_Constant_Declaration --
   ----------------------------------

   procedure Generate_Constant_Declaration (E : Node_Id)
   is
      Typ : Node_Id  := Type_Spec (E);
      Val : constant Value_Id := Value (E);
      Pos : Unsigned_Long_Long;
      Enu : Node_Id;
   begin
      Write_Indentation;
      Write (T_Const);
      Write_Space;
      Generate (Typ);
      Write_Space;
      Generate (Identifier (E));
      Write_Space;
      Write (T_Equal);
      Write_Space;
      if Kind (Typ) = K_Scoped_Name then
         Typ := Reference (Typ);
      end if;
      if Kind (Typ) = K_Enumeration_Type then
         Pos   := Value (Val).IVal;
         Enu := First_Node (Enumerators (Typ));
         while Pos /= 1 loop
            Enu := Next_Node (Enu);
            Pos := Pos - 1;
         end loop;
         Write_Name (IDL_Name (Identifier (Enu)));
      else
         Write_Str (Image (Val));
      end if;
   end Generate_Constant_Declaration;

   -------------------------
   -- Generate_Enumerator --
   -------------------------

   procedure Generate_Enumerator (E : Node_Id) is
   begin
      Write_Indentation;
      Generate (Identifier (E));
   end Generate_Enumerator;

   -------------------------------
   -- Generate_Enumeration_Type --
   -------------------------------

   procedure Generate_Enumeration_Type (E : Node_Id)
   is
      C : Node_Id;
   begin
      Write_Indentation;
      Write (T_Enum);
      Write_Space;
      Generate (Identifier (E));
      Write_Space;
      Write_Line (T_Left_Brace);
      Increment_Indentation;
      C := First_Node (Enumerators (E));
      loop
         Generate (C);
         C := Next_Node (C);
         exit when No (C);
         Write_Line (T_Comma);
      end loop;
      Write_Eol;
      Decrement_Indentation;
      Write_Indentation;
      Write      (T_Right_Brace);
   end Generate_Enumeration_Type;

   -----------------------------------
   -- Generate_Exception_Declaration --
   -----------------------------------

   procedure Generate_Exception_Declaration (E : Node_Id)
   is
      C : Node_Id;
      L : List_Id;
   begin
      Write_Indentation;
      Write (T_Exception);
      Write_Space;
      Generate (Identifier (E));
      L := Members (E);
      if not Is_Empty (L) then
         Write_Space;
         Write_Line (T_Left_Brace);
         Increment_Indentation;
         C := First_Node (L);
         while Present (C) loop
            Generate (C);
            Write_Line (T_Semi_Colon);
            C := Next_Node (C);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
         Write (T_Right_Brace);
      end if;
   end Generate_Exception_Declaration;

   ------------------------
   -- Generate_Expression --
   ------------------------

   procedure Generate_Expression (E : Node_Id)
   is
      C : Node_Id;
   begin
      C := Left_Expr (E);
      if Present (C) then
         Generate (C);
      end if;
      Write_Space;
      Write (Token_Type'Val (Operator (E)));
      Write_Space;
      C := Right_Expr (E);
      if Present (C) then
         Generate (C);
      end if;
   end Generate_Expression;

   ------------------------------
   -- Generate_Fixed_Point_Type --
   ------------------------------

   procedure Generate_Fixed_Point_Type (E : Node_Id) is
   begin
      Write (T_Fixed);
      Write (T_Less);
      Write_Int (Int (N_Total (E)));
      Write (T_Comma);
      Write_Int (Int (N_Scale (E)));
      Write (T_Greater);
   end Generate_Fixed_Point_Type;

   -------------------------------------------
   -- Generate_Forward_Interface_Declaration --
   -------------------------------------------

   procedure Generate_Forward_Interface_Declaration (E : Node_Id) is
   begin
      Write_Indentation;
      Write (T_Interface);
      Write_Space;
      Generate (Identifier (E));
   end Generate_Forward_Interface_Declaration;

   ------------------------------------
   -- Generate_Forward_Structure_Type --
   ------------------------------------

   procedure Generate_Forward_Structure_Type (E : Node_Id) is
   begin
      Write_Indentation;
      Write (T_Struct);
      Write_Space;
      Generate (Identifier (E));
   end Generate_Forward_Structure_Type;

   --------------------------------
   -- Generate_Forward_Union_Type --
   --------------------------------

   procedure Generate_Forward_Union_Type (E : Node_Id) is
   begin
      Write_Indentation;
      Write (T_Union);
      Write_Space;
      Generate (Identifier (E));
   end Generate_Forward_Union_Type;

   -------------------------
   -- Generate_Identifier --
   -------------------------

   procedure Generate_Identifier (E : Node_Id) is
   begin
      Write_Name (IDL_Name (E));
   end Generate_Identifier;

   -------------------------------------
   -- Generate_Initializer_Declaration --
   -------------------------------------

   procedure Generate_Initializer_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Generate_Initializer_Declaration;

   -----------------------------------
   -- Generate_Interface_Declaration --
   -----------------------------------

   procedure Generate_Interface_Declaration (E : Node_Id) is
      F : Node_Id := No_Node;
      I : Node_Id;
      S : List_Id;
      B : List_Id;

   begin
      Write_Indentation;
      Write (T_Interface);
      Write_Space;
      Generate (Identifier (E));

      --  Generate interface names, enter them in scope and make them
      --  visible.

      S := Interface_Spec (E);
      if not Is_Empty (S) then
         Write_Space;
         Write (T_Colon);
         Write_Space;
         I := First_Node (S);
         loop
            Generate (I);
            I := Next_Node (I);
            exit when No (I);
            Write (T_Comma);
            Write_Space;
         end loop;
      end if;

      --  Prepare to inherit operations and attributes from parent
      --  interfaces. Preserve entities from current interface in F.
      --  Empty body of current interface in order to enter attributes
      --  and operations of parent interfaces.

      Write_Space;
      Write_Line (T_Left_Brace);

      B := Interface_Body (E);
      if not Is_Empty (B) then
         Increment_Indentation;
         F := First_Node (B);
         while Present (F) loop
            Generate (F);
            Write_Line (T_Semi_Colon);
            F := Next_Node (F);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
      end if;
      Write      (T_Right_Brace);
   end Generate_Interface_Declaration;

   ----------------------
   -- Generate_Literal --
   ----------------------

   procedure Generate_Literal (E : Node_Id) is
   begin
      Write_Str (Image (Value (E)));
   end Generate_Literal;

   ---------------------
   -- Generate_Member --
   ---------------------

   procedure Generate_Member (E : Node_Id)
   is
      D : Node_Id := First_Node (Declarators (E));
   begin
      Write_Indentation;
      Generate (Type_Spec (E));
      Write_Space;
      loop
         Generate (D);
         D := Next_Node (D);
         exit when No (D);
         Write (T_Comma);
         Write_Space;
      end loop;
   end Generate_Member;

   ---------------------
   -- Generate_Module --
   ---------------------

   procedure Generate_Module (E : Node_Id)
   is
      M : constant Boolean := (Kind (E) = K_Module);
      C : Node_Id;
      L : List_Id;
   begin
      if M then
         Write_Indentation;
         Write (T_Module);
         Write_Space;
         Generate (Identifier (E));
      end if;
      L := Definitions (E);
      if not Is_Empty (L) then
         if M then
            Write_Space;
            Write_Line (T_Left_Brace);
            Increment_Indentation;
         end if;
         C := First_Node (L);
         while Present (C) loop
            Generate (C);
            Write_Line (T_Semi_Colon);
            C := Next_Node (C);
         end loop;
         if M then
            Decrement_Indentation;
            Write (T_Right_Brace);
         end if;
      end if;
   end Generate_Module;

   -------------------------
   -- Generate_Native_Type --
   -------------------------

   procedure Generate_Native_Type (E : Node_Id) is
   begin
      Write_Indentation;
      Write (T_Native);
      Write_Space;
      Generate (Declarator (E));
   end Generate_Native_Type;

   ------------------------------------
   -- Generate_Operation_Declaration --
   ------------------------------------

   procedure Generate_Operation_Declaration (E : Node_Id)
   is
      C : Node_Id;
      L : List_Id;
   begin
      Write_Indentation;
      Generate (Type_Spec (E));
      Write_Space;
      Generate (Identifier (E));
      Write (T_Left_Paren);

      L := Parameters (E);
      if not Is_Empty (L) then
         C := First_Node (L);
         loop
            Generate (C);
            C := Next_Node (C);
            exit when No (C);
            Write (T_Comma);
            Write_Space;
         end loop;
      end if;
      Write (T_Right_Paren);

      L := Exceptions (E);
      if not Is_Empty (L) then
         Write_Space;
         Write (T_Raises);
         Write_Space;
         Write (T_Left_Paren);
         C := First_Node (L);
         loop
            Generate (C);
            C := Next_Node (C);
            exit when No (C);
            Write (T_Comma);
            Write_Space;
         end loop;
         Write (T_Right_Paren);
      end if;

      L := Contexts (E);
      if not Is_Empty (L) then
         Write_Space;
         Write (T_Context);
         Write_Space;
         Write (T_Left_Paren);
         C := First_Node (L);
         loop
            Generate (C);
            C := Next_Node (C);
            exit when No (C);
            Write (T_Comma);
            Write_Space;
         end loop;
         Write (T_Right_Paren);
      end if;
   end Generate_Operation_Declaration;

   -----------------------------------
   -- Generate_Parameter_Declaration --
   -----------------------------------

   procedure Generate_Parameter_Declaration (E : Node_Id) is
   begin
      Write (Token_Type'Val (Parameter_Mode (E)));
      Write_Space;
      Generate (Type_Spec (E));
      Write_Space;
      Generate (Declarator (E));
   end Generate_Parameter_Declaration;

   ------------------------
   -- Generate_Base_Type --
   ------------------------

   procedure Generate_Base_Type (E : Node_Id) is
   begin
      Write_Name (Image (Base_Type (E)));
   end Generate_Base_Type;

   --------------------------
   -- Generate_Scoped_Name --
   --------------------------

   procedure Generate_Scoped_Name (E : Node_Id)
   is
      procedure Generate_Reference_Name (E : Node_Id);

      -----------------------------
      -- Generate_Reference_Name --
      -----------------------------

      procedure Generate_Reference_Name (E : Node_Id)
      is
         S : constant Node_Id := Scope (E);
      begin
         if Kind (S) /= K_Specification then
            Generate_Reference_Name (Identifier (S));
            Write (T_Colon_Colon);
         end if;
         Write_Name (IDL_Name (E));
      end Generate_Reference_Name;

      P : constant Node_Id := Parent (E);

   begin
      if Present (P) then
         if Kind (Reference (P)) /= K_Specification then
            Generate (P);
            Write (T_Colon_Colon);
         end if;
         Generate (Identifier (E));
      else
         Generate_Reference_Name (Identifier (Reference (E)));
      end if;
   end Generate_Scoped_Name;

   ---------------------------
   -- Generate_Sequence_Type --
   ---------------------------

   procedure Generate_Sequence_Type (E : Node_Id) is
   begin
      Generate (Type_Spec (E));
   end Generate_Sequence_Type;

   -------------------------------
   -- Generate_Simple_Declarator --
   -------------------------------

   procedure Generate_Simple_Declarator (E : Node_Id) is
   begin
      Generate (Identifier (E));
   end Generate_Simple_Declarator;

   --------------------------
   -- Generate_State_Member --
   --------------------------

   procedure Generate_State_Member (E : Node_Id) is
   begin
      Dummy (E);
   end Generate_State_Member;

   --------------------
   -- Generate_String --
   --------------------

   procedure Generate_String (E : Node_Id) is
   begin
      Dummy (E);
   end Generate_String;

   ----------------------------
   -- Generate_Structure_Type --
   ----------------------------

   procedure Generate_Structure_Type (E : Node_Id)
   is
      L : List_Id;
      C : Node_Id;
   begin
      Write_Indentation;
      Write (T_Struct);
      Write_Space;
      Generate (Identifier (E));
      Write_Space;
      Write_Line (T_Left_Brace);
      L := Members (E);
      if not Is_Empty (L) then
         Increment_Indentation;
         C := First_Node (L);
         while Present (C) loop
            Generate (C);
            Write_Line (T_Semi_Colon);
            C := Next_Node (C);
         end loop;
         Decrement_Indentation;
         Write_Indentation;
      end if;
      Write      (T_Right_Brace);
   end Generate_Structure_Type;

   ------------------------------
   -- Generate_Type_Declaration --
   ------------------------------

   procedure Generate_Type_Declaration (E : Node_Id)
   is
      D : Node_Id := First_Node (Declarators (E));
   begin
      Write_Indentation;
      Write (T_Typedef);
      Write_Space;
      Generate (Type_Spec (E));
      Write_Space;
      loop
         Generate (D);
         D := Next_Node (D);
         exit when No (D);
         Write (T_Comma);
         Write_Space;
      end loop;
   end Generate_Type_Declaration;

   ------------------------
   -- Generate_Union_Type --
   ------------------------

   procedure Generate_Union_Type (E : Node_Id) is
   begin
      Dummy (E);
   end Generate_Union_Type;

   -----------------------------------
   -- Generate_Value_Box_Declaration --
   -----------------------------------

   procedure Generate_Value_Box_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Generate_Value_Box_Declaration;

   -------------------------------
   -- Generate_Value_Declaration --
   -------------------------------

   procedure Generate_Value_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Generate_Value_Declaration;

   ---------------------------------------
   -- Generate_Value_Forward_Declaration --
   ---------------------------------------

   procedure Generate_Value_Forward_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Generate_Value_Forward_Declaration;

   -----------
   -- Write --
   -----------

   procedure Write      (T : Token_Type) is
   begin
      Write_Str (Image (T));
   end Write;

   -----------------------
   -- Write_Indentation --
   -----------------------

   procedure Write_Indentation is
   begin
      for I in 1 .. N_Space loop
         Write_Char (' ');
      end loop;
   end Write_Indentation;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (T : Token_Type) is
   begin
      Write_Line (Image (T));
   end Write_Line;

   -----------------
   -- Write_Space --
   -----------------

   procedure Write_Space is
   begin
      Write_Char (' ');
   end Write_Space;

end Backend.BE_IDL;
