with Debug;     use Debug;
with Errors;    use Errors;
with Flags;     use Flags;
with Names;     use Names;
with Namet;     use Namet;
with Nodes;     use Nodes;
with Nutils;    use Nutils;
with Output;    use Output;
with Scopes;    use Scopes;
with Types;     use Types;

package body Analyzer is

   procedure Analyze_Abstract_Value_Declaration (E : Node_Id);
   procedure Analyze_Attribute_Declaration (E : Node_Id);
   procedure Analyze_Complex_Declarator (E : Node_Id);
   procedure Analyze_Constant_Declaration (E : Node_Id);
   procedure Analyze_Enumeration_Type (E : Node_Id);
   procedure Analyze_Exception_Declaration (E : Node_Id);
   procedure Analyze_Expression (E : Node_Id);
   procedure Analyze_Fixed_Point_Type (E : Node_Id);
   procedure Analyze_Forward_Interface_Declaration (E : Node_Id);
   procedure Analyze_Forward_Structure_Type (E : Node_Id);
   procedure Analyze_Forward_Union_Type (E : Node_Id);
   procedure Analyze_Initializer_Declaration (E : Node_Id);
   procedure Analyze_Interface_Declaration (E : Node_Id);
   procedure Analyze_Literal (E : Node_Id);
   procedure Analyze_Member (E : Node_Id);
   procedure Analyze_Module (E : Node_Id);
   procedure Analyze_Operation_Declaration (E : Node_Id);
   procedure Analyze_Native_Type (E : Node_Id);
   procedure Analyze_Parameter_Declaration (E : Node_Id);
   procedure Analyze_Scoped_Name (E : Node_Id);
   procedure Analyze_Simple_Declarator (E : Node_Id);
   procedure Analyze_Sequence_Type (E : Node_Id);
   procedure Analyze_State_Member (E : Node_Id);
   procedure Analyze_String (E : Node_Id);
   procedure Analyze_Structure_Type (E : Node_Id);
   procedure Analyze_Type_Declaration (E : Node_Id);
   procedure Analyze_Union_Type (E : Node_Id);
   procedure Analyze_Value_Declaration (E : Node_Id);
   procedure Analyze_Value_Box_Declaration (E : Node_Id);
   procedure Analyze_Value_Forward_Declaration (E : Node_Id);

   -------------
   -- Analyze --
   -------------

   procedure Analyze (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Abstract_Value_Declaration =>
            Analyze_Abstract_Value_Declaration (E);

         when K_Attribute_Declaration =>
            Analyze_Attribute_Declaration (E);

         when K_Complex_Declarator =>
            Analyze_Complex_Declarator (E);

         when K_Constant_Declaration =>
            Analyze_Constant_Declaration (E);

         when K_Enumeration_Type =>
            Analyze_Enumeration_Type (E);

         when K_Exception_Declaration =>
            Analyze_Exception_Declaration (E);

         when K_Expression =>
            Analyze_Expression (E);

         when K_Fixed_Point_Type =>
            Analyze_Fixed_Point_Type (E);

         when K_Forward_Interface_Declaration =>
            Analyze_Forward_Interface_Declaration (E);

         when K_Forward_Structure_Type =>
            Analyze_Forward_Structure_Type (E);

         when K_Forward_Union_Type =>
            Analyze_Forward_Union_Type (E);

         when K_Initializer_Declaration =>
            Analyze_Initializer_Declaration (E);

         when K_Interface_Declaration =>
            Analyze_Interface_Declaration (E);

         when K_Literal =>
            Analyze_Literal (E);

         when K_Member =>
            Analyze_Member (E);

         when K_Module =>
            Analyze_Module (E);

         when K_Operation_Declaration =>
            Analyze_Operation_Declaration (E);

         when K_Native_Type =>
            Analyze_Native_Type (E);

         when K_Parameter_Declaration =>
            Analyze_Parameter_Declaration (E);

         when K_Scoped_Name =>
            Analyze_Scoped_Name (E);

         when K_Simple_Declarator =>
            Analyze_Simple_Declarator (E);

         when K_Sequence_Type =>
            Analyze_Sequence_Type (E);

         when K_Specification =>
            Analyze_Module (E);

         when K_State_Member =>
            Analyze_State_Member (E);

         when K_String | K_Wide_String =>
            Analyze_String (E);

         when K_Structure_Type =>
            Analyze_Structure_Type (E);

         when K_Type_Declaration =>
            Analyze_Type_Declaration (E);

         when K_Union_Type =>
            Analyze_Union_Type (E);

         when K_Value_Declaration =>
            Analyze_Value_Declaration (E);

         when K_Value_Box_Declaration =>
            Analyze_Value_Box_Declaration (E);

         when K_Value_Forward_Declaration =>
            Analyze_Value_Forward_Declaration (E);

         when K_Float .. K_Value_Base =>
            null;

         when others =>
            Dummy (E);
      end case;
   end Analyze;

   ----------------------------------------
   -- Analyze_Abstract_Value_Declaration --
   ----------------------------------------

   procedure Analyze_Abstract_Value_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_Abstract_Value_Declaration;

   -----------------------------------
   -- Analyze_Attribute_Declaration --
   -----------------------------------

   procedure Analyze_Attribute_Declaration (E : Node_Id)
   is
      D : Node_Id := Declarators (E);
   begin
      Analyze (Type_Spec (E));
      while Present (D) loop
         Enter_Name_In_Scope (Identifier (D));
         D := Next_Node (D);
      end loop;
   end Analyze_Attribute_Declaration;

   --------------------------------
   -- Analyze_Complex_Declarator --
   --------------------------------

   procedure Analyze_Complex_Declarator (E : Node_Id)
   is
      C : Node_Id;
   begin
      Enter_Name_In_Scope (Identifier (E));

      --  The array sizes attribute is never empty

      C := First_Node (Array_Sizes (E));
      while Present (C) loop
         Analyze (C);
         C := Next_Node (C);
      end loop;
   end Analyze_Complex_Declarator;

   ----------------------------------
   -- Analyze_Constant_Declaration --
   ----------------------------------

   procedure Analyze_Constant_Declaration (E : Node_Id) is
   begin
      Analyze (Type_Spec (E));
      Enter_Name_In_Scope (Identifier (E));
      Analyze (Expression (E));
   end Analyze_Constant_Declaration;

   ------------------------------
   -- Analyze_Enumeration_Type --
   ------------------------------

   procedure Analyze_Enumeration_Type (E : Node_Id)
   is
      C : Node_Id;
      S : Node_Id;
      N : Node_Id;
   begin
      Enter_Name_In_Scope (Identifier (E));

      N := New_Copy   (Identifier (E));
      S := New_Node (K_Scoped_Name, Loc (E));
      Associate  (S, N);

      C := First_Node (Enumerators (E));
      while Present (C) loop
         Analyze (C);
         C := Next_Node (C);
      end loop;
   end Analyze_Enumeration_Type;

   -----------------------------------
   -- Analyze_Exception_Declaration --
   -----------------------------------

   procedure Analyze_Exception_Declaration (E : Node_Id)
   is
      C : Node_Id;
      L : List_Id;
   begin
      Enter_Name_In_Scope (Identifier (E));
      L := Members (E);
      if not Is_Empty (L) then
         Push_Scope (E);
         C := First_Node (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Node (C);
         end loop;
         Pop_Scope;
      end if;
   end Analyze_Exception_Declaration;

   ------------------------
   -- Analyze_Expression --
   ------------------------

   procedure Analyze_Expression (E : Node_Id)
   is
      C : Node_Id;
   begin
      C := Left_Expr (E);
      if Present (C) then
         Analyze (C);
      end if;
      C := Right_Expr (E);
      if Present (C) then
         Analyze (C);
      end if;
   end Analyze_Expression;

   ------------------------------
   -- Analyze_Fixed_Point_Type --
   ------------------------------

   procedure Analyze_Fixed_Point_Type (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_Fixed_Point_Type;

   -------------------------------------------
   -- Analyze_Forward_Interface_Declaration --
   -------------------------------------------

   procedure Analyze_Forward_Interface_Declaration (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Forward_Interface_Declaration;

   ------------------------------------
   -- Analyze_Forward_Structure_Type --
   ------------------------------------

   procedure Analyze_Forward_Structure_Type (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Forward_Structure_Type;

   --------------------------------
   -- Analyze_Forward_Union_Type --
   --------------------------------

   procedure Analyze_Forward_Union_Type (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Forward_Union_Type;

   -------------------------------------
   -- Analyze_Initializer_Declaration --
   -------------------------------------

   procedure Analyze_Initializer_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_Initializer_Declaration;

   -----------------------------------
   -- Analyze_Interface_Declaration --
   -----------------------------------

   procedure Analyze_Interface_Declaration (E : Node_Id) is
      F : Node_Id := No_Node;
      I : Node_Id;
      C : Node_Id;
      S : List_Id;
      B : List_Id;

      procedure Copy_Inherited_Nodes (P, I : Node_Id);
      --  Copy inherited entities from a parent interface P into a
      --  child interface I. Operations and attributes are the only
      --  inherited entities. Do not copy inherited entities that have
      --  already been copied (diamond diagram).

      -----------------------------
      -- Copy_Inherited_Nodes --
      -----------------------------

      procedure Copy_Inherited_Nodes (P, I : Node_Id)
      is
         E : Node_Id;
         N : Node_Id;
         B : List_Id;
         D : Node_Id;
         S : Node_Id;
      begin
         if No (P) then
            return;
         end if;

         --  Nothing to inherit

         B := Interface_Body (P);
         if Is_Empty (B) then
            return;
         end if;

         --  Node E is the current candidate to be copied. Node B
         --  becomes now the target interface body.

         E := First_Node   (B);
         B := Interface_Body (I);

         while Present (E) loop
            if Kind (E) = K_Attribute_Declaration then
               D := Declarators (E);
               while Present (D) loop
                  S := New_Node (K_Scoped_Name, Loc (D));
                  N := New_Copy (Identifier (D));
                  Associate (S, N);
                  if D_Scopes then
                     Write_Str  ("inherit scoped name ");
                     Write_Name (Name (N));
                     Write_Eol;
                  end if;
                  Enter_Name_In_Scope (N);
                  D := Next_Node (D);
               end loop;

            elsif Kind (E) = K_Operation_Declaration then
               S := New_Node (K_Scoped_Name, Loc (E));
               N := New_Copy (Identifier (E));
               Associate (S, N);
               if D_Scopes then
                  Write_Str  ("inherit scoped name ");
                  Write_Name (Name (N));
                  Write_Eol;
               end if;
               Enter_Name_In_Scope (N);
            end if;

            E := Next_Node (E);
         end loop;
      end Copy_Inherited_Nodes;

   begin
      Enter_Name_In_Scope (Identifier (E));

      --  Analyze interface names, enter them in scope and make them
      --  visible.

      Push_Scope (E);
      S := Interface_Spec (E);
      if not Is_Empty (S) then
         I := First_Node (S);
         while Present (I) loop
            Analyze (I);
            C := Reference (I);
            if Present (C) then
               if Kind (C) = K_Interface_Declaration then
                  Make_Enclosed_Nodes_Visible (C, True, False);

               else
                  if Kind (C) = K_Forward_Interface_Declaration then
                     Error_Loc (1) := Loc (E);
                     DE ("interface cannot inherit " &
                           "from a forward-declared interface");
                  else
                     Error_Loc (1) := Loc (E);
                     DE ("interface cannot inherit " &
                           "from a non-interface");
                  end if;
               end if;
            end if;
            I := Next_Node (I);
         end loop;
      end if;

      --  Prepare to inherit operations and attributes from parent
      --  interfaces. Preserve entities from current interface in F.
      --  Empty body of current interface in order to enter attributes
      --  and operations of parent interfaces.

      B := Interface_Body (E);
      if not Is_Empty (B) then
         F := First_Node (B);
         Set_First_Node (B, No_Node);
         Set_Last_Node  (B, No_Node);
      end if;

      --  Enter attributes and operations of parent interfaces

      S := Interface_Spec (E);
      if not Is_Empty (S) then
         I := First_Node (S);
         while Present (I) loop

            --  The current interface body might be empty

            if Is_Empty (B) then
               B := New_List (K_Interface_Body, Loc (E));
               Set_Interface_Body (E, B);
            end if;

            --  Node I is a scoped name.

            C := Reference (I);
            if Present (C)
              and then Kind (C) = K_Interface_Declaration
            then
               Copy_Inherited_Nodes (Reference (I), E);
            end if;

            I := Next_Node (I);
         end loop;
      end if;

      --  We append and analyze the new entities of the current interface

      while Present (F) loop
         Analyze (F);
         C := F;
         F := Next_Node (F);
         Set_Next_Node (C, No_Node);
         if Is_Attribute_Or_Operation (C) then
            Set_Base_Interface (C, E);
         end if;
         Append_Node_To_List (C, B);
      end loop;
      Pop_Scope;

      --  Remove visibility on the parent interfaces entities

      S := Interface_Spec (E);
      if not Is_Empty (S) then
         I := First_Node (S);
         while Present (I) loop
            C := Reference (I);
            if Present (C)
              and then Kind (C) = K_Interface_Declaration
            then
               Make_Enclosed_Nodes_Visible (C, False, False);
            end if;
            I := Next_Node (I);
         end loop;
      end if;
   end Analyze_Interface_Declaration;

   ---------------------
   -- Analyze_Literal --
   ---------------------

   procedure Analyze_Literal (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_Literal;

   --------------------
   -- Analyze_Member --
   --------------------

   procedure Analyze_Member (E : Node_Id)
   is
      D : Node_Id := Declarators (E);
   begin
      Analyze (Type_Spec (E));
      while Present (D) loop
         Analyze (D);
         D := Next_Node (D);
      end loop;
   end Analyze_Member;

   --------------------
   -- Analyze_Module --
   --------------------

   procedure Analyze_Module (E : Node_Id)
   is
      C : Node_Id;
      L : List_Id;
   begin
      if Kind (E) = K_Module then
         Enter_Name_In_Scope (Identifier (E));
      end if;

      L := Definitions (E);
      if not Is_Empty (L) then
         Push_Scope (E);
         C := First_Node (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Node (C);
         end loop;
         Pop_Scope;
      end if;
   end Analyze_Module;

   -------------------------
   -- Analyze_Native_Type --
   -------------------------

   procedure Analyze_Native_Type (E : Node_Id) is
   begin
      Analyze (Declarator (E));
   end Analyze_Native_Type;

   -----------------------------------
   -- Analyze_Operation_Declaration --
   -----------------------------------

   procedure Analyze_Operation_Declaration (E : Node_Id)
   is
      C : Node_Id;
      L : List_Id;
   begin
      Analyze (Type_Spec (E));
      Enter_Name_In_Scope (Identifier (E));

      L := Parameters (E);
      if not Is_Empty (L) then
         Push_Scope (E);
         C := First_Node (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Node (C);
         end loop;
         Pop_Scope;
      end if;

      L := Exceptions (E);
      if not Is_Empty (L) then
         C := First_Node (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Node (C);
         end loop;
      end if;

      L := Contexts (E);
      if not Is_Empty (L) then
         C := First_Node (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Node (C);
         end loop;
      end if;
   end Analyze_Operation_Declaration;

   -----------------------------------
   -- Analyze_Parameter_Declaration --
   -----------------------------------

   procedure Analyze_Parameter_Declaration (E : Node_Id) is
   begin
      Analyze (Type_Spec (E));
      Analyze (Declarator (E));
   end Analyze_Parameter_Declaration;

   -------------------------
   -- Analyze_Scoped_Name --
   -------------------------

   procedure Analyze_Scoped_Name (E : Node_Id)
   is
      P : Node_Id := Parent (E);
      N : Node_Id   := Identifier (E);
      C : Node_Id;
   begin

      --  Analyze single scoped name. First we have to find a possible
      --  visible entity. If there is one, associate the reference to
      --  the designated entity and check whether the casing is
      --  correct.

      if No (P) then
         C := Current_Node (N);
         if Present (C) then
            Set_Reference (E, C);
            Enter_Name_In_Scope (N);
            Check_Identifier (N, Identifier (C));
         end if;

      --  Analyze multiple scoped names. Analyze parent P first and
      --  then and the entity itself. Find the entity in the
      --  newly-computed parent scope. Check whether the scope is a
      --  correct scope for a scoped name (not an operation for
      --  instance).

      else
         Analyze_Scoped_Name (P);
         P := Reference (P);
         if Present (P) then
            if Is_A_Scope (P) then
               C := Node_In_Scope (N, P);
               if Present (C) then
                  Set_Reference (E, C);
                  Check_Identifier (N, Identifier (C));
               end if;

            else
               if D_Analyzer then
                  W_Full_Tree;
               end if;
               N := Identifier (P);
               Error_Loc  (1) := Loc (N);
               Error_Name (1) := Name (N);
               DE ("#does not form a scope");
            end if;
         end if;
      end if;
   end Analyze_Scoped_Name;

   ---------------------------
   -- Analyze_Sequence_Type --
   ---------------------------

   procedure Analyze_Sequence_Type (E : Node_Id) is
   begin
      Analyze (Type_Spec (E));
   end Analyze_Sequence_Type;

   -------------------------------
   -- Analyze_Simple_Declarator --
   -------------------------------

   procedure Analyze_Simple_Declarator (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Simple_Declarator;

   --------------------------
   -- Analyze_State_Member --
   --------------------------

   procedure Analyze_State_Member (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_State_Member;

   --------------------
   -- Analyze_String --
   --------------------

   procedure Analyze_String (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_String;

   ----------------------------
   -- Analyze_Structure_Type --
   ----------------------------

   procedure Analyze_Structure_Type (E : Node_Id)
   is
      L : List_Id;
      C : Node_Id;
   begin
      Enter_Name_In_Scope (Identifier (E));
      L := Members (E);
      if not Is_Empty (L) then
         Push_Scope (E);
         C := First_Node (L);
         while Present (C) loop
            Analyze (C);
            C := Next_Node (C);
         end loop;
         Pop_Scope;
      end if;
   end Analyze_Structure_Type;

   ------------------------------
   -- Analyze_Type_Declaration --
   ------------------------------

   procedure Analyze_Type_Declaration (E : Node_Id)
   is
      D : Node_Id := Declarators (E);
   begin
      Analyze (Type_Spec (E));
      while Present (D) loop
         Analyze (D);
         D := Next_Node (D);
      end loop;
   end Analyze_Type_Declaration;

   ------------------------
   -- Analyze_Union_Type --
   ------------------------

   procedure Analyze_Union_Type (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Union_Type;

   -----------------------------------
   -- Analyze_Value_Box_Declaration --
   -----------------------------------

   procedure Analyze_Value_Box_Declaration (E : Node_Id) is
   begin
      Enter_Name_In_Scope (Identifier (E));
   end Analyze_Value_Box_Declaration;

   -------------------------------
   -- Analyze_Value_Declaration --
   -------------------------------

   procedure Analyze_Value_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_Value_Declaration;

   ---------------------------------------
   -- Analyze_Value_Forward_Declaration --
   ---------------------------------------

   procedure Analyze_Value_Forward_Declaration (E : Node_Id) is
   begin
      Dummy (E);
   end Analyze_Value_Forward_Declaration;

end Analyzer;
