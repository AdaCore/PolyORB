with Debug;     use Debug;
with Errors;    use Errors;
with Flags;     use Flags;
with Lexer;     use Lexer;
with Locations; use Locations;
with Names;     use Names;
with Namet;     use Namet;
with Nodes;     use Nodes;
with Nutils;    use Nutils;
with Output;    use Output;
with Scopes;    use Scopes;
with Types;     use Types;
with Utils;     use Utils;
with Values;    use Values;

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

   procedure Resolve (E : Node_Id; T : Node_Id);

   procedure Display_Incorrect_Value
     (L  : Location;
      K1 : Node_Kind;
      K2 : Node_Kind := K_Void);

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

         when K_String_Type | K_Wide_String_Type =>
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
      D : Node_Id := First_Node (Declarators (E));
   begin
      Analyze (Type_Spec (E));
      while Present (D) loop
         Analyze (D);
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

   procedure Analyze_Constant_Declaration (E : Node_Id)
   is
      T : Node_Id;
      K : Node_Kind;

   begin
      T := Type_Spec (E);
      Analyze (T);

      --  Resolve base type of T. Types of constant declaration are
      --  limited to integer types, character types, string types,
      --  floating point types, fixed point types.

      while Present (T) loop
         K := Kind (T);
         exit when K = K_Fixed_Point_Type or else K in K_Float .. K_Octet;
         if K = K_Simple_Declarator then
            T := Type_Spec (Declaration (T));

         elsif K = K_Scoped_Name then
            T := Reference (T);

         else
            DE ("invalid type for constant");
            T := No_Node;
         end if;
      end loop;

      --  Analyze expression, evaluate it and then convert result

      Enter_Name_In_Scope (Identifier (E));
      Analyze (Expression (E));
      Resolve (E, T);
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

      N := New_Copy (Identifier (E));
      S := New_Node (K_Scoped_Name, Loc (E));
      Bind_Identifier_To_Entity (N, S);

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

         E := First_Node (B);
         B := Interface_Body (I);

         while Present (E) loop
            if Kind (E) = K_Attribute_Declaration then
               D := First_Node (Declarators (E));
               while Present (D) loop
                  S := New_Node (K_Scoped_Name, Loc (D));
                  N := New_Copy (Identifier (D));
                  Bind_Identifier_To_Entity (N, S);
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
               Bind_Identifier_To_Entity (N, S);
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
      --  Bad_Value body of current interface in order to enter attributes
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
      D : Node_Id := First_Node (Declarators (E));
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
      D : Node_Id := First_Node (Declarators (E));
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

   -----------------------------
   -- Display_Incorrect_Value --
   -----------------------------

   procedure Display_Incorrect_Value
     (L  : Location;
      K1 : Node_Kind;
      K2 : Node_Kind := K_Void)
   is
   begin
      Error_Loc  (1) := L;
      Error_Name (1) := Quoted (Image (K1));
      if K2 = K_Void then
         DE ("value not in range of type of%");
      else
            Error_Name (2) := Quoted (Image (K2));
            DE ("value not in range of type of%or%");
      end if;
   end Display_Incorrect_Value;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (E : Node_Id; T : Node_Id) is

      procedure Cannot_Interpret
        (E : Node_Id;
         S : String;
         T : Node_Kind);
      --  Output an error message to indicate that a value cannot be
      --  cast in a given type. E denotes the entity in which the cast
      --  occurs, V the source type and K the target type.

      function Convert
        (E    : Node_Id;
         T    : Node_Id;
         K    : Node_Kind)
        return Value_Type;
      --  Convert the value from E into type T in the context K. The
      --  conversion depends on the context since for instance, an
      --  integer value is not converted the same way whether it is
      --  performed in a constant declaration or in an expression.

      function In_Range
        (I : Unsigned_Long_Long;
         S : Short_Short;
         F : Long_Long;
         L : Unsigned_Long_Long)
        return Boolean;
      --  Check whether S * I (Sign * Val) is in range F .. L.

      ----------------------
      -- Cannot_Interpret --
      ----------------------

      procedure Cannot_Interpret (E : Node_Id; S : String; T : Node_Kind) is
      begin
         Error_Loc (1)  := Loc (E);
         Error_Name (1) := Quoted (Image (T));
         DE ("cannot interpret " & S & " as%");
      end Cannot_Interpret;

      -------------
      -- Convert --
      -------------

      function Convert
        (E    : Node_Id;
         T    : Node_Id;
         K    : Node_Kind)
         return Value_Type
      is
         KT : Node_Kind;
         RE : Node_Id := E;
         RV : Value_Type;
         R  : Value_Id;
         KE : constant Node_Kind := Kind (E);
         I  : Unsigned_Long_Long;
         S  : Short_Short;

      begin

         --  First resolve a scoped name

         if KE = K_Scoped_Name then
            RE := Reference (E);
            if No (RE) then
               return Bad_Value;
            end if;
         end if;

         --  Resolve the Result Value RV and the Kind of Type KT

         R := Value (RE);
         if R = No_Value then
            return Bad_Value;
         end if;
         RV := Value (R);
         KT := Kind (T);

         case RV.K is
            when K_Short .. K_Unsigned_Long_Long | K_Octet =>

               --  When integer value, cast into integer type

               if KT not in K_Short .. K_Unsigned_Long_Long
                 and then KT /= K_Octet
               then
                  Cannot_Interpret (E, "integer", KT);
                  return Bad_Value;
               end if;
               I := RV.IVal;
               S := RV.Sign;

               --  In a constant declaration, subtyping is
               --  restrictive. In an expression, a literal or a
               --  scoped name, signed or unsigned integers of 8, 16
               --  and 32 bits are handled as signed or unsigned
               --  integers of 32 bits. Therefore, the cast is
               --  performed first to signed integers. Then to
               --  unsigned integers.

               if K /= K_Constant_Declaration then
                  if KT = K_Unsigned_Long_Long or else KT = K_Long_Long then
                     KT := K_Long_Long;
                  else
                     KT := K_Long;
                  end if;
               end if;

               --  When E is not a declaration, cast to signed
               --  integers and then to unsigned integers. When E is a
               --  declaration, cast to the exact type.

               for B in False .. True loop
                  case RV.K is
                     when K_Octet =>
                        if In_Range (I, S, FO, LO) then
                           RV := Convert (RV, KT);
                        end if;

                     when K_Short =>
                        if In_Range (I, S, FS, LS) then
                           RV := Convert (RV, KT);
                        end if;

                     when K_Long =>
                        if In_Range (I, S, FL, LL) then
                           RV := Convert (RV, KT);
                        end if;

                     when K_Long_Long =>
                        if In_Range (I, S, FLL, LLL) then
                           RV := Convert (RV, KT);
                        end if;

                     when K_Unsigned_Short =>
                        if In_Range (I, S, FUS, LUS) then
                           RV := Convert (RV, KT);
                        end if;

                     when K_Unsigned_Long =>
                        if In_Range (I, S, FUL, LUL) then
                           RV := Convert (RV, KT);
                        end if;

                     when K_Unsigned_Long_Long =>
                        if In_Range (I, S, FULL, LULL) then
                           RV := Convert (RV, KT);
                        end if;

                     when others =>
                        null;
                  end case;

                  exit when K = K_Constant_Declaration;

                  --  Switch to unsigned integers

                  if KT = K_Long_Long then
                     KT := K_Unsigned_Long_Long;
                  else
                     KT := K_Unsigned_Long;
                  end if;
               end loop;

               --  Cast cannot be performed. Output an error message
               --  according to the performed operation: exact cast,
               --  32-bits integer cast, 64-bits integer cast.

               if RV.K /= KT then
                  if K = K_Constant_Declaration then
                     Display_Incorrect_Value
                       (Loc (E), KT);

                  elsif KT = K_Unsigned_Long then
                     Display_Incorrect_Value
                       (Loc (E), K_Long, K_Unsigned_Long);

                  else
                     Display_Incorrect_Value
                       (Loc (E), K_Long_Long, K_Unsigned_Long_Long);
                  end if;
                  return Bad_Value;
               end if;

            when K_String | K_String_Type =>
               if RV.K /= K_String
                 and then RV.K /= K_String_Type
               then
                  Cannot_Interpret (E, "string", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Wide_String | K_Wide_String_Type =>
               if RV.K /= K_Wide_String
                 and then RV.K /= K_Wide_String_Type
               then
                  Cannot_Interpret (E, "wide string", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Char =>
               if RV.K /= KT then
                  Cannot_Interpret (E, "character", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Wide_Char =>
               if RV.K /= KT then
                  Cannot_Interpret (E, "wide character", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Fixed_Point_Type =>
               if RV.K /= KT then
                  Cannot_Interpret (E, "fixed point", KT);
                  return Bad_Value;
               end if;

               --  For constant declaration, subtyping is restrictive.
               --  The fixed point value must be truncated to the
               --  appropriate scale. It cannot exceed the appropriate
               --  total number of digits.

               declare
                  Total : Unsigned_Short_Short;
                  Scale : Unsigned_Short_Short;
               begin
                  if K = K_Constant_Declaration then
                     Total := Unsigned_Short_Short (N_Total (T));
                     Scale := Unsigned_Short_Short (N_Scale (T));
                  else
                     Total := Max_Digits;
                     Scale := Max_Digits;
                  end if;
                  Normalize_Fixed_Point_Value (RV, Total, Scale);
                  if RV = Bad_Value then
                     Error_Loc (1) := Loc (E);
                     Error_Int (1) := Int (Total);
                     Error_Int (2) := Int (Scale);
                     DE ("too many digits to fit fixed<$,$>");
                     return RV;
                  end if;
                  RV := Convert (RV, KT);
               end;

            when K_Float .. K_Long_Double =>
               if RV.K not in K_Float .. K_Long_Double then
                  Cannot_Interpret (E, "float", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when K_Boolean =>
               if RV.K /= KT then
                  Cannot_Interpret (E, "boolean", KT);
                  return Bad_Value;
               end if;
               RV := Convert (RV, KT);

            when others =>
               return Bad_Value;
         end case;

         return RV;
      end Convert;

      --------------
      -- In_Range --
      --------------

      function In_Range
        (I : Unsigned_Long_Long;
         S : Short_Short;
         F : Long_Long;
         L : Unsigned_Long_Long)
        return Boolean is
      begin
         if S < 0 then
            if F < 0
              and then I <= Unsigned_Long_Long (-F)
            then
               return True;
            end if;
            return False;
         end if;
         return I <= L;
      end In_Range;

      KE     : Node_Kind;
      RE, LE : Node_Id;
      RV, LV : Value_Type;
      O      : Token_Type;

   begin
      if No (T) then
         return;
      end if;
      if No (E) then
         return;
      end if;
      KE := Kind (E);

      --  For constant declaration, first resolve expression attached
      --  to declaration. The expression is evaluated as described in
      --  the next block. Second convert the value into the exact type
      --  and if the evaluation has been successful, set the constant
      --  value to it.

      if KE = K_Constant_Declaration then
         RE := Expression (E);
         if Present (RE) then
            Resolve (RE, T);
            RV := Convert (RE, T, KE);
            if RV = Bad_Value then
               Set_Value (RE, No_Value);
               return;
            end if;
            Set_Value (E, New_Value (RV));
         end if;

      --  For expression, evaluate left part when possible and then
      --  right part of the expression. Each result is converted into
      --  type T following the specific rules fo subexpression (see
      --  function Convert). Then execute operation and check that the
      --  operation was successful. Do not convert to T at this point.

      elsif KE = K_Expression then
         LE := Left_Expr (E);
         if Present (LE) then

            --  Resolve and convert a possible left subexpression

            Resolve (LE, T);
            LV := Convert (LE, T, KE);
            if LV = Bad_Value then
               Set_Value (E, No_Value);
               return;
            end if;
         end if;

         RE := Right_Expr (E);
         if No (RE) then
            Set_Value (E, No_Value);
            return;
         end if;

         --  Resolve and convert a right subexpression

         Resolve (RE, T);
         RV := Convert (RE, T, KE);
         if RV = Bad_Value then
            Set_Value (E, No_Value);
            return;
         end if;

         --  For binary operator, check that the two operands have the
         --  same type.

         O := Token_Type'Val (Operator (E));
         if Present (LE)
           and then LV.K /= RV.K
         then
            Error_Loc (1)  := Loc (E);
            Error_Name (1) := Quoted (Image (O));
            DE ("invalid operand types for operator%");
            Set_Value (E, No_Value);
            return;
         end if;

         case O is
            when T_Tilde           =>
               RV := not RV;

            when T_Minus           =>
               if No (LE) then
                  RV := -RV;
               else
                  RV := LV - RV;
               end if;

            when T_Plus            =>
               if Present (LE) then
                  RV := LV + RV;
               end if;

            when T_Percent         =>
               RV := LV mod RV;

            when T_Slash           =>
               RV := LV / RV;

            when T_Star            =>
               RV := LV * RV;

            when T_Ampersand       =>
               RV := LV and RV;

            when T_Bar             =>
               RV := LV or RV;

            when T_Circumflex      =>
               RV := LV xor RV;

            when T_Greater_Greater =>
               RV := Shift_Right  (LV, RV);

            when T_Less_Less       =>
               RV := Shift_Left (LV, RV);

            when others            =>
               return;
         end case;

         --  XXXXX: Check whether anything goes wrong. We should
         --  probably take of overflows here.

         if RV = Bad_Value then
            Set_Value (E, No_Value);
            return;
         end if;

         Set_Value (E, New_Value (RV));
      end if;
   end Resolve;

end Analyzer;
