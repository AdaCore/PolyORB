with GNAT.Table;

with Charset;   use Charset;
with Locations; use Locations;
with Namet;     use Namet;
with Output;    use Output;
with Utils;     use Utils;
with Values;    use Values;

with Frontend.Nodes;

with Backend.BE_Ada.Nodes;      use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.IDL_To_Ada; use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Expand;     use Backend.BE_Ada.Expand;

package body Backend.BE_Ada.Nutils is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;

   type Entity_Stack_Entry is record
      Current_Package : Node_Id;
      Current_Entity  : Node_Id;
   end record;

   --  This list contains the forwarded entities.
   Forwarded_Entities : List_Id;

   No_Depth : constant Int := -1;
   package Entity_Stack is
      new GNAT.Table (Entity_Stack_Entry, Int, No_Depth + 1, 10, 10);

   use Entity_Stack;

   procedure New_Operator
     (O : Operator_Type;
      I : String := "");

   ------------------------
   -- Add_Prefix_To_Name --
   ------------------------

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id)
      return Name_Id
   is
   begin
      Set_Str_To_Name_Buffer (Prefix);
      Get_Name_String_And_Append (Name);
      return Name_Find;
   end Add_Prefix_To_Name;

   ------------------------
   -- Add_Suffix_To_Name --
   ------------------------

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id)
      return Name_Id
   is
   begin
      Get_Name_String (Name);
      Add_Str_To_Name_Buffer (Suffix);
      return Name_Find;
   end Add_Suffix_To_Name;

   -----------------------------
   -- Remove_Suffix_From_Name --
   -----------------------------

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id)
     return Name_Id
   is
      Length   : Natural;
      Temp_Str : String (1 .. Suffix'Length);
   begin
      Set_Str_To_Name_Buffer (Suffix);
      Length := Name_Len;
      Get_Name_String (Name);
      if Name_Len > Length then
         Temp_Str := Name_Buffer (Name_Len - Length + 1 .. Name_Len);
         if Suffix = Temp_Str then
            Set_Str_To_Name_Buffer (Name_Buffer (1 .. Name_Len - Length));
            return Name_Find;
         end if;
      end if;
      return Name;
   end Remove_Suffix_From_Name;

   ----------------------
   -- Add_With_Package --
   ----------------------

   procedure Add_With_Package (E : Node_Id) is

      function To_Library_Unit (E : Node_Id) return Node_Id;
      --  Return the library unit which E belongs to in order to with
      --  it. As a special rule, package Standard returns No_Node.

      ---------------------
      -- To_Library_Unit --
      ---------------------

      function To_Library_Unit (E : Node_Id) return Node_Id is
         U : Node_Id;

      begin
         pragma Assert (Kind (E) = K_Designator);
         U := Corresponding_Node (Defining_Identifier (E));

         --  This node is not properly built as the corresponding node
         --  is not set.

         if No (U) then
            if Output_Tree_Warnings then
               Write_Str  ("WARNING: node ");
               Write_Name (Name (Defining_Identifier (E)));
               Write_Line (" has a null corresponding node");
            end if;
            return E;
         end if;

         if BEN.Kind (U) = K_Package_Declaration then
            U := Package_Specification (U);
         end if;

         pragma Assert (Kind (U) = K_Package_Specification
                        or else Kind (U) = K_Package_Instantiation);

         --  This is a subunit and we do not need to add a with for
         --  this unit but for one of its parents.
         --  If the kind of the parent unit name is a K_Package_Instanciation,
         --  we consider it as a subunit.

         if Kind (U) = K_Package_Instantiation
           or else Is_Subunit_Package (U) then
            U := Parent_Unit_Name (E);

            --  This is a special case to handle package Standard

            if No (U) then
               return No_Node;
            end if;
            return To_Library_Unit (U);
         end if;

         return E;
      end To_Library_Unit;

      P           : constant Node_Id := To_Library_Unit (E);
      W           : Node_Id;
      N           : Name_Id;
      B           : Byte;
      D           : Node_Id;
      I           : Node_Id;
      Helper_Name : Name_Id;
      Skel_Name   : Name_Id;
      Impl_Name   : Name_Id;

   begin
      Set_Str_To_Name_Buffer ("Helper");
      Helper_Name := Name_Find;
      Set_Str_To_Name_Buffer ("Skel");
      Skel_Name := Name_Find;
      Set_Str_To_Name_Buffer ("Impl");
      Impl_Name := Name_Find;

      if No (P) then
         return;
      end if;

      --  Build a string "<current_entity>%[s,b] <withed_entity>" that
      --  is the current entity name, a character 's' (resp 'b') to
      --  indicate whether we consider the spec (resp. body) of the
      --  current entity and the withed entity name.

      D := FE_Node (P);
      if Present (D) then

         --  This is a local entity and there is no need for a with clause

         if BEN.Name (Defining_Identifier (P)) /= Helper_Name
           and then BEN.Name (Defining_Identifier (P)) /= Skel_Name
           and then BEN.Name (Defining_Identifier (P)) /= Impl_Name
         then
            if Is_N_Parent_Of_M (D, FE_Node (Current_Entity)) then
               return;
            end if;
         end if;
      end if;

      --  To avoid that a package "with"es itself
      if Defining_Identifier (P) = Defining_Identifier
        (Package_Declaration (Current_Package))
      then
         return;
      end if;

      N := Fully_Qualified_Name (P);
      I := Defining_Identifier (Package_Declaration (Current_Package));
      Get_Name_String (Fully_Qualified_Name (I));
      Add_Char_To_Name_Buffer ('%');
      if Kind (Current_Package) = K_Package_Specification then
         Add_Char_To_Name_Buffer ('s');
      else
         Add_Char_To_Name_Buffer ('b');
      end if;
      Add_Char_To_Name_Buffer (' ');
      Get_Name_String_And_Append (N);
      N := To_Lower (Name_Find);

      --  Get the byte associated to the name in the hash table and
      --  check whether it is already set to 1 which means that the
      --  withed entity is already in the withed package list.

      B := Get_Name_Table_Byte (N);
      if B /= 0 then
         return;
      end if;
      Set_Name_Table_Byte (N, 1);

      if Output_Unit_Withing then
         Write_Name (N);
         Write_Eol;
      end if;

      --  Add entity to the withed packages list

      W := New_Node (K_Withed_Package);
      Set_Defining_Identifier (W, P);
      Append_Node_To_List (W, Withed_Packages (Current_Package));
   end Add_With_Package;

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;

   begin
      Last := Last_Node (L);
      if No (Last) then
         Set_First_Node (L, E);
      else
         Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         Set_Last_Node (L, Last);
         Last := Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   -------------
   -- Convert --
   -------------

   function Convert (K : FEN.Node_Kind) return RE_Id is
   begin
      case K is
         when FEN.K_Float               => return RE_Float;
         when FEN.K_Double              => return RE_Double;
         when FEN.K_Long_Double         => return RE_Long_Double;
         when FEN.K_Short               => return RE_Short;
         when FEN.K_Long                => return RE_Long;
         when FEN.K_Long_Long           => return RE_Long_Long;
         when FEN.K_Unsigned_Short      => return RE_Unsigned_Short;
         when FEN.K_Unsigned_Long       => return RE_Unsigned_Long;
         when FEN.K_Unsigned_Long_Long  => return RE_Unsigned_Long_Long;
         when FEN.K_Char                => return RE_Char;
         when FEN.K_Wide_Char           => return RE_WChar;
         when FEN.K_String              => return RE_String_0;
         when FEN.K_Wide_String         => return RE_Wide_String;
         when FEN.K_Boolean             => return RE_Boolean;
         when FEN.K_Octet               => return RE_Octet;
         when FEN.K_Object              => return RE_Ref_2;
         when others                    =>
            raise Program_Error;
      end case;
   end Convert;

   ---------------------
   -- Copy_Designator --
   ---------------------

   function Copy_Designator
     (Designator : Node_Id;
      Witheded   : Boolean := True)
     return Node_Id
   is
      D : Node_Id;
      P : Node_Id := Parent_Unit_Name (Designator);

   begin
      D := Copy_Node (Designator);
      if Present (P) then
         P := Copy_Designator (P, False);
         Set_Correct_Parent_Unit_Name (D, P);
         if Witheded then
            Add_With_Package (P);
         end if;
      end if;
      return D;
   end Copy_Designator;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (N : Node_Id) return Node_Id is
      C : Node_Id;

   begin
      case Kind (N) is
         when K_Designator =>
            C := New_Node (K_Designator);
            Set_Defining_Identifier (C, Defining_Identifier (N));
            Set_FE_Node (C, FE_Node (N));
            Set_Correct_Parent_Unit_Name (C, Parent_Unit_Name (N));

         when K_Defining_Identifier =>
            C := New_Node (K_Defining_Identifier);
            Set_Name (C, Name (N));
            Set_Correct_Parent_Unit_Name (C, Parent_Unit_Name (N));
            Set_Corresponding_Node (C, Corresponding_Node (N));

         when others =>
            raise Program_Error;
      end case;
      return C;
   end Copy_Node;

   --------------------
   -- Current_Entity --
   --------------------

   function Current_Entity return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_Entity;
      end if;
   end Current_Entity;

   ---------------------
   -- Current_Package --
   ---------------------

   function Current_Package return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_Package;
      end if;
   end Current_Package;

   ---------------------------------------
   -- Defining_Identifier_To_Designator --
   ---------------------------------------

   function Defining_Identifier_To_Designator
     (N                       : Node_Id;
      Copy                    : Boolean := False;
      Keep_Parent             : Boolean := True;
      Keep_Corresponding_Node : Boolean := True)
     return Node_Id
   is
      P      : Node_Id;
      Def_Id : Node_Id := N;
   begin
      pragma Assert (BEN.Kind (N) = K_Defining_Identifier);

      if Copy then
         Def_Id := Copy_Node (N);
      end if;

      if not Keep_Parent then
         Def_Id := Make_Defining_Identifier (BEN.Name (N));
      end if;

      if Keep_Corresponding_Node then
         Set_Corresponding_Node
           (Def_Id,
            Corresponding_Node (N));
      end if;

      P := New_Node (K_Designator);
      Set_Defining_Identifier (P, Def_Id);

      if Keep_Parent then
         Set_Correct_Parent_Unit_Name (P, Parent_Unit_Name (N));
      end if;

      return P;
   end Defining_Identifier_To_Designator;

   --------------------------
   -- Fully_Qualified_Name --
   --------------------------

   function Fully_Qualified_Name (N : Node_Id) return Name_Id is
      Parent_Node : Node_Id := No_Node;
      Parent_Name : Name_Id := No_Name;

   begin
      case Kind (N) is
         when K_Designator =>
            Parent_Node := Parent_Unit_Name (N);

            if not Present (Parent_Node) then
               Parent_Node := Parent_Unit_Name (Defining_Identifier (N));
            end if;

            if Present (Parent_Node) then
               Parent_Name := Fully_Qualified_Name (Parent_Node);
            end if;

            Name_Len := 0;
            if Present (Parent_Node) then
               Get_Name_String (Parent_Name);
               Add_Char_To_Name_Buffer ('.');
            end if;
            Get_Name_String_And_Append (Name (Defining_Identifier (N)));
            return Name_Find;

         when K_Defining_Identifier =>
            Parent_Node := Parent_Unit_Name (N);
            if Present (Parent_Node) then
               Parent_Name := Fully_Qualified_Name (Parent_Node);
            end if;

            Name_Len := 0;
            if Present (Parent_Node) then
               Get_Name_String (Parent_Name);
               Add_Char_To_Name_Buffer ('.');
            end if;
            Get_Name_String_And_Append (Name (N));
            return Name_Find;

         when others =>
            raise Program_Error;
      end case;
   end Fully_Qualified_Name;

   -----------------------
   -- Get_From_Any_Node --
   -----------------------

   function Get_From_Any_Node (T : Node_Id) return Node_Id is
      use Frontend.Nodes;
      Result : Node_Id;
   begin
      if Is_Base_Type (T) then
         if FEN.Kind (T) = FEN.K_Object then
            Result := RE (RE_From_Any_1);
         else
            Result := RE (RE_From_Any_0);
         end if;
      elsif FEN.Kind (T) = K_Scoped_Name then
         declare
            Reference           : constant Node_Id := FEN.Reference (T);
         begin
            Result := Expand_Designator
              (From_Any_Node
               (BE_Node
                (Identifier
                 (Reference))));
         end;
      else
         Result := Expand_Designator
           (From_Any_Node
            (BE_Node
             (Identifier
              (T))));
      end if;
      return Result;
   end Get_From_Any_Node;

   -----------------
   -- Get_TC_Node --
   -----------------

   function Get_TC_Node (T : Node_Id) return Node_Id is
      use Frontend.Nodes;
      Result : Node_Id;
   begin
      if Is_Base_Type (T) then
         Result := Base_Type_TC (FEN.Kind (T));
         --  Adding the dependancy on CORBA.Object
         if FEN.Kind (T) = K_Object then
            Dep_Array (Dep_CORBA_Object) := True;
         end if;
      elsif FEN.Kind (T) = K_Scoped_Name then
         declare
            Reference           : constant Node_Id := FEN.Reference (T);
         begin
            Result := Expand_Designator
              (TC_Node
               (BE_Node
                (Identifier
                 (Reference))));
         end;
      else
         Result := Expand_Designator
           (TC_Node
            (BE_Node
             (Identifier
              (T))));
      end if;
      return Result;
   end Get_TC_Node;

   ---------------------
   -- Get_To_Any_Node --
   ---------------------

   function Get_To_Any_Node (T : Node_Id) return Node_Id is
      use Frontend.Nodes;
      Result : Node_Id;
   begin
      if Is_Base_Type (T) then
         if FEN.Kind (T) = FEN.K_Object then
            Result := RE (RE_To_Any_3);
         else
            Result := RE (RE_To_Any_0);
         end if;
      elsif FEN.Kind (T) = K_Scoped_Name then
         declare
            Reference           : constant Node_Id := FEN.Reference (T);
         begin
            Result := Expand_Designator
              (To_Any_Node
               (BE_Node
                (Identifier
                 (Reference))));
         end;
      else
         Result := Expand_Designator
           (To_Any_Node
            (BE_Node
             (Identifier
              (T))));
      end if;
      return Result;
   end Get_To_Any_Node;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
      S : String := Token_Type'Image (T);
   begin
      To_Lower (S);
      return S (5 .. S'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : Operator_Type) return String is
      S : String := Operator_Type'Image (O);
   begin
      To_Lower (S);
      for I in S'First .. S'Last loop
         if S (I) = '_' then
            S (I) := ' ';
         end if;
      end loop;
      return S (4 .. S'Last);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      --  Keywords.
      for I in Keyword_Type loop
         New_Token (I);
      end loop;

      --  Graphic Characters
      New_Token (Tok_Double_Asterisk, "**");
      New_Token (Tok_Ampersand, "&");
      New_Token (Tok_Minus, "-");
      New_Token (Tok_Plus, "+");
      New_Token (Tok_Asterisk, "*");
      New_Token (Tok_Slash, "/");
      New_Token (Tok_Dot, ".");
      New_Token (Tok_Apostrophe, "'");
      New_Token (Tok_Left_Paren, "(");
      New_Token (Tok_Right_Paren, ")");
      New_Token (Tok_Comma, ",");
      New_Token (Tok_Less, "<");
      New_Token (Tok_Equal, "=");
      New_Token (Tok_Greater, ">");
      New_Token (Tok_Not_Equal, "/=");
      New_Token (Tok_Greater_Equal, ">=");
      New_Token (Tok_Less_Equal, "<=");
      New_Token (Tok_Box, "<>");
      New_Token (Tok_Colon_Equal, ":=");
      New_Token (Tok_Colon, ":");
      New_Token (Tok_Greater_Greater, ">>");
      New_Token (Tok_Less_Less, "<<");
      New_Token (Tok_Semicolon, ";");
      New_Token (Tok_Arrow, "=>");
      New_Token (Tok_Vertical_Bar, "|");
      New_Token (Tok_Dot_Dot, "..");
      New_Token (Tok_Minus_Minus, "--");

      for O in Op_And .. Op_Or_Else loop
         New_Operator (O);
      end loop;
      New_Operator (Op_Double_Asterisk, "**");
      New_Operator (Op_Minus, "-");
      New_Operator (Op_Plus, "+");
      New_Operator (Op_Asterisk, "*");
      New_Operator (Op_Slash, "/");
      New_Operator (Op_Less, "<");
      New_Operator (Op_Equal, "=");
      New_Operator (Op_Greater, ">");
      New_Operator (Op_Not_Equal, "/=");
      New_Operator (Op_Greater_Equal, ">=");
      New_Operator (Op_Less_Equal, "<=");
      New_Operator (Op_Box, "<>");
      New_Operator (Op_Colon_Equal, ":=");
      New_Operator (Op_Colon, "--");
      New_Operator (Op_Greater_Greater, ">>");
      New_Operator (Op_Less_Less, "<<");
      New_Operator (Op_Semicolon, ";");
      New_Operator (Op_Arrow, "=>");
      New_Operator (Op_Vertical_Bar, "|");

      for A in Attribute_Id loop
         Set_Str_To_Name_Buffer (Attribute_Id'Image (A));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         To_Lower (Name_Buffer (1 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         AN (A) := Name_Find;
      end loop;

      for C in Component_Id loop
         Set_Str_To_Name_Buffer (Component_Id'Image (C));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         To_Lower (Name_Buffer (1 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         CN (C) := Name_Find;
      end loop;

      for P in Parameter_Id loop
         Set_Str_To_Name_Buffer (Parameter_Id'Image (P));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         To_Lower (Name_Buffer (1 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         PN (P) := Name_Find;
      end loop;

      for S in Subprogram_Id loop
         Set_Str_To_Name_Buffer (Subprogram_Id'Image (S));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         To_Lower (Name_Buffer (1 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         SN (S) := Name_Find;
      end loop;

      for T in Type_Id loop
         Set_Str_To_Name_Buffer (Type_Id'Image (T));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         To_Lower (Name_Buffer (1 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         TN (T) := Name_Find;
      end loop;

      for V in Variable_Id loop
         Set_Str_To_Name_Buffer (Variable_Id'Image (V));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         Add_Str_To_Name_Buffer (Var_Suffix);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         VN (V) := Name_Find;
      end loop;

      for G in Pragma_Id loop
         Set_Str_To_Name_Buffer (Pragma_Id'Image (G));
         Set_Str_To_Name_Buffer (Name_Buffer (8 .. Name_Len));
         To_Lower (Name_Buffer (1 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         GN (G) := Name_Find;
      end loop;

      for D in Dependancy_Id loop
         Set_Str_To_Name_Buffer (Dependancy_Id'Image (D));
         Set_Str_To_Name_Buffer (Name_Buffer (5 .. Name_Len));
         To_Lower (Name_Buffer (1 .. Name_Len));
         --  Replacing any '_' with '.'
         for Index in 1 .. Name_Len loop
            if Name_Buffer (Index) = '_' then
               Name_Buffer (Index) := '.';
            end if;
         end loop;
         DP (D) := Name_Find;
      end loop;

   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (First_Node (L));
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length
     (L : List_Id)
     return Natural
   is
      N : Node_Id;
      C : Natural := 0;
   begin
      if not Is_Empty (L) then

         N := First_Node (L);
         while Present (N) loop
            C := C + 1;
            N := Next_Node (N);
         end loop;
      end if;
      return C;
   end Length;

   ---------------------------------
   -- Make_Access_Type_Definition --
   ---------------------------------

   function Make_Access_Type_Definition
     (Subtype_Indication : Node_Id;
      Is_All             : Boolean := False;
      Is_Constant        : Boolean := False)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Access_Type_Definition);
      Set_Subtype_Indication (N, Subtype_Indication);
      Set_Is_All (N, Is_All);
      Set_Is_Constant (N, Is_Constant);
      return N;
   end Make_Access_Type_Definition;

   ----------------------
   -- Make_Ada_Comment --
   ----------------------

   function Make_Ada_Comment (N : Name_Id) return Node_Id is
      C : Node_Id;
   begin
      C := New_Node (K_Ada_Comment);
      Set_Defining_Identifier (C, New_Node (K_Defining_Identifier));
      Set_Name (Defining_Identifier (C), N);
      return C;
   end Make_Ada_Comment;

   --------------------------------
   -- Make_Array_Type_Definition --
   --------------------------------

   function Make_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (BEN.K_Array_Type_Definition);
      Set_Range_Constraints (N, Range_Constraints);
      Set_Component_Definition (N, Component_Definition);
      return N;
   end Make_Array_Type_Definition;

   -------------------------------
   -- Make_Assignment_Statement --
   -------------------------------

   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Assignment_Statement);
      Set_Defining_Identifier (N, Variable_Identifier);
      Set_Expression (N, Expression);
      return N;
   end Make_Assignment_Statement;

   -------------------------------
   -- Make_Attribute_Designator --
   -------------------------------

   function Make_Attribute_Designator
     (Prefix    : Node_Id;
      Attribute : Attribute_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Attribute_Designator);
      Set_Prefix (N, Prefix);
      Set_Name
        (N, AN (Attribute));
      return N;
   end Make_Attribute_Designator;

   --------------------------
   -- Make_Block_Statement --
   --------------------------

   function Make_Block_Statement
     (Statement_Identifier : Node_Id := No_Node;
      Declarative_Part     : List_Id;
      Statements           : List_Id;
      Exception_Handler    : List_Id := No_List)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Block_Statement);
      Set_Defining_Identifier (N, Statement_Identifier);
      if Present (Statement_Identifier) then
         Set_Corresponding_Node (Statement_Identifier, N);
      end if;
      Set_Declarative_Part (N, Declarative_Part);
      Set_Statements (N, Statements);
      if not Is_Empty (Exception_Handler) then
         Set_Exception_Handler (N, Exception_Handler);
      end if;
      return N;
   end Make_Block_Statement;

   ---------------------
   -- Make_Case_Label --
   ---------------------

   function Make_Case_Label (Value : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Case_Label);
      Set_Value (N, Value);
      return N;
   end Make_Case_Label;

   -------------------------
   -- Make_Case_Statement --
   -------------------------

   function Make_Case_Statement
     (Expression                  : Node_Id;
      Case_Statement_Alternatives : List_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Case_Statement);
      Set_Expression (N, Expression);
      Set_Case_Statement_Alternatives (N, Case_Statement_Alternatives);
      return N;
   end Make_Case_Statement;

   -------------------------------------
   -- Make_Case_Statement_Alternative --
   -------------------------------------

   function Make_Case_Statement_Alternative
     (Discret_Choice_List : List_Id;
      Statements          : List_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Case_Statement_Alternative);
      Set_Discret_Choice_List (N, Discret_Choice_List);
      Set_Statements (N, Statements);
      return N;
   end Make_Case_Statement_Alternative;

   --------------------------------
   -- Make_Component_Association --
   --------------------------------

   function Make_Component_Association
     (Selector_Name : Node_Id;
      Expression    : Node_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Component_Association);
      Set_Defining_Identifier (N, Selector_Name);
      Set_Expression (N, Expression);
      return N;
   end Make_Component_Association;

   --------------------------------
   -- Make_Component_Declaration --
   --------------------------------

   function Make_Component_Declaration
     (Defining_Identifier : Node_Id;
      Subtype_Indication  : Node_Id;
      Expression          : Node_Id := No_Node)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Component_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Subtype_Indication (N, Subtype_Indication);
      Set_Expression (N, Expression);
      return N;
   end Make_Component_Declaration;

   ----------------------------------
   -- Make_Decimal_Type_Definition --
   ----------------------------------

   function Make_Decimal_Type_Definition
     (Definition : Node_Id)
     return Node_Id is
      N   : Node_Id;
      V   : Value_Id;
      Exp : Node_Id;
   begin
      N := New_Node (K_Decimal_Type_Definition);

      V := New_Floating_Point_Value
        (Long_Double (1.0 / (10 ** (Integer (FEN.N_Scale (Definition))))));

      Exp := Make_Literal (V);
      Set_Scale (N, Exp);

      V := New_Integer_Value
        (Unsigned_Long_Long (FEN.N_Total (Definition)),
         1,
         10);
      Set_Total (N, V);

      return N;
   end Make_Decimal_Type_Definition;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier
     (Name   : Name_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Defining_Identifier);
      Set_Name (N, To_Ada_Name (Name));
      return N;
   end Make_Defining_Identifier;

   ----------------------------------
   -- Make_Derived_Type_Definition --
   ----------------------------------

   function Make_Derived_Type_Definition
     (Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id := No_Node;
      Is_Abstract_Type      : Boolean := False;
      Is_Private_Extention  : Boolean := False;
      Is_Subtype            : Boolean := False)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Derived_Type_Definition);
      Set_Is_Abstract_Type (N, Is_Abstract_Type);
      Set_Is_Private_Extention (N, Is_Private_Extention);
      Set_Subtype_Indication (N, Subtype_Indication);
      Set_Record_Extension_Part (N, Record_Extension_Part);
      Set_Is_Subtype (N, Is_Subtype);
      return N;
   end Make_Derived_Type_Definition;

   ---------------------
   -- Make_Designator --
   ---------------------

   function Make_Designator
     (Designator : Name_Id;
      Parent     : Name_Id := No_Name;
      Is_All     : Boolean := False)
     return Node_Id
   is
      N : Node_Id;
      P : Node_Id;
   begin
      N := New_Node (K_Designator);
      Set_Defining_Identifier (N, Make_Defining_Identifier (Designator));
      Set_Is_All (N, Is_All);

      if Parent /= No_Name then
         P := New_Node (K_Designator);
         Set_Defining_Identifier (P, Make_Defining_Identifier (Parent));
         Set_Correct_Parent_Unit_Name (N, P);
      end if;

      return N;
   end Make_Designator;

   --------------------------
   -- Make_Elsif_Statement --
   --------------------------

   function Make_Elsif_Statement
     (Condition       : Node_Id;
      Then_Statements : List_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Elsif_Statement);
      Set_Condition (N, Condition);
      Set_Then_Statements (N, Then_Statements);
      return N;
   end Make_Elsif_Statement;

   --------------------------------------
   -- Make_Enumeration_Type_Definition --
   --------------------------------------

   function Make_Enumeration_Type_Definition
     (Enumeration_Literals : List_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Enumeration_Type_Definition);
      Set_Enumeration_Literals (N, Enumeration_Literals);
      return N;
   end Make_Enumeration_Type_Definition;

   --------------------------------
   -- Make_Exception_Declaration --
   --------------------------------

   function Make_Exception_Declaration
     (Defining_Identifier : Node_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Exception_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Corresponding_Node (Defining_Identifier, N);
      return N;
   end Make_Exception_Declaration;

   ---------------------
   -- Make_Expression --
   ---------------------

   function Make_Expression
     (Left_Expr  : Node_Id;
      Operator   : Operator_Type := Op_None;
      Right_Expr : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Expression);
      Set_Left_Expr (N, Left_Expr);
      Set_Operator (N, Operator_Type'Pos (Operator));
      Set_Right_Expr (N, Right_Expr);
      return N;
   end Make_Expression;

   ------------------------
   -- Make_For_Statement --
   ------------------------

   function Make_For_Statement
     (Defining_Identifier : Node_Id;
      Range_Constraint    : Node_Id;
      Statements          : List_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_For_Statement);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Range_Constraint (N, Range_Constraint);
      Set_Statements (N, Statements);
      return N;
   end Make_For_Statement;

   --------------------------------
   -- Make_Full_Type_Declaration --
   --------------------------------

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id;
      Discriminant_Spec   : Node_Id := No_Node;
      Parent              : Node_Id := No_Node;
      Is_Subtype          : Boolean := False)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Full_Type_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Corresponding_Node (Defining_Identifier, N);
      Set_Type_Definition (N, Type_Definition);
      Set_Discriminant_Spec (N, Discriminant_Spec);
      if Present (Parent) then
         Set_Parent (N, Parent);
      else
         Set_Parent (N, Current_Package);
      end if;
      Set_Is_Subtype (N, Is_Subtype);
      return N;
   end Make_Full_Type_Declaration;

   -----------------------
   -- Make_If_Statement --
   -----------------------

   function Make_If_Statement
     (Condition        : Node_Id;
      Then_Statements  : List_Id;
      Elsif_Statements : List_Id := No_List;
      Else_Statements  : List_Id := No_List)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_If_Statement);
      Set_Condition (N, Condition);
      Set_Then_Statements (N, Then_Statements);
      Set_Elsif_Statements (N, Elsif_Statements);
      Set_Else_Statements (N, Else_Statements);
      return N;
   end Make_If_Statement;

   ------------------
   -- Make_List_Id --
   ------------------

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node)
     return List_Id
   is
      L : List_Id;
   begin
      L := New_List (K_List_Id);
      Append_Node_To_List (N1, L);
      if Present (N2) then
         Append_Node_To_List (N2, L);

         if Present (N3) then
            Append_Node_To_List (N3, L);
         end if;
      end if;
      return L;
   end Make_List_Id;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal
     (Value             : Value_Id;
      Parent_Designator : Node_Id := No_Node)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Literal);
      Set_Value (N, Value);
      Set_Parent_Designator (N, Parent_Designator);
      return N;
   end Make_Literal;

   -----------------------------
   -- Make_Object_Declaration --
   -----------------------------

   function Make_Object_Declaration
     (Defining_Identifier : Node_Id;
      Constant_Present    : Boolean := False;
      Object_Definition   : Node_Id;
      Expression          : Node_Id := No_Node;
      Parent              : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Object_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Corresponding_Node (Defining_Identifier, N);
      Set_Constant_Present (N, Constant_Present);
      Set_Object_Definition (N, Object_Definition);
      Set_Expression (N, Expression);

      if No (Parent) then
         Set_Parent (N, Current_Package);
      else
         Set_Parent (N, Parent);
      end if;

      return N;
   end Make_Object_Declaration;

   ------------------------------
   -- Make_Package_Declaration --
   ------------------------------

   function Make_Package_Declaration (Identifier : Node_Id) return Node_Id is
      Pkg  : Node_Id;
      Unit : Node_Id;

   begin
      Unit := New_Node (K_Package_Declaration);
      Set_Defining_Identifier (Unit, Identifier);
      Set_Corresponding_Node (Identifier, Unit);
      if Present (Current_Entity)
        and then FEN."/="
        (FEN.Kind
         (FEN.Corresponding_Entity
          (FE_Node
           (Current_Entity))),
         FEN.K_Specification)
      then
         Set_Parent (Unit, Main_Package (Current_Entity));
      end if;
      Pkg := New_Node (K_Package_Specification);
      Set_Withed_Packages (Pkg, New_List (K_Withed_Packages));
      Set_Visible_Part (Pkg, New_List (K_Declaration_List));
      Set_Private_Part (Pkg, New_List (K_Declaration_List));
      Set_Package_Declaration (Pkg, Unit);
      Set_Package_Specification (Unit, Pkg);
      Pkg := New_Node (K_Package_Implementation);
      Set_Withed_Packages (Pkg, New_List (K_Withed_Packages));
      Set_Declarations (Pkg, New_List (K_Declaration_List));
      Set_Statements (Pkg, New_List (K_Statement_List));
      Set_Package_Declaration (Pkg, Unit);
      Set_Package_Implementation (Unit, Pkg);
      return Unit;
   end Make_Package_Declaration;

   --------------------------------
   -- Make_Package_Instanciation --
   --------------------------------

   function Make_Package_Instantiation
     (Defining_Identifier : Node_Id;
      Generic_Package     : Node_Id;
      Parameter_List      : List_Id := No_List)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Package_Instantiation);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Corresponding_Node (Defining_Identifier, N);
      Set_Generic_Package (N, Generic_Package);
      Set_Parameter_List (N, Parameter_List);
      return N;
   end Make_Package_Instantiation;

   ----------------------------------
   -- Make_Parameter_Specification --
   ----------------------------------

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := Mode_In)
      return                Node_Id
   is
      P : Node_Id;

   begin
      P := New_Node (K_Parameter_Specification);
      Set_Defining_Identifier (P, Defining_Identifier);
      Set_Parameter_Type (P, Subtype_Mark);
      Set_Parameter_Mode (P, Parameter_Mode);
      return P;
   end Make_Parameter_Specification;

   ---------------------------
   -- Make_Pragma_Statement --
   ---------------------------

   function Make_Pragma_Statement
     (Expression : Node_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Pragma_Statement);
      Set_Expression (N, Expression);
      return N;
   end Make_Pragma_Statement;

   -------------------------------
   -- Make_Qualified_Expression --
   -------------------------------

   function Make_Qualified_Expression
     (Subtype_Mark  : Node_Id;
      Expression    : Node_Id := No_Node;
      Aggregate     : Node_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Qualified_Expression);
      Set_Subtype_Mark (N, Subtype_Mark);
      Set_Expression (N, Expression);
      Set_Aggregate (N, Aggregate);
      return N;
   end Make_Qualified_Expression;

   ---------------------------
   -- Make_Record_Aggregate --
   ---------------------------

   function Make_Record_Aggregate
     (L : List_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Record_Aggregate);
      Set_Component_Association_List (N, L);
      return N;
   end Make_Record_Aggregate;

   ----------------------------
   -- Make_Record_Definition --
   ----------------------------

   function Make_Record_Definition
     (Component_List : List_Id)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Record_Definition);
      Set_Component_List (N, Component_List);
      return N;
   end Make_Record_Definition;

   ---------------------------------
   -- Make_Record_Type_Definition --
   ---------------------------------

   function Make_Record_Type_Definition
     (Record_Definition : Node_Id;
      Is_Abstract_Type  : Boolean := False;
      Is_Tagged_Type    : Boolean := False;
      Is_Limited_Type   : Boolean := False)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Record_Type_Definition);
      Set_Is_Abstract_Type (N, Is_Abstract_Type);
      Set_Is_Tagged_Type (N, Is_Tagged_Type);
      Set_Is_Limited_Type (N, Is_Limited_Type);
      Set_Record_Definition (N, Record_Definition);
      return N;
   end Make_Record_Type_Definition;

   ---------------------------
   -- Make_Return_Statement --
   ---------------------------

   function Make_Return_Statement
     (Expression : Node_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Return_Statement);
      Set_Expression (N, Expression);
      return N;
   end Make_Return_Statement;

   --------------------------
   -- Make_Subprogram_Call --
   --------------------------

   function Make_Subprogram_Call
     (Defining_Identifier : Node_Id;
      Actual_Parameter_Part : List_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Subprogram_Call);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Actual_Parameter_Part (N, Actual_Parameter_Part);
      return N;
   end Make_Subprogram_Call;

   ------------------------------------
   -- Make_Subprogram_Implementation --
   ------------------------------------

   function Make_Subprogram_Implementation
     (Specification : Node_Id;
      Declarations  : List_Id;
      Statements    : List_Id)

     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Subprogram_Implementation);
      Set_Specification (N, Specification);
      Set_Declarations (N, Declarations);
      Set_Statements (N, Statements);
      return N;
   end Make_Subprogram_Implementation;

   -----------------------------------
   -- Make_Subprogram_Specification --
   -----------------------------------

   function Make_Subprogram_Specification
     (Defining_Identifier : Node_Id;
      Parameter_Profile   : List_Id;
      Return_Type         : Node_Id := No_Node;
      Parent              : Node_Id := Current_Package;
      Renamed_Subprogram  : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Subprogram_Specification);
      Set_Defining_Identifier  (N, Defining_Identifier);
      Set_Parameter_Profile    (N, Parameter_Profile);
      Set_Return_Type          (N, Return_Type);
      Set_Parent               (N, Parent);
      Set_Renamed_Subprogram   (N, Renamed_Subprogram);
      return N;
   end Make_Subprogram_Specification;

   -------------------------
   -- Make_Type_Attribute --
   -------------------------

   function Make_Type_Attribute
     (Designator : Node_Id;
      Attribute  : Attribute_Id)
     return Node_Id
   is
      procedure Get_Scoped_Name_String (S : Node_Id);

      ----------------------------
      -- Get_Scoped_Name_String --
      ----------------------------

      procedure Get_Scoped_Name_String (S : Node_Id) is
         P : Node_Id;

      begin
         P := Parent_Unit_Name (S);
         if Present (P) then
            Get_Scoped_Name_String (P);
            Add_Char_To_Name_Buffer ('.');
         end if;
         Get_Name_String_And_Append (Name (Defining_Identifier (S)));
      end Get_Scoped_Name_String;

   begin
      Name_Len := 0;
      Get_Scoped_Name_String (Designator);
      Add_Char_To_Name_Buffer (''');
      Get_Name_String_And_Append (AN (Attribute));
      return Make_Defining_Identifier (Name_Find);
   end Make_Type_Attribute;

   -----------------------
   -- Make_Variant_Part --
   -----------------------

   function Make_Variant_Part
     (Discriminant        : Node_Id;
      Variant_List        : List_Id)
     return                Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Variant_Part);
      Set_Variants (N, Variant_List);
      Set_Discriminant (N, Discriminant);
      return N;
   end Make_Variant_Part;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return List_Id is
      N : Node_Id;

   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         Set_Loc  (N, Loc (From));
      else
         Set_Loc  (N, No_Location);
      end if;
      return List_Id (N);
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;
   begin
      Entries.Increment_Last;
      N := Entries.Last;
      Entries.Table (N) := Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         Bind_FE_To_Stub (From, N);
         Set_Loc  (N, FEN.Loc (From));
      else
         Set_Loc  (N, No_Location);
      end if;
      return N;
   end New_Node;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token
     (T : Token_Type;
      I : String := "") is
   begin
      if T in Keyword_Type then
         Set_Str_To_Name_Buffer (Image (T));
         Set_Name_Table_Byte (Name_Find, Byte (Token_Type'Pos (T) + 1));

      else
         Set_Str_To_Name_Buffer (I);
      end if;
      Token_Image (T) := Name_Find;
   end New_Token;

   ------------------
   -- New_Operator --
   ------------------

   procedure New_Operator
     (O : Operator_Type;
      I : String := "") is
   begin
      if O in Keyword_Operator then
         Set_Str_To_Name_Buffer (Image (O));
      else
         Set_Str_To_Name_Buffer (I);
      end if;
      Operator_Image (Operator_Type'Pos (O)) := Name_Find;
   end New_Operator;

   ----------------
   -- Pop_Entity --
   ----------------

   procedure Pop_Entity is
   begin
      if Last > No_Depth then
         Decrement_Last;
      end if;
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   procedure Push_Entity (E : Node_Id) is
   begin
      Increment_Last;
      Table (Last).Current_Entity := E;
   end Push_Entity;

   --------------------------
   -- Qualified_Designator --
   --------------------------

   function Qualified_Designator (P : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Designator);
      Set_Defining_Identifier
        (N, Make_Defining_Identifier (Name (P)));
      if Present (Parent_Unit_Name (P)) then
         Set_Correct_Parent_Unit_Name
           (N, Qualified_Designator (Parent_Unit_Name (P)));
      else
         Set_Correct_Parent_Unit_Name (N, No_Node);
      end if;

      return N;
   end Qualified_Designator;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;

   begin
      C := First_Node (L);
      if C = E then
         Set_First_Node (L, Next_Node (E));
         if Last_Node (L) = E then
            Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if Next_Node (C) = E then
               Set_Next_Node (C, Next_Node (E));
               if Last_Node (L) = E then
                  Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

   ----------------------------------
   -- Set_Correct_Parent_Unit_Name --
   ----------------------------------

   procedure Set_Correct_Parent_Unit_Name
     (Child  : Node_Id;
      Parent : Node_Id)
   is
   begin
      pragma Assert (BEN.Kind (Child) = K_Defining_Identifier
                     or else BEN.Kind (Child) = K_Designator);

      pragma Assert (Parent = No_Node
                     or else BEN.Kind (Parent) = K_Defining_Identifier
                     or else BEN.Kind (Parent) = K_Designator);

      case BEN.Kind (Child) is

         when K_Defining_Identifier =>
            if Parent = No_Node then
               Set_Parent_Unit_Name
                 (Child, Parent);
            elsif BEN.Kind (Parent) = K_Defining_Identifier then
                  Set_Parent_Unit_Name
                    (Child, Parent);
            elsif BEN.Kind (Parent) = K_Designator then
               Set_Parent_Unit_Name
                 (Child, Parent);
            else
               raise Program_Error;
            end if;

         when K_Designator =>
            if Parent = No_Node then
               Set_Parent_Unit_Name
                 (Child, Parent);
               if Present (Defining_Identifier (Child)) then
                  Set_Parent_Unit_Name
                    (Defining_Identifier (Child), Parent);
               end if;
            elsif BEN.Kind (Parent) = K_Defining_Identifier then
               Set_Parent_Unit_Name
                 (Child, Defining_Identifier_To_Designator (Parent));
               if Present (Defining_Identifier (Child)) then
                  Set_Parent_Unit_Name
                    (Defining_Identifier (Child), Parent);
               end if;
            elsif BEN.Kind (Parent) = K_Designator then
               Set_Parent_Unit_Name
                 (Child, Parent);
               if Present (Defining_Identifier (Child)) then
                  Set_Parent_Unit_Name
                    (Defining_Identifier (Child),
                     Defining_Identifier (Parent));
               end if;
            else
               raise Program_Error;
            end if;

         when others =>
            raise Program_Error;

      end case;
   end Set_Correct_Parent_Unit_Name;

   -------------------
   -- Set_Forwarded --
   -------------------

   procedure Set_Forwarded (E : Node_Id) is
      N : Node_Id;
   begin
      --  We cannot directly append the node E to the List of forwarded
      --  entities because this node may have to be appended to other lists
      N := New_Node (K_Node_Id);
      Set_FE_Node (N, E);
      if Is_Empty (Forwarded_Entities) then
         Forwarded_Entities := New_List (K_List_Id);
      end if;
      Append_Node_To_List (N, Forwarded_Entities);
   end Set_Forwarded;

   ------------------
   -- Is_Forwarded --
   ------------------

   function Is_Forwarded (E : Node_Id) return Boolean is
      Result : Boolean := False;
      N      : Node_Id;
   begin
      if Is_Empty (Forwarded_Entities) then
         Forwarded_Entities := New_List (K_List_Id);
      end if;
      N := First_Node (Forwarded_Entities);
      while Present (N) loop
         if FE_Node (N) = E then
            Result := True;
         end if;
         N := Next_Node (N);
      end loop;
      return Result;
   end Is_Forwarded;

   ---------------------
   -- Set_Helper_Body --
   ---------------------

   procedure Set_Helper_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Helper_Package (X));
   end Set_Helper_Body;

   ---------------------
   -- Set_Helper_Spec --
   ---------------------

   procedure Set_Helper_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Helper_Package (X));
   end Set_Helper_Spec;

   -------------------
   -- Set_Impl_Body --
   -------------------

   procedure Set_Impl_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Implementation_Package (X));
   end Set_Impl_Body;

   -------------------
   -- Set_Impl_Spec --
   -------------------

   procedure Set_Impl_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Implementation_Package (X));
   end Set_Impl_Spec;

   -------------------
   -- Set_Main_Body --
   -------------------

   procedure Set_Main_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Main_Package (X));
   end Set_Main_Body;

   -------------------
   -- Set_Main_Spec --
   -------------------

   procedure Set_Main_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Main_Package (X));
   end Set_Main_Spec;

   -----------------------
   -- Set_Skeleton_Body --
   -----------------------

   procedure Set_Skeleton_Body (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Implementation (Skeleton_Package (X));
   end Set_Skeleton_Body;

   -----------------------
   -- Set_Skeleton_Spec --
   -----------------------

   procedure Set_Skeleton_Spec (N : Node_Id := No_Node) is
      X : Node_Id := N;
   begin
      if No (X) then
         X := Table (Last).Current_Entity;
      end if;
      Table (Last).Current_Package :=
        Package_Specification (Skeleton_Package (X));
   end Set_Skeleton_Spec;

   -----------------
   -- To_Ada_Name --
   -----------------

   function To_Ada_Name (N : Name_Id) return Name_Id is
      First : Natural := 1;
      Name  : Name_Id;
      V     : Byte;
   begin
      Get_Name_String (N);
      while First <= Name_Len
        and then Name_Buffer (First) = '_'
      loop
         First := First + 1;
      end loop;

      for I in First .. Name_Len loop
         if Name_Buffer (I) = '_'
           and then I < Name_Len
           and then Name_Buffer (I + 1) = '_'
         then
            Name_Buffer (I + 1) := 'U';
         end if;
      end loop;

      if Name_Buffer (Name_Len) = '_' then
         Add_Char_To_Name_Buffer ('U');
      end if;
      Name := Name_Find;

      --  If the identifier collides with an Ada reserved word insert
      --  "IDL_" string before the identifier.

      V := Get_Name_Table_Byte (Name);
      if V > 0 then
         Set_Str_To_Name_Buffer ("IDL_");
         Get_Name_String_And_Append (Name);
         Name := Name_Find;
      end if;
      return Name;
   end To_Ada_Name;

   ------------------
   -- To_Spec_Name --
   ------------------

   function To_Spec_Name (N : Name_Id) return Name_Id is
   begin
      Get_Name_String (N);
      To_Lower (Name_Buffer (1 .. Name_Len));
      if Name_Len > 2
        and then Name_Buffer (Name_Len - 1) = '%'
      then
         Name_Buffer (Name_Len) := 's';
      else
         Add_Str_To_Name_Buffer ("%s");
      end if;
      return Name_Find;
   end To_Spec_Name;

end Backend.BE_Ada.Nutils;
