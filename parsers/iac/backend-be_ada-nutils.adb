with GNAT.Table;

with Charset;   use Charset;
with Locations; use Locations;
with Namet;     use Namet;
with Utils;     use Utils;

with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Frontend.Nodes;

package body Backend.BE_Ada.Nutils is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;

   type Entity_Stack_Entry is record
      Current_Package : Node_Id;
      Current_Entity  : Node_Id;
   end record;

   No_Depth : constant Int := -1;
   package Entity_Stack is
      new GNAT.Table (Entity_Stack_Entry, Int, No_Depth + 1, 10, 10);

   use Entity_Stack;


   ----------------------
   -- Add_With_Package --
   ----------------------

   procedure Add_With_Package (P : Node_Id) is

      procedure Get_Defining_Identifier_Name (N : Node_Id);

      ----------------------------------
      -- Get_Defining_Identifier_Name --
      ----------------------------------

      procedure Get_Defining_Identifier_Name (N : Node_Id) is
         P : constant Node_Id := Parent_Unit_Name (N);

      begin
         if Present (P) then
            Get_Defining_Identifier_Name (P);
            Add_Char_To_Name_Buffer ('.');
         end if;
         Get_Name_String_And_Append (BEN.Name (N));
      end Get_Defining_Identifier_Name;

      W : Node_Id;
      N : Name_Id;
      B : Byte;

   begin

      --  Build a string "<current_entity>%[s,b] <withed_entity>" that
      --  is the current entity name, a character 's' (resp 'b') to
      --  indicate that we consider the spec (resp. body) of the
      --  current entity and the withed entity name.

      Name_Len := 0;
      Get_Defining_Identifier_Name
        (Defining_Identifier (Package_Declaration (Current_Package)));
      Add_Char_To_Name_Buffer ('%');
      if Kind (Current_Package) = K_Package_Specification then
         Add_Char_To_Name_Buffer ('s');
      else
         Add_Char_To_Name_Buffer ('b');
      end if;
      Add_Char_To_Name_Buffer (' ');
      Get_Defining_Identifier_Name
        (Defining_Identifier (P));
      N := To_Lower (Name_Find);

      --  Get the byte associated to the name in the hash table and
      --  check whether it is already set to 1 which means that the
      --  withed entity is already in the withed package list.

      B := Get_Name_Table_Byte (N);
      if B /= 0 then
         return;
      end if;
      Set_Name_Table_Byte (N, 1);

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


   ---------------------
   -- Copy_Designator --
   ---------------------

   function Copy_Designator
     (Designator : Node_Id)
     return Node_Id
   is
      D : Node_Id;
      P : Node_Id := Parent_Unit_Name (Designator);

   begin
      D := Copy_Node (Designator);
      if Present (P) then
         P := Copy_Designator (P);
         Set_Parent_Unit_Name (D, P);
         Add_With_Package (P);
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
            Set_Parent_Unit_Name (C, Parent_Unit_Name (N));

         when K_Defining_Identifier =>
            C := New_Node (K_Defining_Identifier);
            Set_Name (C, Name (N));
            Set_Parent_Unit_Name (C, Parent_Unit_Name (N));

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

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
      S : String := Token_Type'Image (T);
   begin
      To_Lower (S);
      return S (5 .. S'Last);
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

      for A in Attribute_Id loop
         Set_Str_To_Name_Buffer (Attribute_Id'Image (A));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         AN (A) := Name_Find;
      end loop;

      for C in Component_Id loop
         Set_Str_To_Name_Buffer (Component_Id'Image (C));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         CN (C) := Name_Find;
      end loop;

      for P in Parameter_Id loop
         Set_Str_To_Name_Buffer (Parameter_Id'Image (P));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         PN (P) := Name_Find;
      end loop;

      for S in Subprogram_Id loop
         Set_Str_To_Name_Buffer (Subprogram_Id'Image (S));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         SN (S) := Name_Find;
      end loop;

      for T in Type_Id loop
         Set_Str_To_Name_Buffer (Type_Id'Image (T));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         TN (T) := Name_Find;
      end loop;

      for V in Variable_Id loop
         Set_Str_To_Name_Buffer (Variable_Id'Image (V));
         Set_Str_To_Name_Buffer (Name_Buffer (3 .. Name_Len));
         Add_Str_To_Name_Buffer ("_U");
         Capitalize (Name_Buffer (1 .. Name_Len));
         VN (V) := Name_Find;
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

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id is
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
      Record_Extension_Part : Node_Id;
      Is_Abstract_Type      : Boolean := False)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Derived_Type_Definition);
      Set_Is_Abstract_Type (N, Is_Abstract_Type);
      Set_Subtype_Indication (N, Subtype_Indication);
      Set_Record_Extension_Part (N, Record_Extension_Part);
      return N;
   end Make_Derived_Type_Definition;

   ---------------------
   -- Make_Designator --
   ---------------------

   function Make_Designator
     (Identifier : Node_Id;
      Unit_Name : Node_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_Designator);
      Set_Defining_Identifier (N, Identifier);
      Set_Parent_Unit_Name (N, Unit_Name);

      if Present (Unit_Name) then
         Add_With_Package (Unit_Name);
      end if;

      return N;
   end Make_Designator;

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
      return N;
   end Make_Exception_Declaration;

   --------------------------------
   -- Make_Full_Type_Declaration --
   --------------------------------

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id;
      Discriminant_Spec   : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Full_Type_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Type_Definition (N, Type_Definition);
      Set_Discriminant_Spec (N, Discriminant_Spec);
      return N;
   end Make_Full_Type_Declaration;


   -----------------------
   -- Make_If_Statement --
   -----------------------

   function Make_If_Statement
     (Condition : Node_Id;
      Then_Statements : List_Id;
      Else_Statements : List_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (K_If_Statement);
      Set_Condition (N, Condition);
      Set_Then_Statements (N, Then_Statements);
      Set_Else_Statements (N, Else_Statements);
      return N;
   end Make_If_Statement;

   ------------------
   -- Make_List_Id --
   ------------------

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node)
     return List_Id
   is
      L : List_Id;
   begin
      L := New_List (K_List_Id);
      Append_Node_To_List (N1, L);
      if Present (N2) then
         Append_Node_To_List (N2, L);
      end if;
      return L;
   end Make_List_Id;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal
     (Value : Value_Id)
     return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Literal);
      Set_Value (N, Value);
      return N;
   end Make_Literal;

   -----------------------------
   -- Make_Object_Declaration --
   -----------------------------

   function Make_Object_Declaration
     (Defining_Identifier : Node_Id;
      Constant_Present    : Boolean;
      Object_Definition   : Node_Id;
      Expression          : Node_Id)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Object_Declaration);
      Set_Defining_Identifier (N, Defining_Identifier);
      Set_Constant_Present (N, Constant_Present);
      Set_Object_Definition (N, Object_Definition);
      Set_Expression (N, Expression);
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
      if Present (Current_Entity) then
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
      Return_Type         : Node_Id := No_Node)
     return Node_Id
   is
      N : Node_Id;

   begin
      N := New_Node (K_Subprogram_Specification);
      Set_Defining_Identifier  (N, Defining_Identifier);
      Set_Parameter_Profile    (N, Parameter_Profile);
      Set_Return_Type          (N, Return_Type);
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
         BEN.Set_FE_Node (N, From);
         FEN.Set_BE_Node (From, N);
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
         Set_Parent_Unit_Name
           (N, Qualified_Designator (Parent_Unit_Name (P)));
      else
         Set_Parent_Unit_Name (N, No_Node);
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

end Backend.BE_Ada.Nutils;
