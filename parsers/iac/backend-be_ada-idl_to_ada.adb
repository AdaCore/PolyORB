with Namet;     use Namet;
with Types;     use Types;
with Values;    use Values;

with Frontend.Nodes;            use Frontend.Nodes;

with Backend.BE_Ada.Nodes;      use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;     use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;    use Backend.BE_Ada.Runtime;

package body Backend.BE_Ada.IDL_To_Ada is

   Setter : constant Character := 'S';

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;

   function Convert
     (K : FEN.Node_Kind)
     return RE_Id;

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
         when FEN.K_String              => return RE_String_1;
         when FEN.K_Wide_String         => return RE_Wide_String;
         when FEN.K_Boolean             => return RE_Boolean;
         when others                    =>
            raise Program_Error;
      end case;
   end Convert;

   ------------------
   -- Is_Base_Type --
   ------------------

   function Is_Base_Type
     (N : Node_Id)
     return Boolean
   is
   begin
      if FEN.Kind (N) in  FEN.K_Float .. FEN.K_Value_Base then
         return True;
      else
         return False;
      end if;
   end Is_Base_Type;

   -------------------
   -- Link_BE_To_FE --
   -------------------

   procedure Link_BE_To_FE
     (B : Node_Id;
      F : Node_Id)
   is
   begin
      if Present (F) then
         BEN.Set_FE_Node (F, B);
      end if;
   end Link_BE_To_FE;

   -------------------
   -- Link_FE_To_BE --
   -------------------

   procedure Link_FE_To_BE
     (F : Node_Id;
      B : Node_Id)
   is
   begin
      if Present (B) then
         BEN.Set_FE_Node (B, F);
      end if;
   end Link_FE_To_BE;

   ------------------------------
   -- Map_Accessor_Declaration --
   ------------------------------

   function Map_Accessor_Declaration
     (Accessor  : Character;
      Attribute : Node_Id)
     return Node_Id
   is
      Parameter  : Node_Id;
      Parameters : List_Id;
      Param_Type : Node_Id;
      Attr_Name  : Name_Id;

   begin
      Parameters := New_List (K_Parameter_Profile);

      --  Add the dispatching parameter to the parameter profile

      Parameter  := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Self)), RE (RE_Ref_0));
      Append_Node_To_List (Parameter, Parameters);

      Param_Type := Map_Designator
        (Type_Spec (Declaration (Attribute)));

      Link_BE_To_FE
        (Defining_Identifier (Param_Type),
         Type_Spec (Declaration (Attribute)));

      --  For the setter subprogram, add the second parameter To.

      if Accessor = Setter then
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_To)), Param_Type);
         Append_Node_To_List (Parameter, Parameters);
         Param_Type := No_Node;
      end if;

      --  At this point, Param_Type is the returned type. No_Node
      --  stands for the setter. Otherwise it is the getter.

      Attr_Name := To_Ada_Name (IDL_Name (FEN.Identifier (Attribute)));
      Set_Str_To_Name_Buffer ("Set_");
      Name_Buffer (1) := Accessor;
      Get_Name_String_And_Append (Attr_Name);
      return Make_Subprogram_Specification
        (Make_Defining_Identifier (Name_Find), Parameters, Param_Type);
   end Map_Accessor_Declaration;

   ------------------------------------
   -- Map_Declarator_Type_Designator --
   ------------------------------------

   function Map_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
     return Node_Id
   is
      Designator : Node_Id;
      Decl_Name  : Name_Id;

   begin
      Designator := Map_Designator (Type_Decl);

      --  When the declarator is complex, the component type is an
      --  array type.

      if Kind (Declarator) = K_Complex_Declarator then
         Decl_Name := To_Ada_Name (IDL_Name (FEN.Identifier (Declarator)));
         Get_Name_String (Decl_Name);
         Add_Str_To_Name_Buffer ("_Array");
         Decl_Name := Name_Find;
         Append_Node_To_List
           (Make_Full_Type_Declaration
              (Defining_Identifier => Make_Defining_Identifier (Decl_Name),
               Type_Definition     => Make_Array_Type_Definition
                 (Map_Range_Constraints
                    (FEN.Array_Sizes (Declarator)), Designator)),
            Visible_Part (Current_Package));
         Designator := New_Node (K_Designator);
         Set_Defining_Identifier
           (Designator, Make_Defining_Identifier (Decl_Name));
      end if;
      return Designator;
   end Map_Declarator_Type_Designator;

   -----------------------------
   -- Map_Defining_Identifier --
   -----------------------------

   function Map_Defining_Identifier (Entity : Node_Id) return Node_Id is
      use FEN;

      I : Node_Id := Entity;

   begin
      if FEN.Kind (Entity) /= FEN.K_Identifier then
         I := FEN.Identifier (Entity);
      end if;
      return Make_Defining_Identifier (IDL_Name (I));
   end Map_Defining_Identifier;

   --------------------
   -- Map_Designator --
   --------------------

   function Map_Designator (Entity : Node_Id) return Node_Id is
      use FEN;
      P : Node_Id;
      N : Node_Id;
      K : FEN.Node_Kind;
      R : Node_Id;

   begin
      K := FEN.Kind (Entity);
      if K = FEN.K_Scoped_Name then
         R := Reference (Entity);
         if Kind (R) = K_Specification then
            return No_Node;
         end if;
         N := New_Node (K_Designator);
         Set_Defining_Identifier (N, Map_Defining_Identifier (R));
         P := Scope_Entity (Identifier (R));
         if Present (P) then
            Set_Parent_Unit_Name (N, Map_Designator (P));
         end if;

      elsif K in FEN.K_Float .. FEN.K_Value_Base then
         N := RE (Convert (K));

      else
         N := New_Node (K_Designator);
         Set_Defining_Identifier (N, Map_Defining_Identifier (Entity));
      end if;

      P := Parent_Unit_Name (N);
      if Present (P) then
         Add_With_Package (P);
      end if;

      return N;
   end Map_Designator;

   ------------------------------------
   -- Map_Fully_Qualified_Identifier --
   ------------------------------------

   function Map_Fully_Qualified_Identifier
     (Entity : Node_Id)
     return Node_Id is
      use FEN;

      N : Node_Id;
      P : Node_Id;
      I : Node_Id;

   begin
      I := FEN.Identifier (Entity);
      Get_Name_String (IDL_Name (I));
      if Kind (Entity) = K_Specification then
         Add_Str_To_Name_Buffer ("_IDL_File");
      end if;
      N := Make_Defining_Identifier (Name_Find);
      P := FEN.Scope_Entity (I);
      if Present (P)
        and then FEN.Kind (P) /= FEN.K_Specification
      then
         if FEN.Kind (P) = FEN.K_Operation_Declaration then
            I := FEN.Identifier (P);
            P := FEN.Scope_Entity (I);
         end if;
         Set_Parent_Unit_Name (N, Map_Fully_Qualified_Identifier (P));
      end if;
      return N;
   end Map_Fully_Qualified_Identifier;

   ------------------
   -- Map_IDL_Unit --
   ------------------

   function Map_IDL_Unit (Entity : Node_Id) return Node_Id is
      P : Node_Id;
      N : Node_Id;
      M : Node_Id;  -- Main Package;
      D : Node_Id;
      L : List_Id;
      I : Node_Id;

   begin
      P := New_Node (K_IDL_Unit, Entity);

      L := New_List (K_Packages);
      Set_Packages (P, L);

      I := Map_Fully_Qualified_Identifier (Entity);

      --  Main package

      M := Make_Package_Declaration (I);
      Set_Main_Package (P, M);
      Append_Node_To_List (M, L);

      --  Helper package

      Set_Str_To_Name_Buffer ("Helper");
      N := Make_Defining_Identifier (Name_Find);
      Set_Parent_Unit_Name (N, I);
      D := Make_Package_Declaration (N);
      Set_Parent (D, M);
      Set_Helper_Package (P, D);
      Append_Node_To_List (D, L);

      if Kind (Entity) = K_Interface_Declaration then

         --  Skeleton package

         Set_Str_To_Name_Buffer ("Skel");
         N := Make_Defining_Identifier (Name_Find);
         Set_Parent_Unit_Name (N, I);
         D := Make_Package_Declaration (N);
         Set_Parent (D, M);
         Set_Skeleton_Package (P, D);
         Append_Node_To_List (D, L);

         --  Implementation package

         Set_Str_To_Name_Buffer ("Impl");
         N := Make_Defining_Identifier (Name_Find);
         Set_Parent_Unit_Name (N, I);
         D := Make_Package_Declaration (N);
         Set_Parent (D, M);
         Set_Implementation_Package (P, D);
         Append_Node_To_List (D, L);
      end if;

      return P;
   end Map_IDL_Unit;

   ----------------------------
   -- Map_Members_Definition --
   ----------------------------

   function Map_Members_Definition (Members : List_Id) return List_Id is
      Components  : List_Id;
      Member      : Node_Id;
      Declarator  : Node_Id;
      Member_Type : Node_Id;

   begin
      Components := New_List (K_Component_List);
      Member := First_Entity (Members);
      while Present (Member) loop
         Declarator := First_Entity (Declarators (Member));
         Member_Type := Type_Spec (Member);
         while Present (Declarator) loop
            Append_Node_To_List
              (Make_Component_Declaration
               (Map_Defining_Identifier (FEN.Identifier (Declarator)),
                Map_Declarator_Type_Designator (Member_Type, Declarator)),
               Components);
            Declarator := Next_Entity (Declarator);
         end loop;
         Member := Next_Entity (Member);
      end loop;
      return Components;
   end Map_Members_Definition;

   ---------------------------
   -- Map_Range_Constraints --
   ---------------------------

   function Map_Range_Constraints
     (Array_Sizes : List_Id)
     return List_Id
   is
      L : List_Id;
      S : Node_Id;
      R : Node_Id;
      V : Value_Type;

   begin
      L := New_List (K_Range_Constraints);
      S := FEN.First_Entity (Array_Sizes);
      while Present (S) loop
         R := New_Node (K_Range_Constraint);
         Set_First (R, Int0_Val);
         V := Value (FEN.Value (S));
         V.IVal := V.IVal - 1;
         Set_Last (R, New_Value (V));
         Append_Node_To_List (R, L);
         S := FEN.Next_Entity (S);
      end loop;
      return L;
   end Map_Range_Constraints;

   --------------------------------
   -- Map_Repository_Declaration --
   --------------------------------

   function Map_Repository_Declaration (Entity : Node_Id) return Node_Id is

      procedure Get_Repository_String (Entity : Node_Id);

      ---------------------------
      -- Get_Repository_String --
      ---------------------------

      procedure Get_Repository_String (Entity : Node_Id) is
         I : Node_Id;
         S : Node_Id;

      begin
         I := FEN.Identifier (Entity);
         S := Scope_Entity (I);
         if Present (S)
           and then FEN.Kind (S) /= FEN.K_Specification
         then
            Get_Repository_String (S);
            Add_Char_To_Name_Buffer ('/');
         end if;
         Get_Name_String_And_Append (FEN.IDL_Name (I));
      end Get_Repository_String;


      I : Name_Id;
      V : Value_Id;

   begin
      Name_Len := 0;
      case FEN.Kind (Entity) is
         when FEN.K_Interface_Declaration
           | FEN.K_Module =>
            null;

         when FEN.K_Attribute_Declaration
           | FEN.K_Structure_Type
           | FEN.K_Simple_Declarator
           | FEN.K_Complex_Declarator
           | FEN.K_Enumeration_Type
           | FEN.K_Union_Type =>
            Get_Name_String
              (To_Ada_Name (FEN.IDL_Name (FEN.Identifier (Entity))));
            Add_Char_To_Name_Buffer ('_');

         when others =>
            raise Program_Error;
      end case;
      Add_Str_To_Name_Buffer ("Repository_Id");
      I := Name_Find;
      Set_Str_To_Name_Buffer ("IDL:");
      Get_Repository_String (Entity);
      Add_Str_To_Name_Buffer (":1.0");
      V := New_String_Value (Name_Find, False);
      return Make_Object_Declaration
        (Defining_Identifier => Make_Defining_Identifier (I),
         Constant_Present    => True,
         Object_Definition   => RE (RE_String_2),
         Expression          => Make_Literal (V));
   end Map_Repository_Declaration;

   ----------------------
   -- Map_Variant_List --
   ----------------------

   function Map_Variant_List
     (Alternatives : List_Id)
     return List_Id
   is

      Alternative : Node_Id;
      Variants    : List_Id;
      Variant     : Node_Id;
      Choices     : List_Id;
      Choice      : Node_Id;
      Label       : Node_Id;
      Element     : Node_Id;
      Identifier  : Node_Id;

   begin
      Variants := New_List (K_Variant_List);
      Alternative := First_Entity (Alternatives);
      while Present (Alternative) loop
         Variant := New_Node (K_Variant);
         Choices := New_List (K_Discrete_Choice_List);
         Set_Discrete_Choices (Variant, Choices);
         Label   := First_Entity (Labels (Alternative));
         Element := FEN.Element (Alternative);
         while Present (Label) loop

            --  XXX LP this does not work for enumeration type. We
            --  need a fully qualified notation.

            Choice := Make_Literal (FEN.Value (Label));
            Append_Node_To_List (Choice, Choices);
            Label := Next_Entity (Label);
         end loop;
         Identifier := FEN.Identifier (FEN.Declarator (Element));
         Set_Component
           (Variant,
            Make_Component_Declaration
              (Map_Defining_Identifier (Identifier),
               Map_Declarator_Type_Designator
                 (Type_Spec (Element), Identifier)));
         Append_Node_To_List (Variant, Variants);
         Alternative := Next_Entity (Alternative);
      end loop;
      return Variants;
   end Map_Variant_List;

end Backend.BE_Ada.IDL_To_Ada;
