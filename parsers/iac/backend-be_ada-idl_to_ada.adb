with Namet;     use Namet;
with Types;     use Types;
with Values;    use Values;

with Frontend.Nodes;             use Frontend.Nodes;

with Backend.BE_Ada.Nodes;       use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;      use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;     use Backend.BE_Ada.Runtime;
--  with Backend.BE_Ada.Debug;    use Backend.BE_Ada.Debug;

package body Backend.BE_Ada.IDL_To_Ada is

   Setter : constant Character := 'S';

   package BEN renames Backend.BE_Ada.Nodes;

   function Base_Type_TC
     (K : FEN.Node_Kind)
     return Node_Id
   is
   begin
      case K is
         when FEN.K_Float               => return RE (RE_TC_Float);
         when FEN.K_Double              => return RE (RE_TC_Double);
         when FEN.K_Long_Double         => return RE (RE_TC_Long_Double);
         when FEN.K_Short               => return RE (RE_TC_Short);
         when FEN.K_Long                => return RE (RE_TC_Long);
         when FEN.K_Long_Long           => return RE (RE_TC_Long_Long);
         when FEN.K_Unsigned_Short      => return RE (RE_TC_Unsigned_Short);
         when FEN.K_Unsigned_Long       => return RE (RE_TC_Unsigned_Long);
         when FEN.K_Unsigned_Long_Long  => return RE
            (RE_TC_Unsigned_Long_Long);
         when FEN.K_Char                => return RE (RE_TC_Char);
         when FEN.K_Wide_Char           => return RE (RE_TC_WChar);
         when FEN.K_String              => return RE (RE_TC_String);
         when FEN.K_Wide_String         => return RE (RE_TC_Wide_String);
         when FEN.K_Boolean             => return RE (RE_TC_Boolean);
         when FEN.K_Octet               => return RE (RE_TC_Octet);
         when FEN.K_Object              => return RE (RE_TC_Object_0);
         when others                    =>
            raise Program_Error;
      end case;
   end Base_Type_TC;

   ---------------------
   -- Bind_FE_To_Impl --
   ---------------------

   procedure Bind_FE_To_Impl
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Impl_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Impl;

   -----------------------
   -- Bind_FE_To_Helper --
   -----------------------

   procedure Bind_FE_To_Helper
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Helper_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Helper;

   ---------------------
   -- Bind_FE_To_Skel --
   ---------------------

   procedure Bind_FE_To_Skel
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Skel_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Skel;

   ---------------------
   -- Bind_FE_To_Stub --
   ---------------------

   procedure Bind_FE_To_Stub
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Stub_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Stub;

   -------------------
   -- Bind_FE_To_TC --
   -------------------

   procedure Bind_FE_To_TC
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_TC_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_TC;

   -------------------------
   -- Bind_FE_To_From_Any --
   -------------------------

   procedure Bind_FE_To_From_Any
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_From_Any_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_From_Any;

   -----------------------
   -- Bind_FE_To_To_Any --
   -----------------------

   procedure Bind_FE_To_To_Any
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_To_Any_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_To_Any;

   -----------------------
   -- Bind_FE_To_To_Ref --
   -----------------------

   procedure Bind_FE_To_To_Ref
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_To_Ref_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_To_Ref;

   -------------------------
   -- Bind_FE_To_U_To_Ref --
   -------------------------

   procedure Bind_FE_To_U_To_Ref
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_U_To_Ref_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_U_To_Ref;

   -------------------------
   -- Bind_FE_To_Type_Def --
   -------------------------

   procedure Bind_FE_To_Type_Def
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Type_Def_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Type_Def;

   ------------------------
   -- Bind_FE_To_Forward --
   ------------------------

   procedure Bind_FE_To_Forward
     (F : Node_Id;
      B : Node_Id)
   is
      N : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      BEN.Set_Forward_Node (N, B);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (B, F);
   end Bind_FE_To_Forward;

   -------------------------------
   -- Bind_FE_To_Instanciations --
   -------------------------------

   procedure Bind_FE_To_Instanciations
     (F : Node_Id;
      Stub_Package_Node : Node_Id := No_Node;
      Stub_Type_Node : Node_Id := No_Node;
      Helper_Package_Node : Node_Id := No_Node;
      TC_Node : Node_Id := No_Node;
      From_Any_Node : Node_Id := No_Node;
      To_Any_Node : Node_Id := No_Node)
   is
      N : Node_Id;
      I : Node_Id;
   begin
      N := BE_Node (F);

      if No (N) then
         N := New_Node (BEN.K_BE_Ada);
      end if;

      I := BE_Ada_Instanciations (N);

      if No (I) then
         I := New_Node (BEN.K_BE_Ada_Instanciations);
      end if;

      if Present (Stub_Package_Node) then
         Set_Stub_Package_Node (I, Stub_Package_Node);
      end if;

      if Present (Stub_Type_Node) then
         Set_Stub_Type_Node (I, Stub_Type_Node);
      end if;

      if Present (Helper_Package_Node) then
         Set_Helper_Package_Node (I, Helper_Package_Node);
      end if;

      if Present (TC_Node) then
         Set_TC_Node (I, TC_Node);
      end if;

      if Present (From_Any_Node) then
         Set_From_Any_Node (I, From_Any_Node);
      end if;

      if Present (To_Any_Node) then
         Set_To_Any_Node (I, To_Any_Node);
      end if;

      BEN.Set_BE_Ada_Instanciations (N, I);
      FEN.Set_BE_Node (F, N);
      BEN.Set_FE_Node (I, F);
   end Bind_FE_To_Instanciations;

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

   ----------------------
   -- Is_N_Parent_Of_M --
   ----------------------

   function Is_N_Parent_Of_M
     (N : Node_Id;
      M : Node_Id)
     return Boolean
   is
      X : Node_Id := N;
      Y : Node_Id := M;
   begin
      if No (Y) then
         return False;
      else
         if FEN.Kind (X) = K_Identifier then
            X := Corresponding_Entity (X);
         end if;

         if FEN.Kind (Y) = K_Identifier then
            Y := Corresponding_Entity (Y);
         end if;

         if X = Y then
            return True;
         elsif FEN.Kind (Scope_Entity (Identifier (Y))) = K_Specification then
            return False;
         else
            return Is_N_Parent_Of_M (X, Scope_Entity (Identifier (Y)));
         end if;
      end if;
   end Is_N_Parent_Of_M;

   -------------------
   -- Link_BE_To_FE --
   -------------------

   procedure Link_BE_To_FE
     (BE : Node_Id;
      FE : Node_Id)
   is
   begin
      Set_FE_Node (BE, FE);
   end Link_BE_To_FE;

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
      Result     : Node_Id;
   begin
      Parameters := New_List (K_Parameter_Profile);

      --  Add the dispatching parameter to the parameter profile

      Parameter  := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Self)), RE (RE_Ref_0));
      Append_Node_To_List (Parameter, Parameters);

      Param_Type := Map_Designator
        (Type_Spec (Declaration (Attribute)));
      Set_FE_Node (Param_Type, Type_Spec (Declaration (Attribute)));

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
      Result := Make_Subprogram_Specification
        (Make_Defining_Identifier (Name_Find), Parameters, Param_Type);

      --  Link the generated node with the Frontend node

      Link_BE_To_FE (Result, Identifier (Attribute));
      return Result;
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
      Type_Node  : Node_Id;
   begin
      Designator := Map_Designator (Type_Decl);

      --  When the declarator is complex, the component type is an
      --  array type.

      if Kind (Declarator) = K_Complex_Declarator then
         Decl_Name := To_Ada_Name (IDL_Name (FEN.Identifier (Declarator)));
         Get_Name_String (Decl_Name);
         Add_Str_To_Name_Buffer ("_Array");
         Decl_Name := Name_Find;
         Type_Node := Make_Full_Type_Declaration
           (Defining_Identifier => Make_Defining_Identifier (Decl_Name),
            Type_Definition     => Make_Array_Type_Definition
            (Map_Range_Constraints
             (FEN.Array_Sizes (Declarator)), Designator));
         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (Type_Node),
            (Defining_Identifier
             (Main_Package
              (Current_Entity))));
         --  We make a link between the identifier and the type declaration.
         --  This link is useful for the generation of the From_Any and To_Any
         --  functions and the TC_XXX constant necessary for user defined
         --  types.
         Bind_FE_To_Type_Def (FEN.Identifier (Declarator), Type_Node);
         Append_Node_To_List
           (Type_Node,
            Visible_Part (Current_Package));
         Designator := New_Node (K_Designator);
         Set_Defining_Identifier
           (Designator, Defining_Identifier (Type_Node));
         Set_Correct_Parent_Unit_Name
           (Designator,
            (Defining_Identifier
             (Main_Package
              (Current_Entity))));
      end if;

      return Designator;
   end Map_Declarator_Type_Designator;

   -----------------------------
   -- Map_Defining_Identifier --
   -----------------------------

   function Map_Defining_Identifier (Entity : Node_Id) return Node_Id is
      use FEN;

      I      : Node_Id := Entity;
      Result : Node_Id;

   begin
      if FEN.Kind (Entity) /= FEN.K_Identifier then
         I := FEN.Identifier (Entity);
      end if;

      Result := Make_Defining_Identifier (IDL_Name (I));
      if Present (BE_Node (I))
        and then Present (Stub_Node (BE_Node (I)))
        and then BEN.Kind (Stub_Node (BE_Node (I))) = K_IDL_Unit
      then
         Set_Corresponding_Node
           (Result, Main_Package (Stub_Node (BE_Node (I))));
      end if;

      return Result;
   end Map_Defining_Identifier;

   --------------------
   -- Map_Designator --
   --------------------

   function Map_Designator
     (Entity : Node_Id)
     return Node_Id
   is
      use FEN;
      P : Node_Id;
      N : Node_Id;
      K : FEN.Node_Kind;
      R : Node_Id;
      B : Node_Id;
      Ref_Type_Node : Node_Id;
   begin
      K := FEN.Kind (Entity);

      if K = FEN.K_Scoped_Name then
         R := Reference (Entity);

         if Kind (R) = K_Specification then
            return No_Node;
         end if;

         --  Handling the case where R is not a base type nor a user defined
         --  type but an Interface type :
         --  interface myType {...}
         --  In this case we do not return the identifier of the interface name
         --  but the identifier to the Ref type defined in the stub package
         --  relative to the interface.

         N := New_Node (K_Designator);
         if Kind (R) = FEN.K_Interface_Declaration then
            --  Getting the node of the Ref type declaration.
            Ref_Type_Node := Type_Def_Node (BE_Node (Identifier (R)));
            Set_Defining_Identifier
              (N, --  Defining_Identifier (Ref_Type_Node));
               Copy_Node (Defining_Identifier (Ref_Type_Node)));
            Set_FE_Node (N, R);
            P := R;
         elsif Kind (R) = FEN.K_Forward_Interface_Declaration then
            --  Getting the node of the Ref type declaration.
            Ref_Type_Node := Type_Def_Node (BE_Node (Identifier (R)));
            Set_Defining_Identifier
              (N,
               Copy_Node (Defining_Identifier (Ref_Type_Node)));
            Set_FE_Node (N, R);

            Set_Correct_Parent_Unit_Name
              (N,
               Defining_Identifier
               (Stub_Package_Node
                (BE_Ada_Instanciations
                 (BE_Node
                  (Identifier
                   (R))))));
            P := No_Node;
         else
            Set_Defining_Identifier (N, Map_Defining_Identifier (R));
            Set_FE_Node (N, R);
            P := Scope_Entity (Identifier (R));
         end if;

         if Present (P) then
            if Kind (P) = K_Specification then
               B := Defining_Identifier_To_Designator
                 (Defining_Identifier
                  (Main_Package
                   (Stub_Node
                    (BE_Node
                     (Identifier
                      (P))))));
               Set_FE_Node (B, P);
               Set_Correct_Parent_Unit_Name
                 (N, B);
            else
               Set_Correct_Parent_Unit_Name (N, Map_Designator (P));
            end if;
         end if;

      elsif K in FEN.K_Float .. FEN.K_Value_Base then
         N := RE (Convert (K));
         Set_FE_Node (N, Entity);

      else
         N := New_Node (K_Designator);
         Set_Defining_Identifier (N, Map_Defining_Identifier (Entity));

         if K = FEN.K_Interface_Declaration
           or else K = FEN.K_Module
         then
            P := Scope_Entity (Identifier (Entity));
            Set_FE_Node (N, Entity);
            Set_Correct_Parent_Unit_Name (N, Map_Designator (P));

         elsif K = FEN.K_Specification then
            return No_Node;
         end if;
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

         Set_Correct_Parent_Unit_Name (N, Map_Fully_Qualified_Identifier (P));
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
      P := New_Node (K_IDL_Unit, Identifier (Entity));
      L := New_List (K_Packages);
      Set_Packages (P, L);
      I := Map_Fully_Qualified_Identifier (Entity);

      --  Main package

      M := Make_Package_Declaration (I);
      Set_IDL_Unit (M, P);
      Set_Main_Package (P, M);
      Append_Node_To_List (M, L);

      --  Helper package

      Set_Str_To_Name_Buffer ("Helper");
      N := Make_Defining_Identifier (Name_Find);
      Set_Correct_Parent_Unit_Name (N, I);
      D := Make_Package_Declaration (N);
      Set_IDL_Unit (D, P);
      Set_Parent (D, M);
      Set_Helper_Package (P, D);
      Append_Node_To_List (D, L);

      if Kind (Entity) = K_Interface_Declaration then

         if not FEN.Is_Abstract_Interface (Entity) then
            --  No skel or impl packages are generated for
            --  abstract interfaces.

            --  Skeleton package

            Set_Str_To_Name_Buffer ("Skel");
            N := Make_Defining_Identifier (Name_Find);
            Set_Correct_Parent_Unit_Name (N, I);
            D := Make_Package_Declaration (N);
            Set_IDL_Unit (D, P);
            Set_Parent (D, M);
            Set_Skeleton_Package (P, D);
            Append_Node_To_List (D, L);

            --  Implementation package

            Set_Str_To_Name_Buffer ("Impl");
            N := Make_Defining_Identifier (Name_Find);
            Set_Correct_Parent_Unit_Name (N, I);
            D := Make_Package_Declaration (N);
            Set_IDL_Unit (D, P);
            Set_Parent (D, M);
            Set_Implementation_Package (P, D);

            if Impl_Packages_Gen then
               Append_Node_To_List (D, L);
            end if;
         end if;
      end if;

      return P;
   end Map_IDL_Unit;

   ----------------------------
   -- Map_Members_Definition --
   ----------------------------

   function Map_Members_Definition (Members : List_Id) return List_Id is
      Components            : List_Id;
      Member                : Node_Id;
      Declarator            : Node_Id;
      Member_Type           : Node_Id;
      Component_Declaration : Node_Id;
   begin
      Components := New_List (K_Component_List);
      Member := First_Entity (Members);
      while Present (Member) loop
         Declarator := First_Entity (Declarators (Member));
         Member_Type := Type_Spec (Member);
         while Present (Declarator) loop
            Component_Declaration := Make_Component_Declaration
              (Map_Defining_Identifier (FEN.Identifier (Declarator)),
               Map_Declarator_Type_Designator (Member_Type, Declarator));
            Bind_FE_To_Stub
              (Identifier (Declarator), Component_Declaration);
            Append_Node_To_List
              (Component_Declaration, Components);
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
         --  The range constraints may be :
         --  * Literal values
         --  * Previously declared constants (concretly, scoped names)
         R := New_Node (K_Range_Constraint);
         Set_First (R, Int0_Val);
         if FEN.Kind (S) = K_Scoped_Name then
            V := Value (FEN.Value (Reference (S)));
            V.IVal := V.IVal - 1;
         else
            V := Value (FEN.Value (S));
            V.IVal := V.IVal - 1;
         end if;
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
           | FEN.K_Exception_Declaration
           | FEN.K_Operation_Declaration
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
     (Alternatives   : List_Id;
      Literal_Parent : Node_Id := No_Node)
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

            Choice := Make_Literal
              (Value             => FEN.Value (Label),
               Parent_Designator => Literal_Parent);
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
