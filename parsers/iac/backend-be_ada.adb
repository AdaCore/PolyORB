with GNAT.Command_Line; use GNAT.Command_Line;

with Namet;     use Namet;
with Output;    use Output;
with Types;     use Types;
with Values;    use Values;

with Frontend.Nutils;
with Frontend.Nodes;           use Frontend.Nodes;

with Backend.BE_Ada.Debug;     use Backend.BE_Ada.Debug;
with Backend.BE_Ada.Generator; use Backend.BE_Ada.Generator;
with Backend.BE_Ada.Nodes;     use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;    use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;   use Backend.BE_Ada.Runtime;

package body Backend.BE_Ada is

   Print_Ada_Tree : Boolean := False;

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_Ada.Nodes;

   procedure Bind_FE_To_BE (F : Node_Id; B : Node_Id);

   procedure Initialize;

   Getter : constant Character := 'G';
   Setter : constant Character := 'S';

   procedure Visit (E : Node_Id);
   procedure Visit_Attribute_Declaration (E : Node_Id);
   procedure Visit_Constant_Declaration (E : Node_Id);
   procedure Visit_Enumeration_Type (E : Node_Id);
   procedure Visit_Exception_Declaration (E : Node_Id);
   procedure Visit_Interface_Declaration (E : Node_Id);
   procedure Visit_Module (E : Node_Id);
   procedure Visit_Operation_Declaration (E : Node_Id);
   procedure Visit_Specification (E : Node_Id);
   procedure Visit_Structure_Type (E : Node_Id);
   procedure Visit_Type_Declaration (E : Node_Id);
   procedure Visit_Union_Type (E : Node_Id);

   function Make_Accessor_Declaration
     (Accessor  : Character;
      Attribute : Node_Id)
      return      Node_Id;

   function Make_IDL_Unit
     (Entity : Node_Id)
      return   Node_Id;

   function Make_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
      return       Node_Id;

   function Make_Members_Definition
     (Members : List_Id)
      return    List_Id;

   function Make_Package_Declaration
     (Identifier : Node_Id)
      return   Node_Id;

   function Make_Repository_Declaration
     (Entity : Node_Id)
      return   Node_Id;

   function Make_Variant_List
     (Alternatives : List_Id)
      return         List_Id;

   function Marshaller_Body
     (Subp_Spec : Node_Id; Local_Variables : List_Id) return List_Id;
   function Marshaller_Declarations
     (Subp_Spec : Node_Id) return List_Id;

   -------------------
   -- Bind_Together --
   -------------------

   procedure Bind_FE_To_BE (F : Node_Id; B : Node_Id) is
   begin
      if Present (F) then
         Set_BE_Node (F, B);
      end if;
      if Present (B) then
         Set_FE_Node (B, F);
      end if;
   end Bind_FE_To_BE;

   ---------------
   -- Configure --
   ---------------

   procedure Configure is
   begin
      loop
         case Getopt ("t l:") is
            when ASCII.NUL =>
               exit;

            when 't' =>
               Print_Ada_Tree := True;

            when 'l' =>
               Var_Name_Len := Natural'Value (Parameter);

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Configure;

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id) is
   begin
      Initialize;
      Visit_Specification (E);
      if Print_Ada_Tree then
         W_Node_Id (BE_Node (E));
      else
         Generator.Generate (BE_Node (E));
      end if;
   end Generate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Runtime.Initialize;
      Set_Space_Increment (3);
      Int0_Val := New_Integer_Value (0, 1, 10);
      Nutils.Initialize;
   end Initialize;

   ---------------------------------
   -- Make_Accessor_Specification --
   ---------------------------------

   function Make_Accessor_Declaration
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

      --  XXX LP I don't get this.

      if Is_Base_Type (Type_Spec (Declaration (Attribute))) then
         Param_Type := Make_Designator
           (Type_Spec (Declaration (Attribute)));

      else
         Param_Type := Make_Fully_Qualified_Identifier
           (Type_Spec (Declaration (Attribute)));
      end if;

      --  For the setter subprogram, add the second parameter To. T

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
   end Make_Accessor_Declaration;

   -------------------------------------
   -- Make_Declarator_Type_Designator --
   -------------------------------------

   function Make_Declarator_Type_Designator
     (Type_Decl  : Node_Id;
      Declarator : Node_Id)
     return Node_Id
   is
      Designator : Node_Id;
      Decl_Name  : Name_Id;

   begin
      Designator := Make_Designator (Type_Decl);

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
                 (Make_Range_Constraints
                    (FEN.Array_Sizes (Declarator)), Designator)),
            Visible_Part (Current_Package));
         Designator := New_Node (K_Designator);
         Set_Defining_Identifier
           (Designator, Make_Defining_Identifier (Decl_Name));
      end if;
      return Designator;
   end Make_Declarator_Type_Designator;

   -------------------
   -- Make_IDL_Unit --
   -------------------

   function Make_IDL_Unit (Entity : Node_Id) return Node_Id is
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

      I := Make_Fully_Qualified_Identifier (Entity);

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
   end Make_IDL_Unit;

   -----------------------------
   -- Make_Members_Definition --
   -----------------------------

   function Make_Members_Definition (Members : List_Id) return List_Id is
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
               (Make_Defining_Identifier (FEN.Identifier (Declarator)),
                Make_Declarator_Type_Designator (Member_Type, Declarator)),
               Components);
            Declarator := Next_Entity (Declarator);
         end loop;
         Member := Next_Entity (Member);
      end loop;
      return Components;
   end Make_Members_Definition;

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

   ---------------------------------
   -- Make_Repository_Declaration --
   ---------------------------------

   function Make_Repository_Declaration (Entity : Node_Id) return Node_Id is

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
   end Make_Repository_Declaration;

   -----------------------
   -- Make_Variant_List --
   -----------------------

   function Make_Variant_List (Alternatives : List_Id) return List_Id is
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
              (Make_Defining_Identifier (Identifier),
               Make_Declarator_Type_Designator
                 (Type_Spec (Element), Identifier)));
         Append_Node_To_List (Variant, Variants);
         Alternative := Next_Entity (Alternative);
      end loop;
      return Variants;
   end Make_Variant_List;

   ---------------------
   -- Marshaller_Body --
   ---------------------

   function Marshaller_Body
     (Subp_Spec : Node_Id; Local_Variables : List_Id) return List_Id is
      pragma Unreferenced (Local_Variables);
      L     : List_Id;
      N     : Node_Id;
      C     : Node_Id;
      P     : List_Id;
      S     : List_Id;
      Count : Natural;
   begin
      L := New_List (BEN.K_List_Id);

      --  Test if the Self_Ref_U is nil, if it's nil raise exception.
      C := New_Node (BEN.K_Subprogram_Call);
      Set_Defining_Identifier
        (C, RE (RE_Raise_Inv_Objref));
      S := New_List (BEN.K_List_Id);
      Append_Node_To_List
        (Make_Defining_Identifier (VN (V_Def_Sys_Member)), S);
      Set_Actual_Parameter_Part (C, S);
      S := New_List (BEN.K_List_Id);
      Append_Node_To_List (C, S);
      C := New_Node (BEN.K_Subprogram_Call);
      Set_Defining_Identifier
        (C, RE (RE_Is_Nil));
      P := New_List (BEN.K_List_Id);
      Append_Node_To_List
        (Make_Defining_Identifier (VN (V_Self_Ref)), P);
      Set_Actual_Parameter_Part (C, P);
      N := Make_If_Statement (C, S, No_List);
      Append_Node_To_List (N, L);

      --  Create argument list.
      C := New_Node (K_Subprogram_Call);
      Set_Defining_Identifier
        (C, RE (RE_Create));
      P := New_List (BEN.K_List_Id);
      Append_Node_To_List
        (Make_Defining_Identifier (VN (V_Argument_List)), P);
      Set_Actual_Parameter_Part (C, P);
      Append_Node_To_List (C, L);

      Count := Length (Parameter_Profile (Subp_Spec));
      if Count > 1 then
         null; --  Add variables to the parameter list
      end if;

      --  Set result type (maybe void)
      --  --  PolyORB.Types.Identifier (Result_Name)
      C := Make_Subprogram_Call
        (Defining_Identifier   => RE (RE_Identifier),
         Actual_Parameter_Part =>
           Make_List_Id (Make_Defining_Identifier (VN (V_Result_Name))));

      --  -- Name => PolyORB.Types.Identifier (Result_Name)
      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Name)),
         Expression    => C);
      P := Make_List_Id (N);

      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Argument)),
         Expression    => Make_Literal (New_Integer_Value (0, 0, 10)));
      Append_Node_To_List (N, P);

      C := Make_Subprogram_Call
        (Defining_Identifier  => RE (RE_Get_Empty_Any),
         Actual_Parameter_Part =>
           Make_List_Id (Make_Literal (Int0_Val)));
      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Arg_Modes)),
         Expression    => Make_Literal (New_Integer_Value (0, 0, 10)));
      Append_Node_To_List (N, P);

      N := Make_Record_Aggregate (P);

      N := Make_Assignment_Statement
        (Variable_Identifier => Make_Defining_Identifier (VN (V_Result_Name)),
         Expression => N);
      Append_Node_To_List (N, L);
      return L;
   end Marshaller_Body;

   -----------------------------
   -- Marshaller_Declarations --
   -----------------------------

   function Marshaller_Declarations
     (Subp_Spec : Node_Id) return List_Id is
      L : List_Id;
      P : List_Id;
      N : Node_Id;
      V : Value_Id;
      C : Node_Id;
      I : Node_Id;
      X : Name_Id;
      D : Node_Id;
      R : Name_Id;
   begin
      L := New_List (BEN.K_List_Id);

      --  Arg_List_U declaration.
      N := Make_Object_Declaration
        (Defining_Identifier =>
           Make_Defining_Identifier (VN (V_Argument_List)),
         Constant_Present    => False,
         Object_Definition   => RE (RE_Ref_3),
         Expression          => No_Node);
      Append_Node_To_List (N, L);

      P := Parameter_Profile (Subp_Spec);
      I := First_Node (P);
      I := Next_Node (I);
      while Present (I) loop
         --  Arg_Name_U_X declaration
         --  Arg_Name_U_X : PolyORB.Types.Identifier
         --    := PolyORB.Types.To_PolyORB_String ("X")
         --  ** where X is the parameter name.
         X := BEN.Name (Defining_Identifier (I));
         C := Make_Subprogram_Call
           (Defining_Identifier   => RE (RE_To_PolyORB_String),
            Actual_Parameter_Part =>
              Make_List_Id (Make_Literal (New_String_Value (X, False))));

         Set_Str_To_Name_Buffer ("Arg_Name_U_");
         Get_Name_String_And_Append (X);
         R := Name_Find;
         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (R),
            Constant_Present => False,
            Object_Definition => RE (RE_Identifier),
            Expression => C);
         Append_Node_To_List (N, L);

         --  Argument_U_X declaration
         --  Argument_U_X : CORBA.Any := Y.Helper.To_Any (X);
         --  ** where X is the parameter name.
         --  ** where Y is the fully qualified current package Name.
         D := New_Node (K_Designator);

         if BEN.Parameter_Mode (I) = Mode_Out then
            Set_Str_To_Name_Buffer ("To_Any");
            Set_Defining_Identifier
              (D, Make_Defining_Identifier (Name_Find));
            Set_Parent_Unit_Name
              (D, Defining_Identifier (Helper_Package (Current_Entity)));
            C :=  Make_Subprogram_Call
           (Defining_Identifier   => D,
            Actual_Parameter_Part =>
              Make_List_Id (Make_Defining_Identifier (X)));
         else
            null;
         end if;

         Set_Str_To_Name_Buffer ("Argument_U_");
         Get_Name_String_And_Append (X);
         R := Name_Find;
         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (R),
            Constant_Present => False,
            Object_Definition => RE (RE_Any),
            Expression => C);
         Append_Node_To_List (N, L);
         I := Next_Node (I);
      end loop;

      --  Operation_Name_U declaration
      V := New_String_Value
        (BEN.Name (BEN.Defining_Identifier (Subp_Spec)), False);
      N := Make_Object_Declaration
        (Defining_Identifier =>
           Make_Defining_Identifier (VN (V_Operation_Name)),
         Constant_Present    => True,
         Object_Definition   => RE (RE_String_2),
         Expression          => Make_Literal (V));
      Append_Node_To_List (N, L);

      --  Self_Ref_U declaration
      --  Self_Ref_U : CORBA.Object.Ref  := CORBA.Object.Ref (Self);
      C := Make_Subprogram_Call
        (Defining_Identifier   => RE (RE_Ref_2),
         Actual_Parameter_Part =>
           Make_List_Id (Make_Defining_Identifier (PN (P_Self))));

      N := Make_Object_Declaration
        (Defining_Identifier =>
           Make_Defining_Identifier (VN (V_Self_Ref)),
         Constant_Present    => False,
         Object_Definition   => RE (RE_Ref_2),
         Expression          => C);
      Append_Node_To_List (N, L);

      --  Request_U declaration
      N := Make_Object_Declaration
        (Defining_Identifier =>
           Make_Defining_Identifier (VN (V_Request)),
         Constant_Present    => False,
         Object_Definition   => RE (RE_Request_Access),
         Expression          => No_Node);
      Append_Node_To_List (N, L);

      --  Result_U declaration
      --  Result_U : PolyORB.Any.NamedValue;
      N := Make_Object_Declaration
        (Defining_Identifier =>
           Make_Defining_Identifier (VN (V_Result)),
         Constant_Present    => False,
         Object_Definition   => RE (RE_NamedValue),
         Expression          => No_Node);
      Append_Node_To_List (N, L);

      --  Result_Name_U declaration :
      --  Result_Name_U : CORBA.String := CORBA.To_CORBA_String ("Result");

      V := New_String_Value (PN (P_Result), False);
      C := Make_Subprogram_Call
        (Defining_Identifier   => RE (RE_To_CORBA_String),
         Actual_Parameter_Part =>
           Make_List_Id (Make_Literal (V)));

      N := Make_Object_Declaration
        (Defining_Identifier =>
           Make_Defining_Identifier (VN (V_Result_Name)),
         Constant_Present    => False,
         Object_Definition   => RE (RE_String_1),
         Expression          => C);
      Append_Node_To_List (N, L);

      return L;
   end Marshaller_Declarations;

   -----------
   -- Usage --
   -----------

   procedure Usage (Indent : Natural) is
      Hdr : constant String (1 .. Indent - 1) := (others => ' ');
   begin
      Write_Str (Hdr);
      Write_Str ("-t       Dump Ada tree");
      Write_Eol;
   end Usage;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case FEN.Kind (E) is
         when K_Specification =>
            Visit_Specification (E);

         when K_Constant_Declaration =>
            Visit_Constant_Declaration (E);

         when K_Enumeration_Type =>
            Visit_Enumeration_Type (E);

         when K_Exception_Declaration =>
            Visit_Exception_Declaration (E);

         when K_Interface_Declaration =>
            Visit_Interface_Declaration (E);

         when K_Operation_Declaration =>
            Visit_Operation_Declaration (E);

         when K_Structure_Type =>
            Visit_Structure_Type (E);

         when K_Union_Type =>
            Visit_Union_Type (E);

         when K_Attribute_Declaration =>
            Visit_Attribute_Declaration (E);

         when K_Type_Declaration =>
            Visit_Type_Declaration (E);

         when K_Module =>
            Visit_Module (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------------------
   -- Visit_Attribute_Declaration --
   ---------------------------------

   procedure Visit_Attribute_Declaration (E : Node_Id) is
      N : Node_Id;
      A : Node_Id;
      P : Node_Id;
      D : List_Id;
      S : List_Id;

   begin
      A := First_Entity (Declarators (E));
      while Present (A) loop
         Set_Main_Spec;

         Append_Node_To_List
           (Make_Repository_Declaration (A),
            Visible_Part (Current_Package));

         N := Make_Accessor_Declaration
           (Accessor => Getter, Attribute => A);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         Set_Main_Body;
         D := Marshaller_Declarations (N);
         S := Marshaller_Body (N, D);
         P := Make_Subprogram_Implementation (N, D, S);
         Append_Node_To_List (P, Statements (Current_Package));

         if not Is_Readonly (E) then
            Set_Main_Spec;
            N := Make_Accessor_Declaration
              (Accessor => Setter, Attribute => A);
            Append_Node_To_List (N, Visible_Part (Current_Package));

            Set_Main_Body;
            D := Marshaller_Declarations (N);
            S := Marshaller_Body (N, D);
            P := Make_Subprogram_Implementation (N, D, S);
            Append_Node_To_List (P, Statements (Current_Package));
         end if;

         A := Next_Entity (A);
      end loop;
   end Visit_Attribute_Declaration;

   --------------------------------
   -- Visit_Constant_Declaration --
   --------------------------------

   procedure Visit_Constant_Declaration (E : Node_Id) is
      N : Node_Id;

   begin
      Set_Main_Spec;
      N := Make_Object_Declaration
        (Defining_Identifier => Make_Defining_Identifier (E),
         Constant_Present    => True,
         Object_Definition   => Make_Designator (Type_Spec (E)),
         Expression          => Make_Literal (FEN.Value (E)));
      Append_Node_To_List (N, Visible_Part (Current_Package));
   end Visit_Constant_Declaration;

   ----------------------------
   -- Visit_Enumeration_Type --
   ----------------------------

   procedure Visit_Enumeration_Type (E : Node_Id) is
      Enumerator     : Node_Id;
      Enum_Literals  : List_Id;
      Enum_Literal   : Node_Id;
      Enum_Type_Decl : Node_Id;

   begin
      Set_Main_Spec;
      Enum_Literals := New_List (K_Enumeration_Literals);
      Enumerator := First_Entity (Enumerators (E));
      while Present (Enumerator) loop
         Enum_Literal := Make_Defining_Identifier (Enumerator);
         Append_Node_To_List (Enum_Literal, Enum_Literals);
         Enumerator := Next_Entity (Enumerator);
      end loop;

      Enum_Type_Decl :=
        Make_Full_Type_Declaration
          (Make_Defining_Identifier (E),
           Make_Enumeration_Type_Definition (Enum_Literals));

      Set_BE_Node (E, Enum_Type_Decl);
      Append_Node_To_List
        (Enum_Type_Decl,
         Visible_Part (Current_Package));
      Append_Node_To_List
        (Make_Repository_Declaration (E),
         Visible_Part (Current_Package));
   end Visit_Enumeration_Type;

   ---------------------------------
   -- Visit_Exception_Declaration --
   ---------------------------------

   procedure Visit_Exception_Declaration (E : Node_Id) is
      Identifier : Node_Id;
      Profile    : List_Id;
      Parameter  : Node_Id;

   begin
      Set_Main_Spec;
      Append_Node_To_List
        (Make_Exception_Declaration (E),
         Visible_Part (Current_Package));

      Get_Name_String (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
      Add_Str_To_Name_Buffer ("_Members");
      Identifier := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List
        (Make_Full_Type_Declaration
           (Defining_Identifier => Identifier,
            Type_Definition     => Make_Record_Definition
              (Make_Members_Definition (Members (E)))),
         Visible_Part (Current_Package));

      Profile  := New_List (K_Parameter_Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_From)),
         RE (RE_Exception_Occurrence));
      Append_Node_To_List (Parameter, Profile);
      Parameter := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_To)),
         Identifier,
         Mode_Out);
      Append_Node_To_List (Parameter, Profile);

      Append_Node_To_List
        (Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Get_Members)), Profile, No_Node),
         Visible_Part (Current_Package));
   end Visit_Exception_Declaration;

   ---------------------------------
   -- Visit_Interface_Declaration --
   ---------------------------------

   procedure Visit_Interface_Declaration (E : Node_Id) is
      P : Node_Id;
      N : Node_Id;
      L : List_Id;

   begin
      P := Make_IDL_Unit (E);
      Append_Node_To_List (P, Packages (Current_Entity));
      Push_Entity (P);
      Set_Main_Spec;
      L := Interface_Spec (E);
      if FEU.Is_Empty (L) then
         N := RE (RE_Ref_2);
      else
         N := Make_Designator (First_Entity (L));
      end if;
      N :=
        Make_Full_Type_Declaration
        (RE (RE_Ref_0),
         Make_Derived_Type_Definition
             (Subtype_Indication    => N,
              Record_Extension_Part =>
                Make_Record_Type_Definition
                  (Record_Definition => Make_Record_Definition (No_List))));
      Append_Node_To_List
        (N, Visible_Part (Current_Package));
      Append_Node_To_List
        (Make_Repository_Declaration (E), Visible_Part (Current_Package));

      N := First_Entity (Interface_Body (E));
      while Present (N) loop
         Visit (N);
         N := Next_Entity (N);
      end loop;
      Pop_Entity;
   end Visit_Interface_Declaration;

   ------------------
   -- Visit_Module --
   ------------------

   procedure Visit_Module (E : Node_Id) is
      D : Node_Id;
      S : Node_Id;

   begin
      S := Make_IDL_Unit (E);
      Append_Node_To_List (S, Packages (Current_Entity));
      Push_Entity (S);
      D := First_Entity (Definitions (E));
      while Present (D) loop
         Visit (D);
         D := Next_Entity (D);
      end loop;
      Pop_Entity;
   end Visit_Module;

   ---------------------------------
   -- Visit_Operation_Declaration --
   ---------------------------------

   procedure Visit_Operation_Declaration (E : Node_Id) is
      Subp_Body : Node_Id;
      Subp_Spec : Node_Id;
      Profile   : List_Id;
      IDL_Param : Node_Id;
      Ada_Param : Node_Id;
      Mode      : Mode_Id := Mode_In;
      Returns   : Node_Id := No_Node;
      Declarative_Part : List_Id;
      Body_Part  : List_Id;
      Type_Designator : Node_Id;

   begin
      Profile := New_List (K_Parameter_Profile);

      --  Create a dispatching parameter

      Ada_Param := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Self)),
         RE (RE_Ref_0));
      Append_Node_To_List (Ada_Param, Profile);

      --  Create an Ada subprogram parameter for each IDL subprogram
      --  parameter. Check whether there is one inout or out parameter.

      IDL_Param := First_Entity (Parameters (E));
      while Present (IDL_Param) loop
         if Is_Base_Type (Type_Spec (IDL_Param)) then
            Type_Designator := Make_Designator
              (Type_Spec (IDL_Param));
         else
            Type_Designator := Make_Fully_Qualified_Identifier
              (Type_Spec (IDL_Param));
         end if;
         Ada_Param := Make_Parameter_Specification
           (Make_Defining_Identifier (Declarator (IDL_Param)),
            Type_Designator,
            FEN.Parameter_Mode (IDL_Param));
         if FEN.Parameter_Mode (IDL_Param) /= Mode_In then
            Mode := Mode_Out;
         end if;
         Append_Node_To_List (Ada_Param, Profile);
         IDL_Param := Next_Entity (IDL_Param);
      end loop;

      --  If the IDL subprogram is a function, then check whether it
      --  has inout and out parameters. In this case, map the IDL
      --  function as an Ada procedure and not an Ada function.

      if FEN.Kind (Type_Spec (E)) /= K_Void then
         if Mode = Mode_In then
            Returns := Make_Designator (Type_Spec (E));

         --  If the IDL function is mapped as an Ada procedure, add a
         --  new parameter Returns to pass the returned value.

         else
            Ada_Param := Make_Parameter_Specification
              (Make_Defining_Identifier (PN (P_Returns)),
               Make_Designator (Type_Spec (E)),
               Mode_Out);
            Append_Node_To_List (Ada_Param, Profile);
         end if;
      end if;

      --  Add subprogram to main specification

      Set_Main_Spec;
      Subp_Spec := Make_Subprogram_Specification
        (Make_Defining_Identifier (E), Profile, Returns);
      Append_Node_To_List (Subp_Spec, Visible_Part (Current_Package));
      Bind_FE_To_BE (E, Subp_Spec);

      --  Add subprogram to main implementation

      Set_Main_Body;
      Declarative_Part := Marshaller_Declarations (Subp_Spec);
      Body_Part := Marshaller_Body (Subp_Spec, Declarative_Part);
      Subp_Body := Make_Subprogram_Implementation
        (Subp_Spec, Declarative_Part, Body_Part);
      Append_Node_To_List (Subp_Body, Statements (Current_Package));
   end Visit_Operation_Declaration;

   -------------------------
   -- Visit_Specification --
   -------------------------

   procedure Visit_Specification (E : Node_Id) is
      Definition : Node_Id;

   begin
      Push_Entity (Make_IDL_Unit (E));
      Definition := First_Entity (Definitions (E));
      while Present (Definition) loop
         Visit (Definition);
         Definition := Next_Entity (Definition);
      end loop;
      Pop_Entity;
   end Visit_Specification;

   --------------------------
   -- Visit_Structure_Type --
   --------------------------

   procedure Visit_Structure_Type (E : Node_Id) is
      N : Node_Id;

   begin
      Set_Main_Spec;
      N := Make_Full_Type_Declaration
        (Make_Defining_Identifier (E),
         Make_Record_Type_Definition
         (Make_Record_Definition
            (Make_Members_Definition (Members (E)))));
      Bind_FE_To_BE (E, N);
      Append_Node_To_List
        (N, Visible_Part (Current_Package));
      Append_Node_To_List
        (Make_Repository_Declaration (E), Visible_Part (Current_Package));
   end Visit_Structure_Type;

   ----------------------------
   -- Visit_Type_Declaration --
   ----------------------------

   procedure Visit_Type_Declaration (E : Node_Id) is
      D : Node_Id;
      T : Node_Id;
      N : Node_Id;

   begin
      Set_Main_Spec;
      T := Make_Designator (Type_Spec (E));
      D := First_Entity (Declarators (E));
      while Present (D) loop
         if Kind (D) = K_Complex_Declarator then
            N := Make_Full_Type_Declaration
              (Defining_Identifier => Make_Defining_Identifier (D),
               Type_Definition     => Make_Array_Type_Definition
                 (Make_Range_Constraints (FEN.Array_Sizes (D)), T));
         else
            N := Make_Full_Type_Declaration
              (Defining_Identifier => Make_Defining_Identifier (D),
               Type_Definition     => Make_Derived_Type_Definition
                 (Subtype_Indication    => T,
                  Record_Extension_Part => No_Node));
         end if;
         Bind_FE_To_BE (E, N);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Append_Node_To_List
           (Make_Repository_Declaration (D), Visible_Part (Current_Package));
         D := Next_Entity (D);
      end loop;
   end Visit_Type_Declaration;

   ----------------------
   -- Visit_Union_Type --
   ----------------------

   procedure Visit_Union_Type (E : Node_Id) is
      N : Node_Id;
      S : constant Node_Id := Switch_Type_Spec (E);
      T : Node_Id;
      L : List_Id;

   begin
      Set_Main_Spec;
      T := Make_Designator (S);
      L := New_List (K_Component_List);
      Append_Node_To_List
        (Make_Variant_Part
           (Make_Defining_Identifier (CN (C_Switch)),
            Make_Variant_List (Switch_Type_Body (E))),
         L);
      N := Make_Full_Type_Declaration
        (Make_Defining_Identifier (E),
         Make_Record_Type_Definition
           (Make_Record_Definition (L)),
         Make_Component_Declaration
           (Make_Defining_Identifier (CN (C_Switch)), T,
            Make_Type_Attribute (T, A_First)));
      Bind_FE_To_BE (E, N);
      Append_Node_To_List
        (N, Visible_Part (Current_Package));
      Append_Node_To_List
        (Make_Repository_Declaration (E), Visible_Part (Current_Package));
   end Visit_Union_Type;

end Backend.BE_Ada;
