with Namet;     use Namet;
with Values;     use Values;

with Frontend.Nodes;   use Frontend.Nodes;
--  with Frontend.Debug;

with Backend.BE_Ada.Expand;  use Backend.BE_Ada.Expand;
with Backend.BE_Ada.IDL_To_Ada;  use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime; use Backend.BE_Ada.Runtime;
--  with Backend.BE_Ada.Debug;

package body Backend.BE_Ada.Helpers is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;

   package body Package_Spec is

      function From_Any_Spec
        (E : Node_Id)
        return Node_Id;

      function To_Any_Spec
        (E : Node_Id)
        return Node_Id;
      --  return an any conversions functions for a given type (E).

      function Narrowing_Ref_Spec
        (E : Node_Id)
        return Node_Id;
      --  return windening object reference helper.

      function TypeCode_Spec
        (E : Node_Id)
        return Node_Id;
      --  return a TypeCode constant for a given type (E).

      function Widening_Ref_Spec
        (E : Node_Id)
        return Node_Id;
      --  return widening object reference helper.

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);

      -------------------
      -- From_Any_Spec --
      -------------------

      function From_Any_Spec
        (E : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)),
            RE (RE_Any));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_From_Any)),
            Profile,
            Expand_Designator (Stub_Node (BE_Node (Identifier (E)))));
         Set_FE_Node (N, Identifier (E));
         return N;
      end From_Any_Spec;

      ------------------------
      -- Narrowing_Ref_Spec --
      ------------------------

      function Narrowing_Ref_Spec
        (E : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         N := Subtype_Indication
           (Type_Definition (Stub_Node (BE_Node (Identifier (E)))));
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_The_Ref)),
            Make_Type_Attribute
            (Copy_Designator (N), A_CLASS));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Unchecked_To_Ref)),
            Profile, Expand_Designator
            (Stub_Node (BE_Node (Identifier (E)))));
         Set_FE_Node (N, Identifier (E));
         return N;
      end Narrowing_Ref_Spec;

      -----------------
      -- To_Any_Spec --
      -----------------

      function To_Any_Spec
        (E : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)),
            Expand_Designator (Stub_Node (BE_Node (Identifier (E)))));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Any)),
            Profile, RE (RE_Any));
         Set_FE_Node (N, Identifier (E));
         return N;
      end To_Any_Spec;

      -------------------
      -- TypeCode_Spec --
      -------------------

      function TypeCode_Spec
        (E : Node_Id)
        return Node_Id
      is
         N  : Node_Id;
         C  : Node_Id;
         TC : Name_Id;
         P  : Node_Id;
         T  : Node_Id;
      begin
         N := Stub_Node (BE_Node (Identifier (E)));
         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               P := RE (RE_TC_Enum);

            when K_Interface_Declaration =>
               N := Package_Declaration
                 (BEN.Parent (Stub_Node (BE_Node (Identifier (E)))));
               P := RE (RE_TC_Object);

            when  K_Simple_Declarator =>
               T := Type_Spec
                 (Declaration (E));

               if Is_Base_Type (T) then
                  P := RE (RE_TC_Alias);
               elsif Kind (T) = K_Scoped_Name then
                  P := Reference (T);
                  P := Helper_Node (BE_Node (Identifier (P)));
                  P := Copy_Designator
                    (First_Node
                     (Actual_Parameter_Part
                      (BEN.Expression (P))));
               else
                  raise Program_Error;
               end if;

            when K_Complex_Declarator =>
               P := RE (RE_TC_Array);

            when K_Structure_Type =>
               P := RE (RE_TC_Struct);

            when K_Union_Type =>
               null;

            when others =>
               raise Program_Error;
         end case;
         TC := Add_Prefix_To_Name ("TC_", BEN.Name (Defining_Identifier (N)));
         C := Make_Subprogram_Call
           (Defining_Identifier   => RE (RE_To_CORBA_Object),
            Actual_Parameter_Part => Make_List_Id (P));
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (TC),
            Constant_Present    => False,
            Object_Definition   => RE (RE_Object),
            Expression          => C);
         Set_FE_Node (N, Identifier (E));
         return N;
      end TypeCode_Spec;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Specification =>
               Visit_Specification (E);

            when K_Structure_Type =>
               Visit_Structure_Type (E);

            when K_Type_Declaration =>
               Visit_Type_Declaration (E);

            when K_Union_Type =>
               Visit_Union_Type (E);

            when others =>
               null;
         end case;
      end Visit;

      ----------------------------
      -- Visit_Enumeration_Type --
      ----------------------------

      procedure Visit_Enumeration_Type (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Helper_Spec;
         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Append_Node_To_List
           (From_Any_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (To_Any_Spec (E), Visible_Part (Current_Package));
      end Visit_Enumeration_Type;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Stub_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Helper_Spec;
         N := Widening_Ref_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Append_Node_To_List
           (Narrowing_Ref_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (TypeCode_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (From_Any_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (To_Any_Spec (E), Visible_Part (Current_Package));
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
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         D := First_Entity (Definitions (E));
         while Present (D) loop
            Visit (D);
            D := Next_Entity (D);
         end loop;
         Pop_Entity;
      end Visit_Module;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
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
         Set_Helper_Spec;
         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Append_Node_To_List
           (From_Any_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (To_Any_Spec (E), Visible_Part (Current_Package));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         L : List_Id;
         D : Node_Id;
         N : Node_Id;
      begin
         Set_Helper_Spec;
         L := Declarators (E);
         D := First_Entity (L);
         while Present (D) loop
            N := TypeCode_Spec (D);
            Append_Node_To_List
              (N, Visible_Part (Current_Package));
            Bind_FE_To_Helper (Identifier (D), N);
            Append_Node_To_List
              (From_Any_Spec (D), Visible_Part (Current_Package));
            Append_Node_To_List
              (To_Any_Spec (D), Visible_Part (Current_Package));
            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Helper_Spec;
         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Append_Node_To_List
           (From_Any_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (To_Any_Spec (E), Visible_Part (Current_Package));
      end Visit_Union_Type;

      -----------------------
      -- Widening_Ref_Spec --
      -----------------------

      function Widening_Ref_Spec
        (E : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         N := Subtype_Indication
           (Type_Definition (Stub_Node (BE_Node (Identifier (E)))));
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_The_Ref)),
            Make_Type_Attribute
            (Copy_Designator (N), A_CLASS));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Ref)), Profile,
            Expand_Designator
            (Stub_Node (BE_Node (Identifier (E)))));
         return N;
      end Widening_Ref_Spec;

   end Package_Spec;

   package body Package_Body is

      Deferred_Initialization_Body : List_Id;
      Package_Initializarion       : List_Id;

      function Deferred_Initialization_Block
        (E : Node_Id)
         return Node_Id;
      function Img (N : Integer) return String;

      function From_Any_Body
        (E : Node_Id)
        return Node_Id;

      procedure Helper_Initialization (L : List_Id);

      function To_Any_Body
        (E : Node_Id)
        return Node_Id;
      --  returns an any conversions functions for a given type
      --  (E) node.

      function Narrowing_Ref_Body
        (E : Node_Id)
        return Node_Id;
      --  return windening object reference helper.

      function Widening_Ref_Body
        (E : Node_Id)
        return Node_Id;
      --  return widening object reference helper.

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);

      -----------------------------------
      -- Deferred_Initialization_Block --
      -----------------------------------

      function Deferred_Initialization_Block
        (E : Node_Id)
         return Node_Id
      is
         function Add_Parameter
           (TC_Name  : Name_Id;
            Var_Name : Name_Id)
            return Node_Id;

         function Declare_Name
           (Var_Name  : Name_Id;
            Value : Value_Id)
            return Node_Id;

         -------------------
         -- Add_Parameter --
         -------------------

         function Add_Parameter
           (TC_Name  : Name_Id;
            Var_Name : Name_Id)
            return Node_Id
         is
            N : Node_Id;
         begin
            N := Make_Subprogram_Call
              (RE (RE_To_Any_0),
               Make_List_Id (Make_Designator (Var_Name)));
            N := Make_Subprogram_Call
              (RE (RE_Add_Parameter),
               Make_List_Id
               (Make_Designator (TC_Name),
                N));
            return N;
         end Add_Parameter;

         ------------------
         -- Declare_Name --
         ------------------

         function Declare_Name
           (Var_Name  : Name_Id;
            Value : Value_Id)
            return Node_Id
         is
            N : Node_Id;
         begin
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (Var_Name),
               Object_Definition   => RE (RE_String_0),
               Expression          =>
                 Make_Subprogram_Call
               (RE (RE_To_CORBA_String),
                Make_List_Id (Make_Literal (Value))));
            return N;
         end Declare_Name;

         Stub             : Node_Id;
         Helper           : Node_Id;
         N                : Node_Id;
         Entity_TC_Name   : Name_Id;
         Entity_Name_V    : Value_Id;
         Entity_Rep_Id_V  : Value_Id;
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
      begin
         Stub :=  Stub_Node (BE_Node (Identifier (E)));
         Entity_Rep_Id_V := BEN.Value (BEN.Expression (Next_Node (Stub)));
         Helper := Helper_Node (BE_Node (Identifier (E)));

         case FEN.Kind (E) is
            when K_Interface_Declaration =>
               Stub := Package_Declaration
                 (BEN.Parent (Stub));
               Helper := Next_Node (Next_Node (Helper));

            when others =>
               null;
         end case;

         --  Name_U declaration


         Entity_Name_V := New_String_Value
           (BEN.Name (Defining_Identifier (Stub)), False);
         Entity_TC_Name := BEN.Name (Defining_Identifier (Helper));

         N := Declare_Name (VN (V_Name), Entity_Name_V);
         Append_Node_To_List (N, Declarative_Part);

         --  Id_U declaration
         N := Declare_Name (VN (V_Id), Entity_Rep_Id_V);
         Append_Node_To_List (N, Declarative_Part);

         N := Add_Parameter (Entity_TC_Name, VN (V_Name));
         Append_Node_To_List (N, Statements);
         N := Add_Parameter (Entity_TC_Name, VN (V_Id));
         Append_Node_To_List (N, Statements);

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               declare
                  Enumerators : constant List_Id :=
                    Enumeration_Literals (Stub);
                  Enum_Item   : Node_Id := First_Node (Enumerators);
                  Var_Name    : Name_Id;
               begin
                  loop
                     Var_Name := Add_Prefix_To_Name
                        (Image (BEN.Value (Enum_Item)), VN (V_Name));
                     N := Declare_Name (Var_Name, BEN.Value (Enum_Item));
                     Append_Node_To_List (N, Declarative_Part);
                     N := Add_Parameter (Entity_TC_Name, Var_Name);
                     Append_Node_To_List (N, Statements);
                     Enum_Item := Next_Node (Enum_Item);
                     exit when No (Enum_Item);
                  end loop;
               end;

            when others =>
               null;
         end case;

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         return N;
      end Deferred_Initialization_Block;

      ---------
      -- Img --
      ---------

      function Img (N : Integer) return String is
         S : constant String := Integer'Image (N);
      begin
         if S (S'First) = ' ' then
            return S (S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Img;

      -------------------
      -- From_Any_Body --
      -------------------

      function From_Any_Body
        (E : Node_Id)
        return Node_Id
      is
         N           : Node_Id;
         Spec        : Node_Id;
         D           : constant List_Id := New_List (K_List_Id);
         S           : constant List_Id := New_List (K_List_Id);
         function Complex_Declarator_Body (E : Node_Id) return Node_Id;
         function Interface_Declaration_Body (E : Node_Id) return Node_Id;
         function Simple_Declarator_Body (E : Node_Id) return Node_Id;
         function Structure_Type_Body (E : Node_Id) return Node_Id;
         function Union_Type_Body (E : Node_Id) return Node_Id;

         -----------------------------
         -- Complex_Declarator_Body --
         -----------------------------

         function Complex_Declarator_Body (E : Node_Id) return Node_Id is
            pragma Unreferenced (E);
         begin
            return No_Node;
         end Complex_Declarator_Body;

         --------------------------------
         -- Interface_Declaration_Body --
         --------------------------------

         function Interface_Declaration_Body (E : Node_Id) return Node_Id is
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Spec);  --  Second in the list of helpers
            Spec := Next_Node (Next_Node (Spec));
            N := Make_Subprogram_Call
              (Make_Defining_Identifier (SN (S_To_Ref)),
               Make_List_Id
               (Make_Subprogram_Call
                (RE (RE_From_Any_1),
                 Make_List_Id (Make_Defining_Identifier (PN (P_Item))))));
            N := Make_Return_Statement (N);
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Interface_Declaration_Body;

         ----------------------------
         -- Simple_Declarator_Body --
         ----------------------------

         function Simple_Declarator_Body (E : Node_Id) return Node_Id is
            pragma Unreferenced (E);
         begin
            return No_Node;
         end Simple_Declarator_Body;

         -------------------------
         -- Structure_Type_Body --
         -------------------------

         function Structure_Type_Body (E : Node_Id) return Node_Id is
            pragma Unreferenced (E);
         begin
            return No_Node;
         end Structure_Type_Body;

         ----------------
         -- Union_Body --
         ----------------

         function Union_Type_Body (E : Node_Id) return Node_Id is
            pragma Unreferenced (E);
         begin
            return No_Node;
         end Union_Type_Body;

      begin
         case FEN.Kind (E) is
            when K_Complex_Declarator =>
               N := Complex_Declarator_Body (E);

            when K_Enumeration_Type =>
               N := No_Node;

            when K_Interface_Declaration =>
               N := Interface_Declaration_Body (E);

            when K_Simple_Declarator =>
               N := Simple_Declarator_Body (E);

            when K_Structure_Type =>
               N := Structure_Type_Body (E);

            when K_Union_Type =>
               N := Union_Type_Body (E);
            when others =>
               raise Program_Error;
         end case;
         return N;

      end From_Any_Body;

      ---------------------------
      -- Helper_Initialization --
      ---------------------------

      procedure Helper_Initialization (L : List_Id) is
         N         : Node_Id;
         V         : Value_Id;
         Aggregates : List_Id;
      begin
         Aggregates := New_List (K_List_Id);
         N := Defining_Identifier
           (Package_Declaration (Current_Package));
         V := New_String_Value
           (Fully_Qualified_Name (N), False);
         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Name)),
            Expression          =>
              Make_Literal (V));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Conflicts)),
            Expression          =>
              RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Depends)),
            Expression          =>
              RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Provides)),
            Expression          =>
              RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Implicit)),
            Expression          =>
              RE (RE_False));
         Append_Node_To_List (N, Aggregates);

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Init)),
            Expression          =>
              Make_Type_Attribute
            (Make_Designator (SN (S_Deferred_Initialization)),
             A_Access));
         Append_Node_To_List (N, Aggregates);

         N := Make_Record_Aggregate
           (Aggregates);

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Aggregate    => N);

         N := Make_Subprogram_Call
           (RE (RE_Register_Module),
            Make_List_Id (N));
         Append_Node_To_List (N, L);
      end Helper_Initialization;

      -----------------
      -- To_Any_Body --
      -----------------

      function To_Any_Body
        (E : Node_Id)
        return Node_Id
      is
         Spec        : Node_Id;
         D           : constant List_Id := New_List (K_List_Id);
         S           : constant List_Id := New_List (K_List_Id);
         N           : Node_Id;
         M           : Node_Id;
         Helper_Name : Name_Id;
         function Complex_Declarator_Body (E : Node_Id) return Node_Id;
         function Enumeration_Type_Body (E : Node_Id) return Node_Id;
         function Interface_Declaration_Body (E : Node_Id) return Node_Id;
         function Simple_Declarator_Body (E : Node_Id) return Node_Id;
         function Structure_Type_Body (E : Node_Id) return Node_Id;
         function Union_Type_Body (E : Node_Id) return Node_Id;

         -----------------------------
         -- Complex_Declarator_Body --
         -----------------------------

         function Complex_Declarator_Body (E : Node_Id) return Node_Id is
            I                : Integer := 0;
            L                : List_Id;
            Sizes            : constant List_Id :=
              Range_Constraints
              (Type_Definition (Stub_Node (BE_Node (Identifier (E)))));
            Dim              : Node_Id;
            Loop_Statements  : List_Id := No_List;
            Enclosing_Statements : List_Id;
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Next_Node (Spec));
            N := RE (RE_Get_Empty_Any_Aggregate);
            Helper_Name := BEN.Name
              (Defining_Identifier (Helper_Node (BE_Node (Identifier (E)))));
            N := Make_Subprogram_Call
              (N,
               Make_List_Id (Make_Defining_Identifier (Helper_Name)));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => RE (RE_Any),
               Expression => N);
            Append_Node_To_List (N, D);
            L := New_List (K_List_Id);

            Dim := First_Node (Sizes);
            loop
               Set_Str_To_Name_Buffer ("I");
               Add_Str_To_Name_Buffer (Img (I));
               M := Make_Defining_Identifier
                 (Add_Suffix_To_Name (Var_Suffix, Name_Find));
               Append_Node_To_List (M, L);
               Enclosing_Statements := Loop_Statements;
               Loop_Statements := New_List (K_List_Id);
               N := Make_For_Statement
                 (M, Dim, Loop_Statements);

               if I > 0 then
                  Append_Node_To_List (N, Enclosing_Statements);
               else
                  Append_Node_To_List (N, S);
               end if;

               I := I + 1;
               Dim := Next_Node (Dim);
               exit when No (Dim);
            end loop;
            N := Make_Subprogram_Call
              (Make_Defining_Identifier (Helper_Name), L);
            N := Make_Subprogram_Call
              (RE (RE_Add_Aggregate_Element),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Result)), N));
            Append_Node_To_List (N, Loop_Statements);
            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Complex_Declarator_Body;

         ---------------------------
         -- Enumeration_Type_Body --
         ---------------------------

         function Enumeration_Type_Body (E : Node_Id) return Node_Id is
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Next_Node (Spec));
            N := RE (RE_Get_Empty_Any_Aggregate);
            Helper_Name := BEN.Name
              (Defining_Identifier (Helper_Node (BE_Node (Identifier (E)))));
            N := Make_Subprogram_Call
              (N,
               Make_List_Id (Make_Defining_Identifier (Helper_Name)));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => RE (RE_Any),
               Expression => N);
            Append_Node_To_List (N, D);
            N := Make_Subprogram_Call
              (Make_Type_Attribute (Map_Designator (E), A_Pos),
               Make_List_Id (Make_Defining_Identifier (PN (P_Item))));
            N := Make_Subprogram_Call
              (RE (Convert (K_Unsigned_Long)),
               Make_List_Id (N));
            N := Make_Subprogram_Call
              (RE (RE_Any),
               Make_List_Id (N));
            N := Make_Subprogram_Call
              (RE (RE_Add_Aggregate_Element),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Result)), N));
            Append_Node_To_List (N, S);
            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Enumeration_Type_Body;

         --------------------------------
         -- Interface_Declaration_Body --
         --------------------------------

         function Interface_Declaration_Body (E : Node_Id) return Node_Id is
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Next_Node (Spec));
            Spec := (Next_Node (Next_Node (Spec)));
            Helper_Name := BEN.Name (Defining_Identifier (Spec));
            N := Make_Subprogram_Call
              (RE (RE_Ref_2),
               Make_List_Id (Make_Defining_Identifier (PN (P_Item))));
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_A)),
               Object_Definition => RE (RE_Any),
               Expression => Make_Subprogram_Call
               (RE (RE_To_Any_3), Make_List_Id (N)));
            Append_Node_To_List (N, D);
            N := Make_Subprogram_Call
              (RE (RE_Set_Type),
               Make_List_Id (Make_Defining_Identifier (PN (P_A)),
                             Make_Defining_Identifier (Helper_Name)));
            Append_Node_To_List (N, S);
            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_A)));
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Interface_Declaration_Body;

         ----------------------------
         -- Simple_Declarator_Body --
         ----------------------------

         function Simple_Declarator_Body (E : Node_Id) return Node_Id is
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Next_Node (Spec));

            if Is_Base_Type (Type_Spec (Declaration (E))) then
               N := RE (Convert
                        (FEN.Kind (FEN.Type_Spec (FEN.Declaration (E)))));
               M := RE (RE_To_Any_0);
            elsif Kind (Type_Spec (Declaration (E))) = K_Scoped_Name then
               N := Identifier (Reference (Type_Spec (Declaration (E))));
               M := Expand_Designator
                 (Next_Node (Next_Node (Helper_Node (BE_Node (N)))));
               N := Map_Designator (N);
            else
               raise Program_Error;
            end if;

            Helper_Name := BEN.Name
              (Defining_Identifier (Helper_Node (BE_Node (Identifier (E)))));
            N := Make_Subprogram_Call
              (N,
               Make_List_Id (Make_Defining_Identifier (PN (P_Item))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => RE (RE_Any),
               Expression => Make_Subprogram_Call
               (M, Make_List_Id (N)));
            Append_Node_To_List (N, D);
            N := Make_Subprogram_Call
              (RE (RE_Set_Type),
               Make_List_Id (Make_Defining_Identifier (PN (P_Result)),
                             Make_Defining_Identifier (Helper_Name)));
            Append_Node_To_List (N, S);
            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Simple_Declarator_Body;

         -------------------------
         -- Structure_Type_Body --
         -------------------------

         function Structure_Type_Body (E : Node_Id) return Node_Id is
            Member      : Node_Id;
            Declarator  : Node_Id;
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Next_Node (Spec));
            N := RE (RE_Get_Empty_Any_Aggregate);
            Helper_Name := BEN.Name
              (Defining_Identifier (Helper_Node (BE_Node (Identifier (E)))));
            N := Make_Subprogram_Call
              (N,
               Make_List_Id (Make_Defining_Identifier (Helper_Name)));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => RE (RE_Any),
               Expression => N);
            Append_Node_To_List (N, D);
            Member := First_Entity (Members (E));
            while Present (Member) loop
               Declarator := First_Entity (Declarators (Member));
               while Present (Declarator) loop
                  N := Make_Subprogram_Call
                    (RE (RE_Any),
                     Make_List_Id (Map_Designator (Declarator)));
                  N := Make_Subprogram_Call
                    (RE (RE_Add_Aggregate_Element),
                     Make_List_Id
                     (Make_Defining_Identifier (PN (P_Result)),
                      N));
                  Append_Node_To_List (N, S);
                  Declarator := Next_Entity (Declarator);
               end loop;
               Member := Next_Entity (Member);
            end loop;
            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Structure_Type_Body;

         ----------------
         -- Union_Body --
         ----------------

         function Union_Type_Body (E : Node_Id) return Node_Id is
            SwitchItem          : Name_Id;
            Alternative_Name    : Name_Id;
            Choice_List         : List_Id;
            Switch_Alternative  : Node_Id;
            Switch_Alternatives : List_Id;
            Case_Label          : Node_Id;
            V                   : Value_Type;
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Next_Node (Spec));
            N := RE (RE_Get_Empty_Any_Aggregate);
            Helper_Name := BEN.Name
              (Defining_Identifier (Helper_Node (BE_Node (Identifier (E)))));
            N := Make_Subprogram_Call
              (N,
               Make_List_Id (Make_Defining_Identifier (Helper_Name)));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => RE (RE_Any),
               Expression => N);
            Append_Node_To_List (N, D);
            Get_Name_String (PN (P_Item));
            Add_Char_To_Name_Buffer ('.');
            Get_Name_String_And_Append (CN (C_Switch));
            SwitchItem := Name_Find;
            N := Make_Subprogram_Call
              (RE (RE_Any),
               Make_List_Id (Make_Defining_Identifier (SwitchItem)));
            N := Make_Subprogram_Call
              (RE (RE_Add_Aggregate_Element),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Result)),
                N));
            Switch_Alternatives := New_List (K_List_Id);
            Switch_Alternative := First_Entity (Switch_Type_Body (E));
            while Present (Switch_Alternative) loop
               Case_Label := First_Entity (Labels (Switch_Alternative));
               Choice_List := New_List (K_List_Id);
               while Present (Case_Label) loop
                  V := Value (FEN.Value (Case_Label));
                  N := Make_Case_Label (New_Value (V));
                  Append_Node_To_List (N, Choice_List);
                  Case_Label := Next_Entity (Case_Label);
               end loop;
               Get_Name_String (PN (P_Item));
               Add_Char_To_Name_Buffer ('.');
               Get_Name_String_And_Append
                 (FEN.Name (Identifier
                            (Declarator (Element (Switch_Alternative)))));
               Alternative_Name := Name_Find;
               N := Make_Subprogram_Call
                 (RE (RE_Add_Aggregate_Element),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Result)),
                   Make_Defining_Identifier (Alternative_Name)));
               Append_Node_To_List
                 (Make_Case_Statement_Alternative
                  (Choice_List, Make_List_Id (N)), Switch_Alternatives);
               Switch_Alternative := Next_Entity (Switch_Alternative);
            end loop;
            N := Make_Case_Statement
              (Make_Defining_Identifier (SwitchItem),
               Switch_Alternatives);
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Union_Type_Body;

      begin
         case FEN.Kind (E) is
            when K_Complex_Declarator =>
               N := Complex_Declarator_Body (E);

            when K_Enumeration_Type =>
               N := Enumeration_Type_Body (E);

            when K_Interface_Declaration =>
               N := Interface_Declaration_Body (E);

            when K_Simple_Declarator =>
               N := Simple_Declarator_Body (E);

            when K_Structure_Type =>
               N := Structure_Type_Body (E);

            when K_Union_Type =>
               N := Union_Type_Body (E);
            when others =>
               raise Program_Error;
         end case;
         return N;
      end To_Any_Body;

      ------------------------
      -- Narrowing_Ref_Body --
      ------------------------

      function Narrowing_Ref_Body
        (E : Node_Id)
        return Node_Id
      is
         Spec         : Node_Id;
         Declarations : List_Id;
         Statements   : List_Id;
         Param        : Node_Id;
         N            : Node_Id;
         L            : List_Id;
      begin
         Spec := Helper_Node (BE_Node (Identifier (E)));
         Spec := Next_Node (Spec);  --  Second in the list of helpers

         --  Declarative Part

         Declarations := New_List (K_List_Id);
         Param := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (PN (P_Result)),
            Object_Definition =>
              Expand_Designator (Stub_Node (BE_Node (Identifier (E)))));
         Append_Node_To_List (Param, Declarations);

         --  Statements Part

         Statements := New_List (K_List_Id);
         L := New_List (K_List_Id);
         Append_Node_To_List
           (Make_Defining_Identifier (PN (P_Result)), L);
         Append_Node_To_List
           (Make_Subprogram_Call
            (RE (RE_Object_Of),
             Make_List_Id (Make_Defining_Identifier (PN (P_The_Ref)))), L);
         N := Make_Subprogram_Call
           (Defining_Identifier   => Make_Defining_Identifier (SN (S_Set)),
            Actual_Parameter_Part => L);
         Append_Node_To_List (N, Statements);
         N := Make_Return_Statement
           (Make_Defining_Identifier (PN (P_Result)));
         Append_Node_To_List (N, Statements);
         N := Make_Subprogram_Implementation
           (Spec, Declarations, Statements);
         return N;
      end Narrowing_Ref_Body;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Specification =>
               Visit_Specification (E);

            when K_Structure_Type =>
               Visit_Structure_Type (E);

            when K_Type_Declaration =>
               Visit_Type_Declaration (E);

            when K_Union_Type =>
               Visit_Union_Type (E);

            when others =>
               null;
         end case;
      end Visit;

      ----------------------------
      -- Visit_Enumeration_Type --
      ----------------------------

      procedure Visit_Enumeration_Type (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Helper_Body;
         Append_Node_To_List
           (From_Any_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Any_Body (E), Statements (Current_Package));

         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Deferred_Initialization_Body);
      end Visit_Enumeration_Type;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Stub_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Helper_Body;
         Deferred_Initialization_Body := New_List (K_List_Id);
         Package_Initializarion       := New_List (K_List_Id);
         Append_Node_To_List
           (Widening_Ref_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (Narrowing_Ref_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (From_Any_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Any_Body (E), Statements (Current_Package));

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Deferred_Initialization_Body);

         N := Make_Subprogram_Implementation
           (Make_Subprogram_Specification
            (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
             No_List),
            No_List,
            Deferred_Initialization_Body);
         Append_Node_To_List (N, Statements (Current_Package));

         Helper_Initialization (Package_Initializarion);
         Set_Package_Initialization (Current_Package, Package_Initializarion);
         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;

      begin
         D := Stub_Node (BE_Node (Identifier (E)));
         Push_Entity (D);
         D := First_Entity (Definitions (E));
         while Present (D) loop
            Visit (D);
            D := Next_Entity (D);
         end loop;
         Pop_Entity;
      end Visit_Module;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
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
      begin
         Set_Helper_Body;
         Append_Node_To_List
           (From_Any_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Any_Body (E), Statements (Current_Package));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         L : List_Id;
         D : Node_Id;
      begin
         Set_Helper_Body;
         L := Declarators (E);
         D := First_Entity (L);
         while Present (D) loop
            Append_Node_To_List
              (From_Any_Body (D), Statements (Current_Package));
            Append_Node_To_List
              (To_Any_Body (D), Statements (Current_Package));
            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
      begin
         Set_Helper_Body;
         Append_Node_To_List
           (From_Any_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Any_Body (E), Statements (Current_Package));
      end Visit_Union_Type;

      -----------------------
      -- Widening_Ref_Body --
      -----------------------

      function Widening_Ref_Body (E : Node_Id) return Node_Id is
         Spec        : constant Node_Id
           := Helper_Node (BE_Node (Identifier (E)));
         Statements  : List_Id;
         N           : Node_Id;
         M           : Node_Id;
      begin
         Statements := New_List (K_List_Id);
         N := Make_Expression
           (Left_Expr  =>
              Make_Subprogram_Call
            (RE (RE_Is_Nil),
             Make_List_Id (Make_Defining_Identifier (PN (P_The_Ref)))),
            Operator   => Op_And_Then,
            Right_Expr =>
              Make_Subprogram_Call
            (RE (RE_Is_A),
             Make_List_Id (Make_Defining_Identifier (PN (P_The_Ref)),
                           Make_Defining_Identifier (PN (P_Repository_Id)))));
         M := Make_Subprogram_Call
           (Make_Defining_Identifier (SN (S_Unchecked_To_Ref)),
            Make_List_Id (Make_Defining_Identifier (PN (P_The_Ref))));
         M := Make_Return_Statement (M);
         N := Make_If_Statement
           (Condition => N,
            Then_Statements => Make_List_Id (M),
            Else_Statements => No_List);
         Append_Node_To_List (N, Statements);
         N := Make_Subprogram_Call
           (RE (RE_Raise_Bad_Param),
            Make_List_Id (Make_Defining_Identifier
                          (PN (P_Default_Sys_Member))));
         Append_Node_To_List (N, Statements);
         N := Make_Subprogram_Implementation
           (Spec, No_List, Statements);
         return N;
      end Widening_Ref_Body;
   end Package_Body;
end Backend.BE_Ada.Helpers;
