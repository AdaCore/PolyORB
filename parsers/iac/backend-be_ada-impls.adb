with Frontend.Nodes;  use Frontend.Nodes;

with Backend.BE_Ada.IDL_To_Ada;  use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Nodes;       use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;      use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;     use Backend.BE_Ada.Runtime;

package body Backend.BE_Ada.Impls is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;

   package body Package_Spec is

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         N          : Node_Id;
         R          : Node_Id;
         A          : Node_Id;
         Parameter  : Node_Id;
         Parameters : List_Id;
      begin
         Set_Impl_Spec;
         A := First_Entity (Declarators (E));
         while Present (A) loop
            Set_Impl_Spec;
            N := Stub_Node (BE_Node (Identifier (A)));

            if No (N) then
               raise Program_Error;
            end if;

            Parameters := New_List (K_Parameter_Profile);
            Parameter := Make_Parameter_Specification
              (Make_Defining_Identifier (PN (P_Self)),
               Make_Access_Type_Definition
               (Make_Defining_Identifier (TN (T_Object))));
            Append_Node_To_List (Parameter, Parameters);
            R := Copy_Node (Defining_Identifier (N));
            R := Make_Subprogram_Specification
              (R, Parameters, Copy_Designator (Return_Type (N)));
            Append_Node_To_List (R, Visible_Part (Current_Package));
            Bind_FE_To_Impl (Identifier (A), R);

            if not Is_Readonly (E) then
               N := Next_Node (Stub_Node (BE_Node (Identifier (A))));
               Parameters := New_List (K_Parameter_Profile);
               Parameter := Make_Parameter_Specification
                 (Make_Defining_Identifier (PN (P_Self)),
                  Make_Access_Type_Definition
                  (Make_Defining_Identifier (TN (T_Object))));
               Append_Node_To_List (Parameter, Parameters);
               R := Next_Node (First_Node (Parameter_Profile (N)));
               Parameter := Make_Parameter_Specification
                 (Copy_Node (Defining_Identifier (R)),
                  Copy_Designator (Parameter_Type (R)),
                  BEN.Parameter_Mode (R));
               Append_Node_To_List (Parameter, Parameters);
               R := Copy_Node (Defining_Identifier (N));
               R := Make_Subprogram_Specification
                 (R, Parameters, No_Node);
               Append_Node_To_List (R, Visible_Part (Current_Package));
               Link_BE_To_FE (R, Identifier (A));
            end if;

            A := Next_Entity (A);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
         I : Node_Id;
         D : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Impl_Spec;
         I := Make_Defining_Identifier (TN (T_Object));
         N := Make_Full_Type_Declaration
           (I, Make_Derived_Type_Definition
            (Subtype_Indication    => RE (RE_Servant_Base),
             Is_Private_Extention => True));
         Bind_FE_To_Impl (Identifier (E), N);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         D := New_Node (K_Designator);
         Set_Defining_Identifier (D, Copy_Node (I));
         N := Make_Full_Type_Declaration
           (Make_Defining_Identifier (TN (T_Object_Ptr)),
            Make_Access_Type_Definition
            (Make_Type_Attribute (D, A_Class),
             Is_All => True));
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         I := Copy_Node (I);
         N := Make_Full_Type_Declaration
           (I, Make_Derived_Type_Definition
            (Subtype_Indication    => RE (RE_Servant_Base),
             Record_Extension_Part =>
               Make_Record_Definition
             (Make_List_Id (New_Node (K_Null_Statement)))));
         Append_Node_To_List
           (N, Private_Part (Current_Package));
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

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         Stub       : Node_Id;
         Subp_Spec  : Node_Id;
         Profile    : List_Id;
         Stub_Param : Node_Id;
         Impl_Param : Node_Id;
         Returns    : Node_Id := No_Node;
         Type_Designator : Node_Id;

      begin
         Stub := Stub_Node (BE_Node (Identifier (E)));
         Set_Impl_Spec;
         Profile := New_List (K_Parameter_Profile);

         --  Create a dispatching parameter

         Impl_Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Self)),
            Make_Access_Type_Definition
            (Make_Defining_Identifier (TN (T_Object))));
         Append_Node_To_List (Impl_Param, Profile);
         Stub_Param := Next_Node (First_Node (Parameter_Profile (Stub)));
         while Present (Stub_Param) loop
            Type_Designator := Copy_Designator (Parameter_Type (Stub_Param));
            Impl_Param := Make_Parameter_Specification
              (Copy_Node (Defining_Identifier (Stub_Param)),
               Type_Designator,
               BEN.Parameter_Mode (Stub_Param));
            Append_Node_To_List (Impl_Param, Profile);
            Stub_Param := Next_Node (Stub_Param);
         end loop;
         if Present (Return_Type (Stub)) then
            Returns := Copy_Designator (Return_Type (Stub));
         end if;

         Set_Impl_Spec;
         Subp_Spec := Make_Subprogram_Specification
           (Copy_Node (Defining_Identifier (Stub)), Profile, Returns);
         Append_Node_To_List (Subp_Spec, Visible_Part (Current_Package));
         Bind_FE_To_Impl (Identifier (E), Subp_Spec);
      end Visit_Operation_Declaration;

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

   end Package_Spec;

   package body Package_Body is

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         N          : Node_Id;
         A          : Node_Id;
         Subp_Spec  : Node_Id;
         D          : constant List_Id := New_List (K_List_Id);
         S          : constant List_Id := New_List (K_List_Id);
      begin
         Set_Impl_Body;
         A := First_Entity (Declarators (E));
         while Present (A) loop
            Subp_Spec := Impl_Node (BE_Node (Identifier (A)));

            if No (Subp_Spec) then
               raise Program_Error;
            end if;

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition =>
                 Copy_Designator (Return_Type (Subp_Spec)));
            Append_Node_To_List (N, D);
            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Subp_Spec, D, S);
            Append_Node_To_List (N, Statements (Current_Package));

            if not Is_Readonly (E) then
               Subp_Spec := Next_Node (Subp_Spec);
               N := Make_Subprogram_Implementation
                 (Subp_Spec, No_List, No_List);
               Append_Node_To_List (N, Statements (Current_Package));
            end if;

            A := Next_Entity (A);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Impl_Body;
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

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         Stub       : Node_Id;
         Subp_Spec  : Node_Id;
         Returns    : Node_Id := No_Node;
         D          : constant List_Id := New_List (K_List_Id);
         S          : constant List_Id := New_List (K_List_Id);
         N          : Node_Id;

      begin
         Stub := Stub_Node (BE_Node (Identifier (E)));
         Subp_Spec := Impl_Node (BE_Node (Identifier (E)));

         if Present (Return_Type (Stub)) then
            Returns := Copy_Designator (Return_Type (Stub));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition =>
                 Returns);
            Append_Node_To_List (N, D);
            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);
         end if;

         Set_Impl_Body;
         N := Make_Subprogram_Implementation
           (Subp_Spec, D, S);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Operation_Declaration;

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
   end Package_Body;
end Backend.BE_Ada.Impls;
