with Namet;  use Namet;
with Values; use Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;


with Backend.BE_Ada.Expand;      use Backend.BE_Ada.Expand;
with Backend.BE_Ada.IDL_To_Ada;  use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Nodes;       use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;      use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;     use Backend.BE_Ada.Runtime;

pragma Warnings (off);
with Backend.BE_Ada.Debug;     use Backend.BE_Ada.Debug;
with Frontend.Debug;           use Frontend.Debug;
pragma Warnings (on);

package body Backend.BE_Ada.Skels is
   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;
   package FEU renames Frontend.Nutils;

   package body Package_Spec is

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Skeleton_Spec;
         N := Make_Subprogram_Call
           (Make_Defining_Identifier (GN (Pragma_Elaborate_Body)),
            No_List);
         N := Make_Pragma_Statement (N);
         Append_Node_To_List (N, Visible_Part (Current_Package));
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

   end Package_Spec;


   package body Package_Body is

      Invoke_Elsif_Statements : List_Id;
      Package_Initialization  : List_Id;

      function Deferred_Initialization_Body (E : Node_Id) return Node_Id;
      function Gen_Invoke_Part (S : Node_Id) return Node_Id;
      procedure Invoke_Declaration (L : List_Id);
      function Invoke_Spec return Node_Id;
      function Is_A_Invoke_Part return Node_Id;
      function Servant_Is_A_Body (Spec : Node_Id) return Node_Id;
      procedure Skeleton_Initialization (L : List_Id);
      function Non_User_Exception_Handler return Node_Id;

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      function Deferred_Initialization_Body (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Profile    : constant List_Id := New_List (K_List_Id);
         Spec       : Node_Id;
         Statements : constant List_Id := New_List (K_List_Id);
      begin
         Spec := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
            No_List);
         N := Type_Def_Node (BE_Node (Identifier (E)));
         N := Next_Node (N);
         N := Make_Subprogram_Call
           (RE (RE_To_CORBA_String),
            Make_List_Id (Expand_Designator (N)));
         Append_Node_To_List (N, Profile);

         N := Make_Type_Attribute
           (Make_Designator (SN (S_Servant_Is_A)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Type_Attribute
           (Make_Designator (SN (S_Is_A)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Type_Attribute (Make_Designator (SN (S_Invoke)), A_Access);
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call
           (RE (RE_Register_Skeleton), Profile);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Spec, No_List, Statements);

         return N;
      end Deferred_Initialization_Body;

      ---------------------
      -- Gen_Invoke_Part --
      ---------------------

      function Gen_Invoke_Part (S : Node_Id) return Node_Id is
         C                : Node_Id;
         Count            : Natural;
         N                : Node_Id;
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
         Param            : Node_Id;
         Param_Name       : Name_Id;
         Type_Name  : Name_Id;
         New_Name         : Name_Id;
         P                : List_Id;
         Inv_Profile      : constant List_Id := New_List (K_List_Id);
         K                : FEN.Node_Kind := FEN.K_Operation_Declaration;
         TC               : Node_Id;
         From_Any_Helper  : Node_Id;
         To_Any_Helper    : Node_Id;
         FE               : Node_Id;
         Impl_Id          : Node_Id;

         function Exception_Handler_Alternative
           (E : Node_Id)
           return Node_Id;
         --  Generation of an alternative in the exception handler

         -----------------------------------
         -- Exception_Handler_Alternative --
         -----------------------------------

         function Exception_Handler_Alternative
           (E : Node_Id)
           return Node_Id is
            Result     : Node_Id;
            Selector   : Node_Id;
            Expression : Node_Id;
            N          : Node_Id;
            D          : constant List_Id := New_List (K_List_Id);
            S          : constant List_Id := New_List (K_List_Id);
         begin
            --  Getting the Exception name
            N := Expand_Designator
              (Stub_Node
               (BE_Node
                (Identifier
                 (Reference (E)))));

            Selector := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_E)),
               Object_Definition => N);

            --  Declaration of the Members variable
            --  Getting the node corresponding to the declaration of the
            --  "Excp_Name"_Members type.
            N := Type_Def_Node (BE_Node (Identifier (Reference (E))));

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Members)),
               Object_Definition => Defining_Identifier (N));
            Append_Node_To_List (N, D);

            --  Getting the node corresponding to the declaration of the
            --  Get_Members procedure.
            --  This procedure is declared 2 nodes after the member type
            --  definition.

            N := Type_Def_Node (BE_Node (Identifier (Reference (E))));
            N := Next_Node (Next_Node (N));

            N := Defining_Identifier (N);
            N := Make_Subprogram_Call
              (N,
               Make_List_Id
               (Make_Defining_Identifier (PN (P_E)),
                Make_Defining_Identifier (PN (P_Members))));
            Append_Node_To_List (N, S);

            --  Getting the node corresponding to the declaration of the
            --  To_Any procedure in the helper package.

            N := To_Any_Node (BE_Node (Identifier (Reference (E))));

            N := Make_Subprogram_Call
              (Expand_Designator (N),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Members))));

            N := Make_Subprogram_Call
              (RE (RE_Set_Exception),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Request)), N));
            Append_Node_To_List (N, S);

            N := Make_Return_Statement (No_Node);
            Append_Node_To_List (N, S);

            Expression := Make_Block_Statement
              (Declarative_Part => D,
               Statements       => S);

            Result := Make_Component_Association
              (Selector,
               Expression);
            return Result;
         end Exception_Handler_Alternative;


      begin
         --  Implementation.Object'Class (Self.All)'Access

         N := Implementation_Package (Current_Entity);
         N := First_Node (Visible_Part (Package_Specification (N)));
         N := Expand_Designator (N);
         N := Make_Type_Attribute (N, A_Class);
         C := Make_Designator
           (Designator => PN (P_Self),
            Is_All     => True);
         N := Make_Subprogram_Call
           (N, Make_List_Id (C));
         N := Make_Attribute_Designator
           (N, A_Access);
         Append_Node_To_List (N, Inv_Profile);

         Count := Length (Parameter_Profile (S));

         if Count > 1 then
            Param := First_Node (Parameter_Profile (S));
            Param := Next_Node (Param);
            loop
               P := Make_List_Id (Make_Designator (VN (V_Argument_List)));
               Param_Name := BEN.Name (Defining_Identifier (Param));

               --  If the parameter type is a class-wide type, we remove the
               --  "'Class" attribute from the type name
               Type_Name := Fully_Qualified_Name
                 (Parameter_Type (Param));
               Type_Name := Remove_Suffix_From_Name
                 ("'Class", Type_Name);

               N :=  Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (Param_Name),
                  Object_Definition   =>
                    Make_Designator (Type_Name));

               Append_Node_To_List (N, Declarative_Part);
               Append_Node_To_List
                 (Make_Defining_Identifier (Param_Name),
                  Inv_Profile);
               C := Make_Subprogram_Call
                 (Defining_Identifier   => RE (RE_To_CORBA_String),
                  Actual_Parameter_Part =>
                    Make_List_Id (Make_Literal
                                  (New_String_Value (Param_Name, False))));
               New_Name := Add_Prefix_To_Name ("Arg_Name_U_", Param_Name);
               Append_Node_To_List (Make_Designator (New_Name), P);
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (New_Name),
                  Constant_Present => True,
                  Object_Definition => RE (RE_Identifier_0),
                  Expression => C);
               Append_Node_To_List (N, Declarative_Part);

               TC := Get_TC_Node (BEN.FE_Node (Parameter_Type (Param)));

               C :=  Make_Subprogram_Call
                 (Defining_Identifier   => RE (RE_Get_Empty_Any),
                  Actual_Parameter_Part =>
                    Make_List_Id (TC));
               New_Name := Add_Prefix_To_Name ("Argument_U_", Param_Name);
               Append_Node_To_List (Make_Designator (New_Name), P);
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (New_Name),
                  Constant_Present => False,
                  Object_Definition => RE (RE_Any),
                  Expression => C);
               Append_Node_To_List (N, Declarative_Part);

               if BEN.Parameter_Mode (Param) = Mode_Out then
                  N := RE (RE_ARG_OUT_0);
               elsif BEN.Parameter_Mode (Param) = Mode_In then
                  N := RE (RE_ARG_IN_0);
               else
                  N := RE (RE_ARG_INOUT_0);
               end if;

               Append_Node_To_List (N, P);
               N := Make_Subprogram_Call
                 (RE (RE_Add_Item_0),
                  P);
               Append_Node_To_List (N, Statements);
               Param := Next_Node (Param);
               exit when No (Param);
            end loop;
         end if;

         N := Make_Subprogram_Call
           (RE (RE_Arguments),
            Make_List_Id
            (Make_Defining_Identifier (PN (P_Request)),
             Make_Defining_Identifier (VN (V_Argument_List))));
         Append_Node_To_List (N, Statements);

         --  The bloc above implements the generation of :
         --  * The argument conversion from the "Any" type
         --  * The call of the corresponding method implemented by the
         --    programmer.
         --  * The handling of eventuals exceptions thrown by the method.

         --  If the method could potentially throw an exception, the generated
         --  code will be put in a statement bloc. Else, No additional
         --  statement bloc will be used.

         declare
            Inner_Statements  : List_Id := No_List;
            Inner             : Boolean := False;
            Exception_Handler : List_Id := No_List;
            Excp_Node         : Node_Id;
         begin

            --  Looking wether the operation throws exceptions and setting
            --  Inner_statement to the corresponding value
            N := Corresponding_Entity (FE_Node (S));
            if FEN.Kind (N) = K_Operation_Declaration and then
              not FEU.Is_Empty (Exceptions (N)) then
               Inner_Statements  := New_List (K_List_Id);
               Exception_Handler := New_List (K_List_Id);
               Inner             := True;
               --  Creating the exception handler statements
               Excp_Node := First_Entity (Exceptions (N));
               while Present (Excp_Node) loop
                  N := Exception_Handler_Alternative (Excp_Node);
                  Append_Node_To_List (N, Exception_Handler);
                  Excp_Node := Next_Entity (Excp_Node);
               end loop;
            else
               Inner_Statements := Statements;
            end if;

            --  Convert from their Any
            Set_Str_To_Name_Buffer
              ("Convert from Any");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Inner_Statements);

            if Count > 1 then
               Param := First_Node (Parameter_Profile (S));
               Param := Next_Node (Param);
               loop
                  if  BEN.Parameter_Mode (Param) = Mode_In
                    or else BEN.Parameter_Mode (Param) = Mode_Inout then
                     Param_Name := BEN.Name (Defining_Identifier (Param));
                     New_Name := Add_Prefix_To_Name
                       ("Argument_U_", Param_Name);

                     From_Any_Helper := Get_From_Any_Node
                       (BEN.FE_Node
                        (Parameter_Type
                         (Param)));

                     N := Make_Assignment_Statement
                       (Make_Defining_Identifier (Param_Name),
                        Make_Subprogram_Call
                        (From_Any_Helper,
                         Make_List_Id (Make_Designator (New_Name))));
                     Append_Node_To_List (N, Inner_Statements);
                  end if;

                  Param := Next_Node (Param);
                  exit when No (Param);
               end loop;
            end if;

            --  Call Implementation
            Set_Str_To_Name_Buffer
              ("Call Implementation");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Inner_Statements);

            N := Corresponding_Entity (FE_Node (S));
            C := Impl_Node (BE_Node (FE_Node (S)));

            if Kind (N) /= K_Operation_Declaration then
               Get_Name_String (BEN.Name (Defining_Identifier (S)));
               K := K_Attribute_Declaration;

               if Name_Buffer (1) = 'S' then
                  C := Next_Node (C);
               end if;
            end if;

            --  Re-adjusting the parent unit name of the operation. This is
            --  necessary in the case of operations or attributes inherited
            --  from the second until the last parent (multiple inheritence)
            Impl_Id := New_Node (K_Designator);
            Set_Defining_Identifier
              (Impl_Id,
               Copy_Node
               (Defining_Identifier (C)));
            Set_Correct_Parent_Unit_Name
              (Impl_Id,
               Defining_Identifier
               (Implementation_Package
                (Current_Entity)));

            C := Make_Subprogram_Call
              (Copy_Designator (Impl_Id),
               Inv_Profile);

            if Present (Return_Type (S)) then
               --  If the return type is a class-wide type, we remove the
               --  "'Class" attribute from the type name
               Type_Name := Fully_Qualified_Name
                 (Return_Type (S));
               Type_Name := Remove_Suffix_From_Name
                 ("'Class", Type_Name);
               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Result)),
                  Object_Definition =>
                    Make_Designator (Type_Name));

               Append_Node_To_List (N, Declarative_Part);
               C := Make_Assignment_Statement
                 (Make_Defining_Identifier (VN (V_Result)),
                  C);
            end if;

            Append_Node_To_List (C, Inner_Statements);

            if Inner then
               Append_Node_To_List
                 (Make_Block_Statement
                  (Declarative_Part => No_List,
                   Statements => Inner_Statements,
                   Exception_Handler => Exception_Handler),
                  Statements);
            end if;

         end;

         --  Set Result

         if Present (Return_Type (S)) then
            Set_Str_To_Name_Buffer
              ("Setting the result");
            Append_Node_To_List
              (Make_Ada_Comment (Name_Find),
               Statements);

            FE := FE_Node (Return_Type (S));

            To_Any_Helper := Get_To_Any_Node (FE);

            C := Make_Subprogram_Call
              (To_Any_Helper,
               Make_List_Id (Make_Designator (VN (V_Result))));
            N := Make_Subprogram_Call
              (RE (RE_Set_Result),
               Make_List_Id
               (Make_Designator (PN (P_Request)),
                C));
            Append_Node_To_List (N, Statements);
         end if;

         --  Set out arguments


         if Count > 1 then
            Param := First_Node (Parameter_Profile (S));
            Param := Next_Node (Param);
            loop
               if  BEN.Parameter_Mode (Param) = Mode_Out
                 or else BEN.Parameter_Mode (Param) = Mode_Inout then
                  Set_Str_To_Name_Buffer
                    ("Setting out argument");
                  Append_Node_To_List
                    (Make_Ada_Comment (Name_Find),
                     Statements);

                  Param_Name := BEN.Name (Defining_Identifier (Param));
                  New_Name := Add_Prefix_To_Name ("Argument_U_", Param_Name);

                  To_Any_Helper := Get_To_Any_Node
                    (BEN.FE_Node
                     (Parameter_Type
                      (Param)));

                  declare
                     Arg_List : constant List_Id := New_List (K_List_Id);
                  begin
                     Append_Node_To_List
                       (Make_Defining_Identifier (New_Name),
                        Arg_List);
                     Append_Node_To_List
                       (Make_Subprogram_Call
                        (To_Any_Helper,
                         Make_List_Id (Make_Designator (Param_Name))),
                        Arg_List);

                     N := Make_Subprogram_Call
                       (RE (RE_Move_Any_Value),
                        Arg_List);
                     Append_Node_To_List (N, Statements);
                  end;
               end if;

               Param := Next_Node (Param);
               exit when No (Param);
            end loop;
         end if;

         N := Make_Return_Statement (No_Node);
         Append_Node_To_List (N, Statements);
         declare
            Operation_Name   : Name_Id := BEN.Name (Defining_Identifier (S));
         begin
            if K = K_Attribute_Declaration then
               Operation_Name := Add_Prefix_To_Name ("_", Operation_Name);
            end if;

            C := Make_Expression
              (Make_Defining_Identifier (VN (V_Operation)),
               Op_Equal,
               Make_Literal
               (New_String_Value
                (Operation_Name, False)));
         end;

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         N := Make_Elsif_Statement
           (C, Make_List_Id (N));
         return N;
      end Gen_Invoke_Part;

      ------------------------
      -- Invoke_Declaration --
      ------------------------

      procedure Invoke_Declaration (L : List_Id) is
         N : Node_Id;
      begin
         N := Make_Designator
           (Designator => PN (P_Request),
            Is_All     => True);
         N := Make_Subprogram_Call
           (RE (RE_Operation),
            Make_List_Id (N));
         N := Make_Subprogram_Call
           (RE (RE_To_Standard_String),
            Make_List_Id (N));
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Operation)),
            Constant_Present    => True,
            Object_Definition   => RE (RE_String_2),
            Expression          => N);
         Append_Node_To_List (N, L);
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Argument_List)),
            Object_Definition   => RE (RE_Ref_4));
         Append_Node_To_List (N, L);
      end Invoke_Declaration;

      -----------------
      -- Invoke_Spec --
      -----------------

      function Invoke_Spec return Node_Id is
         N       : Node_Id;
         Param   : Node_Id;
         Profile : List_Id;

      begin
         Profile := New_List (K_List_Id);
         Param   := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Self)),
            RE (RE_Servant));
         Append_Node_To_List (Param, Profile);
         Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Request)),
            RE (RE_Object_Ptr));
         Append_Node_To_List (Param, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Invoke)),
            Profile,
            No_Node);
         return N;
      end Invoke_Spec;

      ----------------------
      -- Is_A_Invoke_Part --
      ----------------------

      function Is_A_Invoke_Part return Node_Id is
         C                : Node_Id;
         N                : Node_Id;
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
         Param            : constant String := "Type_Id";
         Param_Name       : Name_Id;
         New_Name         : Name_Id;
         P                : List_Id;
      begin
         P := Make_List_Id (Make_Designator (VN (V_Argument_List)));
         Set_Str_To_Name_Buffer (Param);
         Param_Name := Name_Find;
         N :=  Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (Param_Name),
            Object_Definition   =>
              RE (RE_String_0));
         Append_Node_To_List (N, Declarative_Part);
         C := Make_Subprogram_Call
           (Defining_Identifier   => RE (RE_To_CORBA_String),
            Actual_Parameter_Part =>
              Make_List_Id (Make_Literal
                            (New_String_Value (Param_Name, False))));
         New_Name := Add_Prefix_To_Name ("Arg_Name_U_", Param_Name);
         Append_Node_To_List (Make_Designator (New_Name), P);
         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (New_Name),
            Constant_Present => True,
            Object_Definition => RE (RE_Identifier_0),
            Expression => C);
         Append_Node_To_List (N, Declarative_Part);

         C := Make_Subprogram_Call
           (RE (RE_To_Any_0), Make_List_Id (Make_Designator (Param_Name)));
         New_Name := Add_Prefix_To_Name ("Argument_U_", Param_Name);
         Append_Node_To_List (Make_Designator (New_Name), P);
         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (New_Name),
            Constant_Present => False,
            Object_Definition => RE (RE_Any),
            Expression => C);
         Append_Node_To_List (N, Declarative_Part);
         Append_Node_To_List (RE (RE_ARG_IN_0), P);

         N := Make_Subprogram_Call
           (RE (RE_Add_Item_0),
            P);
         Append_Node_To_List (N, Statements);
         N := Make_Subprogram_Call
           (RE (RE_Arguments),
            Make_List_Id
            (Make_Designator (PN (P_Request)),
             Make_Designator (VN (V_Argument_List))));
         Append_Node_To_List (N, Statements);

         New_Name := Add_Prefix_To_Name ("Argument_U_", Param_Name);
         C := RE (RE_From_Any_0);
         N := Make_Assignment_Statement
           (Make_Defining_Identifier (Param_Name),
            Make_Subprogram_Call
            (C, Make_List_Id (Make_Designator (New_Name))));
         Append_Node_To_List (N, Statements);

         --  Call Implementation
         Set_Str_To_Name_Buffer
           ("Setting the result");
         Append_Node_To_List
           (Make_Ada_Comment (Name_Find),
            Statements);

         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Result)),
            Object_Definition =>
              RE (RE_Boolean));
         Append_Node_To_List (N, Declarative_Part);

         C := Expand_Designator (Main_Package (Current_Entity));
         N := Make_Designator (SN (S_Is_A));
         Set_Correct_Parent_Unit_Name (N, C);
         C := Make_Subprogram_Call
           (RE (RE_To_Standard_String),
            Make_List_Id (Make_Designator (Param_Name)));
         N := Make_Subprogram_Call
           (N,
            Make_List_Id (C));
         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Result)),
            N);
         Append_Node_To_List (N, Statements);
         C := Make_Subprogram_Call
           (RE (RE_To_Any_0),
            Make_List_Id (Make_Designator (VN (V_Result))));
         N := Make_Subprogram_Call
           (RE (RE_Set_Result),
            Make_List_Id
            (Make_Designator (PN (P_Request)),
             C));
         Append_Node_To_List (N, Statements);
         N := Make_Return_Statement (No_Node);
         Append_Node_To_List (N, Statements);
         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         return N;
      end Is_A_Invoke_Part;

      -----------------------
      -- Servant_Is_A_Body --
      -----------------------

      function Servant_Is_A_Body (Spec : Node_Id) return Node_Id is
         Statements : constant List_Id := New_List (K_List_Id);
         N          : Node_Id;
      begin
         N := Implementation_Package (Current_Entity);
         N := First_Node
           (Visible_Part (Package_Specification (N)));
         N := Expand_Designator (N);
         N := Make_Type_Attribute (N, A_Class);
         N := Make_Expression
           (Make_Designator
            (Designator => PN (P_Obj),
             Is_All     => True),
            Op_In,
            N);
         N := Make_Return_Statement (N);
         Append_Node_To_List (N, Statements);

         return  Make_Subprogram_Implementation
           (Spec, No_List, Statements);
      end Servant_Is_A_Body;

      -----------------------------
      -- Skeleton_Initialization --
      -----------------------------

      procedure Skeleton_Initialization (L : List_Id) is
         N         : Node_Id;
         V         : Value_Id;
         Aggregates : List_Id;
      begin
         Aggregates := New_List (K_List_Id);
         N := Defining_Identifier
           (Package_Declaration (Current_Package));
         V := New_String_Value
           (Fully_Qualified_Name (N), False);
         N := Make_Subprogram_Call
           (RE (RE_Add),
            Make_List_Id (Make_Literal (V)));
         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Name)),
            Expression          =>
              N);
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
      end Skeleton_Initialization;

      --------------------------------
      -- Non_User_Exception_Handler --
      --------------------------------

      function Non_User_Exception_Handler return Node_Id is
         Result     : Node_Id;
         Selector   : Node_Id;
         Expression : Node_Id;
         N          : Node_Id;
         D          : constant List_Id := New_List (K_List_Id);
         S          : constant List_Id := New_List (K_List_Id);
      begin
         --  Generation of the "E : others" statement
         Selector := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (PN (P_E)),
            Object_Definition => No_Node);

         --  Body of the exception handler
         N := Make_Subprogram_Call
           (RE (RE_System_Exception_To_Any),
            Make_List_Id
            (Make_Defining_Identifier (PN (P_E))));

         N := Make_Subprogram_Call
           (RE (RE_To_CORBA_Any),
            Make_List_Id (N));

         N := Make_Subprogram_Call
           (RE (RE_Set_Exception),
            Make_List_Id
            (Make_Defining_Identifier (PN (P_Request)), N));
         Append_Node_To_List (N, S);

         N := Make_Return_Statement (No_Node);
         Append_Node_To_List (N, S);

         Expression := Make_Block_Statement
           (Declarative_Part => D,
            Statements       => S);

         Result := Make_Component_Association
           (Selector,
            Expression);
         return Result;
      end Non_User_Exception_Handler;

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

      begin
         A := First_Entity (Declarators (E));
         while Present (A) loop
            N := Stub_Node (BE_Node (Identifier (A)));

            if No (N) then
               raise Program_Error;
            end if;
            N := Gen_Invoke_Part (N);
            Append_Node_To_List (N, Invoke_Elsif_Statements);

            if not Is_Readonly (E) then
               N := Next_Node (Stub_Node (BE_Node (Identifier (A))));
               N := Gen_Invoke_Part (N);
               Append_Node_To_List (N, Invoke_Elsif_Statements);
            end if;

            A := Next_Entity (A);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N                 : Node_Id;
         Spec              : Node_Id;
         D                 : constant List_Id := New_List (K_List_Id);
         C                 : Node_Id;
         Then_Statements   : constant List_Id := New_List (K_List_Id);
         Else_Statements   : constant List_Id := New_List (K_List_Id);
         Invoke_Statements : constant List_Id := New_List (K_List_Id);
         Exception_Handler : Node_Id;
         Param             : Node_Id;
         Profile           : constant List_Id := New_List (K_List_Id);

      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));

         Set_Skeleton_Body;

         Invoke_Elsif_Statements := New_List (K_List_Id);
         Package_Initialization  := New_List (K_List_Id);

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritence, generate the mappings for
         --  the operations and attributes of the parent interface including
         --  the first one.
         Map_Inherited_Entities_Bodies
           (Current_interface    => E,
            Visit_Operation_Subp => Visit_Operation_Declaration'Access,
            Visit_Attribute_Subp => Visit_Attribute_Declaration'Access,
            Skel                 => True);

         Spec := Invoke_Spec;
         Invoke_Declaration (D);
         N := Make_Subprogram_Call
           (RE (RE_Create_List),
            Make_List_Id
            (Make_Literal (Int0_Val),
             Make_Defining_Identifier (VN (V_Argument_List))));
         Append_Node_To_List (N, Invoke_Statements);

         C := Make_Expression
           (Make_Defining_Identifier (VN (V_Operation)),
            Op_Equal,
            Make_Literal
            (New_String_Value
             (Add_Prefix_To_Name ("_", SN (S_Is_A)), False)));
         N := Is_A_Invoke_Part;
         Append_Node_To_List (N, Then_Statements);

         N := Make_Subprogram_Call
           (RE (RE_Raise_Bad_Operation),
            Make_List_Id (RE (RE_Default_Sys_Member)));
         Append_Node_To_List (N, Else_Statements);

         N := Make_If_Statement
           (C,
            Then_Statements,
            Invoke_Elsif_Statements,
            Else_Statements);

         Exception_Handler := Non_User_Exception_Handler;

         N := Make_Block_Statement
           (Declarative_Part  => No_List,
            Statements        =>
              Make_List_Id (N),
            Exception_Handler =>
              Make_List_Id (Exception_Handler));
         Append_Node_To_List (N, Invoke_Statements);

         --  Generation of the Servant_Is_A function

         Param := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Obj)),
            RE (RE_Servant));
         Append_Node_To_List (Param, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Servant_Is_A)),
            Profile,
            RE (RE_Boolean_0));
         Append_Node_To_List (N, Statements (Current_Package));

         N := Servant_Is_A_Body (N);
         Append_Node_To_List (N, Statements (Current_Package));

         N := Make_Subprogram_Implementation
           (Spec, D, Invoke_Statements);
         Append_Node_To_List (N, Statements (Current_Package));

         N := Deferred_Initialization_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         Skeleton_Initialization (Package_Initialization);
         Set_Package_Initialization (Current_Package, Package_Initialization);

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
         Spec   : Node_Id;
         N      : Node_Id;
      begin
         Spec := Stub_Node (BE_Node (Identifier (E)));
         N := Gen_Invoke_Part (Spec);
         Append_Node_To_List (N, Invoke_Elsif_Statements);
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
end Backend.BE_Ada.Skels;
