with Namet;     use Namet;
with Types;     use Types;
with Values;    use Values;

with Frontend.Nutils;
with Frontend.Nodes;            use Frontend.Nodes;
--  with Frontend.Debug;

with Backend.BE_Ada.Expand; use Backend.BE_Ada.Expand;
with Backend.BE_Ada.IDL_To_Ada; use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Nodes;      use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;     use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;    use Backend.BE_Ada.Runtime;
--  with Backend.BE_Ada.Debug;    use Backend.BE_Ada.Debug;

package body Backend.BE_Ada.Stubs is

   Getter : constant Character := 'G';
   Setter : constant Character := 'S';

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;
   package FEU renames Frontend.Nutils;


   function Marshaller_Body
     (Subp_Spec : Node_Id; Local_Variables : List_Id) return List_Id;
   function Marshaller_Declarations
     (Subp_Spec : Node_Id) return List_Id;
   function Local_Is_A_Body (E : Node_Id) return Node_Id;
   function Local_Is_A_Spec return Node_Id;
   function Visible_Is_A_Body return Node_Id;
   function Visible_Is_A_Spec return Node_Id;

   package body Package_Spec is

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
         --  P : Node_Id;
         --  D : List_Id;
         --  S : List_Id;

      begin
         A := First_Entity (Declarators (E));
         while Present (A) loop
            Set_Main_Spec;

            --  Insert repository declaration

            Append_Node_To_List
              (Map_Repository_Declaration (A),
               Visible_Part (Current_Package));

            --  Insert getter specification

            N := Map_Accessor_Declaration
              (Accessor => Getter, Attribute => A);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_Stub (Identifier (A), N);

            if not Is_Readonly (E) then
               Set_Main_Spec;

               --  Insert setter specification

               N := Map_Accessor_Declaration
                 (Accessor => Setter, Attribute => A);
               Append_Node_To_List (N, Visible_Part (Current_Package));
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
           (Defining_Identifier => Map_Defining_Identifier (E),
            Constant_Present    => True,
            Object_Definition   => Map_Designator (Type_Spec (E)),
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
            Enum_Literal := Map_Defining_Identifier (Enumerator);
            Append_Node_To_List (Enum_Literal, Enum_Literals);
            Enumerator := Next_Entity (Enumerator);
         end loop;

         Enum_Type_Decl :=
           Make_Full_Type_Declaration
           (Map_Defining_Identifier (E),
            Make_Enumeration_Type_Definition (Enum_Literals));

         Bind_FE_To_Stub  (Identifier (E), Enum_Type_Decl);
         Append_Node_To_List
           (Enum_Type_Decl,
            Visible_Part (Current_Package));
         Append_Node_To_List
           (Map_Repository_Declaration (E),
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
             (Map_Members_Definition (Members (E)))),
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
         P       : Node_Id;
         N       : Node_Id;
         L       : List_Id;
         I       : Node_Id;
      begin
         P := Map_IDL_Unit (E);
         Append_Node_To_List (P, Packages (Current_Entity));
         Push_Entity (P);
         Set_Main_Spec;
         L := Interface_Spec (E);
         if FEU.Is_Empty (L) then
            N := RE (RE_Ref_2);
         else
            N := Expand_Designator
              (Stub_Node (BE_Node (Identifier (First_Entity (L)))));
         end if;
         I := Make_Defining_Identifier (TN (T_Ref));
         N :=
           Make_Full_Type_Declaration
           (I, Make_Derived_Type_Definition
            (Subtype_Indication    => N,
             Record_Extension_Part =>
               Make_Record_Type_Definition
             (Record_Definition => Make_Record_Definition (No_List))));
         Set_Corresponding_Node (I, N);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Stub (Identifier (E), N);
         N := Map_Repository_Declaration (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Set_FE_Node (N, Identifier (E));

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;
         N := Visible_Is_A_Spec;
         Append_Node_To_List (N, Visible_Part (Current_Package));
         N := Local_Is_A_Spec;
         Append_Node_To_List (N, Private_Part (Current_Package));
         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
         S : Node_Id;

      begin
         S := Map_IDL_Unit (E);
         Append_Node_To_List (S, Packages (Current_Entity));
         Push_Entity (S);
         Set_Main_Spec;
         Append_Node_To_List
           (Map_Repository_Declaration (E), Visible_Part (Current_Package));
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

         Subp_Spec : Node_Id;
         Profile   : List_Id;
         IDL_Param : Node_Id;
         Ada_Param : Node_Id;
         Mode      : Mode_Id := Mode_In;
         Returns   : Node_Id := No_Node;
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
            Type_Designator := Map_Designator
              (Type_Spec (IDL_Param));
            Ada_Param := Make_Parameter_Specification
              (Map_Defining_Identifier (Declarator (IDL_Param)),
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
               Returns := Map_Designator (Type_Spec (E));

               --  If the IDL function is mapped as an Ada procedure, add a
               --  new parameter Returns to pass the returned value.

            else
               Type_Designator := Map_Designator
                 (Type_Spec (E));
               Ada_Param := Make_Parameter_Specification
                 (Make_Defining_Identifier (PN (P_Returns)),
                  Type_Designator,
                  Mode_Out);
               Append_Node_To_List (Ada_Param, Profile);
            end if;
         end if;

         --  Add subprogram to main specification

         Set_Main_Spec;
         Subp_Spec := Make_Subprogram_Specification
           (Map_Defining_Identifier (E), Profile, Returns);
         Append_Node_To_List (Subp_Spec, Visible_Part (Current_Package));
         Bind_FE_To_Stub (Identifier (E), Subp_Spec);
      end Visit_Operation_Declaration;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;

      begin
         Push_Entity (Map_IDL_Unit (E));
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
           (Map_Defining_Identifier (E),
            Make_Record_Type_Definition
            (Make_Record_Definition
             (Map_Members_Definition (Members (E)))));
         Bind_FE_To_Stub (Identifier (E), N);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Append_Node_To_List
           (Map_Repository_Declaration (E), Visible_Part (Current_Package));
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
         T := Map_Designator (Type_Spec (E));
         D := First_Entity (Declarators (E));
         while Present (D) loop
            if Kind (D) = K_Complex_Declarator then
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Map_Defining_Identifier (D),
                  Type_Definition     => Make_Array_Type_Definition
                  (Map_Range_Constraints (FEN.Array_Sizes (D)), T));
            else
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Map_Defining_Identifier (D),
                  Type_Definition     => Make_Derived_Type_Definition
                  (Subtype_Indication    => T,
                   Record_Extension_Part => No_Node));
            end if;
            Bind_FE_To_Stub (Identifier (D), N);
            Append_Node_To_List
              (N, Visible_Part (Current_Package));
            Append_Node_To_List
              (Map_Repository_Declaration (D), Visible_Part (Current_Package));
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
         T := Map_Designator (S);
         L := New_List (K_Component_List);
         Append_Node_To_List
           (Make_Variant_Part
            (Make_Defining_Identifier (CN (C_Switch)),
             Map_Variant_List (Switch_Type_Body (E))),
            L);
         N := Make_Full_Type_Declaration
           (Map_Defining_Identifier (E),
            Make_Record_Type_Definition
            (Make_Record_Definition (L)),
            Make_Component_Declaration
            (Make_Defining_Identifier (CN (C_Switch)), T,
             Make_Type_Attribute (T, A_First)));
         Bind_FE_To_Stub (Identifier (E), N);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Append_Node_To_List
           (Map_Repository_Declaration (E), Visible_Part (Current_Package));
      end Visit_Union_Type;
   end Package_Spec;

   package body Package_Body is

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Specification =>
               Visit_Specification (E);

            when K_Constant_Declaration =>
               null;

            when K_Enumeration_Type =>
               null;

            when K_Exception_Declaration =>
               null;

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Structure_Type =>
               null;

            when K_Union_Type =>
               null;

            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

            when K_Type_Declaration =>
               null;

            when K_Module =>
               null;

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
         S : Node_Id;
         D : List_Id;
         B : List_Id;
      begin
         A := First_Entity (Declarators (E));
         while Present (A) loop
            Set_Main_Body;
            S := Stub_Node (BE_Node (Identifier (A)));
            D := Marshaller_Declarations (S);
            B := Marshaller_Body (S, No_List);
            N := Make_Subprogram_Implementation
              (Specification => S,
               Declarations => D,
               Statements => B);
            Append_Node_To_List (N, Statements (Current_Package));

            if not Is_Readonly (E) then
               Set_Main_Body;
               S := Next_Node (S);
               D := Marshaller_Declarations (S);
               B := Marshaller_Body (S, No_List);
               N := Make_Subprogram_Implementation
                 (Specification => S,
                  Declarations => D,
                  Statements => B);
               Append_Node_To_List (N, Statements (Current_Package));
            end if;

            A := Next_Entity (A);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------
      -- Visit_Interface --
      ---------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Stub_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Helper_Body;
         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;
         N := Visible_Is_A_Body;
         Append_Node_To_List (N, Statements (Current_Package));
         N := Local_Is_A_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
         Pop_Entity;
      end Visit_Interface_Declaration;


      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         pragma Unreferenced (E);
      begin
         null;
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


   ---------------------
   -- Marshaller_Body --
   ---------------------

   function Marshaller_Body
     (Subp_Spec       : Node_Id;
      Local_Variables : List_Id)
      return            List_Id
   is
      Statements    : List_Id;
      N             : Node_Id;
      C             : Node_Id;
      P             : List_Id;
      S             : List_Id;
      Count         : Natural;
      Return_T      : Node_Id;
      I             : Node_Id;
      Param         : Node_Id;
      pragma Unreferenced (Local_Variables);
   begin
      Return_T := Return_Type (Subp_Spec);
      Statements := New_List (BEN.K_List_Id);

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
      N := Make_If_Statement
        (Condition => C,
         Then_Statements => S);
      Append_Node_To_List (N, Statements);

      --  Create argument list.

      C := New_Node (K_Subprogram_Call);
      Set_Defining_Identifier
        (C, RE (RE_Create));
      P := New_List (BEN.K_List_Id);
      Append_Node_To_List
        (Make_Defining_Identifier (VN (V_Argument_List)), P);
      Set_Actual_Parameter_Part (C, P);
      Append_Node_To_List (C, Statements);
      Count := Length (Parameter_Profile (Subp_Spec));

      --  Add arguments  to argument  list

      if Count > 1 then
         P :=  BEN.Parameter_Profile (Subp_Spec);
         I := First_Node (P);
         I := Next_Node (I);
         loop
            P := Make_List_Id (Make_Designator (VN (V_Argument_List)));
            Set_Str_To_Name_Buffer ("Arg_Name_U_");
            Get_Name_String_And_Append (BEN.Name (Defining_Identifier (I)));
            N := Make_Designator (Name_Find);
            Append_Node_To_List (N, P);
            Set_Str_To_Name_Buffer ("Argument_U_");
            Get_Name_String_And_Append (BEN.Name (Defining_Identifier (I)));
            N := Make_Designator (Name_Find);
            N := Make_Subprogram_Call
              (RE (RE_To_PolyORB_Any),
               Make_List_Id (N));
            Append_Node_To_List (N, P);

            if BEN.Parameter_Mode (I) = Mode_Out then
               N := RE (RE_ARG_IN_1);
            elsif BEN.Parameter_Mode (I) = Mode_In then
               N := RE (RE_ARG_OUT_1);
            else
               N := RE (RE_ARG_INOUT_1);
            end if;

            Append_Node_To_List (N, P);
            N := Make_Subprogram_Call
              (RE (RE_Add_Item_1),
               P);
            Append_Node_To_List (N, Statements);
            I := Next_Node (I);
            exit when No (I);
         end loop;
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

      if No (Return_T) then
         I := RE (RE_Get_Empty_Any_1);
         Param := RE (RE_TC_Void);
      elsif Is_Base_Type
        (FE_Node (Return_T))
      then
         Param := Base_Type_TC (FEN.Kind (FE_Node (Return_T)));
         I := RE (RE_Get_Empty_Any_0);
      else
         Param := Identifier (FE_Node (Return_T));
         Param := Helper_Node
           (BE_Node (Identifier
                     (Reference (Corresponding_Entity (Param)))));
         Param := Expand_Designator (Param);
         I := RE (RE_Get_Empty_Any_0);
      end if;

      C := Make_Subprogram_Call
        (Defining_Identifier  => I,
         Actual_Parameter_Part => Make_List_Id (Param));
      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Argument)),
         Expression    => C);
      Append_Node_To_List (N, P);

      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Arg_Modes)),
         Expression    => Make_Literal (New_Integer_Value (0, 0, 10)));
      Append_Node_To_List (N, P);

      N := Make_Record_Aggregate (P);

      N := Make_Assignment_Statement
        (Variable_Identifier =>
           Make_Defining_Identifier (VN (V_Result_Name)),
         Expression => N);
      Append_Node_To_List (N, Statements);

      N := Make_Subprogram_Call
        (RE (RE_Ref_2),
         Make_List_Id (Make_Defining_Identifier (PN (P_Self))));
      N := Make_Subprogram_Call
        (RE (RE_To_PolyORB_Ref), Make_List_Id (N));
      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Target)),
         Expression    => N);
      P := Make_List_Id (N);
      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Operation)),
         Expression    => Make_Defining_Identifier (VN (V_Operation_Name)));
      Append_Node_To_List (N, P);
      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Arg_List)),
         Expression    => Make_Defining_Identifier (VN (V_Argument_List)));
      Append_Node_To_List (N, P);
      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Result)),
         Expression    => Make_Defining_Identifier (VN (V_Result_Name)));
      Append_Node_To_List (N, P);
      N := Make_Component_Association
        (Selector_Name => Make_Defining_Identifier (PN (P_Req)),
         Expression    => Make_Defining_Identifier (VN (V_Request)));
      Append_Node_To_List (N, P);
      N := Make_Subprogram_Call
        (RE (RE_Create_Request),
         P);
      Append_Node_To_List (N, Statements);
      N := Make_Subprogram_Call
        (RE (RE_Flags),
         Make_List_Id (Make_Literal (Int0_Val)));
      N := Make_Subprogram_Call
        (RE (RE_Client_Invoke),
         Make_List_Id
         (Make_Defining_Identifier (VN (V_Request)),
          N));
      Append_Node_To_List (N, Statements);

      --  ???

      P := New_List (K_List_Id);
      C := Make_Designator
        (Designator => PN (P_Argument),
         Parent     => VN (V_Result));
      N := Make_Assignment_Statement
        (C,
         Make_Designator
         (Designator => PN (P_Exception_Info),
          Parent     => VN (V_Request)));
      Append_Node_To_List (N, P);
      N := Make_Subprogram_Call
        (RE (RE_Destroy_Request),
         Make_List_Id
         (Make_Designator (VN (V_Request))));
      Append_Node_To_List (N, P);
      N := Make_Subprogram_Call
        (RE (RE_Raise_From_Any),
         Make_List_Id (Copy_Node (C)));
      Append_Node_To_List (N, P);
      N := Make_Subprogram_Call
        (RE (RE_Is_Empty),
         Make_List_Id (Make_Designator
         (Designator => PN (P_Exception_Info),
          Parent     => VN (V_Request))));
      N := Make_Expression (N, Op_Not);
      N := Make_If_Statement
        (N, P, No_List);
      Append_Node_To_List (N, Statements);
      N := Make_Subprogram_Call
        (RE (RE_Destroy_Request),
         Make_List_Id (Make_Designator (VN (V_Request))));
      Append_Node_To_List (N, Statements);

      --  Retrieve return value

      if Present (Return_T) then

         if Is_Base_Type (BEN.FE_Node (Return_T)) then
            N := RE (RE_From_Any_0);
         else
            N := Identifier (FE_Node (Return_T));
            N := Helper_Node
              (BE_Node (Identifier (Reference (Corresponding_Entity (N)))));
            N := Expand_Designator (Next_Node (N));
         end if;
         C := Make_Subprogram_Call
           (RE (RE_To_CORBA_Any),
            Make_List_Id (Copy_Node (C)));
         N := Make_Return_Statement
           (Make_Subprogram_Call (N, Make_List_Id (C)));
         Append_Node_To_List (N, Statements);
      end if;

      return Statements;
   end Marshaller_Body;

   -----------------------------
   -- Marshaller_Declarations --
   -----------------------------

   function Marshaller_Declarations (Subp_Spec : Node_Id) return List_Id is
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

      --  Arg_List_U declaration

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

         if Is_Base_Type (BEN.FE_Node (Parameter_Type (I))) then

            if BEN.Parameter_Mode (I) = Mode_Out then
               D := RE (RE_Get_Empty_Any_0);
            else
               D := RE (RE_To_Any_0);
            end if;

         else
            if BEN.Parameter_Mode (I) = Mode_Out then
               D := RE (RE_Get_Empty_Any_1);
               Set_Parent_Unit_Name
                 (D, Defining_Identifier (Helper_Package (Current_Entity)));

            else
               D := Identifier (FE_Node (Parameter_Type (I)));
               D := Helper_Node
                 (BE_Node (Identifier (Reference (Corresponding_Entity (D)))));
               D := Expand_Designator (Next_Node (Next_Node (D)));
            end if;

         end if;

         C :=  Make_Subprogram_Call
           (Defining_Identifier   => D,
            Actual_Parameter_Part =>
              Make_List_Id (Make_Defining_Identifier (X)));

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
         Object_Definition   => RE (RE_String_0),
         Expression          => C);
      Append_Node_To_List (N, L);

      return L;
   end Marshaller_Declarations;

   ---------------------
   -- Local_Is_A_Body --
   ---------------------

   function Local_Is_A_Body (E : Node_Id) return Node_Id is
      N             : Node_Id;
      S             : constant List_Id := New_List (K_List_Id);
      M             : Node_Id;
      Repository_Id : Node_Id;
      Object_URL    : Value_Id;

   begin
      N := Stub_Node (BE_Node (Identifier (E)));
      N := Next_Node (N);
      Repository_Id := Expand_Designator (N);
      N := Make_Subprogram_Call
        (RE (RE_Is_Equivalent),
         Make_List_Id
         (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
          Repository_Id));

      if FEN.Kind (E) = K_Interface_Declaration then
         Set_Str_To_Name_Buffer ("IDL:Omg.Org/CORBA/Object:1.0");
      else
         Set_Str_To_Name_Buffer ("IDL:Omg.Org/CORBA/ValueBase:1.0");
      end if;

      Object_URL := New_String_Value (Name_Find, False);
      M := Make_Subprogram_Call
        (RE (RE_Is_Equivalent),
         Make_List_Id
         (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
          Make_Literal (Object_URL)));
      N := Make_Expression
        (N, Op_Or_Else,
         Make_Expression
         (M, Op_Or_Else,
          RE (RE_False)));
      N := Make_Return_Statement (N);
      Append_Node_To_List (N, S);
      N := Make_Subprogram_Implementation
        (Local_Is_A_Spec, No_List, S);
      return N;
   end Local_Is_A_Body;

   ---------------------
   -- Local_Is_A_Spec --
   ---------------------

   function Local_Is_A_Spec return Node_Id is
      N       : Node_Id;
      Profile : List_Id;
      Param   : Node_Id;
   begin
      Param := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
         RE (RE_String_2));
      Profile := New_List (K_Parameter_Profile);
      Append_Node_To_List (Param, Profile);
      N := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_Is_A)),
         Profile,
         RE (RE_Boolean));
      return N;
   end Local_Is_A_Spec;

   -----------------------
   -- Visible_Is_A_Body --
   -----------------------

   function Visible_Is_A_Body return Node_Id is
      N : Node_Id;
      M : Node_Id;
      S : constant List_Id := New_List (K_List_Id);
   begin
      M := Make_Subprogram_Call
        (RE (RE_Ref_1),
         Make_List_Id (Make_Defining_Identifier (PN (P_Self))));
      M := Make_Subprogram_Call
        (RE (RE_Is_A),
         Make_List_Id (M, Make_Defining_Identifier (PN (P_Logical_Type_Id))));
      N := Make_Subprogram_Call
        (Make_Defining_Identifier (SN (S_Is_A)),
         Make_List_Id (Make_Designator (PN (P_Logical_Type_Id))));
      N := Make_Expression
        (RE (RE_False),
         Op_Or_Else,
         Make_Expression
         (N,
          Op_Or_Else,
          M));
      N := Make_Return_Statement (N);
      Append_Node_To_List (N, S);
      N := Make_Subprogram_Implementation
        (Visible_Is_A_Spec, No_List, S);
      return N;
   end Visible_Is_A_Body;

   -----------------------
   -- Visible_Is_A_Spec --
   -----------------------

   function Visible_Is_A_Spec return Node_Id is
      N       : Node_Id;
      Profile : List_Id;
      Param   : Node_Id;
   begin
      Profile := New_List (K_Parameter_Profile);
      Param := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Self)),
         Make_Defining_Identifier (TN (T_Ref)));
      Append_Node_To_List (Param, Profile);
      Param := Make_Parameter_Specification
        (Make_Defining_Identifier (PN (P_Logical_Type_Id)),
         RE (RE_String_2));
      Append_Node_To_List (Param, Profile);
      N := Make_Subprogram_Specification
        (Make_Defining_Identifier (SN (S_Is_A)),
         Profile,
         RE (RE_Boolean));
      return N;
   end Visible_Is_A_Spec;
end Backend.BE_Ada.Stubs;
