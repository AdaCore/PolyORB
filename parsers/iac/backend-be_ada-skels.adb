with Namet;  use Namet;
with Values; use Values;

with Frontend.Nodes;  use Frontend.Nodes;

with Backend.BE_Ada.Nodes;  use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;  use Backend.BE_Ada.Runtime;

package body Backend.BE_Ada.Skels is
   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;

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
         N := BEN.Parent (Stub_Node (BE_Node (Identifier (E))));
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

      function Add_Underscore (N : Name_Id) return Name_Id;

      procedure Invoke_Declaration (L : List_Id);
      function Invoke_Spec return Node_Id;

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      --------------------
      -- Add_Underscore --
      --------------------

      function Add_Underscore (N : Name_Id) return Name_Id is
      begin
         Get_Name_String (N);
         Add_Char_To_Name_Buffer ('_');
         return Name_Find;
      end Add_Underscore;

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
            Object_Definition   => RE (RE_String_1),
            Expression          => N);
         Append_Node_To_List (N, L);
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (VN (V_Argument_List)),
            Object_Definition   => RE (RE_Ref_3));
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
         N                 : Node_Id;
         Spec              : Node_Id;
         D                 : constant List_Id := New_List (K_List_Id);
         C                 : Node_Id;
         Then_Statements   : constant List_Id := New_List (K_List_Id);
         Else_Statements   : constant List_Id := New_List (K_List_Id);
         Invoke_Statements : constant List_Id := New_List (K_List_Id);

      begin
         N := BEN.Parent (Stub_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Skeleton_Body;
         Invoke_Elsif_Statements := New_List (K_List_Id);

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

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
             (Add_Underscore (SN (S_Is_A)), False)));
         N := Make_If_Statement
           (C,
            Then_Statements,
            Invoke_Elsif_Statements,
            Else_Statements);
         Append_Node_To_List (N, Invoke_Statements);
         N := Make_Subprogram_Implementation
           (Spec, D, Invoke_Statements);
         Append_Node_To_List (N, Statements (Current_Package));
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

   end Package_Body;
end Backend.BE_Ada.Skels;
