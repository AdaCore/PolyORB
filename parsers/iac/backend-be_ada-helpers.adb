with Namet;     use Namet;

with Frontend.Nodes;   use Frontend.Nodes;

with Backend.BE_Ada.Expand;  use Backend.BE_Ada.Expand;
with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime; use Backend.BE_Ada.Runtime;


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
      --  return an any conversions functions for a given type
      --  (T) node in the helper package.
      function Narrowing_Ref_Spec
        (E : Node_Id)
        return Node_Id;
      --  return windening object reference helper.
      function TypeCode_Spec
        (E : Node_Id)
        return Node_Id;
      --  return a TypeCode constant for a given type (T) node in the Helper
      --  package.
      function Widening_Ref_Spec
        (E : Node_Id)
        return Node_Id;
      --  return widening object reference helper.

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);

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
            Expand_Designator (BE_Node (Identifier (E))));
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
           (Type_Definition (BE_Node (Identifier (E))));
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_The_Ref)),
            Make_Type_Attribute
            (Copy_Designator (N), A_CLASS));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Unchecked_To_Ref)),
            Profile, Expand_Designator
            (BE_Node (Identifier (E))));
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
            Expand_Designator (BE_Node (Identifier (E))));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Any)),
            Profile, RE (RE_Any));
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
      begin
         Set_Str_To_Name_Buffer ("TC_");
         Get_Name_String_And_Append (FEN.Name (Identifier (E)));
         TC := Name_Find;
         C := Make_Subprogram_Call
           (Defining_Identifier   => RE (RE_TC_Object),
            Actual_Parameter_Part => No_List);
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (TC),
            Constant_Present    => False,
            Object_Definition   => RE (RE_Object),
            Expression          => C);
         return N;
      end TypeCode_Spec;

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

            when K_Type_Declaration =>
               Visit_Type_Declaration (E);

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
         N := BEN.Parent (BE_Node (Identifier (E)));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Helper_Spec;

         Append_Node_To_List
           (Widening_Ref_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (Narrowing_Ref_Spec (E), Visible_Part (Current_Package));

         N := Package_Declaration (BEN.Parent (BE_Node (Identifier (E))));
         Append_Node_To_List
           (TypeCode_Spec (N), Visible_Part (Current_Package));
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
         Push_Entity (BE_Node (Identifier (E)));
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
         Push_Entity (BE_Node (Identifier (E)));
         Definition := First_Entity (Definitions (E));
         while Present (Definition) loop
            Visit (Definition);
            Definition := Next_Entity (Definition);
         end loop;
         Pop_Entity;
      end Visit_Specification;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
      begin
         Append_Node_To_List
           (TypeCode_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (From_Any_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (To_Any_Spec (E), Visible_Part (Current_Package));
      end Visit_Type_Declaration;

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
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Self)),
            Expand_Designator
            (BE_Node (Identifier (E))));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Ref)), Profile, RE (RE_Any));
         return N;
      end Widening_Ref_Spec;

   end Package_Spec;

   package body Package_Body is

      -------------------------
      -- Any_Conversion_Body --
      -------------------------

      procedure Any_Conversion_Body (T : Node_Id) is
         pragma Unreferenced (T);
      begin
         null;
      end Any_Conversion_Body;

      ------------------------
      -- Narrowing_Ref_Body --
      ------------------------

      procedure Narrowing_Ref_Body (E : Node_Id) is
         pragma Unreferenced (E);
      begin
         null;
      end Narrowing_Ref_Body;

      -------------------
      -- TypeCode_Body --
      -------------------

      procedure TypeCode_Body (T : Node_Id) is
         pragma Unreferenced (T);
      begin
         null;
      end TypeCode_Body;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
         pragma Unreferenced (E);
      begin
         null;
      end Visit;

      procedure Widening_Ref_Body (E : Node_Id) is
         pragma Unreferenced (E);
      begin
         null;
      end Widening_Ref_Body;

   end Package_Body;

end Backend.BE_Ada.Helpers;
