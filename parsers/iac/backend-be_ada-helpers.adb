with Frontend.Nodes; use Frontend.Nodes;
--  with Frontend.Debug;

with Backend.BE_Ada.Expand; use Backend.BE_Ada.Expand;
--  with Backend.BE_Ada.Debug;
with Backend.BE_Ada.Nodes; use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime; use Backend.BE_Ada.Runtime;


package body Backend.BE_Ada.Helpers is

   package FEN renames Frontend.Nodes;
   package BEN renames Backend.BE_Ada.Nodes;

   package body Package_Spec is

      procedure Any_Conversion_Spec (T : Node_Id);
      --  Insert an any conversions functions for a given type
      --  (T) node in the helper package.
      procedure Narrowing_Ref_Spec (E : Node_Id);
      --  Insert windening object reference helper.
      procedure TypeCode_Spec (T : Node_Id);
      --  Insert a TypeCode constant for a given type (T) node in the Helper
      --  package.
      procedure Widening_Ref_Spec (E : Node_Id);
      --  Insert widening object reference helper.
      pragma Unreferenced (Narrowing_Ref_Spec, TypeCode_Spec,
                             Any_Conversion_Spec);

      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -------------------------
      -- Any_Conversion_Spec --
      -------------------------

      procedure Any_Conversion_Spec (T : Node_Id) is
         pragma Unreferenced (T);
      begin
         null;
      end Any_Conversion_Spec;

      ------------------------
      -- Narrowing_Ref_Spec --
      ------------------------

      procedure Narrowing_Ref_Spec (E : Node_Id) is
         pragma Unreferenced (E);
      begin
         null;
      end Narrowing_Ref_Spec;

      -------------------
      -- TypeCode_Spec --
      -------------------

      procedure TypeCode_Spec (T : Node_Id) is
         pragma Unreferenced (T);
      begin
         null;
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
         Widening_Ref_Spec (E);
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

      -----------------------
      -- Widening_Ref_Spec --
      -----------------------

      procedure Widening_Ref_Spec (E : Node_Id) is
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
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
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
