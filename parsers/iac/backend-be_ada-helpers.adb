with Namet;      use Namet;
with Values;     use Values;
with Types;      use Types;

with Frontend.Nodes;   use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_Ada.Expand;  use Backend.BE_Ada.Expand;
with Backend.BE_Ada.IDL_To_Ada;  use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime; use Backend.BE_Ada.Runtime;
pragma Warnings (off);
with Frontend.Debug;
with Backend.BE_Ada.Debug;
pragma Warnings (on);

package body Backend.BE_Ada.Helpers is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_Ada.Nodes;

   package body Package_Spec is

      function From_Any_Spec
        (E : Node_Id)
        return Node_Id;

      function From_Any_Spec_Ex
        (E : Node_Id)
        return Node_Id;
      --  convert the exception memebers to the "any" type

      function To_Any_Spec
        (E : Node_Id)
        return Node_Id;
      --  return "any" conversions functions for a given type (E).

      function To_Any_Spec_Ex
        (E : Node_Id)
        return Node_Id;
      --  return an ""any" conversion function for the particular case of
      --  exceptions

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

      function Raise_Excp_Spec
        (Excp_Members : Node_Id;
         Raise_Node   : Node_Id)
        return Node_Id;
      --  return the spec of the Raise_"Exception_Name" procedure

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);

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
         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram
         Set_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         Set_FE_Node (N, Identifier (E));
         return N;
      end From_Any_Spec;

      ----------------------
      -- From_Any_Spec_Ex --
      ----------------------

      function From_Any_Spec_Ex
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
            Defining_Identifier (E));
         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram
         Set_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end From_Any_Spec_Ex;

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
            (Copy_Designator (N), A_Class));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Unchecked_To_Ref)),
            Profile, Expand_Designator
            (Stub_Node (BE_Node (Identifier (E)))));
         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram
         Set_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
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
         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram
         Set_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         Set_FE_Node (N, Identifier (E));
         return N;
      end To_Any_Spec;

      --------------------
      -- To_Any_Spec_Ex --
      --------------------

      function To_Any_Spec_Ex
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
            Defining_Identifier (E));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Any)),
            Profile, RE (RE_Any));
         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram
         Set_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end To_Any_Spec_Ex;

      ---------------------
      -- Raise_Excp_Spec --
      ---------------------

      function Raise_Excp_Spec
        (Excp_Members : Node_Id;
         Raise_Node   : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Members)),
            Defining_Identifier (Excp_Members));
         Append_Node_To_List (Parameter, Profile);

         N := Make_Subprogram_Specification
           (Raise_Node,
            Profile);
         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram
         Set_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end Raise_Excp_Spec;

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
               P := RE (RE_TC_Object_1);

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

            when K_Exception_Declaration =>
               P := RE (RE_TC_Except);

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
         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram
         Set_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
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

            when K_Exception_Declaration =>
               Visit_Exception_Declaration (E);

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
         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Append_Node_To_List
           (From_Any_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (To_Any_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (Narrowing_Ref_Spec (E), Visible_Part (Current_Package));
         Append_Node_To_List
           (Widening_Ref_Spec (E), Visible_Part (Current_Package));
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

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         N            : Node_Id;
         Excp_Members : Node_Id;
         Excp_Name    : Name_Id;
         Raise_Node   : Node_Id;
      begin
         Set_Helper_Spec;
         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);

         --  Obtaining the node corresponding to the declaration of the
         --  "Excp_Name"_Members type.
         --  This type was declared in the third position in the stub spec.

         Excp_Members := Stub_Node (BE_Node (Identifier (E)));
         Excp_Members := Next_Node (Next_Node (Excp_Members));
         Append_Node_To_List
           (From_Any_Spec_Ex (Excp_Members),
            Visible_Part (Current_Package));
         Append_Node_To_List
           (To_Any_Spec_Ex (Excp_Members),
            Visible_Part (Current_Package));

         --  Generation of the Raise_"Exception_Name" spec

         Excp_Name := To_Ada_Name (IDL_Name (FEN.Identifier (E)));
         Raise_Node := Make_Defining_Identifier
           (Add_Prefix_To_Name ("Raise_", Excp_Name));
         Append_Node_To_List
           (Raise_Excp_Spec (Excp_Members, Raise_Node),
            Visible_Part (Current_Package));

         --  Addition of the pragma No_Return
         --  The Argument of the pargma No_Return must be a local name
         N := Make_Subprogram_Call
           (Make_Defining_Identifier (GN (Pragma_No_Return)),
            Make_List_Id
            (Make_Designator
             (BEN.Name (Raise_Node))));
         N := Make_Pragma_Statement (N);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));

      end Visit_Exception_Declaration;

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
            (Copy_Designator (N), A_Class));
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

      function TC_Designator
        (E : Node_Id)
        return Node_Id;
      --  return a designator for a non-base type

      function Raise_Excp_From_Any_Spec
        (Raise_Node : Node_Id)
        return Node_Id;
      --  The spec is situated in the body because this function is not used
      --  outside the helper package. Hoewever the spec is necessary because
      --  of the pragma No_Return.

      function Raise_Excp_From_Any_Body
        (E          : Node_Id;
         Raise_Node : Node_Id)
        return Node_Id;

      function Raise_Excp_Body
        (E : Node_Id)
        return Node_Id;

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);

      -------------------
      -- TC_Designator --
      -------------------
      function TC_Designator
        (E : Node_Id)
        return Node_Id
      is
         T      : Node_Id;
         Result : Node_Id;
         TC     : Name_Id;
      begin
         T := E;
         T := Reference (T);
         T := Stub_Node (BE_Node (Identifier (T)));
         TC := Add_Prefix_To_Name
           ("TC_", BEN.Name (Defining_Identifier (T)));
         Result := Make_Designator (TC);
         return Result;
      end TC_Designator;

      -----------------------------------
      -- Deferred_Initialization_Block --
      -----------------------------------

      function Deferred_Initialization_Block
        (E : Node_Id)
         return Node_Id
      is
         function Add_Parameter
           (TC_Name  : Name_Id;
            Var_Node : Node_Id)
            return Node_Id;

         function Declare_Name
           (Var_Name  : Name_Id;
            Value : Value_Id)
            return Node_Id;

         --  To handle the case of multi-dimension arrays. A TypeCode variable
         --  is declared for each dimension of an array.
         function Declare_Dimension
           (Var_Name  : Name_Id)
           return Node_Id;

         --  Generation of a new variable name to designate a given dimension
         --  of the array.
         function Get_Dimension_Variable_Name
           (Dimension : Natural)
           return Name_Id;

         -------------------
         -- Add_Parameter --
         -------------------

         function Add_Parameter
           (TC_Name  : Name_Id;
            Var_Node : Node_Id)
            return Node_Id
         is
            N : Node_Id;
         begin
            N := Make_Subprogram_Call
              (RE (RE_To_Any_0),
               Make_List_Id (Var_Node));
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

         -----------------------
         -- Declare_Dimension --
         -----------------------

         function Declare_Dimension
           (Var_Name  : Name_Id)
            return Node_Id
         is
            N : Node_Id;
         begin
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (Var_Name),
               Object_Definition   => RE (RE_Object),
               Expression          =>
                 Make_Subprogram_Call
               (RE (RE_To_CORBA_Object),
                Make_List_Id (RE (RE_TC_Array))));
            return N;
         end Declare_Dimension;

         ---------------------------------
         -- Get_Dimension_Variable_Name --
         ---------------------------------

         function Get_Dimension_Variable_Name
           (Dimension : Natural)
           return Name_Id
         is
            Result : Name_Id;
         begin
            Set_Nat_To_Name_Buffer (Nat (Dimension));
            Result := Name_Find;
            Result := Add_Prefix_To_Name ("TC_", Result);
            return Result;
         end Get_Dimension_Variable_Name;

         Stub             : Node_Id;
         Helper           : Node_Id;
         N                : Node_Id;
         Entity_TC_Name   : Name_Id;
         Entity_Name_V    : Value_Id;
         Entity_Rep_Id_V  : Value_Id;
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
         Param1           : Node_Id;
         Param2           : Node_Id;
      begin
         Stub :=  Stub_Node (BE_Node (Identifier (E)));
         Entity_Rep_Id_V := BEN.Value (BEN.Expression (Next_Node (Stub)));
         Helper := Helper_Node (BE_Node (Identifier (E)));

         case FEN.Kind (E) is
            when K_Interface_Declaration =>
               Stub := Package_Declaration
                 (BEN.Parent (Stub));

            when K_Complex_Declarator =>
               declare
                  V                : Value_Type;
                  TC_Dim           : Node_Id          := No_Node;
                  TC_Previous_Name : Name_Id          := No_Name;
                  TC_Name          : Name_Id          := No_Name;
                  Sizes            : constant List_Id :=
                    Range_Constraints
                    (Type_Definition (Stub_Node (BE_Node (Identifier (E)))));
                  Sizes_Reverse    : constant List_Id := New_List (K_List_Id);
                  Constraint       : Node_Id;
                  Dimension        : constant Natural := Length (Sizes);
                  From_N           : Node_Id          := No_Node;
                  To_N             : Node_Id          := No_Node;
                  T                : Node_Id;
               begin
                  --  If the dimension of the array is greater than 1, we must
                  --  generate a TypeCode variable (TC_1, TC_2...) for each
                  --  dimension. The last variable name is TC_"Array_Name".

                  if Dimension > 1 then

                     --  First of all, we create a new list which contains the
                     --  elements of the list Sizes. All manipulations on this
                     --  list will not affect the list Sizes because we create
                     --  new nodes.

                     From_N := First_Node (Sizes);
                     while Present (From_N) loop
                        To_N := New_Node (K_Range_Constraint);
                        Set_Last (To_N, Last (From_N));
                        Append_Node_To_List
                          (To_N,
                           Sizes_Reverse);
                        From_N := Next_Node (From_N);
                     end loop;

                     --  Then, starting from the last node of the new list, we
                     --  take the corresponding size and we generate the TC_
                     --  variable. The first variable (the deepest dimension)
                     --  is the one containing the real type of the array.

                     Constraint := Last_Node (Sizes_Reverse);
                     for Index in 1 .. Dimension - 1 loop
                        TC_Previous_Name := TC_Name;
                        TC_Name := Get_Dimension_Variable_Name (Index);
                        TC_Dim  := Declare_Dimension (TC_Name);
                        V := Value (Last (Constraint));
                        V.IVal := V.IVal + 1;
                        Append_Node_To_List (TC_Dim, Declarative_Part);
                        Param1 := Make_Subprogram_Call
                          (RE (RE_Unsigned_Long),
                           Make_List_Id
                           (Make_Literal (New_Value (V))));

                        if TC_Previous_Name = No_Name then
                           --  The deepest dimension

                           --  If the type of the array is a base type, then
                           --  we have immediate access to this type name. The
                           --  personal types need more work to get the type
                           --  name.
                           T := Type_Spec (Declaration (E));
                           if Is_Base_Type (T) then
                              Param2 := Base_Type_TC
                                (FEN.Kind (T));
                           elsif FEN.Kind (T) = K_Scoped_Name then
                              Param2 := TC_Designator (T);
                           else
                              raise Program_Error;
                           end if;
                        else --  Not the deepest dimension
                           Param2 := Make_Designator (TC_Previous_Name);
                        end if;

                        TC_Dim := Add_Parameter
                          (TC_Name,
                           Param1);
                        Append_Node_To_List (TC_Dim, Statements);
                        TC_Dim := Add_Parameter
                          (TC_Name,
                           Param2);
                        Append_Node_To_List (TC_Dim, Statements);
                        Remove_Node_From_List (Constraint, Sizes_Reverse);
                        Constraint := Last_Node (Sizes_Reverse);
                     end loop;

                     --  The case of the last TC_ variable which represents the
                     --  whole array is handled apart.

                     V := Value (Last (Constraint));
                     V.IVal := V.IVal + 1;
                     Param1 := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id
                        (Make_Literal (New_Value (V))));
                     Param2 := Make_Designator (TC_Name);

                  else --  1 dimension array

                     V := Value (Last (First_Node (Sizes)));
                     V.IVal := V.IVal + 1;
                     Param1 := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id
                        (Make_Literal (New_Value (V))));

                     --  If the type of the array is a base type, then
                     --  we have immediate access to this type name. The
                     --  personal types need more work to get the type
                     --  name.
                     T := Type_Spec (Declaration (E));
                     if Is_Base_Type (T) then
                        Param2 := Base_Type_TC (FEN.Kind (T));
                     elsif FEN.Kind (T) = K_Scoped_Name then
                        Param2 := TC_Designator (T);
                     else
                        raise Program_Error;
                     end if;
                  end if;
               end;

            when K_Enumeration_Type =>
               null;

            when K_Structure_Type =>
               null;

            when K_Exception_Declaration =>
               null;

            when others =>
               raise Program_Error;
         end case;

         if FEN.Kind (E) /= K_Complex_Declarator then
            Param1 := Make_Designator (VN (V_Name));
            Param2 := Make_Designator (VN (V_Id));

            --  Name_U declaration

            Entity_Name_V := New_String_Value
              (BEN.Name (Defining_Identifier (Stub)), False);
            N := Declare_Name (VN (V_Name), Entity_Name_V);
            Append_Node_To_List (N, Declarative_Part);

            --  Id_U declaration

            N := Declare_Name (VN (V_Id), Entity_Rep_Id_V);
            Append_Node_To_List (N, Declarative_Part);
         end if;

         Entity_TC_Name := BEN.Name (Defining_Identifier (Helper));
         N := Add_Parameter (Entity_TC_Name, Param1);
         Append_Node_To_List (N, Statements);
         N := Add_Parameter (Entity_TC_Name, Param2);
         Append_Node_To_List (N, Statements);

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               declare
                  Enumerators : List_Id;
                  Enum_Item   : Node_Id;
                  Var_Name    : Name_Id;
               begin
                  Enumerators := Enumeration_Literals
                    (Type_Definition (Stub));
                  Enum_Item := First_Node (Enumerators);
                  loop
                     Var_Name := Add_Prefix_To_Name
                       (Get_Name_String (BEN.Name (Enum_Item)), VN (V_Name));
                     Param1 := Make_Designator (Var_Name);
                     N := Declare_Name
                       (Var_Name,
                        New_String_Value (BEN.Name (Enum_Item), False));
                     Append_Node_To_List (N, Declarative_Part);
                     N := Add_Parameter (Entity_TC_Name, Param1);
                     Append_Node_To_List (N, Statements);
                     Enum_Item := Next_Node (Enum_Item);
                     exit when No (Enum_Item);
                  end loop;
               end;

            when K_Structure_Type =>
               declare
                  Member     : Node_Id;
                  Declarator : Node_Id;
                  Designator : Node_Id;
                  Arg_Name   : Name_Id;
               begin
                  Member := First_Entity (Members (E));
                  while Present (Member) loop
                     Declarator := First_Entity (Declarators (Member));

                     while Present (Declarator) loop
                        Designator := Map_Designator (Declarator);
                        Get_Name_String (VN (V_Argument_Name));
                        Add_Char_To_Name_Buffer ('_');
                        Get_Name_String_And_Append
                          (BEN.Name (Defining_Identifier (Designator)));
                        Arg_Name := Name_Find;
                        N := Make_Literal
                          (New_String_Value
                           (BEN.Name (Defining_Identifier (Designator)),
                           False));
                        N := Make_Subprogram_Call
                          (RE (RE_To_CORBA_String),
                           Make_List_Id (N));
                        N := Make_Object_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier (Arg_Name),
                           Object_Definition   => RE (RE_String_0),
                           Expression          => N);
                        Append_Node_To_List (N, Declarative_Part);

                        if Is_Base_Type
                          (Type_Spec (Declaration (Declarator)))
                        then
                           Param1 := Base_Type_TC
                             (FEN.Kind (Type_Spec (Declaration (Declarator))));
                        else
                           raise Program_Error;
                        end if;

                        Param2 := Make_Designator (Arg_Name);
                        N := Add_Parameter (Entity_TC_Name, Param1);
                        Append_Node_To_List (N, Statements);
                        N := Add_Parameter (Entity_TC_Name, Param2);
                        Append_Node_To_List (N, Statements);

                        Declarator := Next_Entity (Declarator);
                     end loop;
                     Member := Next_Entity (Member);
                  end loop;
               end;

            when K_Exception_Declaration =>

               declare
                  Raise_From_Any_Access_Node : Node_Id;
                  Raise_From_Any_Name        : Name_Id;
                  Member                     : Node_Id;
                  Members                    : List_Id;
                  Declarator                 : Node_Id;
                  Dcl_Name                   : Name_Id;
                  Arg_Name_Node              : Node_Id;
               begin
                  --  In case where the exception has members, we add two
                  --  two parameter for each member.

                  Members := FEN.Members (E);
                  if not FEU.Is_Empty (Members) then
                     Member := First_Entity (Members);
                     while Present (Member) loop
                        Declarator := First_Entity (Declarators (Member));
                        while Present (Declarator) loop

                           --  Declaring the Arg_Name_"member" variable

                           Dcl_Name := To_Ada_Name
                             (IDL_Name (FEN.Identifier (Declarator)));
                           Set_Str_To_Name_Buffer ("Arg_Name_");
                           Get_Name_String_And_Append (Dcl_Name);
                           Arg_Name_Node := Make_Defining_Identifier
                             (Name_Find);

                           --  Obtaining a string literal of the member name

                           N := Make_Subprogram_Call
                             (RE (RE_To_CORBA_String),
                              Make_List_Id
                              (Make_Literal
                               (New_Value
                                (Value_Type'
                                 (K    => K_String,
                                  SVal => Dcl_Name)))));

                           N := Make_Object_Declaration
                             (Defining_Identifier => Arg_Name_Node,
                              Object_Definition   => RE (RE_String_0),
                              Expression          => N);
                           Append_Node_To_List (N, Declarative_Part);

                           --  Adding the two additional parameters

                           if Is_Base_Type (Type_Spec (Member)) then
                              N := Base_Type_TC
                                (FEN.Kind (Type_Spec (Member)));
                           else
                              raise Program_Error;
                           end if;
                           N := Add_Parameter (Entity_TC_Name, N);
                           Append_Node_To_List (N, Statements);
                           N := Add_Parameter (Entity_TC_Name, Arg_Name_Node);
                           Append_Node_To_List (N, Statements);

                           Declarator := Next_Entity (Declarator);
                        end loop;
                        Member := Next_Entity (Member);
                     end loop;
                  end if;

                  --  Adding the call to the "Register_Exception" procedure

                  Raise_From_Any_Access_Node := Helper_Node
                    (BE_Node (Identifier (E)));
                  Raise_From_Any_Access_Node := Next_Node
                    (Next_Node
                     (Next_Node
                      (Raise_From_Any_Access_Node)));
                  Raise_From_Any_Access_Node := Defining_Identifier
                    (Raise_From_Any_Access_Node);

                  --  The following workaround is due to the fact that we have
                  --  no direct acceess to the "Exception_Name"_Raise_From_Any
                  --  procedure node because its spec is declared in the helper
                  --  body and not in the helper spec and is not used outside
                  --  the helper package.

                  Raise_From_Any_Name := BEN.Name (Raise_From_Any_Access_Node);
                  Raise_From_Any_Name := Add_Suffix_To_Name
                    ("_From_Any",  Raise_From_Any_Name);
                  Raise_From_Any_Access_Node := Make_Designator
                    (Raise_From_Any_Name);
                  Raise_From_Any_Access_Node := Make_Attribute_Designator
                    (Raise_From_Any_Access_Node, A_Access);
                  N := Make_Subprogram_Call
                    (RE (RE_To_PolyORB_Object),
                     Make_List_Id
                     (Make_Designator
                      (Entity_TC_Name)));
                  N := Make_Subprogram_Call
                    (RE (RE_Register_Exception),
                     Make_List_Id
                     (N, Raise_From_Any_Access_Node));
                  Append_Node_To_List (N, Statements);
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
         M           : Node_Id;
         Spec        : Node_Id;
         D           : constant List_Id := New_List (K_List_Id);
         S           : constant List_Id := New_List (K_List_Id);
         function Complex_Declarator_Body (E : Node_Id) return Node_Id;
         function Enumeration_Type_Body (E : Node_Id) return Node_Id;
         function Interface_Declaration_Body (E : Node_Id) return Node_Id;
         function Simple_Declarator_Body (E : Node_Id) return Node_Id;
         function Structure_Type_Body (E : Node_Id) return Node_Id;
         function Union_Type_Body (E : Node_Id) return Node_Id;
         function Exception_Declaration_Body (E : Node_Id) return Node_Id;

         -----------------------------
         -- Complex_Declarator_Body --
         -----------------------------

         function Complex_Declarator_Body (E : Node_Id) return Node_Id is
            I                    : Integer := 0;
            Sizes                : constant List_Id :=
              Range_Constraints
              (Type_Definition (Stub_Node (BE_Node (Identifier (E)))));
            Dim                  : Node_Id;
            Loop_Statements      : List_Id := No_List;
            Enclosing_Statements : List_Id;
            Index_List           : constant List_Id
              := New_List (K_List_Id);
            Item_Offset          : Node_Id;
            Tmp_Expr             : Node_Id;
            V                    : Value_Type;
            Helper               : Node_Id;
            TC                   : Node_Id;
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Spec);
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition =>
                 Copy_Designator (Return_Type (Spec)));
            Append_Node_To_List (N, D);

            Item_Offset := New_Node (K_Expression);
            Dim := First_Node (Sizes);
            loop
               Set_Str_To_Name_Buffer ("I");
               Add_Str_To_Name_Buffer (Img (I));
               M := Make_Defining_Identifier
                 (Add_Suffix_To_Name (Var_Suffix, Name_Find));
               Append_Node_To_List (M, Index_List);
               Enclosing_Statements := Loop_Statements;
               Loop_Statements := New_List (K_List_Id);
               N := Make_For_Statement
                 (M, Dim, Loop_Statements);

               if I > 0 then
                  Append_Node_To_List (N, Enclosing_Statements);
               else
                  Append_Node_To_List (N, S);
               end if;

               V := Value (Last (Dim));
               V.IVal := V.IVal + 1;
               I := I + 1;

               --  Algorithm for building the expression that increments the
               --  offset at each iteration :
               --  * If there is one dimension : the offset is equal to the
               --    counter
               --  * If there is more than one dimension :
               --    offset = V * offset + counter
               --    where V is the size of the range of the current dimension
               --    and counter is the counter of the current dimension.

               if I = 1 then
                  BEN.Set_Left_Expr (Item_Offset, Copy_Node (M));
                  BEN.Set_Operator
                    (Item_Offset, Operator_Type'Pos (Op_None));
               else

                  --  Building the V * offset part

                  Tmp_Expr := New_Node (K_Expression);
                  BEN.Set_Left_Expr (Tmp_Expr, Make_Literal (New_Value (V)));
                  BEN.Set_Operator (Tmp_Expr, Operator_Type'Pos (Op_Asterisk));
                  BEN.Set_Right_Expr (Tmp_Expr, Item_Offset);

                  --  Adding the counter

                  Item_Offset := New_Node (K_Expression);
                  BEN.Set_Left_Expr (Item_Offset, Tmp_Expr);
                  BEN.Set_Operator (Item_Offset, Operator_Type'Pos (Op_Plus));
                  BEN.Set_Right_Expr (Item_Offset, Copy_Node (M));
               end if;
               Dim := Next_Node (Dim);
               exit when No (Dim);
            end loop;

            if Is_Base_Type (Type_Spec (Declaration (E))) then
               TC := Base_Type_TC (FEN.Kind (Type_Spec (Declaration (E))));
               Helper := RE (RE_From_Any_0);
            elsif FEN.Kind (Type_Spec (Declaration (E))) = K_Scoped_Name then
               TC := TC_Designator
                 (Type_Spec
                  (Declaration
                   (E)));
               declare
                  Reference : constant Node_Id := FEN.Reference
                    (Type_Spec (Declaration (E)));
               begin
                  --  As iac evolves, add the corresponding From_Any nodes
                  case FEN.Kind (Reference) is
                     when K_Enumeration_Type =>
                        Helper := Helper_Node
                          (BE_Node (Identifier (Reference)));
                        --  The From_Any function is declared at the second
                        --  place in the Helper spec
                        Helper := Next_Node (Helper);
                        Helper := Defining_Identifier (Helper);
                     when others =>
                        Helper := RE (RE_From_Any_0);
                  end case;
               end;
            else
               raise Program_Error;
            end if;

            N := Make_Subprogram_Call
              (Make_Defining_Identifier (PN (P_Result)),
               Index_List);
            M := Make_Subprogram_Call
              (RE (RE_Get_Aggregate_Element),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Item)),
                TC,
                Make_Subprogram_Call
                (RE (RE_Unsigned_Long),
                 Make_List_Id (Item_Offset))));
            M := Make_Subprogram_Call
              (Helper,
               Make_List_Id (M));
            N := Make_Assignment_Statement
              (N, M);
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
            Spec := Next_Node (Spec);  --  Second in the list of helpers
            N := Make_Subprogram_Call
              (RE (RE_Get_Aggregate_Element),
               Make_List_Id
               (Make_Designator (PN (P_Item)),
                RE (RE_TC_Unsigned_Long),
                Make_Subprogram_Call
                (RE (RE_Unsigned_Long),
                 Make_List_Id (Make_Literal (Int0_Val)))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Index)),
               Object_Definition   => RE (RE_Any),
               Expression          => N);
            Append_Node_To_List (N, D);

            N := Make_Object_Declaration
              (Make_Defining_Identifier (VN (V_Position)),
               True,
               RE (RE_Unsigned_Long),
               Make_Subprogram_Call
               (RE (RE_From_Any_0),
                Make_List_Id (Make_Designator (VN (V_Index)))));
            Append_Node_To_List (N, D);

            N := Copy_Designator (Return_Type (Spec));
            N := Make_Subprogram_Call
              (Make_Type_Attribute (N, A_Val),
               Make_List_Id (Make_Designator (VN (V_Position))));
            N := Make_Return_Statement (N);
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
            Spec := Next_Node (Spec);  --  Second in the list of helpers
            --  Spec := Next_Node (Next_Node (Spec));
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
            Result_Struct_Aggregate  : Node_Id;
            L                        : constant List_Id
              := New_List (K_List_Id);
            Result_Name              : Name_Id;
            Member                   : Node_Id;
            Declarator               : Node_Id;
            Designator               : Node_Id;
            V                        : Value_Type := Value (Int0_Val);
            TC                       : Node_Id;
            Helper                   : Node_Id;
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Spec);

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Index)),
               Object_Definition =>
                 RE (RE_Any));
            Append_Node_To_List (N, D);

            Member := First_Entity (Members (E));
            while Present (Member) loop
               Declarator := First_Entity (Declarators (Member));

               while Present (Declarator) loop
                  Designator := Map_Designator (Declarator);
                  Get_Name_String (VN (V_Result));
                  Add_Char_To_Name_Buffer ('_');
                  Get_Name_String_And_Append
                    (BEN.Name (Defining_Identifier (Designator)));
                  Result_Name := Name_Find;
                  N := Make_Component_Association
                    (Defining_Identifier (Designator),
                     Make_Defining_Identifier (Result_Name));
                  Append_Node_To_List (N, L);
                  N := Make_Object_Declaration
                    (Defining_Identifier =>
                       Make_Defining_Identifier (Result_Name),
                     Object_Definition =>
                       Copy_Designator
                     (Subtype_Indication
                      (Stub_Node (BE_Node (Identifier (Declarator))))));
                  Append_Node_To_List (N, D);

                  if Is_Base_Type (Type_Spec (Declaration (Declarator))) then
                     TC := Base_Type_TC
                       (FEN.Kind (Type_Spec (Declaration (Declarator))));
                     Helper := RE (RE_From_Any_0);
                  else
                     raise Program_Error;
                  end if;

                  N := Make_Subprogram_Call
                    (RE (RE_Get_Aggregate_Element),
                     Make_List_Id
                     (Make_Defining_Identifier (PN (P_Item)),
                      TC,
                      Make_Subprogram_Call
                      (RE (RE_Unsigned_Long),
                       Make_List_Id
                       (Make_Literal (New_Value (V))))));
                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (VN (V_Index)),
                     N);
                  Append_Node_To_List (N, S);
                  N := Make_Subprogram_Call
                    (Helper,
                     Make_List_Id (Make_Designator (VN (V_Index))));
                  N := Make_Assignment_Statement
                    (Make_Defining_Identifier (Result_Name),
                     N);
                  Append_Node_To_List (N, S);
                  V.IVal := V.IVal + 1;
                  Declarator := Next_Entity (Declarator);
               end loop;
               Member := Next_Entity (Member);
            end loop;
            Result_Struct_Aggregate := Make_Record_Aggregate (L);
            N := Make_Return_Statement
              (Result_Struct_Aggregate);
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Structure_Type_Body;

         ----------------
         -- Union_Body --
         ----------------

         function Union_Type_Body (E : Node_Id) return Node_Id is
            pragma Unreferenced (E);
         begin
            return No_Node;
         end Union_Type_Body;

         --------------------------------
         -- Exception_Declaration_Body --
         --------------------------------

         function Exception_Declaration_Body (E : Node_Id) return Node_Id is
            Members     : List_Id;
            Member      : Node_Id;
            Declarator  : Node_Id;
            Member_Id   : Node_Id;
            Member_Type : Node_Id;
            Dcl_Name    : Name_Id;
            Index       : Unsigned_Long_Long;
            Param_List  : List_Id;
            Return_List : List_Id;
            TC_Node     : Node_Id;
         begin
            --  Obtaining the "From_Any" spec node from the helper spec
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node (Spec);  --  Second in the list of helpers

            Members := FEN.Members (E);

            --  The generated code is fondamentally different depending on the
            --  existence or not of members in the exception.

            if FEU.Is_Empty (Members) then

               --  Begin Declarations

               --  Obtaining the node corresponding to the declaration of the
               --  "Excp_Name"_Members type.
               --  This type was declared in the third position in the Stub
               --  spec.

               N := Stub_Node (BE_Node (Identifier (E)));
               N := Next_Node (Next_Node (N));
               N := Defining_Identifier (N);
               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Result)),
                  Object_Definition   => N);
               Append_Node_To_List (N, D);

               --  Adding the necessary pragmas because the parameter of the
               --  function is unreferenced.

               N := Make_Subprogram_Call
                 (Make_Defining_Identifier (GN (Pragma_Warnings)),
                  Make_List_Id
                  (RE (RE_Off)));
               N := Make_Pragma_Statement (N);
               Append_Node_To_List (N, D);

               N := Make_Subprogram_Call
                 (Make_Defining_Identifier (GN (Pragma_Unreferenced)),
                  Make_List_Id
                  (Make_Designator (PN (P_Item))));
               N := Make_Pragma_Statement (N);
               Append_Node_To_List (N, D);

               N := Make_Subprogram_Call
                 (Make_Defining_Identifier (GN (Pragma_Warnings)),
                  Make_List_Id
                  (RE (RE_On)));
               N := Make_Pragma_Statement (N);
               Append_Node_To_List (N, D);

               --  End Declarations
               --  Begin Statements

               N := Make_Return_Statement (Make_Designator (VN (V_Result)));
               Append_Node_To_List (N, S);

               --  End Statements

            else

               --  Begin Declarations

               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Index)),
                  Object_Definition   => RE (RE_Any));
               Append_Node_To_List (N, D);

               --  For each member "member" we declare a variable
               --  Result_"member" which has the same type.
               --  In parallel to the declaration, we built a list for the
               --  returned value.
               Return_List := New_List (K_List_Id);

               Member := First_Entity (Members);
               while Present (Member) loop
                  Declarator := First_Entity (Declarators (Member));
                  while Present (Declarator) loop

                     --  Get the Result_"member" identifier node

                     Dcl_Name := To_Ada_Name
                       (IDL_Name (FEN.Identifier (Declarator)));
                     Set_Str_To_Name_Buffer ("Result_");
                     Get_Name_String_And_Append (Dcl_Name);
                     Member_Id := Make_Defining_Identifier (Name_Find);

                     --  Adding the element to the return list
                     Append_Node_To_List
                       (Make_Component_Association
                        (Make_Designator (Dcl_Name),
                         Member_Id),
                        Return_List);

                     --  Get the member type designator

                     N := Stub_Node (BE_Node (Identifier (Declarator)));
                     Member_Type := Subtype_Indication (N);

                     N := Make_Object_Declaration
                       (Defining_Identifier => Member_Id,
                        Object_Definition   => Member_Type);
                     Append_Node_To_List (N, D);

                     Declarator := Next_Entity (Declarator);
                  end loop;
                  Member := Next_Entity (Member);
               end loop;

               --  End Declarations
               --  Begin Statements
               Index := 0;
               Member := First_Entity (Members);
               while Present (Member) loop
                  Declarator := First_Entity (Declarators (Member));
                  Member_Type := Type_Spec (Member);
                  while Present (Declarator) loop

                     --  Set the value of the "Index_u" variable

                     Param_List := New_List (K_List_Id);
                     Append_Node_To_List
                       (Make_Designator (PN (P_Item)),
                        Param_List);

                     if Is_Base_Type (Member_Type) then
                        TC_Node := Base_Type_TC
                          (FEN.Kind (Member_Type));
                     else
                        raise Program_Error;
                     end if;
                     Append_Node_To_List (TC_Node, Param_List);

                     N := Make_Literal
                       (New_Value
                        (Value_Type'
                         (K => K_Unsigned_Long_Long,
                          IVal => Index,
                          Sign => 0,
                          Base => 10)));
                     N := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id (N));
                     Append_Node_To_List (N, Param_List);

                     N := Make_Subprogram_Call
                       (RE (RE_Get_Aggregate_Element),
                        Param_List);

                     N := Make_Assignment_Statement
                       (Make_Defining_Identifier (VN (V_Index)),
                        N);
                     Append_Node_To_List (N, S);

                     --  Set the value of Result_"member"

                     Dcl_Name := To_Ada_Name
                       (IDL_Name (FEN.Identifier (Declarator)));
                     Set_Str_To_Name_Buffer ("Result_");
                     Get_Name_String_And_Append (Dcl_Name);
                     Member_Id := Make_Defining_Identifier (Name_Find);

                     N := Make_Subprogram_Call
                       (RE (RE_From_Any_0),
                        Make_List_Id
                        (Make_Defining_Identifier (VN (V_Index))));
                     N := Make_Assignment_Statement
                       (Member_Id,
                        N);
                     Append_Node_To_List (N, S);

                     Declarator := Next_Entity (Declarator);
                     Index := Index + 1;
                  end loop;
                  Member := Next_Entity (Member);
               end loop;

               N := Make_Return_Statement
                 (Make_Record_Aggregate (Return_List));
               Append_Node_To_List (N, S);

               --  End Statements

            end if;

            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Exception_Declaration_Body;

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

            when K_Exception_Declaration =>
               N := Exception_Declaration_Body (E);

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
         function Exception_Declaration_Body (E : Node_Id) return Node_Id;

         -----------------------------
         -- Complex_Declarator_Body --
         -----------------------------

         function Complex_Declarator_Body (E : Node_Id) return Node_Id is
            I                    : Integer := 0;
            L                    : List_Id;
            Sizes                : constant List_Id :=
              Range_Constraints
              (Type_Definition (Stub_Node (BE_Node (Identifier (E)))));
            Dim                  : Node_Id;
            Loop_Statements      : List_Id := No_List;
            Enclosing_Statements : List_Id;
            Helper               : Node_Id;
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

            if Is_Base_Type (Type_Spec (Declaration (E))) then
               Helper := RE (RE_To_Any_0);
            elsif FEN.Kind (Type_Spec (Declaration (E))) = K_Scoped_Name then
               declare
                  Reference : constant Node_Id := FEN.Reference
                    (Type_Spec (Declaration (E)));
               begin
                  --  As iac evolves, add the corresponding To_Any nodes
                  case FEN.Kind (Reference) is
                     when K_Enumeration_Type =>
                        Helper := Helper_Node
                          (BE_Node (Identifier (Reference)));
                        --  The From_Any function is declared at the third
                        --  place in the Helper spec
                        Helper := Next_Node (Next_Node (Helper));
                        Helper := Defining_Identifier (Helper);
                     when others =>
                        Helper := RE (RE_To_Any_0);
                  end case;
               end;
            else
               raise Program_Error;
            end if;

            N := Make_Subprogram_Call
              (Make_Defining_Identifier (PN (P_Item)), L);
            N := Make_Subprogram_Call
              (Helper,
               Make_List_Id (N));
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
              (RE (RE_To_Any_0),
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
         end  Enumeration_Type_Body;

         --------------------------------
         -- Interface_Declaration_Body --
         --------------------------------

         function Interface_Declaration_Body (E : Node_Id) return Node_Id is
         begin
            Spec := Helper_Node (BE_Node (Identifier (E)));

            --  Getting the identifier of the TC_"Interface_name" variable
            --  declared at the first place in the Helper spec.
            Helper_Name := BEN.Name (Defining_Identifier (Spec));

            --  Getting the node of the To_Any method spec declared at the 3rd
            --  place of the helper spec.
            Spec := Next_Node (Next_Node (Spec));
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
                             Make_Designator (Helper_Name)));
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
            Member          : Node_Id;
            Declarator      : Node_Id;
            Item_Designator : Node_Id;
            Designator      : Node_Id;
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
               Item_Designator := Make_Designator (PN (P_Item));
               while Present (Declarator) loop
                  Designator := Map_Designator (Declarator);
                  Set_Parent_Unit_Name (Designator, Item_Designator);
                  N := Make_Subprogram_Call
                    (RE (RE_To_Any_0),
                     Make_List_Id (Designator));
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
            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);
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

         --------------------------------
         -- Exception_Declaration_Body --
         --------------------------------

         function Exception_Declaration_Body (E : Node_Id) return Node_Id is
            Members       : List_Id;
            Member        : Node_Id;
            Declarator    : Node_Id;
            Member_Type   : Node_Id;
            To_Any_Helper : Node_Id;
         begin
            --  Obtaining the "To_Any" spec node from the helper spec
            Spec := Helper_Node (BE_Node (Identifier (E)));
            Spec := Next_Node
              (Next_Node
               (Spec));  --  Third in the list of helpers

            --  Begin Declarations

            --  Obtaining the node corresponding to the declaration of the
            --  TC_"Excp_Name" constant.

            N := Helper_Node (BE_Node (Identifier (E)));
            N := Defining_Identifier (N);
            N := Make_Subprogram_Call
              (RE (RE_Get_Empty_Any_Aggregate),
               Make_List_Id
               (N));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Result)),
               Object_Definition   => RE (RE_Any),
               Expression => N);
            Append_Node_To_List (N, D);

            --  Adding the necessary pragmas because the parameter of the
            --  function is unreferenced in case of nonexistence of exception
            --  members.

            Members := FEN.Members (E);

            if FEU.Is_Empty (Members) then

               N := Make_Subprogram_Call
                 (Make_Defining_Identifier (GN (Pragma_Warnings)),
                  Make_List_Id
                  (RE (RE_Off)));
               N := Make_Pragma_Statement (N);
               Append_Node_To_List (N, D);

               N := Make_Subprogram_Call
                 (Make_Defining_Identifier (GN (Pragma_Unreferenced)),
                  Make_List_Id
                  (Make_Designator (PN (P_Item))));
               N := Make_Pragma_Statement (N);
               Append_Node_To_List (N, D);

               N := Make_Subprogram_Call
                 (Make_Defining_Identifier (GN (Pragma_Warnings)),
                  Make_List_Id
                  (RE (RE_On)));
               N := Make_Pragma_Statement (N);
               Append_Node_To_List (N, D);

               --  End Declarations
            else
               --  Begin Statements

               Member := First_Entity (Members);
               while Present (Member) loop
                  Declarator := First_Entity (Declarators (Member));
                  Member_Type := Type_Spec (Member);
                  while Present (Declarator) loop

                     if Is_Base_Type (Member_Type) then
                        To_Any_Helper := RE (RE_To_Any_0);
                     else
                        raise Program_Error;
                     end if;

                     N := Make_Designator
                       (To_Ada_Name
                        (IDL_Name (FEN.Identifier (Declarator))),
                        PN (P_Item));

                     N := Make_Subprogram_Call
                       (To_Any_Helper,
                        Make_List_Id (N));

                     N := Make_Subprogram_Call
                       (RE (RE_Add_Aggregate_Element),
                        Make_List_Id
                        (Make_Defining_Identifier (VN (V_Result)),
                         N));
                     Append_Node_To_List (N, S);

                     Declarator := Next_Entity (Declarator);
                  end loop;
                  Member := Next_Entity (Member);
               end loop;
            end if;

            N := Make_Return_Statement (Make_Designator (VN (V_Result)));
            Append_Node_To_List (N, S);

            --  End Statements

            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Exception_Declaration_Body;

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

            when K_Exception_Declaration =>
               N := Exception_Declaration_Body (E);

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
         --  The spec of the Narrowing_Ref is declared at the 4th place in the
         --  Helper package spec
         Spec := Helper_Node (BE_Node (Identifier (E)));
         Spec := Next_Node (Next_Node (Next_Node (Spec)));

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

      ------------------------------
      -- Raise_Excp_From_Any_Spec --
      ------------------------------

      function Raise_Excp_From_Any_Spec
        (Raise_Node : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)),
            RE (RE_Any_1));
         Append_Node_To_List (Parameter, Profile);

         N := Make_Subprogram_Specification
           (Raise_Node,
            Profile);
         return N;
      end Raise_Excp_From_Any_Spec;

      ------------------------------
      -- Raise_Excp_From_Any_Body --
      ------------------------------

      function Raise_Excp_From_Any_Body
        (E          : Node_Id;
         Raise_Node : Node_Id)
        return Node_Id
      is
         Spec            : constant Node_Id :=
           Raise_Excp_From_Any_Spec (Raise_Node);
         Declarations    : constant List_Id :=
           New_List (K_List_Id);
         Statements      : constant List_Id :=
           New_List (K_List_Id);
         N               : Node_Id;
         From_Any_Helper : Node_Id;
         Excp_Members    : Node_Id;
      begin

         --  Begin Declarations

         --  Obtaining the node corresponding to the declaration of the
         --  "Excp_Name"_Members type.
         --  This type was declared in the third position in the stub spec.

         Excp_Members := Stub_Node (BE_Node (Identifier (E)));
         Excp_Members := Next_Node (Next_Node (Excp_Members));

         --  Preparing the call to From_Any
         N := Make_Subprogram_Call
           (RE (RE_To_CORBA_Any),
            Make_List_Id (Make_Defining_Identifier (PN (P_Item))));
         From_Any_Helper := Helper_Node (BE_Node (Identifier (E)));
         From_Any_Helper := Next_Node (From_Any_Helper);
         From_Any_Helper := Defining_Identifier (From_Any_Helper);
         N := Make_Subprogram_Call
           (From_Any_Helper,
            Make_List_Id (N));

         --  Declaration of the Members variable
         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (PN (P_Members)),
            Constant_Present => True,
            Object_Definition => Defining_Identifier (Excp_Members),
            Expression => N);
         Append_Node_To_List (N, Declarations);

         --  End Declarations

         --  Begin Statements

         N := Make_Defining_Identifier
           (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         N := Make_Attribute_Designator (N, A_Identity);
         N := Make_Subprogram_Call
           (RE (RE_User_Raise_Exception),
            Make_List_Id
            (N, Make_Defining_Identifier (PN (P_Members))));
         Append_Node_To_List (N, Statements);

         --  End Statements

         N := Make_Subprogram_Implementation
           (Spec, Declarations, Statements);

         return N;
      end Raise_Excp_From_Any_Body;

      ---------------------
      -- Raise_Excp_Body --
      ---------------------

      function Raise_Excp_Body
        (E          : Node_Id)
        return Node_Id
      is
         Spec         : Node_Id;
         Statements   : constant List_Id :=
           New_List (K_List_Id);
         N            : Node_Id;
      begin
         --  The spec was declared at the forth position in the helper spec

         Spec := Helper_Node (BE_Node (Identifier (E)));
         Spec := Next_Node (Next_Node (Next_Node (Spec)));

         --  Begin Statements

         N := Make_Defining_Identifier
           (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         N := Make_Attribute_Designator (N, A_Identity);
         N := Make_Subprogram_Call
           (RE (RE_User_Raise_Exception),
            Make_List_Id
            (N, Make_Defining_Identifier (PN (P_Members))));
         Append_Node_To_List (N, Statements);

         --  End Statements

         N := Make_Subprogram_Implementation
           (Spec, No_List, Statements);

         return N;
      end Raise_Excp_Body;

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

            when K_Exception_Declaration =>
               Visit_Exception_Declaration (E);

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
         Deferred_Initialization_Body_Backup : List_Id;
         Package_Initializarion_Backup       : List_Id;
      begin
         N := BEN.Parent (Stub_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Helper_Body;

         --  Handling the case of interfaces nested in modules :
         --  we save a backup of the Deferred_Initialization_Body and the
         --  Package_Initializarion lists because the helper package of
         --  a module is different from the helper package of an interface.

         Deferred_Initialization_Body_Backup :=
           Deferred_Initialization_Body;
         Package_Initializarion_Backup :=
           Package_Initializarion;
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

         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Deferred_Initialization_Body);

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         N := Make_Subprogram_Implementation
           (Make_Subprogram_Specification
            (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
             No_List),
            No_List,
            Deferred_Initialization_Body);
         Append_Node_To_List (N, Statements (Current_Package));

         Helper_Initialization (Package_Initializarion);
         Set_Package_Initialization (Current_Package, Package_Initializarion);

         --  Restoring old values

         Deferred_Initialization_Body :=
           Deferred_Initialization_Body_Backup;
         Package_Initializarion :=
           Package_Initializarion_Backup;
         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
         N : Node_Id;
         Deferred_Initialization_Body_Backup : List_Id;
         Package_Initializarion_Backup       : List_Id;
      begin
         D := Stub_Node (BE_Node (Identifier (E)));
         Push_Entity (D);

         --  Deferred initialisation
         Set_Helper_Body;

         --  Handling the case of modules nested in modules :
         --  we save a backup of the Deferred_Initialization_Body and the
         --  Package_Initializarion lists because the helper package of
         --  a module is different from the helper package of an interface.

         Deferred_Initialization_Body_Backup :=
           Deferred_Initialization_Body;
         Package_Initializarion_Backup :=
           Package_Initializarion;
         Deferred_Initialization_Body := New_List (K_List_Id);
         Package_Initializarion       := New_List (K_List_Id);
         --  N := Deferred_Initialization_Block (E);
         --  Append_Node_To_List (N, Deferred_Initialization_Body);

         D := First_Entity (Definitions (E));
         while Present (D) loop
            Visit (D);
            D := Next_Entity (D);
         end loop;

         N := Make_Subprogram_Implementation
           (Make_Subprogram_Specification
            (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
             No_List),
            No_List,
            Deferred_Initialization_Body);
         Append_Node_To_List (N, Statements (Current_Package));
         Helper_Initialization (Package_Initializarion);
         Set_Package_Initialization (Current_Package, Package_Initializarion);

         --  Restoring old values

         Deferred_Initialization_Body :=
           Deferred_Initialization_Body_Backup;
         Package_Initializarion :=
           Package_Initializarion_Backup;
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
         Set_Helper_Body;
         Append_Node_To_List
           (From_Any_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Any_Body (E), Statements (Current_Package));
         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Deferred_Initialization_Body);
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         L : List_Id;
         D : Node_Id;
         N : Node_Id;
      begin
         Set_Helper_Body;
         L := Declarators (E);
         D := First_Entity (L);
         while Present (D) loop
            Append_Node_To_List
              (From_Any_Body (D), Statements (Current_Package));
            Append_Node_To_List
              (To_Any_Body (D), Statements (Current_Package));

            if FEN.Kind (D) = K_Complex_Declarator then
               N := Deferred_Initialization_Block (D);
               Append_Node_To_List (N, Deferred_Initialization_Body);
            end if;

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

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         Subp_Body_Node : Node_Id;
         Excp_Name      : Name_Id;
         Raise_Node     : Node_Id;
         Deferred_Init  : Node_Id;
      begin
         Set_Helper_Body;
         Subp_Body_Node := From_Any_Body (E);
         Append_Node_To_List
           (Subp_Body_Node, Statements (Current_Package));

         Subp_Body_Node := To_Any_Body (E);
         Append_Node_To_List
           (Subp_Body_Node, Statements (Current_Package));

         --  Generation of the Raise_"Exception_Name"_From_Any spec

         Excp_Name := To_Ada_Name (IDL_Name (FEN.Identifier (E)));
         Excp_Name := Add_Prefix_To_Name ("Raise_", Excp_Name);
         Excp_Name := Add_Suffix_To_Name ("_From_Any", Excp_Name);
         Raise_Node := Make_Defining_Identifier (Excp_Name);
         Subp_Body_Node := Raise_Excp_From_Any_Spec (Raise_Node);
         Append_Node_To_List
           (Subp_Body_Node, Statements (Current_Package));

         --  Addition of the pragma No_Return
         --  The argument of the pragma No_Return must be a local name
         Subp_Body_Node := Make_Subprogram_Call
           (Make_Defining_Identifier (GN (Pragma_No_Return)),
            Make_List_Id
            (Make_Designator
             (BEN.Name (Raise_Node))));
         Subp_Body_Node := Make_Pragma_Statement (Subp_Body_Node);
         Append_Node_To_List
           (Subp_Body_Node, Statements (Current_Package));

         --  Generation of the Raise_"Exception_Name"_From_Any body

         Subp_Body_Node := Raise_Excp_From_Any_Body (E, Raise_Node);
         Append_Node_To_List
           (Subp_Body_Node, Statements (Current_Package));

         --  Generation of the Raise_"Exception_Name" body

         Subp_Body_Node := Raise_Excp_Body (E);
         Append_Node_To_List
           (Subp_Body_Node, Statements (Current_Package));

         --  Generation of the corresponding instructions in the
         --  Deferred_initialisation procedure.
         Deferred_Init := Deferred_Initialization_Block (E);
         Append_Node_To_List (Deferred_Init, Deferred_Initialization_Body);

      end Visit_Exception_Declaration;

      -----------------------
      -- Widening_Ref_Body --
      -----------------------

      function Widening_Ref_Body (E : Node_Id) return Node_Id is
         Spec        : Node_Id;
         Statements  : List_Id;
         N           : Node_Id;
         M           : Node_Id;
      begin
         --  The spec of the Widening_Ref is declared at the 5th place in the
         --  Helper package spec
         Spec := Helper_Node (BE_Node (Identifier (E)));
         Spec := Next_Node (Next_Node (Spec));
         Spec := Next_Node (Next_Node (Spec));

         Statements := New_List (K_List_Id);
         N := Make_Expression
           (Left_Expr  =>
              Make_Subprogram_Call
            (RE (RE_Is_Nil),
             Make_List_Id (Make_Defining_Identifier (PN (P_The_Ref)))),
            Operator   => Op_Or_Else,
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
            Make_List_Id (RE (RE_Default_Sys_Member)));
         Append_Node_To_List (N, Statements);
         N := Make_Subprogram_Implementation
           (Spec, No_List, Statements);
         return N;
      end Widening_Ref_Body;
   end Package_Body;
end Backend.BE_Ada.Helpers;