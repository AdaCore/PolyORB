------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--               B A C K E N D . B E _ A D A . H E L P E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                           Copyright (c) 2005                             --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Namet;      use Namet;
with Values;     use Values;
with Charset;    use Charset;

with Frontend.Nodes;   use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_Ada.Expand;  use Backend.BE_Ada.Expand;
with Backend.BE_Ada.IDL_To_Ada;  use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Nodes;   use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;  use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime; use Backend.BE_Ada.Runtime;

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

      function From_Any_Spec_Fixed
        (E : Node_Id)
        return Node_Id
        renames From_Any_Spec_Ex;

      function From_Any_Spec_Seq
        (E : Node_Id)
        return Node_Id;

      function To_Any_Spec
        (E : Node_Id)
        return Node_Id;

      function To_Any_Spec_Ex
        (E : Node_Id)
        return Node_Id;

      function To_Any_Spec_Fixed
        (E : Node_Id)
        return Node_Id
        renames To_Any_Spec_Ex;

      function To_Any_Spec_Seq
        (E : Node_Id)
        return Node_Id;

      function U_To_Ref_Spec
        (E : Node_Id)
        return Node_Id;

      function TypeCode_Spec
        (E        : Node_Id;
         Backend  : Boolean := False;
         Init_Var : Boolean := True)
        return Node_Id;
      --  returns a TypeCode constant for a given type (E).
      --  When Backend is true, E is assumed to be a backend node of a full
      --  type definition.

      function TypeCode_Dimension_Spec
        (Declarator : Node_Id;
         Dim        : Natural)
        return Node_Id;
      --  returns a TypeCode constant for a dimension of an array other than
      --  the last dimension

      function TypeCode_Dimension_Declarations
        (Declarator : Node_Id)
        return List_Id;
      --  Multidimensional arrays : when they are converted to the
      --  Any type, the multidimensional arrays are seen as nested
      --  arrays. So, for each dimension from the first until the
      --  before last dimension we declare a type code constant.
      --  This function returns a list of TC_Dimensions_'N' constant
      --  declarations

      function To_Ref_Spec
        (E : Node_Id)
        return Node_Id;

      function Raise_Excp_Spec
        (Excp_Members : Node_Id;
         Raise_Node   : Node_Id)
        return Node_Id;
      --  return the spec of the Raise_"Exception_Name" procedure

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Forward_Interface_Declaration (E : Node_Id);
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
            Expand_Designator (Type_Def_Node (BE_Node (Identifier (E)))));

         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            (Defining_Identifier (Helper_Package (Current_Entity))));
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

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end From_Any_Spec_Ex;

      -----------------------
      -- From_Any_Spec_Seq --
      -----------------------

      function From_Any_Spec_Seq
        (E : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
         Seq_Type  : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)),
            RE (RE_Any));
         Append_Node_To_List (Parameter, Profile);

         Seq_Type := Make_Defining_Identifier (TN (T_Sequence));
         Set_Correct_Parent_Unit_Name (Seq_Type, Defining_Identifier (E));

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_From_Any)),
            Profile,
            Seq_Type);

         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end From_Any_Spec_Seq;

      -------------------
      -- U_To_Ref_Spec --
      -------------------

      function U_To_Ref_Spec
        (E : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         N := Subtype_Indication
           (Type_Definition (Type_Def_Node (BE_Node (Identifier (E)))));
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_The_Ref)),
            Make_Type_Attribute
            (Map_Ref_Type_Ancestor (E),
             A_Class));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Map_Narrowing_Designator (E, True),
            Profile, Expand_Designator
            (Type_Def_Node (BE_Node (Identifier (E)))));

         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end U_To_Ref_Spec;

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
            Expand_Designator (Type_Def_Node (BE_Node (Identifier (E)))));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Any)),
            Profile, RE (RE_Any));

         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
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

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end To_Any_Spec_Ex;

      ---------------------
      -- To_Any_Spec_Seq --
      ---------------------

      function To_Any_Spec_Seq
        (E : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
         Seq_Type  : Node_Id;
      begin
         Seq_Type := Make_Defining_Identifier (TN (T_Sequence));
         Set_Correct_Parent_Unit_Name (Seq_Type,  Defining_Identifier (E));

         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)), Seq_Type);
         Append_Node_To_List (Parameter, Profile);

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Any)),
            Profile, RE (RE_Any));

         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end To_Any_Spec_Seq;

      -----------------
      -- To_Ref_Spec --
      -----------------

      function To_Ref_Spec
        (E : Node_Id)
        return Node_Id
      is
         Profile   : List_Id;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         N := Subtype_Indication
           (Type_Definition (Type_Def_Node (BE_Node (Identifier (E)))));
         Profile  := New_List (K_Parameter_Profile);
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_The_Ref)),
            Make_Type_Attribute
            (Map_Ref_Type_Ancestor (E),
             A_Class));
         Append_Node_To_List (Parameter, Profile);
         N := Make_Subprogram_Specification
           (Map_Narrowing_Designator (E, False),
            Profile,
            Expand_Designator
            (Type_Def_Node (BE_Node (Identifier (E)))));

         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end To_Ref_Spec;

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

         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end Raise_Excp_Spec;

      -------------------
      -- TypeCode_Spec --
      -------------------

      function TypeCode_Spec
        (E        : Node_Id;
         Backend  : Boolean := False;
         Init_Var : Boolean := True)
        return Node_Id
      is
         N  : Node_Id := E;
         C  : Node_Id := No_Node;
         TC : Name_Id;
         P  : Node_Id;
         T  : Node_Id;
      begin
         if Init_Var then
            if Backend then
               case BEN.Kind (Type_Definition (N)) is
                  when K_Decimal_Type_Definition =>
                     P := RE (RE_TC_Fixed);

                  when others =>
                     raise Program_Error;
               end case;
            else
               N := Type_Def_Node (BE_Node (Identifier (E)));
               case FEN.Kind (E) is
                  when K_Enumeration_Type =>
                     P := RE (RE_TC_Enum);

                  when K_Forward_Interface_Declaration =>
                     N := Stub_Package_Node
                       (BE_Ada_Instanciations
                        (BE_Node
                         (Identifier (E))));
                     P := RE (RE_TC_Object_1);

                  when K_Interface_Declaration =>
                     N := Package_Declaration
                       (BEN.Parent (Type_Def_Node (BE_Node (Identifier (E)))));
                     P := RE (RE_TC_Object_1);

                  when  K_Simple_Declarator =>
                     T := Type_Spec
                       (Declaration (E));

                     --  The default TC for type definition is TC_Alias...

                     P := RE (RE_TC_Alias);

                     --  ...However, there are some exceptions

                     if Kind (T) = K_Scoped_Name and then
                       Get_CORBA_Predefined_Entity (T) = RE_Null
                       and then
                       FEN.Kind (Reference (T)) /= K_Interface_Declaration
                       and then
                       FEN.Kind (Reference (T)) /= K_Structure_Type
                       and then
                       Present (BE_Node (Identifier (Reference (T))))
                     then
                        P := Reference (T);
                        P := TC_Node (BE_Node (Identifier (P)));
                        P := Copy_Designator
                          (First_Node
                           (Actual_Parameter_Part
                            (BEN.Expression (P))));
                     end if;

                  when K_Complex_Declarator =>
                     P := RE (RE_TC_Array);

                  when K_Structure_Type =>
                     P := RE (RE_TC_Struct);

                  when K_Union_Type =>
                     P := RE (RE_TC_Union);

                  when K_Exception_Declaration =>
                     P := RE (RE_TC_Except);

                  when others =>
                     raise Program_Error;
               end case;
            end if;
            C := Make_Subprogram_Call
              (Defining_Identifier   => RE (RE_To_CORBA_Object),
               Actual_Parameter_Part => Make_List_Id (P));
         end if;

         TC := Add_Prefix_To_Name ("TC_", BEN.Name (Defining_Identifier (N)));

         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (TC),
            Constant_Present    => False,
            Object_Definition   => RE (RE_Object),
            Expression          => C);
         --  Setting the correct parent unit name, for the future calls of the
         --  subprogram
         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Helper_Package (Current_Entity)));
         return N;
      end TypeCode_Spec;

      -----------------------------
      -- TypeCode_Dimension_Spec --
      -----------------------------

      function TypeCode_Dimension_Spec
        (Declarator : Node_Id;
         Dim        : Natural)
        return Node_Id
      is
         N       : Node_Id;
         TC_Name : Name_Id;
      begin

         --  Building the TypeCode variable name

         N := Defining_Identifier
           (Type_Def_Node
            (BE_Node
             (Identifier
              (Declarator))));
         TC_Name := Add_Prefix_To_Name ("TC_", BEN.Name (N));
         Get_Name_String (TC_Name);
         Add_Str_To_Name_Buffer ("_TC_Dimension_");
         Add_Nat_To_Name_Buffer (Int (Dim));
         TC_Name := Name_Find;

         --  Expression of the constant

         N :=  Make_Subprogram_Call
           (Defining_Identifier   => RE (RE_To_CORBA_Object),
            Actual_Parameter_Part => Make_List_Id (RE (RE_TC_Array)));

         --  Declaration of the variable

         N := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (TC_Name),
            Constant_Present    => False,
            Object_Definition   => RE (RE_Object),
            Expression          => N);

         return N;
      end TypeCode_Dimension_Spec;

      -------------------------------------
      -- TypeCode_Dimension_Declarations --
      -------------------------------------

      function TypeCode_Dimension_Declarations
        (Declarator : Node_Id)
        return List_Id is
         pragma Assert (FEN.Kind (Declarator) = K_Complex_Declarator);
         Dim : constant Natural
           := FEU.Length (FEN.Array_Sizes (Declarator));
         L   : List_Id;
         N   : Node_Id;
      begin
         L := New_List (K_List_Id);
         if Dim > 1 then
            for I in 1 .. Dim - 1 loop
               N := TypeCode_Dimension_Spec (Declarator, I);
               Append_Node_To_List (N, L);
            end loop;
         end if;
         return L;
      end TypeCode_Dimension_Declarations;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when K_Forward_Interface_Declaration =>
               Visit_Forward_Interface_Declaration (E);

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
         Bind_FE_To_TC (Identifier (E), N);

         N := From_Any_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_From_Any (Identifier (E), N);

         N := To_Any_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_To_Any (Identifier (E), N);
      end Visit_Enumeration_Type;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Helper_Spec;

         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Bind_FE_To_TC (Identifier (E), N);

         N := From_Any_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_From_Any (Identifier (E), N);

         N := To_Any_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_To_Any (Identifier (E), N);

         N := U_To_Ref_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_U_To_Ref (Identifier (E), N);

         N := To_Ref_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_To_Ref (Identifier (E), N);

      end Visit_Forward_Interface_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
         Is_Local : constant Boolean := Is_Local_Interface (E);
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Helper_Spec;

         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Bind_FE_To_TC (Identifier (E), N);

         --  Local interfaces dont have Any conversion methods

         if not Is_Local then
            N := From_Any_Spec (E);
            Append_Node_To_List
              (N, Visible_Part (Current_Package));
            Bind_FE_To_From_Any (Identifier (E), N);

            N := To_Any_Spec (E);
            Append_Node_To_List
              (N, Visible_Part (Current_Package));
            Bind_FE_To_To_Any (Identifier (E), N);
         end if;

         N := U_To_Ref_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_U_To_Ref (Identifier (E), N);

         N := To_Ref_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_To_Ref (Identifier (E), N);

         --  Visit the entities declared inside the interface.

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritence, generate the mappings for
         --  the operations and attributes of the parents except the first one.

         Map_Inherited_Entities_Specs
           (Current_interface    => E,
            Visit_Operation_Subp => null,
            Helper               => True);

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Helper_Spec) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
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
         N          : Node_Id;
         L          : List_Id;
         Member     : Node_Id;
         Declarator : Node_Id;
      begin
         Set_Helper_Spec;

         --  For each complex declarator, a new type is defined (see the stub
         --  generation for more details). For each defined type, a TC_XXXX
         --  Constant, a From_Any and a To_Any functions must be generated.

         Member := First_Entity (Members (E));
         while Present (Member) loop
            Declarator := First_Entity (Declarators (Member));
            while Present (Declarator) loop
               if FEN.Kind (Declarator) = K_Complex_Declarator then
                  N := TypeCode_Spec (Declarator);
                  Append_Node_To_List
                    (N, Visible_Part (Current_Package));
                  Bind_FE_To_Helper (Identifier (Declarator), N);
                  Bind_FE_To_TC (Identifier (Declarator), N);

                  L := TypeCode_Dimension_Declarations (Declarator);
                  Append_Node_To_List
                    (First_Node (L), Visible_Part (Current_Package));

                  N := From_Any_Spec (Declarator);
                  Append_Node_To_List
                    (N, Visible_Part (Current_Package));
                  Bind_FE_To_From_Any (Identifier (Declarator), N);

                  N := To_Any_Spec (Declarator);
                  Append_Node_To_List
                    (N, Visible_Part (Current_Package));
                  Bind_FE_To_To_Any (Identifier (Declarator), N);
               end if;
               Declarator := Next_Entity (Declarator);
            end loop;
            Member := Next_Entity (Member);
         end loop;

         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Bind_FE_To_TC (Identifier (E), N);

         N := From_Any_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_From_Any (Identifier (E), N);

         N := To_Any_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_To_Any (Identifier (E), N);
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is

         --  This procedure performs an instanciation bindings on evry
         --  entity of the 'D_List' parameter
         procedure Iterative_Instanciations_Binding
           (D_List              : List_Id;
            Stub_Package_Node   : Node_Id := No_Node;
            Stub_Type_Node      : Node_Id := No_Node;
            Helper_Package_Node : Node_Id := No_Node;
            TC_Node             : Node_Id := No_Node;
            From_Any_Node       : Node_Id := No_Node;
            To_Any_Node         : Node_Id := No_Node);

         --------------------------------------
         -- Iterative_Instanciations_Binding --
         --------------------------------------

         procedure Iterative_Instanciations_Binding
           (D_List              : List_Id;
            Stub_Package_Node   : Node_Id := No_Node;
            Stub_Type_Node      : Node_Id := No_Node;
            Helper_Package_Node : Node_Id := No_Node;
            TC_Node             : Node_Id := No_Node;
            From_Any_Node       : Node_Id := No_Node;
            To_Any_Node         : Node_Id := No_Node)
         is
            Entity : Node_Id := First_Entity (D_List);
         begin
            while Present (Entity) loop
               Bind_FE_To_Instanciations
                 (F                   => Identifier (Entity),
                  Stub_Package_Node   => Stub_Package_Node,
                  Stub_Type_Node      => Stub_Type_Node,
                  Helper_Package_Node => Helper_Package_Node,
                  TC_Node             => TC_Node,
                  From_Any_Node       => From_Any_Node,
                  To_Any_Node         => To_Any_Node);
               Entity := Next_Entity (Entity);
            end loop;
         end Iterative_Instanciations_Binding;

         L       : List_Id;
         D       : Node_Id;
         N       : Node_Id;
         T       : Node_Id;
         TC_Dims : List_Id;
      begin
         Set_Helper_Spec;
         L := Declarators (E);
         T := Type_Spec (E);
         D := First_Entity (L);

         --  Handling the case of a fixed point type definition
         --  We must define a new fixed type. All the declarators will be
         --  derived from this new type.

         if FEN.Kind (T) = K_Fixed_Point_Type then
            declare
               F : Node_Id;
            begin
               F := Stub_Type_Node
                 (BE_Ada_Instanciations
                  (BE_Node
                   (Identifier
                    (D))));
               N := TypeCode_Spec
                 (F,
                  Backend => True);
               Iterative_Instanciations_Binding
                 (D_List         => L,
                  TC_Node        => N);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));

               N := From_Any_Spec_Fixed (F);
               Iterative_Instanciations_Binding
                 (D_List         => L,
                  From_Any_Node  => N);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));

               N := To_Any_Spec_Fixed (F);
               Iterative_Instanciations_Binding
                 (D_List         => L,
                  To_Any_Node    => N);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));
            end;
         end if;

         --  Handling the case of a Sequence type definition

         if FEN.Kind (T) = K_Sequence_Type then
            declare
               S : Node_Id;
            begin
               S := Stub_Package_Node
                 (BE_Ada_Instanciations
                  (BE_Node
                   (Identifier (D))));

               --  The TypeCode variable isn't initialized in the spec, it will
               --  assigned a value in the helper initialization.

               N := TypeCode_Spec
                 (S,
                  Backend  => True,
                  Init_Var => False);
               Iterative_Instanciations_Binding
                 (D_List         => L,
                  TC_Node        => N);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));

               N := From_Any_Spec_Seq (S);
               Iterative_Instanciations_Binding
                 (D_List         => L,
                  From_Any_Node  => N);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));

               N := To_Any_Spec_Seq (S);
               Iterative_Instanciations_Binding
                 (D_List         => L,
                  To_Any_Node    => N);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));
            end;
         end if;

         while Present (D) loop
            N := TypeCode_Spec (D);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_Helper (Identifier (D), N);
            Bind_FE_To_TC (Identifier (D), N);

            if FEN.Kind (D) = K_Complex_Declarator then
               TC_Dims := TypeCode_Dimension_Declarations (D);
               Append_Node_To_List
                 (First_Node (TC_Dims), Visible_Part (Current_Package));
            end if;

            --  If the new type is defined basing on an interface type,
            --  and then if this is not an array type, then we dont generate
            --  From_Any nor To_Any. We use those of the original type.

            if FEN.Kind (T) = K_Scoped_Name
              and then
              Is_Object_Type (T)
              and then
              FEN.Kind (D) = K_Simple_Declarator then

               --  For local interface, we generate nothing

               if not ((FEN.Kind (Reference (T)) =
                        K_Interface_Declaration
                        or else
                        FEN.Kind (Reference (T)) =
                        K_Forward_Interface_Declaration)
                       and then Is_Local_Interface (Reference (T)))
               then
                  N := Get_From_Any_Node (T);
                  Bind_FE_To_From_Any (Identifier (D), N);

                  N := Get_To_Any_Node (T);
                  Bind_FE_To_To_Any (Identifier (D), N);
               end if;
            else
               N := From_Any_Spec (D);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));
               Bind_FE_To_From_Any (Identifier (D), N);

               N := To_Any_Spec (D);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));
               Bind_FE_To_To_Any (Identifier (D), N);
            end if;

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
         Bind_FE_To_TC (Identifier (E), N);

         --  Obtaining the node corresponding to the declaration of the
         --  "Excp_Name"_Members type.

         Excp_Members := Type_Def_Node (BE_Node (Identifier (E)));

         N := From_Any_Spec_Ex (Excp_Members);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_From_Any (Identifier (E), N);

         N := To_Any_Spec_Ex (Excp_Members);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_To_Any (Identifier (E), N);

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
         N            : Node_Id;
         Alternatives : List_Id;
         Alternative  : Node_Id;
         Declarator   : Node_Id;
         TC_Dims      : List_Id;
      begin
         Set_Helper_Spec;

         --  For each complex declarator, a new type is defined (see the stub
         --  generation for more details). For each defined type, a TC_XXXX
         --  Constant, a From_Any and a To_Any functions must be generated.

         Alternatives := Switch_Type_Body (E);
         Alternative := First_Entity (Alternatives);
         while Present (Alternative) loop
            Declarator := FEN.Declarator (FEN.Element (Alternative));
            if FEN.Kind (Declarator) = K_Complex_Declarator then
               N := TypeCode_Spec (Declarator);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));
               Bind_FE_To_Helper (Identifier (Declarator), N);
               Bind_FE_To_TC (Identifier (Declarator), N);

               TC_Dims := TypeCode_Dimension_Declarations (Declarator);
               Append_Node_To_List
                 (First_Node (TC_Dims), Visible_Part (Current_Package));

               N := From_Any_Spec (Declarator);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));
               Bind_FE_To_From_Any (Identifier (Declarator), N);

               N := To_Any_Spec (Declarator);
               Append_Node_To_List
                 (N, Visible_Part (Current_Package));
               Bind_FE_To_To_Any (Identifier (Declarator), N);
            end if;
            Alternative := Next_Entity (Alternative);
         end loop;

         N := TypeCode_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_Helper (Identifier (E), N);
         Bind_FE_To_TC (Identifier (E), N);

         N := From_Any_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_From_Any (Identifier (E), N);

         N := To_Any_Spec (E);
         Append_Node_To_List
           (N, Visible_Part (Current_Package));
         Bind_FE_To_To_Any (Identifier (E), N);
      end Visit_Union_Type;

   end Package_Spec;

   package body Package_Body is

      package BEU renames Backend.BE_Ada.Nutils;

      Deferred_Initialization_Body : List_Id;
      Package_Initializarion       : List_Id;
      Dependency_List              : List_Id;

      procedure Add_Dependency (Dep : Node_Id);
      --  When a Helper package is initialized by the PolyORB Initialization
      --  Manager, all the packages this package depends on must be
      --  initialized. For exemple, When we create a complex TypeCode, the
      --  TypeCodes used must be already created.
      --  This procedure add the dependency 'Dep' to the current Helper
      --  package

      function Deferred_Initialization_Block
        (E : Node_Id)
         return Node_Id;

      function Declare_Any_Array
        (A_Name         : Name_Id;
         A_First        : Natural;
         A_Last         : Natural)
        return Node_Id;

      function Nth_Element (A_Name : Name_Id; Nth : Nat) return Node_Id;

      function From_Any_Body
        (E : Node_Id)
        return Node_Id;

      procedure Helper_Initialization (L : List_Id);

      function To_Any_Body
        (E : Node_Id)
        return Node_Id;

      function U_To_Ref_Body
        (E : Node_Id)
        return Node_Id;

      function To_Ref_Body
        (E : Node_Id)
        return Node_Id;

      function Raise_Excp_From_Any_Spec
        (Raise_Node : Node_Id)
        return Node_Id;
      --  The spec is located in the body because this function is not used
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
      procedure Visit_Forward_Interface_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);

      --------------------
      -- Add_Dependency --
      --------------------

      procedure Add_Dependency (Dep : Node_Id) is

         function "=" (Name : Name_Id; Node : Node_Id) return Boolean;
         function Is_Internal_Unit (Unit : Node_Id) return Boolean;

         ---------
         -- "=" --
         ---------

         function "=" (Name : Name_Id; Node : Node_Id) return Boolean is
         begin
            return Name = Fully_Qualified_Name (Node);
         end "=";

         function Is_Internal_Unit (Unit : Node_Id) return Boolean is
            N : Node_Id := Unit;
         begin
            if BEN.Kind (N) = K_Designator then
               N := Defining_Identifier (N);
            end if;

            return BEN.Kind (Corresponding_Node (N)) = K_Package_Instantiation;
         end Is_Internal_Unit;

         Dep_Name : Name_Id;
         V        : Value_Id;
         N        : Node_Id;
         M        : Node_Id;
         Append   : Boolean := True;
      begin
         if Is_Internal_Unit (Dep) then
            return;
         end if;

         Dep_Name := BEU.Fully_Qualified_Name (Dep);

         --  Particular case : We don't add dependencies on :
         --  * The Helper package itself

         if Dep_Name = Defining_Identifier
           (Helper_Package (Current_Entity))
         then
            return;

         --  Particular case : A dependency on CORBA.Object.Helper implies
         --  a dependency on CORBA.Object

         elsif Dep_Name = RU (RU_CORBA_Object_Helper, False) then
            Add_Dependency (RU (RU_CORBA_Object, False));
            return;

         --  Particular case : We lower the case of these entities
         --  * CORBA
         --  * CORBA.Helper
         --  * CORBA.Object

         elsif Dep_Name = RU (RU_CORBA, False)
           or else Dep_Name = RU (RU_CORBA_Helper, False)
           or else Dep_Name = RU (RU_CORBA_Object, False)
         then
            Get_Name_String (Dep_Name);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Dep_Name := Name_Find;

         --  Particular case : Some PolyORB units have a customized
         --  initialization name

         elsif Dep_Name = RU (RU_PolyORB_Exceptions, False) then
            Set_Str_To_Name_Buffer ("exceptions");
            Dep_Name := Name_Find;
         end if;

         --  Check wether the dependency is already added

         M := First_Node (Dependency_List);
         while Present (M) loop
            if Values.Value (BEN.Value (M)).SVal = Dep_Name then
               Append := False;
            end if;
            M := Next_Node (M);
         end loop;

         if Append then
            V := New_String_Value (Dep_Name, False);
            N := Make_Literal (V);
            Append_Node_To_List (N, Dependency_List);
         end if;
      end Add_Dependency;

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

         --  Generate a TC constant for a fixed point type. We regenerate it
         --  here because there is no simple way to link a K_Fixed_Point_Type
         --  node to the backend tree.
         function Get_TC_Fixed_Point
           (E : Node_Id)
           return Node_Id;

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

         ------------------------
         -- Get_TC_Fixed_Point --
         ------------------------

         function Get_TC_Fixed_Point
           (E : Node_Id)
           return Node_Id
         is
            Fixed_Name : Name_Id;
            Result     : Node_Id;
         begin
            pragma Assert (FEN.Kind (E) = K_Fixed_Point_Type);

            Set_Str_To_Name_Buffer ("TC_Fixed_");
            Add_Nat_To_Name_Buffer (Nat (N_Total (E)));
            Add_Char_To_Name_Buffer ('_');
            Add_Nat_To_Name_Buffer (Nat (N_Scale (E)));
            Fixed_Name := Name_Find;

            Result := Make_Defining_Identifier (Fixed_Name);
            Set_Correct_Parent_Unit_Name
              (Result,
               Defining_Identifier
               (Helper_Package (Current_Entity)));
            return Result;
         end Get_TC_Fixed_Point;

         Stub             : Node_Id;
         N                : Node_Id;
         Entity_TC_Name   : Name_Id;
         Entity_Name_V    : Value_Id;
         Entity_Rep_Id_V  : Value_Id;
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
         Param1           : Node_Id;
         Param2           : Node_Id;
      begin
         --  Extract from polyorb-any.ads concerning the Encoding of TypeCodes:
         --  10. For sequence and array, the first parameter will
         --      be the length of the sequence or the array and the second
         --      the content type. As for strings, an unbounded sequence will
         --      have a length of 0.
         --
         --  11. For fixed, the first parameter will be the digits
         --      number and the second the scale.
         --
         --  So, we dont need the definitions below :

         if FEN.Kind (E) /= K_Complex_Declarator
           and then FEN.Kind (E) /= K_Fixed_Point_Type then

            --  For the forward interfaces, we use the name and the Rep_Id
            --  of the forwarded interface.

            if FEN.Kind (E) = K_Forward_Interface_Declaration then
               Stub :=  Type_Def_Node (BE_Node (Identifier (Forward (E))));
               Entity_Rep_Id_V := BEN.Value
                 (BEN.Expression (Next_Node (Stub)));
            else
               Stub :=  Type_Def_Node (BE_Node (Identifier (E)));
               Entity_Rep_Id_V := BEN.Value
                 (BEN.Expression (Next_Node (Stub)));
            end if;
         end if;

         --  The fixed point types constitute a particular case since
         --  they don't have a corresponding node in the frontend tree

         if FEN.Kind (E) /= K_Fixed_Point_Type then
            Entity_TC_Name := BEN.Name
              (Defining_Identifier
               (Get_TC_Node (E)));
         else
            Entity_TC_Name := BEN.Name (Get_TC_Fixed_Point (E));
         end if;

         case FEN.Kind (E) is
            when K_Interface_Declaration
              | K_Forward_Interface_Declaration =>
               Stub := Package_Declaration
                 (BEN.Parent (Stub));

            when K_Complex_Declarator =>
               declare
                  V                : Value_Type;
                  TC               : Node_Id;
                  TC_Dim           : Node_Id          := No_Node;
                  TC_Previous_Name : Name_Id          := No_Name;
                  TC_Name          : Name_Id          := No_Name;
                  Sizes            : constant List_Id :=
                    Range_Constraints
                    (Type_Definition
                     (Type_Def_Node (BE_Node (Identifier (E)))));
                  Sizes_Reverse    : constant List_Id := New_List (K_List_Id);
                  Constraint       : Node_Id;
                  Dimension        : constant Natural := Length (Sizes);
                  From_N           : Node_Id          := No_Node;
                  To_N             : Node_Id          := No_Node;
                  T                : Node_Id;
               begin
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

                     --  The TC_XXXX constants used here are the ones declared
                     --  in the Helper spec

                     TC := TC_Node (BE_Node (Identifier (E)));
                     Constraint := Last_Node (Sizes_Reverse);
                     for Index in 1 .. Dimension - 1 loop
                        TC_Dim := Next_N_Node (TC, Dimension - Index);

                        TC_Previous_Name := TC_Name;
                        TC_Name := BEN.Name (BEN.Defining_Identifier (TC_Dim));
                        V := Values.Value (BEN.Value (Last (Constraint)));
                        V.IVal := V.IVal + 1;
                        Param1 := Make_Subprogram_Call
                          (RE (RE_Unsigned_Long),
                           Make_List_Id
                           (Make_Literal (New_Value (V))));

                        if TC_Previous_Name = No_Name then

                           --  The deepest dimension

                           T := Type_Spec (Declaration (E));
                           Param2 := Get_TC_Node (T);
                           Add_Dependency (Parent_Unit_Name (Param2));
                        else --  Not the deepest dimension
                           Param2 := Make_Designator (TC_Previous_Name);
                        end if;

                        N := Add_Parameter
                          (TC_Name,
                           Param1);
                        Append_Node_To_List (N, Statements);
                        N := Add_Parameter
                          (TC_Name,
                           Param2);
                        Append_Node_To_List (N, Statements);
                        Remove_Node_From_List (Constraint, Sizes_Reverse);
                        Constraint := Last_Node (Sizes_Reverse);
                     end loop;

                     --  The case of the last TC_ variable which represents the
                     --  whole array is handled apart.

                     V := Values.Value (BEN.Value (Last (Constraint)));
                     V.IVal := V.IVal + 1;
                     Param1 := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id
                        (Make_Literal (New_Value (V))));
                     Param2 := Make_Designator (TC_Name);

                  else --  1 dimension array

                     V := Values.Value (BEN.Value (Last (First_Node (Sizes))));
                     V.IVal := V.IVal + 1;
                     Param1 := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id
                        (Make_Literal (New_Value (V))));

                     T := Type_Spec (Declaration (E));
                     Param2 := Get_TC_Node (T);
                     Add_Dependency (Parent_Unit_Name (Param2));
                  end if;
               end;

            when K_Fixed_Point_Type =>
               declare
                  V : Value_Id;
               begin
                  V := New_Integer_Value
                    (Unsigned_Long_Long (N_Total (E)),
                     1,
                     10);
                  Param1 := Make_Literal (V);
                  Param1 := Make_Subprogram_Call
                    (RE (RE_Unsigned_Short),
                     Make_List_Id (Param1));

                  V := New_Integer_Value
                    (Unsigned_Long_Long (N_Scale (E)),
                     1,
                     10);
                  Param2 := Make_Literal (V);
                  Param2 := Make_Subprogram_Call
                    (RE (RE_Short),
                     Make_List_Id (Param2));
               end;

            when K_Simple_Declarator =>
               null;

            when K_Enumeration_Type =>
               null;

            when K_Structure_Type =>
               null;

            when K_Union_Type =>
               null;

            when K_Exception_Declaration =>
               null;

            when others =>
               raise Program_Error;
         end case;

         if FEN.Kind (E) /= K_Complex_Declarator
           and then FEN.Kind (E) /= K_Fixed_Point_Type then
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
                       (Get_Name_String (BEN.Name (Enum_Item)) & '_',
                        VN (V_Name));
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

            when K_Union_Type =>
               declare
                  Switch_Alternative  : Node_Id;
                  Choice              : Node_Id;
                  Choices             : List_Id;
                  Label               : Node_Id;
                  To_Any_Helper       : Node_Id;
                  TC_Helper           : Node_Id;
                  Declarator          : Node_Id;
                  Designator          : Node_Id;
                  Arg_Name            : Name_Id;
                  Switch_Type         : Node_Id;
                  Literal_Parent      : Node_Id := No_Node;
                  Orig_Type           : constant Node_Id :=
                    FEU.Get_Original_Type
                    (Switch_Type_Spec (E));
                  Statements_List     : constant List_Id :=
                    New_List (K_List_Id);
                  Default_Index       : Value_Id :=
                    New_Integer_Value (0, 1, 10); --  (0)
                  There_Is_Default    : Boolean           :=
                    False;
               begin

                  --  Getting the dicriminator type and the To_Any node
                  --  corresponding to it

                  TC_Helper := Get_TC_Node (Switch_Type_Spec (E));
                  Add_Dependency (Parent_Unit_Name (TC_Helper));

                  To_Any_Helper := Get_To_Any_Node (Switch_Type_Spec (E));
                  if Is_Base_Type (Switch_Type_Spec (E)) then
                     Switch_Type := RE
                       (Convert
                        (FEN.Kind
                         (Switch_Type_Spec (E))));

                  elsif FEN.Kind (Orig_Type) = K_Enumeration_Type then
                     Switch_Type := Map_Designator (Switch_Type_Spec (E));
                     Literal_Parent := Map_Designator
                       (Scope_Entity
                        (Identifier
                         (Orig_Type)));
                  else
                     Switch_Type := Map_Designator (Switch_Type_Spec (E));
                  end if;

                  --  The third parameter is the discriminator type

                  N := Add_Parameter (Entity_TC_Name, TC_Helper);
                  Append_Node_To_List (N, Statements);

                  --  The forth parameter is the index of default case as a
                  --  long. we put the ramining parameter in an intermediary
                  --  list. When we get the defaul case index, we add the
                  --  intermediari list to the statements list.

                  Switch_Alternative := First_Entity (Switch_Type_Body (E));
                  while Present (Switch_Alternative) loop
                     Choices := New_List (K_List_Id);
                     Label   := First_Entity (Labels (Switch_Alternative));
                     while Present (Label) loop

                        Choice := Make_Literal
                          (Value             => FEN.Value (Label),
                           Parent_Designator => Literal_Parent);

                        --  If this is not a case statement, then we increment
                        --  the default case index. The value of Default_Index
                        --  will be correctly set up after the end of the two
                        --  loops

                        if BEN.Value (Choice) /= No_Value then
                           Set_Value
                             (Default_Index,
                              Value (Default_Index) + Value (Int1_Val));
                        else
                           There_Is_Default := True;
                        end if;

                        Append_Node_To_List (Choice, Choices);
                        Label := Next_Entity (Label);
                     end loop;

                     --  Declaring the argument name "Element" string

                     Declarator := FEN.Declarator
                       (Element (Switch_Alternative));

                     --  Getting the TC_XXX constant corresponding to the
                     --  element type.

                     if FEN.Kind (Declarator) = K_Simple_Declarator then
                        TC_Helper := Get_TC_Node
                          (Type_Spec
                           (Element
                            (Switch_Alternative)));
                     else --  Complex Declatator
                        TC_Helper := Expand_Designator
                          (TC_Node
                           (BE_Node
                            (Identifier
                             (Declarator))));
                     end if;
                     Add_Dependency (Parent_Unit_Name (TC_Helper));

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

                     --  For each case statement, 3 parameters are added :
                     --  * member label
                     --  * member type
                     --  * member name
                     --  This implies that the same element may be declared
                     --  more than once but with a different label.

                     Choice := First_Node (Choices);
                     while Present (Choice) loop
                        if BEN.Value (Choice) /= No_Value then
                           --  We make a copy of the Choice value to avoid
                           --  adding the next nodes of Choice to the
                           --  argument list

                           N := Make_Literal
                             (Value             =>
                                BEN.Value (Choice),
                              Parent_Designator =>
                                BEN.Parent_Designator (Choice));

                           N := Make_Qualified_Expression
                             (Subtype_Mark => Switch_Type,
                              Aggregate    => Make_Record_Aggregate
                                (Make_List_Id (N)));

                           N := Make_Subprogram_Call
                             (To_Any_Helper,
                              Make_List_Id (N));
                           N := Add_Parameter (Entity_TC_Name, N);
                           Append_Node_To_List (N, Statements_List);

                           N := Add_Parameter (Entity_TC_Name, TC_Helper);
                           Append_Node_To_List (N, Statements_List);

                           N := Add_Parameter
                             (Entity_TC_Name,
                              Make_Defining_Identifier (Arg_Name));
                           Append_Node_To_List (N, Statements_List);
                        else --  The default case
                           N := Make_Type_Attribute (Switch_Type, A_First);

                           N := Make_Subprogram_Call
                             (To_Any_Helper,
                              Make_List_Id (N));
                           N := Add_Parameter (Entity_TC_Name, N);
                           Append_Node_To_List (N, Statements_List);

                           N := Add_Parameter (Entity_TC_Name, TC_Helper);
                           Append_Node_To_List (N, Statements_List);

                           N := Add_Parameter
                             (Entity_TC_Name,
                              Make_Defining_Identifier (Arg_Name));
                           Append_Node_To_List (N, Statements_List);

                        end if;
                        Choice := Next_Node (Choice);
                     end loop;

                     Switch_Alternative := Next_Entity (Switch_Alternative);
                  end loop;
                  if not There_Is_Default then
                     Default_Index := New_Integer_Value (1, -1, 10); --  (-1)
                  end if;

                  --  Forth parameter

                  N := Make_Literal
                    (Value           => Default_Index);
                  N := Make_Subprogram_Call
                    (RE (RE_Long),
                     Make_List_Id (N));
                  N := Add_Parameter (Entity_TC_Name, N);
                  Append_Node_To_List (N, Statements);

                  --  Append the Statements_List list to the end of the
                  --  Statements list (we only append the first node, the
                  --  others are appended automatically)

                  Append_Node_To_List
                    (First_Node (Statements_List),
                     Statements);

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

                        --  For simple declarators :

                        if FEN.Kind (Declarator) = K_Simple_Declarator then
                           Param1 := Get_TC_Node
                             (Type_Spec
                              (Declaration
                               (Declarator)));
                        else --  Complex Declatator
                           Param1 := Expand_Designator
                             (TC_Node
                              (BE_Node
                               (Identifier
                                (Declarator))));
                        end if;
                        Add_Dependency (Parent_Unit_Name (Param1));

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
                  Register_Excp_Node         : constant Node_Id :=
                    RE (RE_Register_Exception);
               begin

                  --  Add a dependency to initialize correctly the modules

                  Add_Dependency (Parent_Unit_Name (Register_Excp_Node));

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

                           N := Get_TC_Node (Type_Spec (Member));
                           Add_Dependency (Parent_Unit_Name (N));
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
                  --  no direct access to the "Exception_Name"_Raise_From_Any
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
                    (Register_Excp_Node,
                     Make_List_Id
                     (N, Raise_From_Any_Access_Node));
                  Append_Node_To_List (N, Statements);
               end;

            when K_Simple_Declarator =>
               declare
                  T : Node_Id;
               begin
                  T := Type_Spec (Declaration (E));
                  if Is_Base_Type (T)
                    or else FEN.Kind (T) = K_Scoped_Name then
                     N := Get_TC_Node (T);
                  elsif FEN.Kind (T) = K_Fixed_Point_Type then

                     --  For types defined basing on a fixed point type, we
                     --  use the TypeCode constant of the fixed point type.

                     N := Get_TC_Fixed_Point (T);
                  elsif FEN.Kind (T) = K_Sequence_Type then
                     N := Expand_Designator
                       (TC_Node
                        (BE_Ada_Instanciations
                         (BE_Node
                          (Identifier (E)))));
                  elsif Kind (T) = K_String_Type or else
                    Kind (T) = K_Wide_String_Type then
                     declare
                        Pkg_Inst : constant Node_Id :=
                          (Defining_Identifier
                           (Stub_Package_Node
                            (BE_Ada_Instanciations
                             (BE_Node
                              (Identifier (E))))));
                     begin

                        --  Getting the identifir of the TypeCode function
                        --  located in the instanciated package Bounded_...
                        --  in the stub spec

                        if Kind (T) = K_String_Type then
                           N := RE (RE_TC_Bounded_String);
                        else
                           N := RE (RE_TC_Bounded_Wide_String);
                        end if;
                        Set_Correct_Parent_Unit_Name (N, Copy_Node (Pkg_Inst));
                     end;
                  else
                     raise Program_Error;
                  end if;
                  Add_Dependency (Parent_Unit_Name (N));

                  N := Add_Parameter (Entity_TC_Name, N);
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

      -----------------------
      -- Declare_Any_Array --
      -----------------------

      function Declare_Any_Array
        (A_Name         : Name_Id;
         A_First        : Natural;
         A_Last         : Natural)
        return Node_Id
      is
         N : Node_Id;
         R : Node_Id;
         L : List_Id;
         First : Value_Id;
         Last  : Value_Id;
      begin
         First := New_Integer_Value
           (Unsigned_Long_Long (A_First), 1, 10);
         Last  := New_Integer_Value
           (Unsigned_Long_Long (A_Last), 1, 10);

         R := New_Node (K_Range_Constraint);
         Set_First (R, Make_Literal (First));
         Set_Last  (R, Make_Literal (Last));

         L := New_List (K_Range_Constraints);
         Append_Node_To_List (R, L);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
            (A_Name),
            Object_Definition   => Make_Array_Type_Definition
            (L, RE (RE_Any)));

         return N;
      end Declare_Any_Array;

      -----------------
      -- Nth_Element --
      -----------------

      function Nth_Element (A_Name : Name_Id; Nth : Nat) return Node_Id is
         Nth_Value : Value_Id;
         N         : Node_Id;
      begin
         Nth_Value := New_Integer_Value (Unsigned_Long_Long (Nth), 1, 10);
         N := Make_Subprogram_Call
           (Make_Defining_Identifier (A_Name),
            Make_List_Id (Make_Literal (Nth_Value)));
         return N;
      end Nth_Element;

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
            I                    : Nat := 0;
            Sizes                : constant List_Id :=
              Range_Constraints
              (Type_Definition (Type_Def_Node (BE_Node (Identifier (E)))));
            Dimension            : constant Natural := BEU.Length (Sizes);
            Dim                  : Node_Id;
            Loop_Statements      : List_Id := No_List;
            Enclosing_Statements : List_Id;
            Index_List           : constant List_Id
              := New_List (K_List_Id);
            Helper               : Node_Id;
            TC                   : Node_Id;
            Index_Node           : Node_Id := No_Node;
            Prev_Index_Node      : Node_Id;
            Aux_Node             : Node_Id;
         begin
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition =>
                 Copy_Designator (Return_Type (Spec)));
            Append_Node_To_List (N, D);

            N := Declare_Any_Array (PN (P_Aux), 0, Dimension - 1);
            Append_Node_To_List (N, D);

            Dim := First_Node (Sizes);
            TC := TC_Node (BE_Node (Identifier (E)));
            loop
               Set_Str_To_Name_Buffer ("I");
               Add_Nat_To_Name_Buffer (I);
               Prev_Index_Node := Index_Node;
               Index_Node := Make_Defining_Identifier
                 (Add_Suffix_To_Name (Var_Suffix, Name_Find));
               Append_Node_To_List (Index_Node, Index_List);
               Enclosing_Statements := Loop_Statements;
               Loop_Statements := New_List (K_List_Id);
               N := Make_For_Statement
                 (Index_Node, Dim, Loop_Statements);

               if I > 0 then
                  Aux_Node := Nth_Element (PN (P_Aux), I);
                  Aux_Node := Make_Assignment_Statement
                    (Aux_Node,
                     Make_Subprogram_Call
                     (RE (RE_Get_Aggregate_Element),
                      Make_List_Id
                      (Nth_Element (PN (P_Aux), I - 1),
                       Expand_Designator (TC),
                       Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id (Copy_Node (Prev_Index_Node))))));
                  Append_Node_To_List (Aux_Node, Enclosing_Statements);
                  Append_Node_To_List (N, Enclosing_Statements);
               else
                  Aux_Node := Nth_Element (PN (P_Aux), I);
                  Aux_Node := Make_Assignment_Statement
                    (Aux_Node,
                     Make_Defining_Identifier (PN (P_Item)));
                  Append_Node_To_List (Aux_Node, S);
                  Append_Node_To_List (N, S);
               end if;

               I := I + 1;

               --  Although we use only TC_XXX_TC_Dimension_N in the enclosing
               --  loops, the assignment above must be done at the end and not
               --  at the beginning of the loop. This is due to the fact that
               --  the statemets of a For loop are computed in the iteration
               --  which comes after the one in which the for loop is created.

               TC := Next_Node (TC);
               Dim := Next_Node (Dim);
               exit when No (Dim);
            end loop;

            TC := Get_TC_Node (Type_Spec (Declaration (E)));
            Helper := Get_From_Any_Node (Type_Spec (Declaration (E)));

            N := Make_Subprogram_Call
              (Make_Defining_Identifier (PN (P_Result)),
               Index_List);
            M := Make_Subprogram_Call
              (RE (RE_Get_Aggregate_Element),
               Make_List_Id
               (Nth_Element (PN (P_Aux), I - 1),
                TC,
                Make_Subprogram_Call
                (RE (RE_Unsigned_Long),
                 Make_List_Id (Copy_Node (Index_Node)))));
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
            Spec := From_Any_Node (BE_Node (Identifier (E)));

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
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            N := Make_Subprogram_Call
              (Map_Narrowing_Designator (E, False),
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
         begin
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            if Is_Base_Type (Type_Spec (Declaration (E))) then
               N := RE (Convert
                        (FEN.Kind
                         (FEN.Type_Spec
                          (FEN.Declaration (E)))));
               M := Get_From_Any_Node (Type_Spec (Declaration (E)));
            elsif Kind (Type_Spec (Declaration (E))) = K_Scoped_Name then
               N := Map_Designator (Type_Spec (Declaration (E)));
               M := Get_From_Any_Node (Type_Spec (Declaration (E)));
            elsif Kind (Type_Spec (Declaration (E))) = K_Fixed_Point_Type then

               --  Getting the identifier of the type defined in the stub spec

               N := Expand_Designator
                 (Stub_Type_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (E)))));

               --  Getting the node of the From_Any function of the Fixed_X_Y
               --  type

               M := Expand_Designator
                 (From_Any_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (E)))));
            elsif Kind (Type_Spec (Declaration (E))) = K_Sequence_Type then

               --  Getting the identifier of the Sequence type located in the
               --  instanciated package IDL_SEQUENCE_... in the stub spec

               N := Make_Defining_Identifier (TN (T_Sequence));
               Set_Correct_Parent_Unit_Name
                 (N,
                  Defining_Identifier
                  (Stub_Package_Node
                   (BE_Ada_Instanciations
                    (BE_Node
                     (Identifier (E))))));

               --  Getting the node of the From_Any function of the Sequence
               --  type located in the instanciated package IDL_SEQUENCE_...
               --  in the stub spec

               M := Expand_Designator
                 (From_Any_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (E)))));
            elsif Kind (Type_Spec (Declaration (E))) = K_String_Type or else
              Kind (Type_Spec (Declaration (E))) = K_Wide_String_Type then
               declare
                  Pkg_Inst : constant Node_Id :=
                    (Defining_Identifier
                     (Stub_Package_Node
                      (BE_Ada_Instanciations
                       (BE_Node
                        (Identifier (E))))));
               begin

                  --  Getting the identifier of the Bounded_[Wide_]String type
                  --  located in the instanciated package Bounded_... in the
                  --  stub spec

                  if Kind (Type_Spec (Declaration (E))) = K_String_Type then
                     N := Make_Defining_Identifier
                       (TN (T_Bounded_String));
                  else
                     N := Make_Defining_Identifier
                       (TN (T_Bounded_Wide_String));
                  end if;
                  Set_Correct_Parent_Unit_Name (N, Copy_Node (Pkg_Inst));

                  --  Getting the node of the From_Any function of the String
                  --  type located in the instanciated package Bounded_...
                  --  in the stub spec

                  M := Make_Defining_Identifier (SN (S_From_Any));
                  Set_Correct_Parent_Unit_Name (M, Copy_Node (Pkg_Inst));
               end;
            else
               raise Program_Error;
            end if;

            M := Make_Subprogram_Call
              (M,
               Make_List_Id (Make_Defining_Identifier (PN (P_Item))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Constant_Present    => True,
               Object_Definition   => N,
               Expression          => M);
            Append_Node_To_List (N, D);

            N := Make_Subprogram_Call
              (Return_Type (Spec),
               Make_List_Id (Make_Defining_Identifier (PN (P_Result))));

            N := Make_Return_Statement (N);
            Append_Node_To_List (N, S);

            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
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
            Spec := From_Any_Node (BE_Node (Identifier (E)));

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

                  if FEN.Kind (Declarator) = K_Simple_Declarator then
                     N := Make_Object_Declaration
                       (Defining_Identifier => Make_Defining_Identifier
                          (Result_Name),
                        Object_Definition => Copy_Designator
                          (Subtype_Indication
                           (Stub_Node
                            (BE_Node
                             (Identifier
                              (Declarator))))));
                     Append_Node_To_List (N, D);
                     TC := Get_TC_Node (Type_Spec (Declaration (Declarator)));
                     Helper := Get_From_Any_Node
                       (Type_Spec
                        (Declaration
                         (Declarator)));
                  else --  Complex declarator
                     N := Expand_Designator
                       (Type_Def_Node
                        (BE_Node
                         (Identifier
                          (Declarator))));
                     N := Make_Object_Declaration
                       (Defining_Identifier =>
                          Make_Defining_Identifier (Result_Name),
                        Object_Definition => N);
                     Append_Node_To_List (N, D);
                     TC := Expand_Designator
                       (TC_Node
                        (BE_Node
                         (Identifier
                          (Declarator))));
                     Helper := Expand_Designator
                       (From_Any_Node
                        (BE_Node
                         (Identifier
                          (Declarator))));

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

         ---------------------
         -- Union_Type_Body --
         ---------------------

         function Union_Type_Body (E : Node_Id) return Node_Id is
            Alternative_Name    : Name_Id;
            Switch_Alternative  : Node_Id;
            Switch_Alternatives : List_Id;
            Variant             : Node_Id;
            Choice              : Node_Id;
            Choices             : List_Id;
            Label               : Node_Id;
            From_Any_Helper     : Node_Id;
            TC_Helper           : Node_Id;
            Switch_Type         : Node_Id;
            Block_List          : List_Id;
            Literal_Parent      : Node_Id := No_Node;
            Orig_Type           : constant Node_Id :=
              FEU.Get_Original_Type (Switch_Type_Spec (E));
         begin
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            --  Declarative Part

            --  Getting the From_Any function the TC_XXX constant and the
            --  Ada type nodes corresponding to the discriminant type.

            TC_Helper := Get_TC_Node (Switch_Type_Spec (E));
            From_Any_Helper := Get_From_Any_Node (Switch_Type_Spec (E));

            if Is_Base_Type (Switch_Type_Spec (E)) then
               Switch_Type := RE (Convert (FEN.Kind (Switch_Type_Spec (E))));
            elsif FEN.Kind (Orig_Type) =  K_Enumeration_Type then
               Switch_Type := Map_Designator (Switch_Type_Spec (E));
               Literal_Parent := Map_Designator
                 (Scope_Entity
                  (Identifier
                   (Orig_Type)));
            else
               Switch_Type := Map_Designator (Switch_Type_Spec (E));
            end if;

            --  Declaration of the "Label_Any" Variable.

            N := Make_Subprogram_Call
              (RE (RE_Get_Aggregate_Element),
               Make_List_Id
               (Make_Designator (PN (P_Item)),
                TC_Helper,
                Make_Subprogram_Call
                (RE (RE_Unsigned_Long),
                 Make_List_Id (Make_Literal (Int0_Val)))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Label_Any)),
               Object_Definition   => RE (RE_Any),
               Expression          => N);
            Append_Node_To_List (N, D);

            --  Converting the "Label_Value" to to the discriminant type.

            N := Make_Subprogram_Call
              (From_Any_Helper,
               Make_List_Id
               (Make_Defining_Identifier (VN (V_Label_Any))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Label)),
               Constant_Present    => True,
               Object_Definition   => Switch_Type,
               Expression          => N);
            Append_Node_To_List (N, D);

            --  Declaring the "Result" variable

            N := Make_Subprogram_Call
              (Copy_Designator (Return_Type (Spec)),
               Make_List_Id
               (Make_Defining_Identifier (VN (V_Label))));

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => N);
            Append_Node_To_List (N, D);

            --  Declaring the "Index" variable

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Index)),
               Object_Definition   => RE (RE_Any));
            Append_Node_To_List (N, D);

            --  According to the IDL grammar, each "case" alternative contains
            --  exactly 1 element. So, there is no need to declare the "I"
            --  variable like it is done by Idlac.

            --  Statements

            Switch_Alternatives := New_List (K_Variant_List);
            Switch_Alternative := First_Entity (Switch_Type_Body (E));
            while Present (Switch_Alternative) loop
               Variant := New_Node (K_Variant);
               Choices := New_List (K_Discrete_Choice_List);
               Set_Discrete_Choices (Variant, Choices);
               Label   := First_Entity (Labels (Switch_Alternative));
               while Present (Label) loop

                  Choice := Make_Literal
                    (Value             => FEN.Value (Label),
                     Parent_Designator => Literal_Parent);
                  Append_Node_To_List (Choice, Choices);
                  Label := Next_Entity (Label);
               end loop;

               --  Getting the field fullname

               Alternative_Name := BEU.To_Ada_Name
                 (FEN.IDL_Name
                  (Identifier
                   (Declarator
                    (Element
                     (Switch_Alternative)))));
               Get_Name_String (PN (P_Result));
               Add_Char_To_Name_Buffer ('.');
               Get_Name_String_And_Append (Alternative_Name);
               Alternative_Name := Name_Find;

               --  Getting the From_Any function the TC_XXX constant and the
               --  Ada type nodes corresponding to the element type.

               if FEN.Kind
                 (Declarator
                  (Element
                   (Switch_Alternative)))
                 = K_Simple_Declarator
               then
                  TC_Helper := Get_TC_Node
                    (Type_Spec
                     (Element
                      (Switch_Alternative)));
                  From_Any_Helper := Get_From_Any_Node
                    (Type_Spec
                     (Element
                      (Switch_Alternative)));
               else --  Complex Declatator
                  TC_Helper := Expand_Designator
                    (TC_Node
                     (BE_Node
                      (Identifier
                       (Declarator
                        (Element
                         (Switch_Alternative))))));
                  From_Any_Helper := Expand_Designator
                    (From_Any_Node
                     (BE_Node
                      (Identifier
                       (Declarator
                        (Element
                         (Switch_Alternative))))));
               end if;

               if Is_Base_Type (Type_Spec (Element (Switch_Alternative))) then
                  Switch_Type := RE
                    (Convert
                     (FEN.Kind
                      (Type_Spec
                       (Element
                        (Switch_Alternative)))));
               elsif FEN.Kind (Type_Spec (Element (Switch_Alternative))) =
                 K_Scoped_Name
               then
                  Switch_Type := Map_Designator
                    (Type_Spec
                     (Element
                      (Switch_Alternative)));
               else
                  raise Program_Error;
               end if;

               Block_List := New_List (K_List_Id);

               --  Assigning the value to the "Index" variable

               N := Make_Subprogram_Call
                 (RE (RE_Get_Aggregate_Element),
                  Make_List_Id
                  (Make_Designator (PN (P_Item)),
                   TC_Helper,
                   Make_Subprogram_Call
                   (RE (RE_Unsigned_Long),
                    Make_List_Id (Make_Literal (Int1_Val)))));
               N := Make_Assignment_Statement
                 (Make_Defining_Identifier (VN (V_Index)),
                  N);
               Append_Node_To_List (N, Block_List);

               --  Converting the Any value

               N := Make_Subprogram_Call
                 (From_Any_Helper,
                  Make_List_Id
                  (Make_Defining_Identifier (VN (V_Index))));
               N := Make_Assignment_Statement
                 (Make_Defining_Identifier (Alternative_Name),
                  N);
               Append_Node_To_List (N, Block_List);

               N := Make_Block_Statement
                 (Declarative_Part => No_List,
                  Statements       => Block_List);

               Set_Component (Variant, N);
               Append_Node_To_List (Variant, Switch_Alternatives);

               Switch_Alternative := Next_Entity (Switch_Alternative);
            end loop;

            N := Make_Variant_Part
            (Make_Defining_Identifier (VN (V_Label)),
             Switch_Alternatives);
            Append_Node_To_List (N, S);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);

            N := Make_Subprogram_Implementation
              (Spec, D, S);
            return N;
         end Union_Type_Body;

         --------------------------------
         -- Exception_Declaration_Body --
         --------------------------------

         function Exception_Declaration_Body (E : Node_Id) return Node_Id is
            Members         : List_Id;
            Member          : Node_Id;
            Declarator      : Node_Id;
            Member_Id       : Node_Id;
            Member_Type     : Node_Id;
            Dcl_Name        : Name_Id;
            Index           : Unsigned_Long_Long;
            Param_List      : List_Id;
            Return_List     : List_Id;
            TC_Node         : Node_Id;
            From_Any_Helper : Node_Id;
         begin
            --  Obtaining the "From_Any" spec node from the helper spec
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            Members := FEN.Members (E);

            --  The generated code is fondamentally different depending on the
            --  existence or not of members in the exception.

            if FEU.Is_Empty (Members) then

               --  Begin Declarations

               --  Obtaining the node corresponding to the declaration of the
               --  "Excp_Name"_Members type.

               N := Type_Def_Node (BE_Node (Identifier (E)));
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

                     TC_Node := Get_TC_Node (Member_Type);
                     From_Any_Helper := Get_From_Any_Node (Member_Type);

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
                       (From_Any_Helper,
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

            when K_Interface_Declaration
              | K_Forward_Interface_Declaration =>
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
         N                : Node_Id;
         Dep              : Node_Id;
         V                : Value_Id;
         Aggregates       : List_Id;
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
      begin
         --  Declarative part
         --  Adding 'use' clause to make the code more readable

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings_Lists));
         Append_Node_To_List (N, Declarative_Part);

         --  Statements

         --  The package name

         Aggregates := New_List (K_List_Id);
         N := Defining_Identifier
           (Package_Declaration (Current_Package));
         V := New_String_Value
           (Fully_Qualified_Name (N), False);
         N := Make_Expression (Make_Literal (V), Op_Plus);
         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Name)),
            Expression          =>
              N);
         Append_Node_To_List (N, Aggregates);

         --  The conflicts

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Conflicts)),
            Expression          =>
              RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Building the dependancy list of the package

         N := RE (RE_Empty);

         --  Dependencies

         if not Is_Empty (Dependency_List) then
            Dep := First_Node (Dependency_List);
            while Present (Dep) loop
               N := Make_Expression (N, Op_And_Symbol, Dep);
               Dep := Next_Node (Dep);
            end loop;
         end if;

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Depends)),
            Expression     => N);
         Append_Node_To_List (N, Aggregates);

         --  Provides

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Provides)),
            Expression          =>
              RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Implicit

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Implicit)),
            Expression          =>
              RE (RE_False));
         Append_Node_To_List (N, Aggregates);

         --  Init procedure

         N := Make_Component_Association
           (Selector_Name  =>
              Make_Defining_Identifier (PN (P_Init)),
            Expression          =>
              Make_Type_Attribute
            (Make_Designator (SN (S_Deferred_Initialization)),
             A_Access));
         Append_Node_To_List (N, Aggregates);

         --  Registering the module

         N := Make_Record_Aggregate
           (Aggregates);

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Aggregate    => N);

         N := Make_Subprogram_Call
           (RE (RE_Register_Module),
            Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         --  Building the initialization block statement

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
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
         function Forward_Interface_Declaration_Body
           (E : Node_Id)
           return Node_Id;
         function Interface_Declaration_Body (E : Node_Id) return Node_Id;
         function Simple_Declarator_Body (E : Node_Id) return Node_Id;
         function Structure_Type_Body (E : Node_Id) return Node_Id;
         function Union_Type_Body (E : Node_Id) return Node_Id;
         function Exception_Declaration_Body (E : Node_Id) return Node_Id;

         -----------------------------
         -- Complex_Declarator_Body --
         -----------------------------

         function Complex_Declarator_Body (E : Node_Id) return Node_Id is
            I                    : Nat := 0;
            L                    : List_Id;
            Sizes                : constant List_Id :=
              Range_Constraints
              (Type_Definition (Type_Def_Node (BE_Node (Identifier (E)))));
            Dimension            : constant Natural := BEU.Length (Sizes);
            Dim                  : Node_Id;
            Loop_Statements      : List_Id := No_List;
            Enclosing_Statements : List_Id;
            Helper               : Node_Id;
            TC                   : Node_Id;
            Result_Node          : Node_Id;
         begin
            Spec := To_Any_Node (BE_Node (Identifier (E)));

            N := Declare_Any_Array (PN (P_Result), 0, Dimension - 1);
            Append_Node_To_List (N, D);

            L := New_List (K_List_Id);
            TC := TC_Node (BE_Node (Identifier (E)));
            Dim := First_Node (Sizes);
            loop
               Set_Str_To_Name_Buffer ("I");
               Add_Nat_To_Name_Buffer (I);
               M := Make_Defining_Identifier
                 (Add_Suffix_To_Name (Var_Suffix, Name_Find));
               Append_Node_To_List (M, L);
               Enclosing_Statements := Loop_Statements;
               Loop_Statements := New_List (K_List_Id);
               N := Make_For_Statement
                 (M, Dim, Loop_Statements);

               if I > 0 then
                  Result_Node := Nth_Element (PN (P_Result), I);
                  Result_Node := Make_Assignment_Statement
                    (Result_Node,
                     Make_Subprogram_Call
                     (RE (RE_Get_Empty_Any_Aggregate),
                      Make_List_Id
                      (Expand_Designator (TC))));
                  Append_Node_To_List (Result_Node, Enclosing_Statements);

                  Append_Node_To_List (N, Enclosing_Statements);

                  Result_Node := Make_Subprogram_Call
                    (RE (RE_Add_Aggregate_Element),
                     Make_List_Id
                     (Nth_Element (PN (P_Result), I - 1),
                      Nth_Element (PN (P_Result), I)));
                  Append_Node_To_List (Result_Node, Enclosing_Statements);
               else
                  Result_Node := Nth_Element (PN (P_Result), I);
                  Result_Node := Make_Assignment_Statement
                    (Result_Node,
                     Make_Subprogram_Call
                     (RE (RE_Get_Empty_Any_Aggregate),
                      Make_List_Id
                      (Expand_Designator (TC))));
                  Append_Node_To_List (Result_Node, S);

                  Append_Node_To_List (N, S);
               end if;

               I := I + 1;
               TC := Next_Node (TC);
               Dim := Next_Node (Dim);
               exit when No (Dim);
            end loop;

            Helper := Get_To_Any_Node (Type_Spec (Declaration (E)));

            N := Make_Subprogram_Call
              (Make_Defining_Identifier (PN (P_Item)), L);
            N := Make_Subprogram_Call
              (Helper,
               Make_List_Id (N));
            N := Make_Subprogram_Call
              (RE (RE_Add_Aggregate_Element),
               Make_List_Id
               (Nth_Element (PN (P_Result), I - 1), N));
            Append_Node_To_List (N, Loop_Statements);
            N := Make_Return_Statement
              (Nth_Element (PN (P_Result), 0));
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
            Spec := To_Any_Node (BE_Node (Identifier (E)));

            N := RE (RE_Get_Empty_Any_Aggregate);
            Helper_Name := BEN.Name
              (Defining_Identifier (TC_Node (BE_Node (Identifier (E)))));
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

         ----------------------------------------
         -- Forward_Interface_Declaration_Body --
         ----------------------------------------

         function Forward_Interface_Declaration_Body
           (E : Node_Id)
           return Node_Id
         is
         begin
            Spec := To_Any_Node (BE_Node (Identifier (E)));
            N := Make_Subprogram_Call
              (RE (RE_Ref_2),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Item))));
            N := Make_Subprogram_Call
              (RE (RE_To_Any_3),
               Make_List_Id (N));
            N := Make_Return_Statement (N);
            Append_Node_To_List (N, S);
            N := Make_Subprogram_Implementation
              (Spec, No_List, S);
            return N;
         end Forward_Interface_Declaration_Body;

         --------------------------------
         -- Interface_Declaration_Body --
         --------------------------------

         function Interface_Declaration_Body (E : Node_Id) return Node_Id is
         begin
            Spec := TC_Node (BE_Node (Identifier (E)));

            --  Getting the identifier of the TC_"Interface_name" variable
            --  declared in the Helper spec.

            Helper_Name := BEN.Name (Defining_Identifier (Spec));

            --  Getting the node of the To_Any method spec declared at the 3rd
            --  place of the helper spec.

            Spec := To_Any_Node (BE_Node (Identifier (E)));
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
            Spec := To_Any_Node (BE_Node (Identifier (E)));

            if Is_Base_Type (Type_Spec (Declaration (E))) then
               N := RE (Convert
                        (FEN.Kind (FEN.Type_Spec (FEN.Declaration (E)))));
               M := Get_To_Any_Node (Type_Spec (Declaration (E)));
            elsif Kind (Type_Spec (Declaration (E))) = K_Scoped_Name then
               N := Map_Designator (Type_Spec (Declaration (E)));
               M := Get_To_Any_Node (Type_Spec (Declaration (E)));
            elsif Kind (Type_Spec (Declaration (E))) = K_Fixed_Point_Type then

               --  Getting the identifier of the type defined in the stub spec

               N := Expand_Designator
                 (Stub_Type_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (E)))));
               --  Getting the node of the From_Any function of the Fixed_X_Y
               --  type

               M := Expand_Designator
                 (To_Any_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (E)))));
            elsif Kind (Type_Spec (Declaration (E))) = K_Sequence_Type then

               --  Getting the identifier of the Sequence type located in the
               --  instanciated package IDL_SEQUENCE_... in the stub spec.

               N := Make_Defining_Identifier (TN (T_Sequence));
               Set_Correct_Parent_Unit_Name
                 (N,
                  Defining_Identifier
                  (Stub_Package_Node
                   (BE_Ada_Instanciations
                    (BE_Node
                     (Identifier (E))))));

               --  Getting the node of the To_Any function of the Sequence
               --  type located in the instanciated package IDL_SEQUENCE_...
               --  in the stub spec

               M := Expand_Designator
                 (To_Any_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (E)))));

            elsif Kind (Type_Spec (Declaration (E))) = K_String_Type or else
              Kind (Type_Spec (Declaration (E))) = K_Wide_String_Type then
               declare
                  Pkg_Inst : constant Node_Id :=
                    (Defining_Identifier
                     (Stub_Package_Node
                      (BE_Ada_Instanciations
                       (BE_Node
                        (Identifier (E))))));
               begin

                  --  Getting the identifier of the Bounded_[Wide_]String type
                  --  located in the instanciated package Bounded_... in the
                  --  stub spec

                  if Kind (Type_Spec (Declaration (E))) = K_String_Type then
                     N := Make_Defining_Identifier
                       (TN (T_Bounded_String));
                  else
                     N := Make_Defining_Identifier
                       (TN (T_Bounded_Wide_String));
                  end if;
                  Set_Correct_Parent_Unit_Name (N, Copy_Node (Pkg_Inst));

                  --  Getting the node of the To_Any function of the String
                  --  type located in the instanciated package Bounded_...
                  --  in the stub spec

                  M := Make_Defining_Identifier (SN (S_To_Any));
                  Set_Correct_Parent_Unit_Name (M, Copy_Node (Pkg_Inst));
               end;
            else
               raise Program_Error;
            end if;

            Helper_Name := BEN.Name
              (Defining_Identifier (TC_Node (BE_Node (Identifier (E)))));
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
            To_Any_Helper   : Node_Id;
         begin
            Spec := To_Any_Node (BE_Node (Identifier (E)));

            N := RE (RE_Get_Empty_Any_Aggregate);
            Helper_Name := BEN.Name
              (Defining_Identifier (TC_Node (BE_Node (Identifier (E)))));
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
                  Set_Correct_Parent_Unit_Name (Designator, Item_Designator);

                  --  Getting the declarator type in order to call the right
                  --  To_Any function

                  if FEN.Kind (Declarator) = K_Simple_Declarator then
                     To_Any_Helper := Get_To_Any_Node
                       (Type_Spec
                        (Declaration
                         (Declarator)));
                  else --  Complex declarator
                     To_Any_Helper := Expand_Designator
                       (To_Any_Node
                        (BE_Node
                         (Identifier
                          (Declarator))));
                  end if;

                  N := Make_Subprogram_Call
                    (To_Any_Helper,
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

         ---------------------
         -- Union_Type_Body --
         ---------------------

         function Union_Type_Body (E : Node_Id) return Node_Id is
            Switch_Item         : Name_Id;
            Alternative_Name    : Name_Id;
            Switch_Alternative  : Node_Id;
            Switch_Alternatives : List_Id;
            Variant             : Node_Id;
            Choice              : Node_Id;
            Choices             : List_Id;
            Label               : Node_Id;
            To_Any_Helper       : Node_Id;
            Literal_Parent      : Node_Id := No_Node;
            Orig_Type           : constant Node_Id :=
              FEU.Get_Original_Type (Switch_Type_Spec (E));
         begin
            Spec := To_Any_Node (BE_Node (Identifier (E)));

            --  Declarative Part

            N := RE (RE_Get_Empty_Any_Aggregate);
            N := Make_Subprogram_Call
              (N,
               Make_List_Id
               (Expand_Designator (TC_Node (BE_Node (Identifier (E))))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => RE (RE_Any),
               Expression => N);
            Append_Node_To_List (N, D);

            --  Statements

            --  Getting the "Item.Switch" name

            Get_Name_String (PN (P_Item));
            Add_Char_To_Name_Buffer ('.');
            Get_Name_String_And_Append (CN (C_Switch));
            Switch_Item := Name_Find;

            --  Getting the To_Any function of the union Switch

            To_Any_Helper := Get_To_Any_Node (Switch_Type_Spec (E));

            if FEN.Kind (Orig_Type) = K_Enumeration_Type then
               Literal_Parent := Map_Designator
                 (Scope_Entity
                  (Identifier
                   (Orig_Type)));
            end if;

            N := Make_Subprogram_Call
              (To_Any_Helper,
               Make_List_Id (Make_Defining_Identifier (Switch_Item)));
            N := Make_Subprogram_Call
              (RE (RE_Add_Aggregate_Element),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Result)),
                N));
            Append_Node_To_List (N, S);

            Switch_Alternatives := New_List (K_Variant_List);
            Switch_Alternative := First_Entity (Switch_Type_Body (E));
            while Present (Switch_Alternative) loop
               Variant := New_Node (K_Variant);
               Choices := New_List (K_Discrete_Choice_List);
               Set_Discrete_Choices (Variant, Choices);
               Label   := First_Entity (Labels (Switch_Alternative));
               while Present (Label) loop

                  Choice := Make_Literal
                    (Value             => FEN.Value (Label),
                     Parent_Designator => Literal_Parent);
                  Append_Node_To_List (Choice, Choices);
                  Label := Next_Entity (Label);
               end loop;

               --  Getting the field fullname

               Alternative_Name := BEU.To_Ada_Name
                 (FEN.IDL_Name
                  (Identifier
                   (Declarator
                    (Element
                     (Switch_Alternative)))));
               Get_Name_String (PN (P_Item));
               Add_Char_To_Name_Buffer ('.');
               Get_Name_String_And_Append (Alternative_Name);
               Alternative_Name := Name_Find;

               --  Getting the To_Any function node corresponding to the
               --  element type.
               --  Getting the From_Any function the TC_XXX constant and the
               --  Ada type nodes corresponding to the element type.

               if FEN.Kind
                 (Declarator
                  (Element
                   (Switch_Alternative)))
                 = K_Simple_Declarator
               then
                  To_Any_Helper := Get_To_Any_Node
                    (Type_Spec
                     (Element
                      (Switch_Alternative)));
               else --  Complex Declatator
                  To_Any_Helper := Expand_Designator
                    (To_Any_Node
                     (BE_Node
                      (Identifier
                       (Declarator
                        (Element
                         (Switch_Alternative))))));
               end if;

               N := Make_Subprogram_Call
                 (To_Any_Helper,
                  Make_List_Id
                  (Make_Defining_Identifier
                   (Alternative_Name)));

               N := Make_Subprogram_Call
                 (RE (RE_Add_Aggregate_Element),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Result)),
                   N));
               Set_Component (Variant, N);
               Append_Node_To_List (Variant, Switch_Alternatives);

               Switch_Alternative := Next_Entity (Switch_Alternative);
            end loop;
            N := Make_Variant_Part
            (Make_Defining_Identifier (Switch_Item),
             Switch_Alternatives);
            Append_Node_To_List (N, S);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
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
            Spec := To_Any_Node (BE_Node (Identifier (E)));

            --  Begin Declarations

            --  Obtaining the node corresponding to the declaration of the
            --  TC_"Excp_Name" constant.

            N := TC_Node (BE_Node (Identifier (E)));
            N := Expand_Designator (N);
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

                     To_Any_Helper := Get_To_Any_Node (Member_Type);

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

            when K_Forward_Interface_Declaration =>
               N := Forward_Interface_Declaration_Body (E);

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

      -------------------
      -- U_To_Ref_Body --
      -------------------

      function U_To_Ref_Body
        (E : Node_Id)
        return Node_Id
      is
         Spec         : Node_Id;
         Declarations : List_Id;
         Statements   : List_Id;
         Param        : Node_Id;
         N            : Node_Id;
         L            : List_Id;
         S_Set_Node   : Node_Id;
      begin
         --  The spec of the Unhecked_To_Ref function

         Spec := U_To_Ref_Node (BE_Node (Identifier (E)));

         --  Declarative Part

         Declarations := New_List (K_List_Id);
         Param := Make_Object_Declaration
           (Defining_Identifier =>
              Make_Defining_Identifier (PN (P_Result)),
            Object_Definition =>
              Expand_Designator (Type_Def_Node (BE_Node (Identifier (E)))));
         Append_Node_To_List (Param, Declarations);

         --  Statements Part

         S_Set_Node := Make_Defining_Identifier (SN (S_Set));

         --  Depending on the nature of node E :
         --  * If E is an Interface declaration, we use the Set function
         --    inherited from CORBA.Object.Ref
         --  * If E is a forward Interface declaration, we use the Set function
         --    defined in the instanciated package.

         if FEN.Kind (E) = K_Forward_Interface_Declaration then
            Set_Correct_Parent_Unit_Name
              (S_Set_Node,
               Defining_Identifier
               (Stub_Package_Node
                (BE_Ada_Instanciations
                 (BE_Node
                  (Identifier
                   (E))))));
         end if;
         Statements := New_List (K_List_Id);
         L := New_List (K_List_Id);
         Append_Node_To_List
           (Make_Defining_Identifier (PN (P_Result)), L);
         Append_Node_To_List
           (Make_Subprogram_Call
            (RE (RE_Object_Of),
             Make_List_Id (Make_Defining_Identifier (PN (P_The_Ref)))), L);
         N := Make_Subprogram_Call
           (Defining_Identifier   => S_Set_Node,
            Actual_Parameter_Part => L);
         Append_Node_To_List (N, Statements);
         N := Make_Return_Statement
           (Make_Defining_Identifier (PN (P_Result)));
         Append_Node_To_List (N, Statements);
         N := Make_Subprogram_Implementation
           (Spec, Declarations, Statements);
         return N;
      end U_To_Ref_Body;

      -----------------
      -- To_Ref_Body --
      -----------------

      function To_Ref_Body (E : Node_Id) return Node_Id is
         Spec        : Node_Id;
         Statements  : List_Id;
         N           : Node_Id;
         M           : Node_Id;
         Rep_Id      : Node_Id;
      begin
         --  The spec of the To_Ref function

         Spec := To_Ref_Node (BE_Node (Identifier (E)));

         --  The value of the Rep_Id depends on the nature of E node :
         --  * K_Interface_Declaration : we use the variable Repository_Id
         --    declared in the stub.
         --  * K_Forward_Interface_Declaration : we cannot use the
         --    Repository_Id variable because it designates another entity.
         --    So, we build a literal string value.

         if FEN.Kind (E) = K_Interface_Declaration then
            Rep_Id := Make_Defining_Identifier (PN (P_Repository_Id));
         elsif FEN.Kind (E) = K_Forward_Interface_Declaration then
            Rep_Id := Make_Literal
              (BEN.Value
               (BEN.Expression
                (Next_Node
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (Forward
                     (E))))))));

         else
            raise Program_Error;
         end if;

         Statements := New_List (K_List_Id);
         N := Make_Expression
           (Left_Expr => Make_Subprogram_Call
            (RE (RE_Is_Nil),
             Make_List_Id (Make_Defining_Identifier (PN (P_The_Ref)))),
            Operator   => Op_Or_Else,
            Right_Expr => Make_Subprogram_Call
            (RE (RE_Is_A),
             Make_List_Id
             (Make_Defining_Identifier (PN (P_The_Ref)),
              Rep_Id)));
         M := Make_Subprogram_Call
           (Map_Narrowing_Designator (E, True),
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
      end To_Ref_Body;

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

         Excp_Members := Type_Def_Node (BE_Node (Identifier (E)));

         --  Preparing the call to From_Any

         N := Make_Subprogram_Call
           (RE (RE_To_CORBA_Any),
            Make_List_Id (Make_Defining_Identifier (PN (P_Item))));
         From_Any_Helper := Expand_Designator
           (From_Any_Node
            (BE_Node
             (Identifier
              (E))));

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

            when K_Forward_Interface_Declaration =>
               Visit_Forward_Interface_Declaration (E);

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

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Helper_Body;

         Append_Node_To_List
           (From_Any_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Any_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (U_To_Ref_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Ref_Body (E), Statements (Current_Package));

         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Deferred_Initialization_Body);
      end Visit_Forward_Interface_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
         Deferred_Initialization_Body_Backup : List_Id;
         Package_Initializarion_Backup       : List_Id;
         Dependency_List_Backup              : List_Id;
         Is_Local : constant Boolean := Is_Local_Interface (E);
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
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
         Dependency_List_Backup :=
           Dependency_List;

         Deferred_Initialization_Body := New_List (K_List_Id);
         Package_Initializarion       := New_List (K_List_Id);
         Dependency_List              := New_List (K_List_Id);

         if not Is_Local then
            Append_Node_To_List
              (From_Any_Body (E), Statements (Current_Package));
            Append_Node_To_List
              (To_Any_Body (E), Statements (Current_Package));
         end if;

         Append_Node_To_List
           (U_To_Ref_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Ref_Body (E), Statements (Current_Package));

         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Deferred_Initialization_Body);

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritence, generate the mappings for
         --  the operations and attributes of the parents except the first one.

         Map_Inherited_Entities_Bodies
           (Current_interface    => E,
            Visit_Operation_Subp => null,
            Helper               => True);

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
         Dependency_List :=
           Dependency_List_Backup;

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
         Dependency_List_Backup              : List_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Helper_Body) then
            D := Stub_Node (BE_Node (Identifier (E)));
            Push_Entity (D);

            Set_Helper_Body;

            --  Handling the case of modules nested in modules :
            --  we save a backup of the Deferred_Initialization_Body and the
            --  Package_Initializarion lists because the helper package of
            --  a module is different from the helper package of an interface.

            Deferred_Initialization_Body_Backup :=
              Deferred_Initialization_Body;
            Package_Initializarion_Backup :=
              Package_Initializarion;
            Dependency_List_Backup :=
              Dependency_List;

            Deferred_Initialization_Body := New_List (K_List_Id);
            Package_Initializarion       := New_List (K_List_Id);
            Dependency_List              := New_List (K_List_Id);

            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            --  If no statement have been added to the package before the
            --  deferred initialiazation subprogram, the body is kept empty
            --  and is not generated.

            if not Is_Empty (Statements (Current_Package)) then
               N := Make_Subprogram_Implementation
                 (Make_Subprogram_Specification
                  (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
                   No_List),
                  No_List,
                  Deferred_Initialization_Body);
               Append_Node_To_List (N, Statements (Current_Package));
               Helper_Initialization (Package_Initializarion);
               Set_Package_Initialization
                 (Current_Package, Package_Initializarion);
            end if;

            --  Restoring old values

            Deferred_Initialization_Body :=
              Deferred_Initialization_Body_Backup;
            Package_Initializarion :=
              Package_Initializarion_Backup;
            Dependency_List :=
              Dependency_List_Backup;

            Pop_Entity;
         end if;
      end Visit_Module;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         Set_Helper_Spec;

         Deferred_Initialization_Body := New_List (K_List_Id);
         Package_Initializarion       := New_List (K_List_Id);
         Dependency_List              := New_List (K_List_Id);

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
         N          : Node_Id;
         Member     : Node_Id;
         Declarator : Node_Id;
      begin
         Set_Helper_Body;

         --  For each complex declarator, a new type is defined (see the stub
         --  generation for more details). For each defined type, a TC_XXXX
         --  Constant, a From_Any and a To_Any functions must be generated.

         Member := First_Entity (Members (E));
         while Present (Member) loop
            Declarator := First_Entity (Declarators (Member));
            while Present (Declarator) loop
               if FEN.Kind (Declarator) = K_Complex_Declarator then
                  Append_Node_To_List
                    (From_Any_Body (Declarator), Statements (Current_Package));
                  Append_Node_To_List
                    (To_Any_Body (Declarator), Statements (Current_Package));
                  N := Deferred_Initialization_Block (Declarator);
                  Append_Node_To_List (N, Deferred_Initialization_Body);
               end if;
               Declarator := Next_Entity (Declarator);
            end loop;
            Member := Next_Entity (Member);
         end loop;

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
         T : Node_Id;

         procedure Visit_Fixed_Type_Declaration
           (Type_Node : Node_Id;
            Declarator : Node_Id);
         procedure Visit_Sequence_Type_Declaration
           (Type_Node : Node_Id;
            Declarator : Node_Id);

         ----------------------------------
         -- Visit_Fixed_Type_Declaration --
         ----------------------------------

         procedure Visit_Fixed_Type_Declaration
           (Type_Node : Node_Id;
            Declarator : Node_Id)
         is
            F            : Node_Id;
            Package_Name : Name_Id;
            Package_Id   : Node_Id;
            Profile      : List_Id;
            Parameter    : Node_Id;
            Renamed_Subp : Node_Id;
         begin
            --  Instanciation of the package :

            F := Stub_Type_Node
              (BE_Ada_Instanciations
               (BE_Node
                (Identifier (Declarator))));
            F := Defining_Identifier (F);
            Set_Str_To_Name_Buffer ("CDR_");
            Get_Name_String_And_Append (BEN.Name (F));
            Package_Name := Name_Find;
            Package_Id := Make_Defining_Identifier (Package_Name);
            N := Make_Subprogram_Call
              (RU (RU_CORBA_Fixed_Point),
               Make_List_Id (F));
            N := Make_Package_Instantiation
              (Defining_Identifier => Package_Id,
               Generic_Package     => N);
            Append_Node_To_List (N, Statements (Current_Package));

            --  The From_Any and To_Any functions for the fixed point type
            --  are homonymes of those of the instanciated package.

            --  From_Any

            Renamed_Subp := Make_Defining_Identifier (SN (S_From_Any));
            Set_Correct_Parent_Unit_Name (Renamed_Subp, Package_Id);
            Profile  := New_List (K_Parameter_Profile);
            Parameter := Make_Parameter_Specification
              (Make_Defining_Identifier (PN (P_Item)),
               RE (RE_Any));
            Append_Node_To_List (Parameter, Profile);
            N := Make_Subprogram_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (SN (S_From_Any)),
               Parameter_Profile   =>
                 Profile,
               Return_Type         =>
                 F,
               Renamed_Subprogram  =>
                 Renamed_Subp);
            Append_Node_To_List (N, Statements (Current_Package));

            --  To_Any

            Renamed_Subp := Make_Defining_Identifier (SN (S_To_Any));
            Set_Correct_Parent_Unit_Name (Renamed_Subp, Package_Id);
            Profile  := New_List (K_Parameter_Profile);
            Parameter := Make_Parameter_Specification
              (Make_Defining_Identifier (PN (P_Item)),
               F);
            Append_Node_To_List (Parameter, Profile);
            N := Make_Subprogram_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (SN (S_To_Any)),
               Parameter_Profile   =>
                 Profile,
               Return_Type         =>
                 RE (RE_Any),
               Renamed_Subprogram  =>
                 Renamed_Subp);
            Append_Node_To_List (N, Statements (Current_Package));

            N := Deferred_Initialization_Block (Type_Node);
            Append_Node_To_List (N, Deferred_Initialization_Body);
         end Visit_Fixed_Type_Declaration;

         -------------------------------------
         -- Visit_Sequence_Type_Declaration --
         -------------------------------------

         procedure Visit_Sequence_Type_Declaration
           (Type_Node : Node_Id;
            Declarator : Node_Id)
         is
            S                 : Node_Id;
            Package_Name      : Name_Id;
            Package_Id        : Node_Id;
            Profile           : List_Id;
            Parameter         : Node_Id;
            Renamed_Subp      : Node_Id;
            From_Any_Helper   : Node_Id;
            To_Any_Helper     : Node_Id;
            TC_Helper         : Node_Id;
            TC_Package_Helper : Node_Id;
            Seq_Type          : Node_Id;
            Max_Size_Literal  : Node_Id;
            Init_Block_List    : List_Id;
         begin
            --  Instanciation of the package :

            S := Stub_Package_Node
              (BE_Ada_Instanciations
               (BE_Node
                (Identifier (Declarator))));
            S := Defining_Identifier (S);
            Package_Name := Add_Suffix_To_Name
              ("_Helper",
               BEN.Name (S));
            Package_Id := Make_Defining_Identifier (Package_Name);

            --  getting the TypeCode, the From_any and the To_Any functions
            --  nodes corresponding to the spec type of the sequence.

            TC_Helper := Get_TC_Node (Type_Spec (Type_Node));
            From_Any_Helper := Get_From_Any_Node (Type_Spec (Type_Node));
            To_Any_Helper := Get_To_Any_Node (Type_Spec (Type_Node));

            Profile  := New_List (K_List_Id);
            Append_Node_To_List
              (Make_Component_Association
               (Make_Defining_Identifier (PN (P_Element_From_Any)),
                From_Any_Helper),
               Profile);
            Append_Node_To_List
              (Make_Component_Association
               (Make_Defining_Identifier (PN (P_Element_To_Any)),
                To_Any_Helper),
               Profile);

            --  Here, we must add manually "with" clauses to :
            --  PolyORB.Sequences.Bounded.CORBA_Helper
            --   or
            --  PolyORB.Sequences.Unbounded.CORBA_Helper

            if Present (Max_Size (T)) then
               Add_With_Package
                 (RU (RU_PolyORB_Sequences_Bounded_CORBA_Helper));
            else
               Add_With_Package
                 (RU (RU_PolyORB_Sequences_Unbounded_CORBA_Helper));
            end if;

            N := RE (RE_CORBA_Helper);
            Set_Correct_Parent_Unit_Name (N, S);

            N := Make_Package_Instantiation
              (Defining_Identifier => Package_Id,
               Generic_Package     => N,
               Parameter_List      => Profile);
            Append_Node_To_List (N, Statements (Current_Package));

            --  The From_Any and To_Any functions for the sequence type
            --  rename those of the instanciated package.

            --  From_Any

            Renamed_Subp := Make_Defining_Identifier (SN (S_From_Any));
            Set_Correct_Parent_Unit_Name (Renamed_Subp, Package_Id);

            Profile  := New_List (K_Parameter_Profile);
            Parameter := Make_Parameter_Specification
              (Make_Defining_Identifier (PN (P_Item)),
               RE (RE_Any));
            Append_Node_To_List (Parameter, Profile);

            Seq_Type := Make_Defining_Identifier (TN (T_Sequence));
            Set_Correct_Parent_Unit_Name (Seq_Type, S);

            N := Make_Subprogram_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (SN (S_From_Any)),
               Parameter_Profile   =>
                 Profile,
               Return_Type         =>
                 Seq_Type,
               Renamed_Subprogram  =>
                 Renamed_Subp);
            Append_Node_To_List (N, Statements (Current_Package));

            --  To_Any

            Renamed_Subp := Make_Defining_Identifier (SN (S_To_Any));
            Set_Correct_Parent_Unit_Name (Renamed_Subp, Package_Id);

            Seq_Type := Make_Defining_Identifier (TN (T_Sequence));
            Set_Correct_Parent_Unit_Name (Seq_Type, S);

            Profile  := New_List (K_Parameter_Profile);
            Parameter := Make_Parameter_Specification
              (Make_Defining_Identifier (PN (P_Item)),
               Seq_Type);
            Append_Node_To_List (Parameter, Profile);

            N := Make_Subprogram_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (SN (S_To_Any)),
               Parameter_Profile   =>
                 Profile,
               Return_Type         =>
                 RE (RE_Any),
               Renamed_Subprogram  =>
                 Renamed_Subp);
            Append_Node_To_List (N, Statements (Current_Package));

            --  The deferred initialisation part

            Init_Block_List := New_List (K_List_Id);

            --  Unbounded, sequences have "0" as limit

            if Present (Max_Size (T)) then
               Max_Size_Literal := Make_Literal
                 (FEN.Value (Max_Size (T)));
            else
               Max_Size_Literal := Make_Literal
                 (New_Integer_Value (0, 1, 10));
            end if;

            TC_Package_Helper := Expand_Designator
              (TC_Node
               (BE_Ada_Instanciations
                (BE_Node
                 (Identifier
                  (Declarator)))));
            N := Make_Assignment_Statement
              (TC_Package_Helper,
               Make_Subprogram_Call
               (RE (RE_Build_Sequence_TC),
                Make_List_Id
                (TC_Helper,
                 Max_Size_Literal)));
            Append_Node_To_List (N, Init_Block_List);

            N := Make_Defining_Identifier (SN (S_Initialize));
            Set_Correct_Parent_Unit_Name (N, Package_Id);

            N := Make_Subprogram_Call
              (N,
               Make_List_Id
               (Make_Component_Association
                (RE (RE_Element_TC), TC_Helper),
                Make_Component_Association
                (RE (RE_Sequence_TC), TC_Package_Helper)));
            Append_Node_To_List (N, Init_Block_List);

            N := Make_Block_Statement
              (Declarative_Part => No_List,
               Statements       => Init_Block_List);

            Append_Node_To_List (N, Deferred_Initialization_Body);
         end Visit_Sequence_Type_Declaration;

      begin
         Set_Helper_Body;
         L := Declarators (E);
         T := Type_Spec (E);
         D := First_Entity (L);

         --  Handling the particular cases such as fixed point types definition
         --  and sequence types definitions

         case (FEN.Kind (T)) is

            when  K_Fixed_Point_Type =>
               Visit_Fixed_Type_Declaration (T, D);

            when K_Sequence_Type =>
               Visit_Sequence_Type_Declaration (T, D);

            when others =>
               null;

         end case;

         while Present (D) loop

            --  If the new type is defined basing on an interface type, then
            --  we dont generate From_Any nor To_Any. We use those of the
            --  original type.

            if FEN.Kind (T) = K_Scoped_Name
              and then
              Is_Object_Type (T)
              and then
              FEN.Kind (D) = K_Simple_Declarator then
               null; --  We add nothing
            else
               Append_Node_To_List
                 (From_Any_Body (D), Statements (Current_Package));
               Append_Node_To_List
                 (To_Any_Body (D), Statements (Current_Package));
            end if;

            N := Deferred_Initialization_Block (D);
            Append_Node_To_List (N, Deferred_Initialization_Body);

            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N            : Node_Id;
         Alternatives : List_Id;
         Alternative  : Node_Id;
         Declarator   : Node_Id;
      begin
         Set_Helper_Body;

         --  For each complex declarator, a new type is defined (see the stub
         --  generation for more details). For each defined type, a TC_XXXX
         --  Constant, a From_Any and a To_Any functions must be generated.

         Alternatives := Switch_Type_Body (E);
         Alternative := First_Entity (Alternatives);
         while Present (Alternative) loop
            Declarator := FEN.Declarator (FEN.Element (Alternative));
            if FEN.Kind (Declarator) = K_Complex_Declarator then
               Append_Node_To_List
                 (From_Any_Body (Declarator), Statements (Current_Package));
               Append_Node_To_List
                 (To_Any_Body (Declarator), Statements (Current_Package));
               N := Deferred_Initialization_Block (Declarator);
               Append_Node_To_List (N, Deferred_Initialization_Body);
            end if;
            Alternative := Next_Entity (Alternative);
         end loop;

         Append_Node_To_List
           (From_Any_Body (E), Statements (Current_Package));
         Append_Node_To_List
           (To_Any_Body (E), Statements (Current_Package));

         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Deferred_Initialization_Body);
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
   end Package_Body;
end Backend.BE_Ada.Helpers;
