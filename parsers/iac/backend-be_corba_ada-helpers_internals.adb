------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                 BACKEND.BE_CORBA_ADA.HELPERS_INTERNALS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                           Copyright (c) 2006                             --
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

with Namet;  use Namet;
with Values; use Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Helpers;     use Backend.BE_CORBA_Ada.Helpers;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Expand;      use Backend.BE_CORBA_Ada.Expand;

package body Backend.BE_CORBA_Ada.Helpers_Internals is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;

   --  FIXME : The constant above should be removed once all the
   --  shadow any code generation is acheived. It's used only to make
   --  IAC work in intermediary phase

   Generate_Shadow_Routines : constant Boolean := False;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Forward_Interface_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);

      function Initialize_Spec (E : Node_Id) return Node_Id;
      --  Returns the spec of the Initialize procedure that builds the
      --  TypeCode corresponding to the IDL type E.

      --  The routines below build the type declarations necessary for
      --  the Shadow Any Tree.

      function Wrap_Spec (E : Node_Id) return Node_Id;
      --  Builds the spec of the Wrap function corresponding to IDL
      --  type E

      function Pointer_Declaration (E : Node_Id) return Node_Id;
      --  Makes a pointer type declaration corresponding to the mapped
      --  Ada type of the IDL type E

      function Content_Declaration (E : Node_Id) return Node_Id;
      --  Makes a record type declaration correponding to the
      --  Aggregate container for IDL type E

      function Clone_Spec (E : Node_Id) return Node_Id;
      function Finalize_Value_Spec (E : Node_Id) return Node_Id;
      function Get_Aggregate_Count_Spec (E : Node_Id) return Node_Id;
      function Set_Aggregate_Count_Spec (E : Node_Id) return Node_Id;
      function Get_Aggregate_Element_Spec (E : Node_Id) return Node_Id;
      function Set_Aggregate_Element_Spec (E : Node_Id) return Node_Id;
      --  Specs for the routines that manipulate the aggregate
      --  container

      function Lengths_Constant_Declaration (E : Node_Id) return Node_Id;
      --  Makes a constant declaration containing the length of the
      --  several dimensions of an array

      procedure Aggregate_Container_Routines (E : Node_Id);
      --  Used for code factorization. This procedure assumes that the
      --  current package spec has been properly set

      ---------------------
      -- Initialize_Spec --
      ---------------------

      function Initialize_Spec (E : Node_Id) return Node_Id is
         N        : Node_Id;
         Spg_Name : Name_Id;
      begin
         case FEN.Kind (E) is
            when K_Fixed_Point_Type =>
               Spg_Name := BEN.Name
                 (Defining_Identifier (Type_Def_Node (BE_Node (E))));

            when K_Sequence_Type =>
               Spg_Name := BEN.Name
                 (Defining_Identifier (Instantiation_Node (BE_Node (E))));

            when K_Complex_Declarator =>
               Spg_Name := Add_Suffix_To_Name
                 ("_Array", To_Ada_Name (FEN.IDL_Name (Identifier (E))));

            when others =>
               Spg_Name := To_Ada_Name (FEN.IDL_Name (Identifier (E)));
         end case;

         Spg_Name := Add_Prefix_To_Name ("Initialize_", Spg_Name);

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (Spg_Name), No_List);
         return N;
      end Initialize_Spec;

      ---------------
      -- Wrap_Spec --
      ---------------

      function Wrap_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := Make_Attribute_Designator
           (RE (RE_Content), A_Class);
         N       : Node_Id;
         P_Type  : Node_Id;
      begin
         --  Getting the mapped Ada type corresponding to the IDL type
         --  E

         case FEN.Kind (E) is
            when K_String_Type =>
               P_Type := Make_Designator (TN (T_Bounded_String));
               Set_Homogeneous_Parent_Unit_Name
                 (P_Type,
                  Expand_Designator (Instantiation_Node (BE_Node (E))));

            when K_Wide_String_Type =>
               P_Type := Make_Designator (TN (T_Bounded_Wide_String));
               Set_Homogeneous_Parent_Unit_Name
                 (P_Type,
                  Expand_Designator (Instantiation_Node (BE_Node (E))));

            when K_Enumeration_Type
              | K_Complex_Declarator
              | K_Simple_Declarator
              | K_Union_Type
              | K_Structure_Type =>
               P_Type :=
                 Expand_Designator (Type_Def_Node (BE_Node (Identifier (E))));

            when K_Fixed_Point_Type =>
               P_Type := Expand_Designator (Type_Def_Node (BE_Node (E)));

            when K_Sequence_Type =>
               P_Type := Make_Designator (TN (T_Sequence));
               Set_Homogeneous_Parent_Unit_Name
                 (P_Type,
                  Expand_Designator (Instantiation_Node (BE_Node (E))));

            when others =>
               raise Program_Error;
         end case;

         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_X)),
            Subtype_Mark        => Make_Access_Type_Definition (P_Type),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  Create the subprogram spec

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier (SN (S_Wrap)),
            Parameter_Profile   => Profile,
            Return_Type         => Returns);

         return N;
      end Wrap_Spec;

      -------------------------
      -- Pointer_Declaration --
      -------------------------

      function Pointer_Declaration (E : Node_Id) return Node_Id is
         Ptr_Type_Name : constant Name_Id := Map_Pointer_Type_Name (E);
         N             : Node_Id;
      begin
         N := Make_Full_Type_Declaration
           (Defining_Identifier => Make_Defining_Identifier (Ptr_Type_Name),
            Type_Definition     => Make_Access_Type_Definition
              (Subtype_Indication => Expand_Designator
                 (Type_Def_Node (BE_Node (Identifier (E)))),
               Is_All             => True));
         return N;
      end Pointer_Declaration;

      -------------------------
      -- Content_Declaration --
      -------------------------

      function Content_Declaration (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Components : constant List_Id := New_List (K_Component_List);
      begin
         --  If E is a complex declarator, and then if the dimension
         --  of the array is gretaer than 1, then we declare an array
         --  type having the 'Dim - 1' size

         if FEN.Kind (E) = K_Complex_Declarator
           and then FEU.Is_Multidimensional_Array (E)
         then
            declare
               Dim : constant Natural := FEU.Length (Array_Sizes (E));
            begin
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (Map_Indices_Name (E)),
                  Type_Definition     => Make_Array_Type_Definition
                    (Range_Constraints    => Make_List_Id
                       (Make_Range_Constraint
                        (Make_Literal (Int1_Val),
                         Make_Literal (New_Integer_Value
                                       (Unsigned_Long_Long
                                        (Dim - 1), 1, 10)))),
                     Component_Definition => RE (RE_Integer)));
               Append_Node_To_List (N, Visible_Part (Current_Package));
            end;
         end if;

         --  The container record declaration

         --  Building the component list of the container depending on
         --  the kind f 'E'

         --  All the containers contain a component 'V' which is a
         --  pointer to the Ada type mapped from the IDL type 'E'

         N := Make_Component_Declaration
           (Defining_Identifier => Make_Defining_Identifier (CN (C_V)),
            Subtype_Indication  => Make_Designator
              (Map_Pointer_Type_Name (E)));
         Append_Node_To_List (N, Components);

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               --  For enumeration type, we add an alised field
               --  corresponding to an unsigned long variable

               N := Make_Component_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (CN (C_Repr_Cache)),
                  Subtype_Indication  => RE (RE_Unsigned_Long_1),
                  Aliased_Present     => True);
               Append_Node_To_List (N, Components);

            when K_Complex_Declarator =>
               if FEU.Is_Multidimensional_Array (E) then
                  --  The Dimen field

                  N := Make_Component_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (CN (C_Dimen)),
                     Subtype_Indication  => RE (RE_Positive));
                  Append_Node_To_List (N, Components);

                  --  The Indices field

                  N := Make_Component_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (CN (C_Indices)),
                     Subtype_Indication  => Make_Designator
                       (Map_Indices_Name (E)));
                  Append_Node_To_List (N, Components);
               end if;

            when K_Union_Type =>
               --  For unions, we add an aliased field that
               --  corresponds to the union switch

               N := Make_Component_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (CN (C_Switch_Cache)),
                  Subtype_Indication  => Map_Designator (Switch_Type_Spec (E)),
                  Aliased_Present     => True);
               Append_Node_To_List (N, Components);

            when others =>
               null;
         end case;

         N := Make_Full_Type_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (Map_Container_Name (E)),
            Type_Definition     => Make_Derived_Type_Definition
              (Subtype_Indication    => RE (RE_Aggregate_Content),
               Record_Extension_Part => Make_Record_Type_Definition
                 (Make_Record_Definition (Components))));

         return N;
      end Content_Declaration;

      ----------------
      -- Clone_Spec --
      ----------------

      function Clone_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := RE (RE_Content_Ptr);
         N       : Node_Id;
      begin
         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_ACC)),
            Subtype_Mark        => Make_Designator (Map_Container_Name (E)),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Into)),
            Subtype_Mark        => RE (RE_Content_Ptr),
            Parameter_Mode      => Mode_In,
            Expression          => Make_Null_Statement);
         Append_Node_To_List (N, Profile);

         --  Create the subprogram spec

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier (SN (S_Clone)),
            Parameter_Profile   => Profile,
            Return_Type         => Returns);

         return N;
      end Clone_Spec;

      -------------------------
      -- Finalize_Value_Spec --
      -------------------------

      function Finalize_Value_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := No_Node;
         N       : Node_Id;
      begin
         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_ACC)),
            Subtype_Mark        => Make_Designator (Map_Container_Name (E)),
            Parameter_Mode      => Mode_Inout);
         Append_Node_To_List (N, Profile);

         --  Create the subprogram spec

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_Finalize_Value)),
            Parameter_Profile   => Profile,
            Return_Type         => Returns);

         return N;
      end Finalize_Value_Spec;

      ------------------------------
      -- Get_Aggregate_Count_Spec --
      ------------------------------

      function Get_Aggregate_Count_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := RE (RE_Unsigned_Long_1);
         N       : Node_Id;
      begin
         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_ACC)),
            Subtype_Mark        => Make_Designator (Map_Container_Name (E)),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  Create the subprogram spec

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_Get_Aggregate_Count)),
            Parameter_Profile   => Profile,
            Return_Type         => Returns);

         return N;
      end Get_Aggregate_Count_Spec;

      ------------------------------
      -- Set_Aggregate_Count_Spec --
      ------------------------------

      function Set_Aggregate_Count_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := No_Node;
         N       : Node_Id;
      begin
         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_ACC)),
            Subtype_Mark        => Make_Designator (Map_Container_Name (E)),
            Parameter_Mode      => Mode_Inout);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Count)),
            Subtype_Mark        => RE (RE_Unsigned_Long_1),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  Create the subprogram spec

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_Set_Aggregate_Count)),
            Parameter_Profile   => Profile,
            Return_Type         => Returns);

         return N;
      end Set_Aggregate_Count_Spec;

      --------------------------------
      -- Get_Aggregate_Element_Spec --
      --------------------------------

      function Get_Aggregate_Element_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := Make_Attribute_Designator
           (RE (RE_Content), A_Class);
         N       : Node_Id;
      begin
         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_ACC)),
            Subtype_Mark        => Make_Access_Type_Definition
              (Make_Designator (Map_Container_Name (E))),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_TC)),
            Subtype_Mark        => RE (RE_Object_7),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Index)),
            Subtype_Mark        => RE (RE_Unsigned_Long_1),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Mech)),
            Subtype_Mark        => Make_Access_Type_Definition
              (RE (RE_Mechanism)),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  Create the subprogram spec

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_Get_Aggregate_Element)),
            Parameter_Profile   => Profile,
            Return_Type         => Returns);

         return N;
      end Get_Aggregate_Element_Spec;

      --------------------------------
      -- Set_Aggregate_Element_Spec --
      --------------------------------

      function Set_Aggregate_Element_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := No_Node;
         N       : Node_Id;
      begin
         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_ACC)),
            Subtype_Mark        => Make_Designator (Map_Container_Name (E)),
            Parameter_Mode      => Mode_Inout);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_TC)),
            Subtype_Mark        => RE (RE_Object_7),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Index)),
            Subtype_Mark        => RE (RE_Unsigned_Long_1),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_From_C)),
            Subtype_Mark        => Make_Attribute_Designator
              (RE (RE_Any_Container), A_Class),
            Parameter_Mode      => Mode_Inout);
         Append_Node_To_List (N, Profile);

         --  Create the subprogram spec

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_Set_Aggregate_Element)),
            Parameter_Profile   => Profile,
            Return_Type         => Returns);

         return N;
      end Set_Aggregate_Element_Spec;

      ----------------------------------
      -- Lengths_Constant_Declaration --
      ----------------------------------

      function Lengths_Constant_Declaration (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Complex_Declarator);

         Elements : constant List_Id := New_List (K_Element_List);
         Dims     : Unsigned_Long_Long := 1;
         N        : Node_Id;
         S        : Node_Id;
         V        : Value_Type;
      begin
         --  For each dimension, we build an element association :
         --  Dimension_Index => Dimension_Size

         S := First_Entity (Array_Sizes (E));
         loop
            --  The range constraints may be :
            --  * Literal values
            --  * Previously declared constants (concretely, scoped
            --  names)

            if FEN.Kind (S) = K_Scoped_Name then
               V := Value (FEN.Value (Reference (S)));
            else
               V := Value (FEN.Value (S));
            end if;

            N := Make_Element_Association
              (Index      => Make_Literal (New_Integer_Value (Dims, 1, 10)),
               Expression => Make_Literal (New_Value (V)));
            Append_Node_To_List (N, Elements);

            S := Next_Entity (S);

            exit when No (S);

            Dims := Dims + 1;
         end loop;

         --  Define the array type

         N := Make_Array_Type_Definition
           (Range_Constraints    => Make_List_Id
              (Make_Range_Constraint
               (Make_Literal (Int1_Val),
                Make_Literal (New_Integer_Value (Dims, 1, 10)))),
            Component_Definition => RE (RE_Unsigned_Long_1));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (Map_Lengths_Name (E)),
            Constant_Present    => True,
            Object_Definition    => N,
            Expression          => Make_Array_Aggregate (Elements));
         return N;
      end Lengths_Constant_Declaration;

      ----------------------------------
      -- Aggregate_Container_Routines --
      ----------------------------------

      procedure Aggregate_Container_Routines (E : Node_Id) is
         N : Node_Id;
      begin
         --  The Pointer declaration

         N := Pointer_Declaration (E);
         Bind_FE_To_BE (Identifier (E), N, B_Pointer_Type);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         --  The container

         N := Content_Declaration (E);
         Bind_FE_To_BE (Identifier (E), N, B_Aggr_Container);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         --  Override the abstract subprograms

         N := Get_Aggregate_Element_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Get_Aggregate_Element);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         --  For complex declarator and structure types, we don't
         --  override the Set_Aggregate_Element procedure

         if FEN.Kind (E) /= K_Complex_Declarator and then
           FEN.Kind (E) /= K_Structure_Type
         then
            N := Set_Aggregate_Element_Spec (E);
            Bind_FE_To_BE (Identifier (E), N, B_Set_Aggregate_Element);
            Append_Node_To_List (N, Visible_Part (Current_Package));
         end if;

         N := Get_Aggregate_Count_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Get_Aggregate_Count);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         N := Set_Aggregate_Count_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Set_Aggregate_Count);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         N := Clone_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Clone);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         N := Finalize_Value_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Finalize_Value);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         --  For complex declarators, we declare an additiona constant

         if FEN.Kind (E) = K_Complex_Declarator then
            N := Lengths_Constant_Declaration (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
         end if;
      end Aggregate_Container_Routines;

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
         Set_Internals_Spec;

         if Generate_Shadow_Routines then

            --  The aggregate container routines

            Aggregate_Container_Routines (E);

            --  The wrap function spec

            N := Wrap_Spec (E);
            Bind_FE_To_BE (Identifier (E), N, B_Wrap);
            Append_Node_To_List (N, Visible_Part (Current_Package));

         end if;

         --  The initialize procedure

         N := Initialize_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Initialize);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Enumeration_Type;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Internals_Spec;

         N := Initialize_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Initialize);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Forward_Interface_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Internals_Spec;

         N := Initialize_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Initialize);
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
         if not Map_Particular_CORBA_Parts (E, PK_Init_Spec) then
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
         D : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         D := First_Entity (Definitions (E));
         while Present (D) loop
            Visit (D);
            D := Next_Entity (D);
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
         Set_Internals_Spec;

         if Generate_Shadow_Routines then

            --  The aggregate container routines

            Aggregate_Container_Routines (E);

            --  The wrap function spec

            N := Wrap_Spec (E);
            Bind_FE_To_BE (Identifier (E), N, B_Wrap);
            Append_Node_To_List (N, Visible_Part (Current_Package));

         end if;

         --  See the comment message in
         --  Helpers.Package_Spec.Visit_Structure_Type for more
         --  details on the instructions below

         Member := First_Entity (Members (E));
         while Present (Member) loop
            Declarator := First_Entity (Declarators (Member));
            while Present (Declarator) loop
               if FEN.Kind (Declarator) = K_Complex_Declarator then

                  if Generate_Shadow_Routines then

                     --  The aggregate container routines

                     Aggregate_Container_Routines (Declarator);

                     --  The wrap function spec

                     N := Wrap_Spec (Declarator);
                     Bind_FE_To_BE (Identifier (Declarator), N, B_Wrap);
                     Append_Node_To_List (N, Visible_Part (Current_Package));

                  end if;

                  --  The initialize procedure

                  N := Initialize_Spec (Declarator);
                  Bind_FE_To_BE (Identifier (Declarator), N, B_Initialize);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end if;

               Declarator := Next_Entity (Declarator);
            end loop;
            Member := Next_Entity (Member);
         end loop;

         --  The initialize procedure

         N := Initialize_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Initialize);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         N : Node_Id;
         D : Node_Id;
         T : constant Node_Id := Type_Spec (E);
      begin
         Set_Internals_Spec;

         case FEN.Kind (T) is
            when K_Fixed_Point_Type =>
               declare
                  Package_Node : Node_Id;
                  Fixed_Type_Node : Node_Id;
               begin
                  --  We instantiate the generic helper package here
                  --  because we need it in the Wrap function body

                  Package_Node := Make_Defining_Identifier
                    (Map_Fixed_Type_Helper_Name (T));

                  Fixed_Type_Node := Expand_Designator
                    (Type_Def_Node (BE_Node (T)));

                  N := Make_Package_Instantiation
                    (Defining_Identifier => Package_Node,
                     Generic_Package     => RU (RU_CORBA_Fixed_Point),
                     Parameter_List      => Make_List_Id (Fixed_Type_Node));
                  Append_Node_To_List (N, Visible_Part (Current_Package));

                  --  The Initialize Spec

                  N := Initialize_Spec (T);
                  Bind_FE_To_BE (T, N, B_Initialize);
                  Append_Node_To_List (N, Visible_Part (Current_Package));

                  if Generate_Shadow_Routines then
                     --  The wrap function spec

                     N := Wrap_Spec (T);
                     Bind_FE_To_BE (T, N, B_Wrap);
                     Append_Node_To_List (N, Visible_Part (Current_Package));
                  end if;
               end;

            when K_Sequence_Type =>
               declare
                  S            : Node_Id;
                  Package_Node : Node_Id;
                  Elt_From_Any : Node_Id;
                  Elt_To_Any   : Node_Id;
--                    Elt_Wrap     : Node_Id;
                  Profile      : constant List_Id := New_List (K_List_Id);
               begin
                  if Generate_Shadow_Routines then
                     --  The wrap function spec

                     N := Wrap_Spec (T);
                     Bind_FE_To_BE (T, N, B_Wrap);
                     Append_Node_To_List (N, Visible_Part (Current_Package));
                  end if;

                  --  We instantiate the generic helper package here
                  --  because we need it in the initialization routine

                  S := Expand_Designator (Instantiation_Node (BE_Node (T)));
                  Package_Node := Make_Defining_Identifier
                    (Map_Sequence_Pkg_Helper_Name (T));

                  --  getting the the From_any, the To_Any and the
                  --  Wrap functions nodes corresponding to the
                  --  elements of the sequence.

                  Elt_From_Any := Get_From_Any_Node (Type_Spec (T));
                  Elt_To_Any := Get_To_Any_Node (Type_Spec (T));
--                    Elt_Wrap := Get_Wrap_Node (Type_Spec (T));

                  Append_Node_To_List
                    (Make_Parameter_Association
                     (Make_Defining_Identifier (PN (P_Element_From_Any)),
                      Elt_From_Any),
                     Profile);
                  Append_Node_To_List
                    (Make_Parameter_Association
                     (Make_Defining_Identifier (PN (P_Element_To_Any)),
                      Elt_To_Any),
                     Profile);
                  --  FIXME: Add the wrap element
--                    Append_Node_To_List
--                      (Make_Parameter_Association
--                       (Make_Defining_Identifier (PN (P_Element_Wrap)),
--                        Elt_Wrap),
--                       Profile);

                  if Present (Max_Size (T)) then
                     N := RE (RE_CORBA_Helper_1);
                  else
                     N := RE (RE_CORBA_Helper_2);
                  end if;

                  --  Change the parent of the generic package

                  Set_Homogeneous_Parent_Unit_Name (N, S);

                  N := Make_Package_Instantiation
                    (Defining_Identifier => Package_Node,
                     Generic_Package     => N,
                     Parameter_List      => Profile);
                  Append_Node_To_List (N, Visible_Part (Current_Package));

                  N := Initialize_Spec (T);
                  Bind_FE_To_BE (T, N, B_Initialize);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end;

            when K_String_Type | K_Wide_String_Type =>
               if Generate_Shadow_Routines then
                  --  The wrap function spec

                  N := Wrap_Spec (T);
                  Bind_FE_To_BE (T, N, B_Wrap);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end if;

            when others =>
               null;
         end case;

         D := First_Entity (Declarators (E));
         while Present (D) loop
            if FEN.Kind (D) = K_Complex_Declarator then
               if Generate_Shadow_Routines then
                  --  The aggregate container routines

                  Aggregate_Container_Routines (D);
               end if;
            end if;

            --  We do not generate the Wrap body if the type is not an
            --  Object type and then if the declarator is simple

            if not ((FEN.Kind (T) = K_Scoped_Name
                     and then Is_Object_Type (T)
                     and then FEN.Kind (D) = K_Simple_Declarator)
                    or else FEN.Kind (T) = K_String_Type
                    or else FEN.Kind (T) = K_Wide_String_Type
                    or else FEN.Kind (T) = K_Fixed_Point_Type
                    or else FEN.Kind (T) = K_Sequence_Type)
            then
               if Generate_Shadow_Routines then
                  --  The wrap function spec

                  N := Wrap_Spec (D);
                  Bind_FE_To_BE (Identifier (D), N, B_Wrap);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end if;
            end if;

            --  The initialize procedure

            N := Initialize_Spec (D);
            Bind_FE_To_BE (Identifier (D), N, B_Initialize);
            Append_Node_To_List (N, Visible_Part (Current_Package));

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
         Set_Internals_Spec;

         if Generate_Shadow_Routines then

            --  The aggregate container routines

            Aggregate_Container_Routines (E);

            --  The wrap function spec

            N := Wrap_Spec (E);
            Bind_FE_To_BE (Identifier (E), N, B_Wrap);
            Append_Node_To_List (N, Visible_Part (Current_Package));

         end if;

         --  See the comment message in
         --  Helpers.Package_Spec.Visit_Union_Type for more
         --  details on the instructions below

         Alternatives := Switch_Type_Body (E);
         Alternative := First_Entity (Alternatives);
         while Present (Alternative) loop
            Declarator := FEN.Declarator (FEN.Element (Alternative));
            if FEN.Kind (Declarator) = K_Complex_Declarator then
               if Generate_Shadow_Routines then
                  --  The aggregate container routines

                  Aggregate_Container_Routines (Declarator);

                  --  The wrap function spec

                  N := Wrap_Spec (Declarator);
                  Bind_FE_To_BE (Identifier (Declarator), N, B_Wrap);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end if;

               --  The initialize procedure

               N := Initialize_Spec (Declarator);
               Bind_FE_To_BE (Identifier (Declarator), N, B_Initialize);
               Append_Node_To_List (N, Visible_Part (Current_Package));
            end if;

            Alternative := Next_Entity (Alternative);
         end loop;

         --  The initialize procedure

         N := Initialize_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Initialize);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Union_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Internals_Spec;

         N := Initialize_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Initialize);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Exception_Declaration;

   end Package_Spec;

   ------------------
   -- Package_Body --
   ------------------

   package body Package_Body is

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Forward_Interface_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);

      function Raise_Excp_From_Any_Spec
        (Raise_Node : Node_Id)
        return Node_Id;
      --  The spec is located in the body because this function is not
      --  used outside the helper package. However the spec is
      --  necessary because of the pragma No_Return.

      function Raise_Excp_From_Any_Body
        (E          : Node_Id;
         Raise_Node : Node_Id)
        return Node_Id;

      function Initialized_Identifier (E : Node_Id) return Node_Id;
      --  Return a defining identifier designing the boolean flag that
      --  controls the IDL type E

      function Initialized_Flag_Declaration (E : Node_Id) return Node_Id;
      --  Declares a Boolean flag useful for the initialization of a
      --  TypeCode corresponding to the IDL type E

      function Initialize_Body (E : Node_Id) return Node_Id;
      --  Returns the body of the Initialize procedure that builds the
      --  TypeCode corresponding to the IDL type E

      procedure Initialize_Routine
        (E                : Node_Id;
         Declaration_List : List_Id;
         Statements       : List_Id);
      --  Fills the lists Declaration_List and Statements with the
      --  routines initializing the IDL type E

      procedure Handle_Dependency (N : Node_Id; Statements : List_Id);
      --  This procedure handles the dependency on the TypeCode
      --  corresponding to the node N. If the node N is a CORBA type,
      --  it adds the necessary dependency to the Helper
      --  initialization. If the node N belongs to the current IDL
      --  specification, it calls the Initialize_XXX function that
      --  build its TypeCode

      function Wrap_Body (E : Node_Id) return Node_Id;
      function Clone_Body (E : Node_Id) return Node_Id;
      function Finalize_Value_Body (E : Node_Id) return Node_Id;
      function Get_Aggregate_Count_Body (E : Node_Id) return Node_Id;
      function Set_Aggregate_Count_Body (E : Node_Id) return Node_Id;
      function Get_Aggregate_Element_Body (E : Node_Id) return Node_Id;
      function Set_Aggregate_Element_Body (E : Node_Id) return Node_Id;
      --  Bodies for the routines that manipulate the aggregate
      --  container

      procedure Aggregate_Container_Routines (E : Node_Id);
      --  Used for code factorization. This procedure assumes that the
      --  current package body has been properly set.

      function Get_Aggr_Count (E : Node_Id) return Node_Id;
      --  Factorize some code between Get_Aggregate_Count_Body and
      --  Set_Aggregate_Count_Body

      ---------------
      -- Wrap_Body --
      ---------------

      function Wrap_Body (E : Node_Id) return Node_Id is
         function Copy_Subprogram_Spec (S : Node_Id) return Node_Id;
         --  In some cases, the body of the Wrap spec simply renames
         --  another subprogram. We cannot set the 'Renamed_Entity'
         --  field of the original spec. We use this function to make
         --  a copy of the spec

         --------------------------
         -- Copy_Subprogram_Spec --
         --------------------------

         function Copy_Subprogram_Spec (S : Node_Id) return Node_Id is
            pragma Assert (BEN.Kind (S) = K_Subprogram_Specification);
         begin
            return Make_Subprogram_Specification
              (Defining_Identifier (S),
               Parameter_Profile (S),
               Return_Type (S),
               Parent (S));
         end Copy_Subprogram_Spec;

         Spec       : Node_Id;
         N          : Node_Id;
      begin
         case FEN.Kind (E) is
            when K_String_Type | K_Wide_String_Type =>
               --  For bounded string types, we simply rename the Wrap
               --  function of the instantiated generic package

               Spec := Copy_Subprogram_Spec (Wrap_Node (BE_Node (E)));
               N := Make_Designator (SN (S_Wrap));
               Set_Homogeneous_Parent_Unit_Name
                 (N, Expand_Designator (Instantiation_Node (BE_Node (E))));
               Set_Renamed_Entity (Spec, N);
               N := Spec;

            when K_Enumeration_Type
              | K_Complex_Declarator
              | K_Union_Type
              | K_Structure_Type =>
               declare
                  Statements : constant List_Id := New_List (K_Statement_List);
                  Aggr_List  : constant List_Id := New_List (K_Element_List);
               begin
                  Spec := Wrap_Node (BE_Node (Identifier (E)));

                  --  The first component

                  N := Make_Type_Conversion
                    (Subtype_Mark => Make_Designator
                       (Map_Pointer_Type_Name (E)),
                     Expression   => Make_Designator (PN (P_X)));
                  N := Make_Component_Association
                    (Make_Defining_Identifier (CN (C_V)), N);
                  Append_Node_To_List (N, Aggr_List);

                  --  Inner case statement to add the record aggregate
                  --  depending on the IDL node kind

                  case FEN.Kind (E) is
                     when K_Enumeration_Type =>
                        --  The Repr_Cache field

                        N := Make_Component_Association
                          (Make_Defining_Identifier (CN (C_Repr_Cache)),
                           Make_Literal (Int0_Val));
                        Append_Node_To_List (N, Aggr_List);

                     when K_Complex_Declarator =>
                        if FEU.Is_Multidimensional_Array (E) then
                           --  The Dimen switch ( =1 if the array is
                           --  multidimensional)

                           N := Make_Component_Association
                             (Make_Defining_Identifier (CN (C_Dimen)),
                              Make_Literal (Int1_Val));
                           Append_Node_To_List (N, Aggr_List);

                           --  The Indices switch

                           N := Make_Element_Association
                             (No_Node, Make_Literal (Int0_Val));
                           N := Make_Array_Aggregate (Make_List_Id (N));
                           N := Make_Component_Association
                             (Make_Defining_Identifier (CN (C_Indices)), N);
                           Append_Node_To_List (N, Aggr_List);
                        end if;

                     when K_Union_Type =>
                        --  The Switch_Cache field

                        N := Make_Component_Association
                          (Make_Defining_Identifier (CN (C_Switch_Cache)),
                           Make_Designator (CN (C_Switch), PN (P_X)));
                        Append_Node_To_List (N, Aggr_List);

                     when K_Structure_Type =>
                        null;
                     when others =>
                        raise Program_Error;
                  end case;

                  N := Make_Record_Aggregate
                    (Aggr_List, RE (RE_Aggregate_Content));
                  N := Make_Qualified_Expression
                    (Make_Designator (Map_Container_Name (E)), N);
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, Statements);

                  N := Make_Subprogram_Implementation
                    (Spec, No_List, Statements);
               end;

            when K_Simple_Declarator =>
               declare
                  O          : constant Node_Id := Type_Spec (Declaration (E));
                  Statements : constant List_Id := New_List (K_Statement_List);
               begin
                  Spec := Wrap_Node (BE_Node (Identifier (E)));

                  --  We simply call the wrap function of the
                  --  redefined type

                  N := Make_Type_Conversion
                    (Map_Designator (O),
                     Make_Designator (PN (P_X), Is_All => True));
                  N := Make_Attribute_Designator (N, A_Unrestricted_Access);

                  N := Make_Subprogram_Call
                    (Get_Wrap_Node (O), Make_List_Id (N));
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, Statements);

                  N := Make_Subprogram_Implementation
                    (Spec, No_List, Statements);
               end;

            when K_Fixed_Point_Type =>
               --  For fixed point types, we simply rename the Wrap
               --  function of the instantiated helper generic package

               Spec := Copy_Subprogram_Spec (Wrap_Node (BE_Node (E)));
               N := Make_Designator (SN (S_Wrap));
               Set_Homogeneous_Parent_Unit_Name
                 (N, Make_Designator (Map_Fixed_Type_Helper_Name (E)));
               Set_Renamed_Entity (Spec, N);
               N := Spec;

            when K_Sequence_Type =>
               --  For sequence types, we simply rename the Wrap
               --  function of the instantiated helper generic package

               Spec := Copy_Subprogram_Spec (Wrap_Node (BE_Node (E)));
               N := Make_Designator (SN (S_Wrap));
               Set_Homogeneous_Parent_Unit_Name
                 (N, Make_Designator (Map_Sequence_Pkg_Helper_Name (E)));
               Set_Renamed_Entity (Spec, N);
               N := Spec;

            when others =>
               raise Program_Error;
         end case;

         return N;
      end Wrap_Body;

      ----------------
      -- Clone_Body --
      ----------------

      function Clone_Body (E : Node_Id) return Node_Id is
         Spec       : constant Node_Id := Clone_Node
           (BE_Node (Identifier (E)));
         Dcl_Part   : constant List_Id := New_List (K_Declaration_List);
         Statements : constant List_Id := New_List (K_Statement_List);
         N          : Node_Id;
         Converted  : Node_Id;
      begin
         --  Common declarative part

         N := Make_Used_Type (RE (RE_Content_Ptr));
         Append_Node_To_List (N, Dcl_Part);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Target)),
            Object_Definition   => RE (RE_Content_Ptr));
         Append_Node_To_List (N, Dcl_Part);

         --  Common statements

         --  IF statement

         declare
            Then_Statements : constant List_Id := New_List (K_Statement_List);
            Else_Statements : constant List_Id := New_List (K_Statement_List);
            Condition       : Node_Id;
         begin
            --  Inner IF statement

            Condition  := Make_Expression
              (Make_Designator (PN (P_Into), Is_All => True),
               Op_Not_In,
               Make_Designator (Map_Container_Name (E)));
            N := Make_If_Statement
              (Condition       => Condition,
               Then_Statements =>
                 Make_List_Id (Make_Return_Statement (Make_Null_Statement)));
            Append_Node_To_List (N, Then_Statements);

            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Target)),
               Make_Defining_Identifier (PN (P_Into)));
            Append_Node_To_List (N, Then_Statements);

            --  Else statement

            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Target)),
               Make_Object_Instantiation (Make_Designator
                                          (Map_Container_Name (E))));
            Append_Node_To_List (N, Else_Statements);

            N := Make_Designator (PN (P_Target), Is_All => True);
            Converted := Make_Type_Conversion
              (Make_Designator (Map_Container_Name (E)), N);
            N := Make_Selected_Component
              (Converted, Make_Designator (CN (C_V)));
            N := Make_Assignment_Statement
              (N, Make_Object_Instantiation
               (Expand_Designator (Type_Def_Node (BE_Node (Identifier (E))))));
            Append_Node_To_List (N, Else_Statements);

            Condition := Make_Expression
              (Make_Designator (PN (P_Into), Is_All => True),
               Op_Not_Equal,
               Make_Null_Statement);
            N := Make_If_Statement
              (Condition       => Condition,
               Then_Statements => Then_Statements,
               Else_Statements => Else_Statements);
            Append_Node_To_List (N, Statements);
         end;

         N := Make_Selected_Component
           (Converted, Make_Designator (CN (C_V), Is_All => True));
         N := Make_Assignment_Statement
           (N, Make_Designator (CN (C_V), PN (P_ACC), True));
         Append_Node_To_List (N, Statements);

         --  Specific part

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               --  Assign the remaining record fields

               N := Make_Selected_Component
                 (Converted, Make_Designator (CN (C_Repr_Cache)));
               N := Make_Assignment_Statement
                 (N, Make_Designator (CN (C_Repr_Cache), PN (P_ACC)));
               Append_Node_To_List (N, Statements);

            when K_Union_Type =>
               --  Add a pragma statement

               N := Make_Pragma_Statement
                 (Pragma_Supress,
                  Make_List_Id (RE (RE_Discriminant_Check)));
               Append_Node_To_List (N, Dcl_Part);

               --  Assign the remaining record fields

               N := Make_Selected_Component
                 (Converted, Make_Designator (CN (C_Switch_Cache)));
               N := Make_Assignment_Statement
                 (N, Make_Designator (CN (C_Switch_Cache), PN (P_ACC)));
               Append_Node_To_List (N, Statements);

            when K_Complex_Declarator =>
               if FEU.Is_Multidimensional_Array (E) then
                  --  Assign the remaining record fields

                  N := Make_Selected_Component
                    (Converted, Make_Designator (CN (C_Dimen)));
                  N := Make_Assignment_Statement
                    (N, Make_Designator (CN (C_Dimen), PN (P_ACC)));
                  Append_Node_To_List (N, Statements);

                  N := Make_Selected_Component
                    (Converted, Make_Designator (CN (C_Indices)));
                  N := Make_Assignment_Statement
                    (N, Make_Designator (CN (C_Indices), PN (P_ACC)));
                  Append_Node_To_List (N, Statements);
               end if;

            when others =>
               null;
         end case;

         --  The return statement

         N := Make_Return_Statement (Make_Designator (PN (P_Target)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Clone_Body;

      -------------------------
      -- Finalize_Value_Body --
      -------------------------

      function Finalize_Value_Body (E : Node_Id) return Node_Id is
         Spec       : constant Node_Id := Finalize_Value_Node
           (BE_Node (Identifier (E)));
         Dcl_Part   : constant List_Id := New_List (K_Declaration_List);
         Statements : constant List_Id := New_List (K_Statement_List);
         N          : Node_Id;
      begin
         --  The deallocation procedure declaration

         N := Make_Instantiated_Subprogram
           (RE (RE_Unchecked_Deallocation),
            Make_List_Id
            (Expand_Designator (Type_Def_Node (BE_Node (Identifier (E)))),
             Make_Designator (Map_Pointer_Type_Name (E))));
         N := Make_Subprogram_Specification
           (Defining_Identifier     => Make_Defining_Identifier (SN (S_Free)),
            Parameter_Profile       => No_List,
            Instantiated_Subprogram => N);
         Append_Node_To_List (N, Dcl_Part);

         --  The deallocation procedure call

         N := Make_Subprogram_Call
           (Make_Designator (SN (S_Free)),
            Make_List_Id (Make_Designator (CN (C_V), PN (P_ACC))));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Finalize_Value_Body;

      ------------------------------
      -- Get_Aggregate_Count_Body --
      ------------------------------

      function Get_Aggregate_Count_Body (E : Node_Id) return Node_Id is
         Spec       : constant Node_Id := Get_Aggregate_Count_Node
           (BE_Node (Identifier (E)));
         Dcl_Part   : constant List_Id := New_List (K_Declaration_List);
         Statements : constant List_Id := New_List (K_Statement_List);
         N          : Node_Id;
         Returns    : constant Node_Id := Get_Aggr_Count (E);
      begin
         --  The ACC formal parampeter is used only in case of a
         --  multidimensional array

         if FEN.Kind (E) /= K_Complex_Declarator
           or else not FEU.Is_Multidimensional_Array (E)
         then
            N := Make_Pragma_Statement
              (Pragma_Unreferenced,
               Make_List_Id (Make_Designator (PN (P_ACC))));
            Append_Node_To_List (N, Dcl_Part);
         end if;

         --  The return statement

         N := Make_Return_Statement (Returns);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Get_Aggregate_Count_Body;

      ------------------------------
      -- Set_Aggregate_Count_Body --
      ------------------------------

      function Set_Aggregate_Count_Body (E : Node_Id) return Node_Id is
         Spec       : constant Node_Id := Set_Aggregate_Count_Node
           (BE_Node (Identifier (E)));
         Dcl_Part   : constant List_Id := New_List (K_Declaration_List);
         Statements : constant List_Id := New_List (K_Statement_List);
         Aggr_Count : constant Node_Id := Get_Aggr_Count (E);
         N          : Node_Id;
         C          : Node_Id;
      begin
         N := Make_Used_Type (RE (RE_Unsigned_Long_1));
         Append_Node_To_List (N, Dcl_Part);

         --  The ACC formal parampeter is used only in case of a
         --  multidimensional array

         if FEN.Kind (E) /= K_Complex_Declarator
           or else not FEU.Is_Multidimensional_Array (E)
         then
            N := Make_Pragma_Statement
              (Pragma_Unreferenced,
               Make_List_Id (Make_Designator (PN (P_ACC))));
            Append_Node_To_List (N, Dcl_Part);
         end if;

         --  The if statement

         C := Make_Expression
           (Make_Designator (PN (P_Count)), Op_Not_Equal, Aggr_Count);
         N := Make_Raise_Statement (Make_Designator (EN (E_Program_Error)));
         N := Make_If_Statement (C, Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Set_Aggregate_Count_Body;

      --------------------------------
      -- Get_Aggregate_Element_Body --
      --------------------------------

      function Get_Aggregate_Element_Body (E : Node_Id) return Node_Id is
         Spec         : constant Node_Id := Get_Aggregate_Element_Node
           (BE_Node (Identifier (E)));
         Dcl_Part     : constant List_Id := New_List (K_Declaration_List);
         Statements   : constant List_Id := New_List (K_Statement_List);
         Unref_Params : constant List_Id := New_List (K_List_Id);
         N            : Node_Id;
      begin
         N := Make_Used_Type (RE (RE_Unsigned_Long_1));
         Append_Node_To_List (N, Dcl_Part);
         N := Make_Used_Type (RE (RE_Mechanism));
         Append_Node_To_List (N, Dcl_Part);

         --  IDL node kind dependant part

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               --  Unreferenced parameters

               Append_Node_To_List (Make_Designator (PN (P_TC)), Unref_Params);
               Append_Node_To_List (Make_Designator (PN (P_Index)),
                                    Unref_Params);

               N := Make_Assignment_Statement
                 (Make_Designator (CN (C_Repr_Cache), PN (P_ACC)),
                  Make_Subprogram_Call
                  (Make_Attribute_Designator
                   (Expand_Designator
                    (Type_Def_Node (BE_Node (Identifier (E)))), A_Pos),
                   Make_List_Id
                   (Make_Designator (CN (C_V), PN (P_ACC), True))));
               Append_Node_To_List (N, Statements);

               N := Make_Assignment_Statement
                 (Make_Designator (PN (P_Mech), Is_All => True),
                  RE (RE_By_Value));
               Append_Node_To_List (N, Statements);

               N := Make_Return_Statement
                 (Make_Subprogram_Call
                  (RE (RE_Wrap_1),
                   Make_List_Id
                   (Make_Attribute_Designator
                    (Make_Designator
                     (CN (C_Repr_Cache),
                      PN (P_ACC)),
                     A_Unrestricted_Access))));
               Append_Node_To_List (N, Statements);

            when others =>
               --  FIXME: To be completed

               Append_Node_To_List (Make_Designator (PN (P_ACC)),
                                    Unref_Params);
               Append_Node_To_List (Make_Designator (PN (P_TC)), Unref_Params);
               Append_Node_To_List (Make_Designator (PN (P_Index)),
                                    Unref_Params);
               Append_Node_To_List (Make_Designator (PN (P_Mech)),
                                    Unref_Params);

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (PN (P_Dummy)),
                  Object_Definition   => RE (RE_Content));
               Append_Node_To_List (N, Dcl_Part);

               N := Make_Pragma_Statement
                 (Pragma_Warnings,
                  Make_List_Id (RE (RE_Off),
                                Make_Designator (PN (P_Dummy))));
               Append_Node_To_List (N, Dcl_Part);

               N := Make_Return_Statement (Make_Designator (PN (P_Dummy)));
               Append_Node_To_List (N, Statements);
         end case;

         --  Adding a pragma Unreferenced statement (if necessary)

         if not Is_Empty (Unref_Params) then
            N := Make_Pragma_Statement (Pragma_Unreferenced, Unref_Params);
            Append_Node_To_List (N, Dcl_Part);
         end if;

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Get_Aggregate_Element_Body;

      --------------------------------
      -- Set_Aggregate_Element_Body --
      --------------------------------

      function Set_Aggregate_Element_Body (E : Node_Id) return Node_Id is
         Spec         : constant Node_Id := Set_Aggregate_Element_Node
           (BE_Node (Identifier (E)));
         Dcl_Part     : constant List_Id := New_List (K_Declaration_List);
         Statements   : constant List_Id := New_List (K_Statement_List);
         Unref_Params : constant List_Id := New_List (K_List_Id);
         N            : Node_Id;
      begin
         --  FIXME: To be completed

         Append_Node_To_List (Make_Designator (PN (P_ACC)), Unref_Params);
         Append_Node_To_List (Make_Designator (PN (P_TC)), Unref_Params);
         Append_Node_To_List (Make_Designator (PN (P_Index)), Unref_Params);
         Append_Node_To_List (Make_Designator (PN (P_From_C)), Unref_Params);

         --  Adding a pragma Unreferenced statement (if necessary)

         if not Is_Empty (Unref_Params) then
            N := Make_Pragma_Statement (Pragma_Unreferenced, Unref_Params);
            Append_Node_To_List (N, Dcl_Part);
         end if;

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Set_Aggregate_Element_Body;

      ----------------------------------
      -- Aggregate_Container_Routines --
      ----------------------------------

      procedure Aggregate_Container_Routines (E : Node_Id) is
         N : Node_Id;
      begin
         --  Bodies of the overridden abstract subprograms

         N := Get_Aggregate_Element_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         --  For complex declarator and structure types, we don't
         --  override the Set_Aggregate_Element procedure

         if FEN.Kind (E) /= K_Complex_Declarator and then
           FEN.Kind (E) /= K_Structure_Type
         then
            N := Set_Aggregate_Element_Body (E);
            Append_Node_To_List (N, Statements (Current_Package));
         end if;

         N := Get_Aggregate_Count_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         N := Set_Aggregate_Count_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         N := Clone_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         N := Finalize_Value_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Aggregate_Container_Routines;

      --------------------
      -- Get_Aggr_Count --
      --------------------

      function Get_Aggr_Count (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               --  Enumeration types have only one aggregate

               N := Make_Literal (Int1_Val);

            when K_Union_Type =>
               --  Union types have two aggregates, the switch and the
               --  corresponding element

               N := Make_Literal (New_Integer_Value (2, 1, 10));

            when K_Structure_Type =>
               declare
                  Member_Count : Unsigned_Long_Long := 0;
                  Member       : Node_Id;
                  D            : Node_Id;
               begin
                  --  Count the number of declarators

                  Member := First_Entity (Members (E));
                  while Present (Member) loop
                     D := First_Entity (Declarators (Member));

                     while Present (D) loop
                        Member_Count := Member_Count + 1;
                        D := Next_Entity (D);
                     end loop;

                     Member := Next_Entity (Member);
                  end loop;

                  N := Make_Literal (New_Integer_Value (Member_Count, 1, 10));
               end;

            when K_Complex_Declarator =>
               declare
                  Dim : constant Natural := FEU.Length (Array_Sizes (E));
               begin
                  if Dim = 1 then
                     N := Make_Subprogram_Call
                       (Make_Designator (Map_Lengths_Name (E)),
                        Make_List_Id (Make_Literal (Int1_Val)));
                  else
                     N := Make_Subprogram_Call
                       (Make_Designator (Map_Lengths_Name (E)),
                        Make_List_Id
                        (Make_Designator (CN (C_Dimen), PN (P_ACC))));
                  end if;
               end;

            when others =>
               raise Program_Error;

         end case;

         return N;
      end Get_Aggr_Count;

      ----------------------------
      -- Initialized_Identifier --
      ----------------------------

      function Initialized_Identifier (E : Node_Id) return Node_Id is
         Flag_Name : Name_Id;
      begin
         case FEN.Kind (E) is
            when K_Fixed_Point_Type =>
               Flag_Name := BEN.Name
                 (Defining_Identifier (Type_Def_Node (BE_Node (E))));

            when K_Sequence_Type =>
               Flag_Name := BEN.Name
                 (Defining_Identifier (Instantiation_Node (BE_Node (E))));

            when K_Complex_Declarator =>
               Flag_Name := Add_Suffix_To_Name
                 ("_Array", To_Ada_Name (FEN.IDL_Name (Identifier (E))));

            when others =>
               Flag_Name := To_Ada_Name (FEN.IDL_Name (Identifier (E)));
         end case;

         Flag_Name := Add_Suffix_To_Name ("_Initialized", Flag_Name);

         return Make_Defining_Identifier (Flag_Name);
      end Initialized_Identifier;

      ----------------------------------
      -- Initialized_Flag_Declaration --
      ----------------------------------

      function Initialized_Flag_Declaration (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N := Make_Object_Declaration
           (Defining_Identifier => Initialized_Identifier (E),
            Object_Definition   => RE (RE_Boolean_2),
            Expression          => RE (RE_False));
         return N;
      end Initialized_Flag_Declaration;

      ---------------------
      -- Initialize_Body --
      ---------------------

      function Initialize_Body (E : Node_Id) return Node_Id is
         N                : Node_Id;
         Spec             : Node_Id;
         Declarative_Part : constant List_Id := New_List (K_Declaration_List);
         Statements       : constant List_Id := New_List (K_Statement_List);
         Then_Statements  : constant List_Id := New_List (K_Statement_List);
         Condition        : Node_Id;
      begin
         if FEN.Kind (E) = K_Fixed_Point_Type or else
           FEN.Kind (E) = K_Sequence_Type
         then
            Spec := Initialize_Node (BE_Node (E));
         else
            Spec := Initialize_Node (BE_Node (Identifier (E)));
         end if;

         --  Declare the boolean flag global variable that indicates
         --  whether the TypeCode has been initialized or not. There
         --  is no harm this variable is global, because the
         --  initialization is done only once at the beginning of the
         --  application and its not supposed to be done by more than
         --  one task

         N := Initialized_Flag_Declaration (E);
         Append_Node_To_List (N, BEN.Statements (Current_Package));

         --  Build the IF statement that controls the initialization
         --  of the TypeCode

         Condition := Make_Expression (Initialized_Identifier (E), Op_Not);
         N := Make_Assignment_Statement
           (Initialized_Identifier (E), RE (RE_True));
         Append_Node_To_List (N, Then_Statements);

         --  Append the initialization routines

         Initialize_Routine (E, Declarative_Part, Then_Statements);

         N := Make_If_Statement
           (Condition       => Condition,
            Then_Statements => Then_Statements);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Spec, Declarative_Part, Statements);
         return N;
      end Initialize_Body;

      ------------------------
      -- Initialize_Routine --
      ------------------------

      procedure Initialize_Routine
        (E                : Node_Id;
         Declaration_List : List_Id;
         Statements       : List_Id)
      is
         function Add_Parameter
           (TC_Name : Name_Id; Var_Node : Node_Id)
           return Node_Id;
         --  Makes a call to the Add_Parameter Routine with the given
         --  parameters

         function Declare_Name
           (Var_Name : Name_Id; Value : Value_Id)
           return Node_Id;
         --  Makes a variable declaration using the given parameters

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
               Make_List_Id (Make_Designator (TC_Name), N));

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
              (Defining_Identifier => Make_Defining_Identifier (Var_Name),
               Object_Definition   => RE (RE_String_0),
               Expression          => Make_Subprogram_Call
                 (RE (RE_To_CORBA_String),
                  Make_List_Id (Make_Literal (Value))));

            return N;
         end Declare_Name;

         Stub             : Node_Id;
         N                : Node_Id;
         Entity_TC_Name   : Name_Id;
         Entity_Name_V    : Value_Id;
         Entity_Rep_Id_V  : Value_Id;
         Param1           : Node_Id;
         Param2           : Node_Id;
         Helper_Package   : constant Node_Id :=
           Parent (Package_Declaration (Current_Package));
         Dependencies     : constant List_Id :=
           Get_GList (Helper_Package, GL_Dependencies);
      begin
         --  Extract from polyorb-any.ads concerning the Encoding of
         --  TypeCodes:

         --  10. For sequence and array, the first parameter will
         --      be the length of the sequence or the array and the second
         --      the content type. As for strings, an unbounded sequence will
         --      have a length of 0.

         --  11. For fixed, the first parameter will be the digits
         --      number and the second the scale.

         --  So, we don't need the definitions below :

         if FEN.Kind (E) /= K_Complex_Declarator
           and then FEN.Kind (E) /= K_Sequence_Type
           and then FEN.Kind (E) /= K_Fixed_Point_Type
         then

            --  For the forward interfaces, we use the name and the
            --  Rep_Id of the forwarded interface. The Repository_Id
            --  is declared just after the type definition

            if FEN.Kind (E) = K_Forward_Interface_Declaration then
               Stub := Type_Def_Node (BE_Node (Identifier (Forward (E))));
            else
               Stub := Type_Def_Node (BE_Node (Identifier (E)));
            end if;

            Entity_Rep_Id_V := BEN.Value (BEN.Expression (Next_Node (Stub)));
         end if;

         Entity_TC_Name := BEN.Name
           (Defining_Identifier (Get_TC_Node (E, False)));

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
                  Sizes            : constant List_Id := Range_Constraints
                    (Type_Definition
                     (Type_Def_Node
                      (BE_Node
                       (Identifier
                        (E)))));
                  Sizes_Reverse    : constant List_Id := New_List (K_List_Id);
                  Constraint       : Node_Id;
                  Dimension        : constant Natural := Length (Sizes);
                  From_N           : Node_Id          := No_Node;
                  To_N             : Node_Id          := No_Node;
                  T                : Node_Id;
               begin
                  if Dimension > 1 then

                     --  First of all, we create a new list which
                     --  contains the elements of the list Sizes. All
                     --  manipulations on this list will not affect
                     --  the Sizes list because we create new nodes.

                     From_N := First_Node (Sizes);
                     while Present (From_N) loop
                        To_N := New_Node (K_Range_Constraint);
                        Set_Last (To_N, Last (From_N));
                        Append_Node_To_List
                          (To_N,
                           Sizes_Reverse);
                        From_N := Next_Node (From_N);
                     end loop;

                     --  The TC_XXXX constants used here are the ones
                     --  declared in the Helper spec

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

                           Handle_Dependency (T, Statements);
                           Helpers.Package_Body.Add_Dependency
                             (Parent_Unit_Name (Param2), Dependencies);

                        else --  Not the deepest dimension
                           Param2 := Make_Designator (TC_Previous_Name);
                        end if;

                        N := Add_Parameter (TC_Name, Param1);
                        Append_Node_To_List (N, Statements);
                        N := Add_Parameter (TC_Name, Param2);
                        Append_Node_To_List (N, Statements);

                        Remove_Node_From_List (Constraint, Sizes_Reverse);
                        Constraint := Last_Node (Sizes_Reverse);
                     end loop;

                     --  The case of the last TC_ variable which
                     --  represents the whole array is handled apart.

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

                     Handle_Dependency (T, Statements);
                     Helpers.Package_Body.Add_Dependency
                       (Parent_Unit_Name (Param2), Dependencies);
                  end if;
               end;

            when K_Fixed_Point_Type =>
               Param1 := Make_Literal
                 (New_Integer_Value
                  (Unsigned_Long_Long (N_Total (E)), 1, 10));
               Param1 := Make_Subprogram_Call
                 (RE (RE_Unsigned_Short),
                  Make_List_Id (Param1));

               Param2 := Make_Literal
                 (New_Integer_Value
                  (Unsigned_Long_Long (N_Scale (E)), 1, 10));
               Param2 := Make_Subprogram_Call
                 (RE (RE_Short),
                  Make_List_Id (Param2));

            when K_Sequence_Type =>
               declare
                  Max_Size_Literal : Node_Id;
                  TC_Sequence      : Node_Id;
                  TC_Element       : Node_Id;
                  Seq_Package      : Node_Id;
               begin
                  --  Unbounded, sequences have "0" as limit

                  if Present (Max_Size (E)) then
                     Max_Size_Literal := Make_Literal
                       (FEN.Value (Max_Size (E)));
                  else
                     Max_Size_Literal := Make_Literal
                       (New_Integer_Value (0, 1, 10));
                  end if;

                  TC_Element := Get_TC_Node (Type_Spec (E));
                  TC_Sequence := Get_TC_Node (E);

                  N := Make_Assignment_Statement
                    (TC_Sequence,
                     Make_Subprogram_Call
                     (RE (RE_Build_Sequence_TC),
                      Make_List_Id
                      (TC_Element,
                       Max_Size_Literal)));
                  Append_Node_To_List (N, Statements);

                  Seq_Package := Make_Defining_Identifier
                    (Map_Sequence_Pkg_Helper_Name (E));

                  N := Make_Defining_Identifier (SN (S_Initialize));
                  Set_Homogeneous_Parent_Unit_Name (N, Seq_Package);

                  N := Make_Subprogram_Call
                    (N,
                     Make_List_Id
                     (Make_Parameter_Association
                      (RE (RE_Element_TC), TC_Element),
                      Make_Parameter_Association
                      (RE (RE_Sequence_TC), TC_Sequence)));
                  Append_Node_To_List (N, Statements);
               end;

            when K_Simple_Declarator
              | K_Enumeration_Type
              | K_Structure_Type
              | K_Union_Type
              | K_Exception_Declaration =>
               null;

            when others =>
               raise Program_Error;
         end case;

         if FEN.Kind (E) /= K_Complex_Declarator
           and then FEN.Kind (E) /= K_Sequence_Type
           and then FEN.Kind (E) /= K_Fixed_Point_Type
         then
            Param1 := Make_Designator (VN (V_Name));
            Param2 := Make_Designator (VN (V_Id));

            --  Name_U declaration

            Entity_Name_V := New_String_Value
              (BEN.Name (Defining_Identifier (Stub)), False);
            N := Declare_Name (VN (V_Name), Entity_Name_V);
            Append_Node_To_List (N, Declaration_List);

            --  Id_U declaration

            N := Declare_Name (VN (V_Id), Entity_Rep_Id_V);
            Append_Node_To_List (N, Declaration_List);
         end if;

         --  Add the two parameters

         if FEN.Kind (E) /= K_Sequence_Type then
            N := Add_Parameter (Entity_TC_Name, Param1);
            Append_Node_To_List (N, Statements);
            N := Add_Parameter (Entity_TC_Name, Param2);
            Append_Node_To_List (N, Statements);
         end if;

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
                     Append_Node_To_List (N, Declaration_List);
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
                  Statement_List     : constant List_Id :=
                    New_List (K_List_Id);
                  Default_Index       : Value_Id :=
                    New_Integer_Value (0, 1, 10); --  (0)
                  There_Is_Default    : Boolean           :=
                    False;
                  T                   : Node_Id;
               begin

                  --  Getting the discriminator type and the To_Any
                  --  node corresponding to it

                  TC_Helper := Get_TC_Node (Switch_Type_Spec (E));

                  Handle_Dependency (Switch_Type_Spec (E), Statements);
                  Helpers.Package_Body.Add_Dependency
                    (Parent_Unit_Name (TC_Helper), Dependencies);

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

                  --  The forth parameter is the index of default case
                  --  as a long. we put the remaining parameter in an
                  --  intermediary list. When we get the default case
                  --  index, we add the intermediary list to the
                  --  statements list.

                  Switch_Alternative := First_Entity (Switch_Type_Body (E));
                  while Present (Switch_Alternative) loop
                     Choices := New_List (K_List_Id);
                     Label   := First_Entity (Labels (Switch_Alternative));
                     while Present (Label) loop

                        Choice := Make_Literal
                          (Value             => FEN.Value (Label),
                           Parent_Designator => Literal_Parent);

                        --  If this is not a case statement, then we
                        --  increment the default case index. The
                        --  value of Default_Index will be correctly
                        --  set up after the end of the two loops

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

                     --  Getting the TC_XXX constant corresponding to
                     --  the element type.

                     if FEN.Kind (Declarator) = K_Simple_Declarator then
                        T := Type_Spec (Element (Switch_Alternative));
                        Handle_Dependency (T, Statements);
                        TC_Helper := Get_TC_Node (T);
                     else --  Complex Declarator
                        T := Identifier (Declarator);
                        Handle_Dependency (Declarator, Statements);
                        TC_Helper := Expand_Designator (TC_Node (BE_Node (T)));
                     end if;

                     Helpers.Package_Body.Add_Dependency
                       (Parent_Unit_Name (TC_Helper), Dependencies);

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
                     Append_Node_To_List (N, Declaration_List);

                     --  For each case statement, 3 parameters are added :
                     --  * member label
                     --  * member type
                     --  * member name
                     --  This implies that the same element may be declared
                     --  more than once but with a different label.

                     Choice := First_Node (Choices);
                     while Present (Choice) loop
                        if BEN.Value (Choice) /= No_Value then
                           --  We make a copy of the Choice value to
                           --  avoid adding the next nodes of Choice
                           --  to the argument list

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
                           Append_Node_To_List (N, Statement_List);

                           N := Add_Parameter (Entity_TC_Name, TC_Helper);
                           Append_Node_To_List (N, Statement_List);

                           N := Add_Parameter
                             (Entity_TC_Name,
                              Make_Defining_Identifier (Arg_Name));
                           Append_Node_To_List (N, Statement_List);
                        else --  The default case
                           N := Make_Type_Attribute (Switch_Type, A_First);

                           N := Make_Subprogram_Call
                             (To_Any_Helper,
                              Make_List_Id (N));
                           N := Add_Parameter (Entity_TC_Name, N);
                           Append_Node_To_List (N, Statement_List);

                           N := Add_Parameter (Entity_TC_Name, TC_Helper);
                           Append_Node_To_List (N, Statement_List);

                           N := Add_Parameter
                             (Entity_TC_Name,
                              Make_Defining_Identifier (Arg_Name));
                           Append_Node_To_List (N, Statement_List);

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

                  --  Append the Statement_List list to the end of
                  --  the Statements list (we only append the first
                  --  node, the others are appended automatically)

                  Append_Node_To_List
                    (First_Node (Statement_List),
                     Statements);

               end;

            when K_Structure_Type =>
               declare
                  Member     : Node_Id;
                  Declarator : Node_Id;
                  Designator : Node_Id;
                  Arg_Name   : Name_Id;
                  T          : Node_Id;
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
                        Append_Node_To_List (N, Declaration_List);

                        --  For simple declarators :

                        if FEN.Kind (Declarator) = K_Simple_Declarator then
                           T := Type_Spec (Declaration (Declarator));
                           Handle_Dependency (T, Statements);
                           Param1 := Get_TC_Node (T);
                        else --  Complex Declarator
                           T := Identifier (Declarator);
                           Handle_Dependency (Declarator, Statements);
                           Param1 := Expand_Designator (TC_Node (BE_Node (T)));
                        end if;

                        Helpers.Package_Body.Add_Dependency
                          (Parent_Unit_Name (Param1), Dependencies);

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
                  Member                     : Node_Id;
                  Members                    : List_Id;
                  Declarator                 : Node_Id;
                  Dcl_Name                   : Name_Id;
                  Arg_Name_Node              : Node_Id;
                  Register_Excp_Node         : constant Node_Id :=
                    RE (RE_Register_Exception);
               begin

                  --  Add a dependency to initialize correctly the
                  --  modules

                  Helpers.Package_Body.Add_Dependency
                    (Parent_Unit_Name (Register_Excp_Node), Dependencies);

                  --  In case where the exception has members, we add
                  --  two two parameter for each member.

                  Members := FEN.Members (E);
                  if not FEU.Is_Empty (Members) then
                     Member := First_Entity (Members);
                     while Present (Member) loop
                        Declarator := First_Entity (Declarators (Member));
                        while Present (Declarator) loop

                           --  Declaring the Arg_Name_"member"
                           --  variable

                           Dcl_Name := To_Ada_Name
                             (IDL_Name (FEN.Identifier (Declarator)));
                           Set_Str_To_Name_Buffer ("Arg_Name_");
                           Get_Name_String_And_Append (Dcl_Name);
                           Arg_Name_Node := Make_Defining_Identifier
                             (Name_Find);

                           --  Obtaining a string literal of the
                           --  member name

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
                           Append_Node_To_List (N, Declaration_List);

                           --  Adding the two additional parameters

                           N := Get_TC_Node (Type_Spec (Member));

                           Handle_Dependency (Type_Spec (Member), Statements);
                           Helpers.Package_Body.Add_Dependency
                             (Parent_Unit_Name (N), Dependencies);

                           N := Add_Parameter (Entity_TC_Name, N);
                           Append_Node_To_List (N, Statements);
                           N := Add_Parameter (Entity_TC_Name, Arg_Name_Node);
                           Append_Node_To_List (N, Statements);

                           Declarator := Next_Entity (Declarator);
                        end loop;
                        Member := Next_Entity (Member);
                     end loop;
                  end if;

                  Raise_From_Any_Access_Node := Make_Designator
                    (Map_Raise_From_Any_Name (E));
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
                  Handle_Dependency (T, Statements);

                  if Is_Base_Type (T)
                    or else FEN.Kind (T) = K_Scoped_Name
                    or else FEN.Kind (T) = K_Fixed_Point_Type
                    or else FEN.Kind (T) = K_Sequence_Type
                  then
                     N := Get_TC_Node (T);

                  elsif Kind (T) = K_String_Type or else
                    Kind (T) = K_Wide_String_Type then
                     declare
                        Pkg_Inst : constant Node_Id :=
                          (Defining_Identifier
                           (Instantiation_Node
                            (BE_Node
                             (T))));
                     begin

                        --  Getting the identifier of the TypeCode
                        --  function located in the instantiated
                        --  package Bounded_...  in the stub spec

                        if Kind (T) = K_String_Type then
                           N := RE (RE_TC_Bounded_String);
                        else
                           N := RE (RE_TC_Bounded_Wide_String);
                        end if;
                        Set_Homogeneous_Parent_Unit_Name
                          (N, Copy_Node (Pkg_Inst));
                     end;
                  else
                     raise Program_Error;
                  end if;
                  Helpers.Package_Body.Add_Dependency
                    (Parent_Unit_Name (N), Dependencies);

                  N := Add_Parameter (Entity_TC_Name, N);
                  Append_Node_To_List (N, Statements);
               end;

            when others =>
               null;
         end case;
      end Initialize_Routine;

      -----------------------
      -- Handle_Dependency --
      -----------------------

      procedure Handle_Dependency (N : Node_Id; Statements : List_Id) is
         Init_Spg : constant Node_Id := Get_Initialize_Node (N);
      begin
         if Present (Init_Spg) and then BEN.Kind (Init_Spg) /= K_Node_Id then
            Append_Node_To_List
              (Make_Subprogram_Call (Init_Spg, No_List), Statements);
         end if;
      end Handle_Dependency;

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

         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Message)),
            RE (RE_String_2));
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

         --  Obtaining the node corresponding to the declaration of
         --  the "Excp_Name"_Members type.

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
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Members)),
            Constant_Present    => True,
            Object_Definition   => Defining_Identifier (Excp_Members),
            Expression          => N);
         Append_Node_To_List (N, Declarations);

         --  End Declarations

         --  Begin Statements

         N := Make_Defining_Identifier
           (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         N := Make_Attribute_Designator (N, A_Identity);
         N := Make_Subprogram_Call
           (RE (RE_User_Raise_Exception),
            Make_List_Id
            (N,
             Make_Defining_Identifier (PN (P_Members)),
             Make_Defining_Identifier (PN (P_Message))));
         Append_Node_To_List (N, Statements);

         --  End Statements

         N := Make_Subprogram_Implementation
           (Spec, Declarations, Statements);

         return N;
      end Raise_Excp_From_Any_Body;

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
         Set_Internals_Body;

         if Generate_Shadow_Routines then

            --  The aggregate container routines

            Aggregate_Container_Routines (E);

            --  The Wrap function body

            N := Wrap_Body (E);
            Append_Node_To_List (N, Statements (Current_Package));

         end if;

         --  Initialize

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Enumeration_Type;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Internals_Body;

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Forward_Interface_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Internals_Body;

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

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
         if not Map_Particular_CORBA_Parts (E, PK_Init_Body) then
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
         D : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         D := First_Entity (Definitions (E));
         while Present (D) loop
            Visit (D);
            D := Next_Entity (D);
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
         Set_Internals_Body;

         if Generate_Shadow_Routines then

            --  The aggregate container routines

            Aggregate_Container_Routines (E);

            --  The Wrap function body

            N := Wrap_Body (E);
            Append_Node_To_List (N, Statements (Current_Package));

         end if;

         --  See the comment message in
         --  Helpers.Package_Spec.Visit_Structure_Type for more
         --  details on the instructions below

         Member := First_Entity (Members (E));
         while Present (Member) loop
            Declarator := First_Entity (Declarators (Member));
            while Present (Declarator) loop
               if FEN.Kind (Declarator) = K_Complex_Declarator then

                  if Generate_Shadow_Routines then

                     --  The aggregate container routines

                     Aggregate_Container_Routines (Declarator);

                     --  The Wrap function body

                     N := Wrap_Body (Declarator);
                     Append_Node_To_List (N, Statements (Current_Package));

                  end if;

                  --  Initialize

                  N := Initialize_Body (Declarator);
                  Append_Node_To_List (N, Statements (Current_Package));
               end if;

               Declarator := Next_Entity (Declarator);
            end loop;
            Member := Next_Entity (Member);
         end loop;

         --  Initialize

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         N : Node_Id;
         D : Node_Id;
         T : constant Node_Id := Type_Spec (E);
      begin
         Set_Internals_Body;

         case (FEN.Kind (T)) is

            when K_Fixed_Point_Type =>
               if Generate_Shadow_Routines then
                  --  The Wrap function body

                  N := Wrap_Body (T);
                  Append_Node_To_List (N, Statements (Current_Package));
               end if;

               --  The Initialize body

               N := Initialize_Body (T);
               Append_Node_To_List (N, Statements (Current_Package));

            when K_Sequence_Type =>
               if Generate_Shadow_Routines then
                  --  The Wrap function body

                  N := Wrap_Body (T);
                  Append_Node_To_List (N, Statements (Current_Package));
               end if;

               --  The Initialize body

               N := Initialize_Body (T);
               Append_Node_To_List (N, Statements (Current_Package));

            when K_String_Type | K_Wide_String_Type =>
               if Generate_Shadow_Routines then
                  --  The Wrap function body

                  N := Wrap_Body (T);
                  Append_Node_To_List (N, Statements (Current_Package));
               end if;

            when others =>
               null;
         end case;

         D := First_Entity (Declarators (E));
         while Present (D) loop
            if FEN.Kind (D) = K_Complex_Declarator then
               if Generate_Shadow_Routines then
                  --  The aggregate container routines

                  Aggregate_Container_Routines (D);
               end if;
            end if;

            --  We do not generate the Wrap body if the type is an
            --  Object type, a bounded [wide] string type, a fixed
            --  point type or a sequence type

            if not ((FEN.Kind (T) = K_Scoped_Name
                     and then Is_Object_Type (T)
                     and then FEN.Kind (D) = K_Simple_Declarator)
                    or else FEN.Kind (T) = K_String_Type
                    or else FEN.Kind (T) = K_Wide_String_Type
                    or else FEN.Kind (T) = K_Fixed_Point_Type
                    or else FEN.Kind (T) = K_Sequence_Type)
            then
               if Generate_Shadow_Routines then
                  --  The Wrap function body

                  N := Wrap_Body (D);
                  Append_Node_To_List (N, Statements (Current_Package));
               end if;
            end if;

            --  Initialize

            N := Initialize_Body (D);
            Append_Node_To_List (N, Statements (Current_Package));

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
         Set_Internals_Body;

         if Generate_Shadow_Routines then

            --  The aggregate container routines

            Aggregate_Container_Routines (E);

            --  The Wrap function body

            N := Wrap_Body (E);
            Append_Node_To_List (N, Statements (Current_Package));

         end if;

         --  See the comment message in
         --  Helpers.Package_Spec.Visit_Union_Type for more
         --  details on the instructions below

         Alternatives := Switch_Type_Body (E);
         Alternative := First_Entity (Alternatives);
         while Present (Alternative) loop
            Declarator := FEN.Declarator (FEN.Element (Alternative));
            if FEN.Kind (Declarator) = K_Complex_Declarator then
               if Generate_Shadow_Routines then
                  --  The aggregate container routines

                  Aggregate_Container_Routines (Declarator);

                  --  The Wrap function body

                  N := Wrap_Body (Declarator);
                  Append_Node_To_List (N, Statements (Current_Package));
               end if;

               --  Initialize

               N := Initialize_Body (Declarator);
               Append_Node_To_List (N, Statements (Current_Package));
            end if;

            Alternative := Next_Entity (Alternative);
         end loop;

         --  Initialize

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Union_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         N          : Node_Id;
         Raise_Node : Node_Id;
      begin
         Set_Internals_Body;

         --  Generation of the Raise_"Exception_Name"_From_Any spec

         Raise_Node := Make_Defining_Identifier
           (Map_Raise_From_Any_Name (E));
         N := Raise_Excp_From_Any_Spec (Raise_Node);
         Append_Node_To_List (N, Statements (Current_Package));

         --  Addition of the pragma No_Return. The argument of the
         --  pragma No_Return must be a local name

         N := Make_Pragma_Statement
           (Pragma_No_Return,
            Make_List_Id (Make_Designator (BEN.Name (Raise_Node))));
         Append_Node_To_List (N, Statements (Current_Package));

         --  Generation of the Raise_"Exception_Name"_From_Any body

         N := Raise_Excp_From_Any_Body (E, Raise_Node);
         Append_Node_To_List (N, Statements (Current_Package));

         --  The body of the Initialize routine

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Exception_Declaration;

   end Package_Body;

end Backend.BE_CORBA_Ada.Helpers_Internals;
