------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 BACKEND.BE_CORBA_ADA.HELPERS_INTERNALS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Namet;    use Namet;
with Platform; use Platform;
with Values;   use Values;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

package body Backend.BE_CORBA_Ada.Helpers_Internals is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;

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
      --  Return the spec of the Initialize procedure that builds the
      --  TypeCode corresponding to the IDL type E.

      --  The routines below build the type declarations necessary for
      --  the ``Shadow Any Tree''.

      function From_Any_Container_Spec (E : Node_Id) return Node_Id;
      --  Return the additional `From_Any' function spec corresponding
      --  to the enumeration type `E'. The purpose of this is to
      --  factorize code between the implementation of the classical
      --  `From_Any' function of the enumeration types and for the
      --  unions having an enumeration switch.

      function Wrap_Spec (E : Node_Id) return Node_Id;
      --  Build the spec of the Wrap function corresponding to IDL
      --  type E.

      function Element_Wrap_Spec (E : Node_Id) return Node_Id;
      --  Builds the spec of the Element_Wrap function corresponding
      --  to the type spec of the sequence type E.

      function Pointer_Declaration (E : Node_Id) return Node_Id;
      --  Makes a pointer type declaration corresponding to the mapped
      --  Ada type of the IDL type E.

      function Content_Declaration (E : Node_Id) return Node_Id;
      --  Makes a record type declaration correponding to the
      --  Aggregate container for IDL type E.

      function Clone_Spec (E : Node_Id) return Node_Id;
      function Finalize_Value_Spec (E : Node_Id) return Node_Id;
      function Get_Aggregate_Count_Spec (E : Node_Id) return Node_Id;
      function Set_Aggregate_Count_Spec (E : Node_Id) return Node_Id;
      function Get_Aggregate_Element_Spec (E : Node_Id) return Node_Id;
      function Set_Aggregate_Element_Spec (E : Node_Id) return Node_Id;
      --  Specs for the routines that manipulate the aggregate
      --  container.

      function Lengths_Constant_Declaration (E : Node_Id) return Node_Id;
      --  Makes a constant declaration containing the length of the
      --  several dimensions of an array.

      procedure Aggregate_Container_Routines (E : Node_Id);
      --  Used for code factorization. This procedure assumes that the
      --  current package spec has been properly set.

      ---------------------
      -- Initialize_Spec --
      ---------------------

      function Initialize_Spec (E : Node_Id) return Node_Id is
         N        : Node_Id;
         Spg_Name : Name_Id;
      begin
         --  Build the defining identifier for the initialization
         --  subprogram.

         case FEN.Kind (E) is
            when K_Complex_Declarator =>
               Spg_Name := Add_Suffix_To_Name
                 ("_Array", To_Ada_Name (FEN.IDL_Name (Identifier (E))));

            when K_Fixed_Point_Type =>
               Spg_Name := BEN.Name
                 (Defining_Identifier (Type_Def_Node (BE_Node (E))));

            when K_Sequence_Type
              | K_String_Type
              | K_Wide_String_Type =>
               Spg_Name := BEN.Name
                 (Defining_Identifier (Instantiation_Node (BE_Node (E))));

            when others =>
               Spg_Name := To_Ada_Name (FEN.IDL_Name (Identifier (E)));
         end case;

         --  Add the prefix to the initialization subprogram
         --  identifier.

         Spg_Name := Add_Prefix_To_Name ("Initialize_", Spg_Name);

         --  Make the parameterless subprogram spec

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (Spg_Name), No_List);

         return N;
      end Initialize_Spec;

      -----------------------------
      -- From_Any_Container_Spec --
      -----------------------------

      function From_Any_Container_Spec (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Enumeration_Type);

         Profile   : constant List_Id := New_List (K_Parameter_Profile);
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_C)),
            Make_Attribute_Reference (RE (RE_Any_Container), A_Class));
         Append_Node_To_List (Parameter, Profile);

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_From_Any)),
            Profile,
            Get_Type_Definition_Node (E));

         return N;
      end From_Any_Container_Spec;

      ---------------
      -- Wrap_Spec --
      ---------------

      function Wrap_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := Make_Attribute_Reference
           (RE (RE_Content), A_Class);
         N       : Node_Id;
         P_Type  : Node_Id;
      begin
         --  Get the mapped Ada type corresponding to the IDL entity E

         case FEN.Kind (E) is
            when K_String_Type
              | K_Wide_String_Type
              | K_Fixed_Point_Type
              | K_Sequence_Type
              | K_Enumeration_Type
              | K_Complex_Declarator
              | K_Simple_Declarator
              | K_Union_Type
              | K_Structure_Type =>
               P_Type := Get_Type_Definition_Node (E);

            when others =>
               declare
                  Msg : constant String := "Cannot generate Wrap spec for a "
                    & FEN.Node_Kind'Image (FEN.Kind (E));
               begin
                  raise Program_Error with Msg;
               end;
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

      -----------------------
      -- Element_Wrap_Spec --
      -----------------------

      function Element_Wrap_Spec (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Sequence_Type);

         Profile : constant List_Id := New_List (K_Parameter_Profile);
         Returns : constant Node_Id := Make_Attribute_Reference
           (RE (RE_Content), A_Class);
         T       : constant Node_Id := Type_Spec (E);
         P_Type  : constant Node_Id := Get_Type_Definition_Node (T);
         N       : Node_Id;
      begin
         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_X)),
            Subtype_Mark        => Make_Access_Type_Definition (P_Type),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  Create the subprogram spec

         N := Make_Subprogram_Specification
           (Defining_Identifier => Map_Wrap_Element_Identifier (E),
            Parameter_Profile   => Profile,
            Return_Type         => Returns);

         return N;
      end Element_Wrap_Spec;

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
              (Subtype_Indication => Get_Type_Definition_Node (E),
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
         --  If E is a complex declarator and then if the dimension
         --  of the array is greater than 1, then we declare an array
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
         --  the kind f 'E'.

         --  All the containers contain a component 'V' which is a
         --  pointer to the Ada type mapped from the IDL type 'E'.

         N := Make_Component_Declaration
           (Defining_Identifier => Make_Defining_Identifier (CN (C_V)),
            Subtype_Indication  => Make_Identifier
              (Map_Pointer_Type_Name (E)));
         Append_Node_To_List (N, Components);

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               --  For enumeration type, we add an alised field
               --  corresponding to an unsigned long variable.

               N := Make_Component_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (CN (C_Repr_Cache)),
                  Subtype_Indication  => RE (RE_Unsigned_Long_1),
                  Aliased_Present     => True);
               Append_Node_To_List (N, Components);

            when K_Complex_Declarator =>
               --  If the array type is multidimensional, then we add
               --  some extra fileds.

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
                     Subtype_Indication  => Make_Identifier
                       (Map_Indices_Name (E)));
                  Append_Node_To_List (N, Components);
               end if;

            when K_Union_Type =>
               --  For unions, we add an aliased field that
               --  corresponds to the union switch.

               N := Make_Component_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (CN (C_Switch_Cache)),
                  Subtype_Indication  => Map_Expanded_Name
                    (Switch_Type_Spec (E)),
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
            Subtype_Mark        => Make_Identifier (Map_Container_Name (E)),
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
            Subtype_Mark        => Make_Identifier (Map_Container_Name (E)),
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
            Subtype_Mark        => Make_Identifier (Map_Container_Name (E)),
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
            Subtype_Mark        => Make_Identifier (Map_Container_Name (E)),
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
         Returns : constant Node_Id := Make_Attribute_Reference
           (RE (RE_Content), A_Class);
         N       : Node_Id;
      begin
         --  Build the parameters list

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_ACC)),
            Subtype_Mark        => Make_Access_Type_Definition
              (Make_Identifier (Map_Container_Name (E)),
               Is_Not_Null      => True),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_TC)),
            Subtype_Mark        => RE (RE_Object_Ptr_2),
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
              (RE (RE_Mechanism),
               Is_Not_Null => True),
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
            Subtype_Mark        => Make_Identifier (Map_Container_Name (E)),
            Parameter_Mode      => Mode_Inout);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_TC)),
            Subtype_Mark        => RE (RE_Object_Ptr_2),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Index)),
            Subtype_Mark        => RE (RE_Unsigned_Long_1),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_From_C)),
            Subtype_Mark        => Make_Attribute_Reference
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

         --  For complex declarators, we declare an additional
         --  constant.

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

         --  The enumeration types are a special case. They require an
         --  additional `From_Any' function that converts containers
         --  to a value from the enumeration type.

         N := From_Any_Container_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_From_Any_Container);

         --  The aggregate container routines

         Aggregate_Container_Routines (E);

         --  The wrap function spec

         N := Wrap_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Wrap);
         Append_Node_To_List (N, Visible_Part (Current_Package));

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
         if not Map_Particular_CORBA_Parts (E, PK_Helper_Internals_Spec) then
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
      begin
         Set_Internals_Spec;

         --  The aggregate container routines

         Aggregate_Container_Routines (E);

         --  The wrap function spec

         N := Wrap_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Wrap);
         Append_Node_To_List (N, Visible_Part (Current_Package));

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

         --  Extra code generation for fixed point types, sequence
         --  types and bounded [wide] string types.

         case FEN.Kind (T) is
            when K_Fixed_Point_Type =>
               declare
                  Package_Node : Node_Id;
                  Fixed_Type_Node : Node_Id;
               begin
                  --  We instantiate the generic helper package here
                  --  because we need it in the Wrap function body.

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

                  --  The wrap function spec

                  N := Wrap_Spec (T);
                  Bind_FE_To_BE (T, N, B_Wrap);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end;

            when K_Sequence_Type =>
               declare
                  S            : Node_Id;
                  Package_Node : Node_Id;
                  Elt_From_Any : Node_Id;
                  Elt_To_Any   : Node_Id;
                  Elt_Wrap     : Node_Id;
                  Profile      : constant List_Id := New_List (K_List_Id);
               begin
                  --  Do not generate anything if the sequence element
                  --  type has a local interface component.

                  if not FEU.Has_Local_Component (T) then
                     --  The element wrap function

                     N := Element_Wrap_Spec (T);
                     Bind_FE_To_BE (T, N, B_Element_Wrap);
                     Append_Node_To_List (N, Visible_Part (Current_Package));

                     --  The wrap function spec

                     N := Wrap_Spec (T);
                     Bind_FE_To_BE (T, N, B_Wrap);
                     Append_Node_To_List (N, Visible_Part (Current_Package));

                     --  We instantiate the generic helper package here
                     --  because we need it in the initialization routine

                     S := Expand_Designator (Instantiation_Node (BE_Node (T)));
                     Package_Node := Make_Defining_Identifier
                       (Map_Sequence_Pkg_Helper_Name (T));

                     --  Get the the From_any, the To_Any and the Wrap
                     --  functions nodes corresponding to the elements
                     --  of the sequence.

                     Elt_From_Any := Get_From_Any_Node (Type_Spec (T));
                     Elt_To_Any := Get_To_Any_Node (Type_Spec (T));

                     Elt_Wrap := Expand_Designator
                       (Element_Wrap_Node (BE_Node (T)));

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

                     Append_Node_To_List
                       (Make_Parameter_Association
                        (Make_Defining_Identifier (PN (P_Element_Wrap)),
                         Elt_Wrap),
                        Profile);

                     if Present (Max_Size (T)) then
                        N := RE (RE_CORBA_Helper_1);
                     else
                        N := RE (RE_CORBA_Helper_2);
                     end if;

                     --  Change the parent of the generic package

                     N := Make_Selected_Component (S, Selector_Name (N));

                     N := Make_Package_Instantiation
                       (Defining_Identifier => Package_Node,
                        Generic_Package     => N,
                        Parameter_List      => Profile);
                     Append_Node_To_List (N, Visible_Part (Current_Package));
                  end if;

                  N := Initialize_Spec (T);
                  Bind_FE_To_BE (T, N, B_Initialize);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end;

            when K_String_Type | K_Wide_String_Type =>
               --  The Initialize Spec

               N := Initialize_Spec (T);
               Bind_FE_To_BE (T, N, B_Initialize);
               Append_Node_To_List (N, Visible_Part (Current_Package));

               --  The wrap function spec

               N := Wrap_Spec (T);
               Bind_FE_To_BE (T, N, B_Wrap);
               Append_Node_To_List (N, Visible_Part (Current_Package));

            when others =>
               null;
         end case;

         --  General case

         D := First_Entity (Declarators (E));
         while Present (D) loop
            if FEN.Kind (D) = K_Complex_Declarator then
               --  The aggregate container routines

               Aggregate_Container_Routines (D);
            end if;

            --  We do not generate the `Wrap' spec if the defined type is:
            --  1 - derived from an object type
            --  2 - a fixed point type
            --  3 - a bounded string type
            --  4 - a sequence type

            if not (((FEN.Kind (T) = K_Scoped_Name
                      or else FEN.Kind (T) = K_Object)
                     and then FEN.Kind (D) = K_Simple_Declarator)
                    or else FEN.Kind (T) = K_String_Type
                    or else FEN.Kind (T) = K_Wide_String_Type
                    or else FEN.Kind (T) = K_Fixed_Point_Type
                    or else FEN.Kind (T) = K_Sequence_Type)
            then
               --  The wrap function spec

               N := Wrap_Spec (D);
               Bind_FE_To_BE (Identifier (D), N, B_Wrap);
               Append_Node_To_List (N, Visible_Part (Current_Package));
            else
               --  Bind the wrap function created for the type
               --  specifier to the declarator. If we handle a
               --  sequence type, verifies that the wrap function has
               --  been created before performing the binding.

               if FEN.Kind (T) /= K_Sequence_Type
                 or else not FEU.Has_Local_Component (T)
               then
                  N := Get_Wrap_Node (T, False);
                  Bind_FE_To_BE (Identifier (D), N, B_Wrap);
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
      begin
         Set_Internals_Spec;

         --  The aggregate container routines

         Aggregate_Container_Routines (E);

         --  The wrap function spec

         N := Wrap_Spec (E);
         Bind_FE_To_BE (Identifier (E), N, B_Wrap);
         Append_Node_To_List (N, Visible_Part (Current_Package));

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
      --  controls the IDL type E.

      function Initialized_Flag_Declaration (E : Node_Id) return Node_Id;
      --  Declares a Boolean flag useful for the initialization of a
      --  TypeCode corresponding to the IDL type E.

      function Initialize_Body (E : Node_Id) return Node_Id;
      --  Returns the body of the Initialize procedure that builds the
      --  TypeCode corresponding to the IDL type E.

      procedure Initialize_Routine
        (E                : Node_Id;
         Declaration_List : List_Id;
         Statements       : List_Id);
      --  Fills the lists Declaration_List and Statements with the
      --  routines initializing the IDL type E.

      function From_Any_Container_Body (E : Node_Id) return Node_Id;
      --  Return the additional `From_Any' function body corresponding
      --  to the enumeration type `E'.

      procedure Handle_Dependency (N : Node_Id; Statements : List_Id);
      --  This procedure handles the dependency on the TypeCode
      --  corresponding to the node N. If the node N is a CORBA type,
      --  it adds the necessary dependency to the Helper
      --  initialization. If the node N belongs to the current IDL
      --  specification, it calls the Initialize_XXX function that
      --  build its TypeCode.

      function Wrap_Body (E : Node_Id) return Node_Id;
      --  Builds the body of the Wrap function corresponding to IDL
      --  type E.

      function Element_Wrap_Body (E : Node_Id) return Node_Id;
      --  Builds the body of the Element_Wrap function corresponding
      --  to the type spec of the sequence type E.

      function Clone_Body (E : Node_Id) return Node_Id;
      function Finalize_Value_Body (E : Node_Id) return Node_Id;
      function Get_Aggregate_Count_Body (E : Node_Id) return Node_Id;
      function Set_Aggregate_Count_Body (E : Node_Id) return Node_Id;
      function Get_Aggregate_Element_Body (E : Node_Id) return Node_Id;
      function Set_Aggregate_Element_Body (E : Node_Id) return Node_Id;
      --  Bodies for the routines that manipulate the aggregate
      --  container.

      procedure Aggregate_Container_Routines (E : Node_Id);
      --  Used for code factorization. This procedure assumes that the
      --  current package body has been properly set.

      function Get_Aggr_Count (E : Node_Id) return Node_Id;
      --  Factorize some code between Get_Aggregate_Count_Body and
      --  Set_Aggregate_Count_Body.

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

         Spec : Node_Id;
         N    : Node_Id;
      begin
         case FEN.Kind (E) is
            when K_String_Type | K_Wide_String_Type =>
               --  For bounded string types, we simply rename the Wrap
               --  function of the instantiated generic package.

               Spec := Copy_Subprogram_Spec (Wrap_Node (BE_Node (E)));
               N := Make_Selected_Component
                 (Expand_Designator (Instantiation_Node (BE_Node (E))),
                  Make_Identifier (SN (S_Wrap)));
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
                    (Subtype_Mark => Make_Identifier
                       (Map_Pointer_Type_Name (E)),
                     Expression   => Make_Identifier (PN (P_X)));
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
                           Make_Selected_Component (PN (P_X), CN (C_Switch)));
                        Append_Node_To_List (N, Aggr_List);

                     when K_Structure_Type =>
                        null;
                     when others =>
                        declare
                           Msg : constant String :=
                             "Cannor generate Wrap body for a "
                             & FEN.Node_Kind 'Image (FEN.Kind (E));
                        begin
                           raise Program_Error with Msg;
                        end;
                  end case;

                  N := Make_Record_Aggregate
                    (Aggr_List, RE (RE_Aggregate_Content));
                  N := Make_Qualified_Expression
                    (Make_Identifier (Map_Container_Name (E)), N);
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, Statements);

                  N := Make_Subprogram_Body
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
                    (Map_Expanded_Name (O),
                     Make_Explicit_Dereference (Make_Identifier (PN (P_X))));
                  N := Make_Attribute_Reference (N, A_Unrestricted_Access);

                  N := Make_Subprogram_Call
                    (Get_Wrap_Node (O), Make_List_Id (N));
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, Statements);

                  N := Make_Subprogram_Body
                    (Spec, No_List, Statements);
               end;

            when K_Fixed_Point_Type =>
               --  For fixed point types, we simply rename the Wrap
               --  function of the instantiated helper generic
               --  package.

               Spec := Copy_Subprogram_Spec (Wrap_Node (BE_Node (E)));
               N := Make_Selected_Component
                 (Make_Identifier (Map_Fixed_Type_Helper_Name (E)),
                  Make_Identifier (SN (S_Wrap)));
               Set_Renamed_Entity (Spec, N);
               N := Spec;

            when K_Sequence_Type =>
               --  For sequence types, we simply rename the Wrap
               --  function of the instantiated helper generic
               --  package.

               Spec := Copy_Subprogram_Spec (Wrap_Node (BE_Node (E)));
               N := Make_Selected_Component
                 (Make_Identifier (Map_Sequence_Pkg_Helper_Name (E)),
                  Make_Identifier (SN (S_Wrap)));
               Set_Renamed_Entity (Spec, N);
               N := Spec;

            when others =>
               raise Program_Error;
         end case;

         return N;
      end Wrap_Body;

      -----------------------
      -- Element_Wrap_Body --
      -----------------------

      function Element_Wrap_Body (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Sequence_Type);

         Spec       : constant Node_Id := Element_Wrap_Node (BE_Node (E));
         Statements : constant List_Id := New_List (K_Statement_List);
         O          : constant Node_Id :=
           FEU.Get_Original_Type_Declarator (Type_Spec (E));
         --  The original type of the sequence type spec

         W          : constant Node_Id := Get_Wrap_Node (O);
         N          : Node_Id;
      begin
         N := Make_Explicit_Dereference (Make_Identifier (PN (P_X)));

         --  If the type spec of the sequence is a user defined type,
         --  we have to cast it to the original type.

         Cast_When_Necessary (N, Type_Spec (E), O, True);

         --  Get an 'Unrestricted_Access to the parameter

         N := Make_Attribute_Reference (N, A_Unrestricted_Access);

         --  Call the original type `Wrap'

         N := Make_Subprogram_Call (W, Make_List_Id (N));
         N := Make_Return_Statement (N);
         Append_Node_To_List (N, Statements);

         --  Make the body

         N := Make_Subprogram_Body (Spec, No_List, Statements);

         return N;
      end Element_Wrap_Body;

      ----------------
      -- Clone_Body --
      ----------------

      function Clone_Body (E : Node_Id) return Node_Id is
         Spec       : constant Node_Id := Clone_Node
           (BE_Node (Identifier (E)));
         Dcl_Part   : constant List_Id := New_List (K_Declaration_List);
         Statements : constant List_Id := New_List (K_Statement_List);
         N          : Node_Id;
         Expr       : Node_Id;
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
              (Make_Explicit_Dereference (Make_Identifier (PN (P_Into))),
               Op_Not_In,
               Make_Identifier (Map_Container_Name (E)));
            N := Make_If_Statement
              (Condition       => Condition,
               Then_Statements =>
                 Make_List_Id (Make_Return_Statement (Make_Null_Statement)));
            Append_Node_To_List (N, Then_Statements);

            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Target)),
               Make_Defining_Identifier (PN (P_Into)));
            Append_Node_To_List (N, Then_Statements);

            --  Build a designator to 'ACC.V.all'

            Expr := Make_Explicit_Dereference
              (Make_Selected_Component (PN (P_ACC), CN (C_V)));

            --  Build the type conversion Content_Ü_<Type>
            --  (Target.all)

            Converted := Make_Type_Conversion
              (Make_Identifier (Map_Container_Name (E)),
               Make_Explicit_Dereference (Make_Identifier (PN (P_Target))));

            N := Make_Assignment_Statement
              (Make_Explicit_Dereference
               (Make_Selected_Component
                (Converted, Make_Identifier (CN (C_V)))),
               Expr);
            Append_Node_To_List (N, Then_Statements);

            --  Else statement

            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Target)),
               Make_Object_Instantiation (Make_Identifier
                                          (Map_Container_Name (E))));
            Append_Node_To_List (N, Else_Statements);

            --  For discriminated types (mapped from IDL unions), the
            --  cloned copy has to be allocated with the proper
            --  discriminent constraints. This construct is also valid
            --  for non-discriminated types.

            N := Make_Qualified_Expression
              (Subtype_Mark => Expand_Designator
                 (Type_Def_Node (BE_Node (Identifier (E)))),
               Operand      => Expr);

            Expr := Make_Object_Instantiation (N);

            N := Make_Assignment_Statement
              (Make_Selected_Component
               (Converted, Make_Identifier (CN (C_V))),
               Expr);
            Append_Node_To_List (N, Else_Statements);

            Condition := Make_Expression
              (Make_Identifier (PN (P_Into)),
               Op_Not_Equal,
               Make_Null_Statement);
            N := Make_If_Statement
              (Condition       => Condition,
               Then_Statements => Then_Statements,
               Else_Statements => Else_Statements);
            Append_Node_To_List (N, Statements);
         end;

         --  Specific part

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               --  Assign the remaining record fields

               N := Make_Selected_Component
                 (Converted, Make_Identifier (CN (C_Repr_Cache)));
               N := Make_Assignment_Statement
                 (N, Make_Selected_Component (PN (P_ACC), CN (C_Repr_Cache)));
               Append_Node_To_List (N, Statements);

            when K_Union_Type =>
               --  Suppress discriminant checks

               N := Make_Pragma
                 (Pragma_Suppress,
                  Make_List_Id (RE (RE_Discriminant_Check)));
               Append_Node_To_List (N, Dcl_Part);

               --  Assign the remaining record fields

               N := Make_Selected_Component
                 (Converted, Make_Identifier (CN (C_Switch_Cache)));
               N := Make_Assignment_Statement
                 (N, Make_Selected_Component
                  (PN (P_ACC), CN (C_Switch_Cache)));
               Append_Node_To_List (N, Statements);

            when K_Complex_Declarator =>
               if FEU.Is_Multidimensional_Array (E) then
                  --  Assign the remaining record fields

                  N := Make_Selected_Component
                    (Converted, Make_Identifier (CN (C_Dimen)));
                  N := Make_Assignment_Statement
                    (N, Make_Selected_Component (PN (P_ACC), CN (C_Dimen)));
                  Append_Node_To_List (N, Statements);

                  N := Make_Selected_Component
                    (Converted, Make_Identifier (CN (C_Indices)));
                  N := Make_Assignment_Statement
                    (N, Make_Selected_Component (PN (P_ACC), CN (C_Indices)));
                  Append_Node_To_List (N, Statements);
               end if;

            when others =>
               null;
         end case;

         --  The return statement

         N := Make_Return_Statement (Make_Identifier (PN (P_Target)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Body (Spec, Dcl_Part, Statements);
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
           (RU (RU_Ada_Unchecked_Deallocation),
            Make_List_Id
            (Expand_Designator (Type_Def_Node (BE_Node (Identifier (E)))),
             Make_Identifier (Map_Pointer_Type_Name (E))));
         N := Make_Subprogram_Specification
           (Defining_Identifier     => Make_Defining_Identifier (SN (S_Free)),
            Parameter_Profile       => No_List,
            Instantiated_Subprogram => N);
         Append_Node_To_List (N, Dcl_Part);

         --  The deallocation procedure call

         N := Make_Subprogram_Call
           (Make_Identifier (SN (S_Free)),
            Make_List_Id (Make_Selected_Component (PN (P_ACC), CN (C_V))));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Body (Spec, Dcl_Part, Statements);
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
         --  multidimensional array.

         if FEN.Kind (E) /= K_Complex_Declarator
           or else not FEU.Is_Multidimensional_Array (E)
         then
            N := Make_Pragma
              (Pragma_Unreferenced,
               Make_List_Id (Make_Identifier (PN (P_ACC))));
            Append_Node_To_List (N, Dcl_Part);
         end if;

         --  The return statement

         N := Make_Return_Statement (Returns);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Body (Spec, Dcl_Part, Statements);
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
         --  multidimensional array.

         if FEN.Kind (E) /= K_Complex_Declarator
           or else not FEU.Is_Multidimensional_Array (E)
         then
            N := Make_Pragma
              (Pragma_Unreferenced,
               Make_List_Id (Make_Identifier (PN (P_ACC))));
            Append_Node_To_List (N, Dcl_Part);
         end if;

         --  The if statement

         C := Make_Expression
           (Make_Identifier (PN (P_Count)), Op_Not_Equal, Aggr_Count);
         N := Make_Raise_Statement (Make_Identifier (EN (E_Program_Error)));
         N := Make_If_Statement (C, Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Body (Spec, Dcl_Part, Statements);
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

         --  ACC.V might be uninitialized and have an invalid
         --  representation (case of Get_Aggregate_Element being
         --  called from within an unmarshall routine), in which case
         --  we know that we will overwrite the invalid value without
         --  using it; we must disable validity checks here so that we
         --  do not fail a runtime check on the bogus value.

         N := Make_Pragma (Pragma_Suppress,
                Make_List_Id
                  (RE (RE_Id'Value
                        ("RE_" & Platform.Validity_Check_Name))));
         Append_Node_To_List (N, Dcl_Part);

         --  The TypeCode formal is of no use here (we always
         --  statically know the type of each aggregate element).

         Append_Node_To_List (Make_Identifier (PN (P_TC)),
                              Unref_Params);

         --  IDL node kind dependant part

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               begin
                  --  An enum always has exactly one element, so we
                  --  can ignore the provided index.

                  Append_Node_To_List (Make_Identifier (PN (P_Index)),
                                       Unref_Params);

                  --  Statements

                  --  Setting the position of the enumerator

                  N := Make_Assignment_Statement
                    (Make_Selected_Component (PN (P_ACC), CN (C_Repr_Cache)),
                     Make_Subprogram_Call
                      (Make_Attribute_Reference
                       (Expand_Designator
                        (Type_Def_Node (BE_Node (Identifier (E)))), A_Pos),
                         Make_List_Id
                          (Make_Explicit_Dereference
                           (Make_Selected_Component (PN (P_ACC), CN (C_V))))));
                  Append_Node_To_List (N, Statements);

                  --  Setting the Mechanism

                  N := Make_Assignment_Statement
                    (Make_Explicit_Dereference (Make_Identifier (PN (P_Mech))),
                     RE (RE_By_Value));
                  Append_Node_To_List (N, Statements);

                  --  Return value

                  N := Make_Return_Statement
                    (Make_Subprogram_Call
                     (RE (RE_Wrap_1),
                      Make_List_Id
                      (Make_Attribute_Reference
                       (Make_Selected_Component
                        (PN (P_ACC), CN (C_Repr_Cache)),
                        A_Unrestricted_Access))));
                  Append_Node_To_List (N, Statements);
               end;

            when K_Complex_Declarator =>
               declare
                  Element   : Node_Id;
                  Orig_Type : Node_Id;
                  T         : Node_Id;
                  Wrap_Node : Node_Id;
               begin
                  --  Setting the Mechanism

                  N := Make_Assignment_Statement
                    (Make_Explicit_Dereference (Make_Identifier (PN (P_Mech))),
                     RE (RE_By_Reference));
                  Append_Node_To_List (N, Statements);

                  --  Generated code depends on the nature of the
                  --  array (one dimension or multidimensional).

                  if FEU.Is_Multidimensional_Array (E) then
                     declare
                        If_Sts    : constant List_Id :=
                          New_List (K_Statement_List);
                        Else_Sts  : constant List_Id :=
                          New_List (K_Statement_List);
                        Dimension : constant Unsigned_Long_Long :=
                          Unsigned_Long_Long
                          (FEU.Length (FEN.Array_Sizes (E)));
                        Condition : Node_Id;
                        Dcl_Part  : constant List_Id :=
                          New_List (K_Declaration_List);
                        Sts_Part  : constant List_Id :=
                          New_List (K_Statement_List);
                        Count     : Unsigned_Long_Long;
                        Elements  : constant List_Id := New_List (K_List_Id);
                     begin
                        --  Build the IF statement condition

                        Condition := Make_Expression
                          (Make_Selected_Component (PN (P_ACC), CN (C_Dimen)),
                           Op_Less,
                           Make_Literal (New_Integer_Value
                                         (Dimension, 1, 10)));

                        --  The IF statement block

                        --  Declarative part

                        N := Make_Object_Declaration
                          (Defining_Identifier => Make_Defining_Identifier
                             (PN (P_R_ACC)),
                           Object_Definition   => Expand_Designator
                             (Aggr_Container_Node (BE_Node (Identifier (E)))),
                           Expression          => Make_Explicit_Dereference
                             (Make_Identifier (PN (P_ACC))));
                        Append_Node_To_List (N, Dcl_Part);

                        --  Statements

                        N := Make_Selected_Component
                          (PN (P_R_ACC), CN (C_Dimen));
                        N := Make_Subprogram_Call
                          (Make_Selected_Component
                           (PN (P_R_ACC), CN (C_Indices)),
                           Make_List_Id (N));
                        N := Make_Assignment_Statement
                          (N, Make_Type_Conversion
                           (RE (RE_Integer),
                            Make_Identifier (PN (P_Index))));
                        Append_Node_To_List (N, Sts_Part);

                        N := Make_Selected_Component
                          (PN (P_R_ACC), CN (C_Dimen));
                        N := Make_Expression
                          (N, Op_Plus, Make_Literal (Int1_Val));
                        N := Make_Assignment_Statement
                          (Make_Selected_Component
                           (PN (P_R_ACC), CN (C_Dimen)), N);
                        Append_Node_To_List (N, Sts_Part);

                        N := Make_Return_Statement
                          (Make_Identifier (PN (P_R_ACC)));
                        Append_Node_To_List (N, Sts_Part);

                        --  Building the block statement and appending
                        --  it to the IF statements.

                        N := Make_Block_Statement
                          (Declarative_Part => Dcl_Part,
                           Statements       => Sts_Part);
                        Append_Node_To_List (N, If_Sts);

                        --  ELSE statement

                        --  We loop through the dimensions to get the
                        --  element.

                        Count := 1;

                        while Count < Dimension loop
                           N := Make_Selected_Component
                             (PN (P_ACC), CN (C_Indices));
                           N := Make_Subprogram_Call
                             (N,
                              Make_List_Id
                              (Make_Literal
                               (New_Integer_Value (Count, 1, 10))));
                           Append_Node_To_List (N, Elements);

                           Count := Count + 1;
                        end loop;

                        --  Add the last selector to the list

                        N := Make_Type_Conversion
                          (RE (RE_Integer), Make_Identifier (PN (P_Index)));
                        Append_Node_To_List (N, Elements);

                        --  Selecting the array element

                        N := Make_Subprogram_Call
                          (Make_Selected_Component
                           (PN (P_ACC), CN (C_V)),
                           Elements);

                        --  Get the original type of the array element

                        T := Type_Spec (Declaration (E));
                        Orig_Type := FEU.Get_Original_Type_Declarator (T);

                        --  Get the Wrap node of the original element
                        --  type.

                        Wrap_Node := Get_Wrap_Node (Orig_Type);

                        --  Cast the array element when necessary

                        Cast_When_Necessary (N, T, Orig_Type, True);

                        --  Call the Wrap of the array element

                        N := Make_Subprogram_Call
                          (Wrap_Node,
                           Make_List_Id
                           (Make_Attribute_Reference
                            (N, A_Unrestricted_Access)));
                        N := Make_Return_Statement (N);
                        Append_Node_To_List (N, Else_Sts);

                        --  Build the IF statement

                        N := Make_If_Statement
                          (Condition, If_Sts, No_List, Else_Sts);
                        Append_Node_To_List (N, Statements);
                     end;

                  else
                     --  Accessing the wanted element

                     Element := Make_Subprogram_Call
                       (Make_Selected_Component (PN (P_ACC), CN (C_V)),
                        Make_List_Id
                        (Make_Type_Conversion (RE (RE_Integer),
                                               Make_Identifier
                                               (PN (P_Index)))));

                     --  Get the original type of the array element

                     T := Type_Spec (Declaration (E));
                     Orig_Type := FEU.Get_Original_Type_Declarator (T);

                     --  Get the Wrap node of the original element
                     --  type.

                     Wrap_Node := Get_Wrap_Node (Orig_Type);

                     --  Cast the array element when necessary

                     Cast_When_Necessary (Element, T, Orig_Type, True);

                     --  Return value

                     N := Make_Return_Statement
                       (Make_Subprogram_Call
                        (Wrap_Node,
                         Make_List_Id
                         (Make_Attribute_Reference
                          (Element,
                           A_Unrestricted_Access))));
                     Append_Node_To_List (N, Statements);
                  end if;
               end;

            when K_Union_Type =>
               declare
                  If_Sts              : constant List_Id :=
                    New_List (K_Statement_List);
                  Else_Sts            : constant List_Id :=
                    New_List (K_Statement_List);
                  Condition           : Node_Id;
                  Switch_Item         : Node_Id;
                  Component_Node      : Node_Id;
                  Switch_Alternative  : Node_Id;
                  Switch_Alternatives : List_Id;
                  Variant             : Node_Id;
                  Choice              : Node_Id;
                  Choices             : List_Id;
                  Label               : Node_Id;
                  Wrap_Node           : Node_Id;
                  Literal_Parent      : Node_Id := No_Node;
                  Orig_Type           : constant Node_Id :=
                    FEU.Get_Original_Type_Specifier (Switch_Type_Spec (E));
                  O                   : Node_Id;
                  T                   : Node_Id;
               begin
                  --  Index = 0: discriminant

                  Condition := Make_Expression
                    (Make_Identifier (PN (P_Index)),
                     Op_Equal,
                     Make_Literal (Int0_Val));

                  --  Setting the Mechanism.
                  --  Discriminant must be managed by value, because changing
                  --  the discriminant value requires a complete record
                  --  aggregate assignment. We provide a distinct component as
                  --  we do not want the current discriminant to be altered
                  --  in place.

                  N := Make_Assignment_Statement
                    (Make_Explicit_Dereference (Make_Identifier (PN (P_Mech))),
                     RE (RE_By_Value));
                  Append_Node_To_List (N, If_Sts);

                  --  Switch cache value

                  N := Make_Selected_Component
                    (Make_Selected_Component (PN (P_ACC), CN (C_V)),
                     Make_Identifier (CN (C_Switch)));
                  N := Make_Assignment_Statement
                    (Make_Selected_Component
                     (PN (P_ACC), CN (C_Switch_Cache)),
                     N);
                  Append_Node_To_List (N, If_Sts);

                  --  Get the Original type of the union switch

                  O := FEU.Get_Original_Type_Declarator (Switch_Type_Spec (E));

                  --  Get the Wrap fonction corresponding to the
                  --  switch original type.

                  Wrap_Node := Get_Wrap_Node (O);

                  N := Make_Selected_Component
                    (PN (P_ACC), CN (C_Switch_Cache));

                  --  Cast N if the switch type is an alias type

                  Cast_When_Necessary (N, Switch_Type_Spec (E), O, True);

                  --  Return statement

                  N := Make_Subprogram_Call
                    (Wrap_Node,
                     Make_List_Id
                     (Make_Attribute_Reference
                      (N, A_Unrestricted_Access)));
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, If_Sts);

                  --  Index = 1: union member

                  --  The Assert Pragma

                  N := Make_Expression
                    (Make_Identifier (PN (P_Index)),
                     Op_Equal,
                     Make_Literal (Int1_Val));
                  N := Make_Pragma (Pragma_Assert, Make_List_Id (N));
                  Append_Node_To_List (N, Else_Sts);

                  --  Setting the Mechanism

                  N := Make_Assignment_Statement
                    (Make_Explicit_Dereference (Make_Identifier (PN (P_Mech))),
                     RE (RE_By_Reference));
                  Append_Node_To_List (N, Else_Sts);

                  --  For each component of the union, we call its
                  --  corresponding wrap function.

                  N := Make_Selected_Component (PN (P_ACC), CN (C_V));
                  Switch_Item := Make_Selected_Component
                    (N,
                     Make_Identifier (CN (C_Switch)));

                  if FEN.Kind (Orig_Type) = K_Enumeration_Type then
                     Literal_Parent := Map_Expanded_Name
                       (Scope_Entity
                        (Identifier
                         (Orig_Type)));
                  end if;

                  Switch_Alternatives := New_List (K_Variant_List);
                  Switch_Alternative := First_Entity (Switch_Type_Body (E));

                  while Present (Switch_Alternative) loop
                     Variant := New_Node (K_Variant);
                     Choices := New_List (K_Discrete_Choice_List);
                     Set_Discrete_Choices (Variant, Choices);
                     Label   := First_Entity (Labels (Switch_Alternative));

                     while Present (Label) loop
                        Choice := Make_Literal
                          (Value  => FEN.Value (Label),
                           Parent => Literal_Parent);
                        Append_Node_To_List (Choice, Choices);

                        Label := Next_Entity (Label);
                     end loop;

                     --  Get the type spec of the element

                     T := Type_Spec (Element (Switch_Alternative));

                     --  Get the original type declarator of T

                     O := FEU.Get_Original_Type_Declarator (T);

                     --  Get the Wrap fonction corresponding to the
                     --  component original type.

                     Wrap_Node := Get_Wrap_Node (O);

                     --  Get the full name of the component
                     N := Make_Selected_Component (PN (P_ACC), CN (C_V));

                     Component_Node := Make_Selected_Component
                       (N,
                        Make_Identifier
                        (To_Ada_Name
                         (FEN.IDL_Name
                          (Identifier
                           (Declarator
                            (Element
                             (Switch_Alternative)))))));

                     --  Cast the component when necessary

                     Cast_When_Necessary (Component_Node, T, O, True);

                     --  Call the Wrap function

                     N := Make_Attribute_Reference
                       (Component_Node, A_Unrestricted_Access);

                     N := Make_Subprogram_Call (Wrap_Node, Make_List_Id (N));

                     --  Return the result

                     N := Make_Return_Statement (N);

                     --  Build the variant

                     Set_Component (Variant, N);
                     Append_Node_To_List (Variant, Switch_Alternatives);

                     Switch_Alternative := Next_Entity (Switch_Alternative);
                  end loop;

                  --  Build the switch case

                  N := Make_Variant_Part (Switch_Item, Switch_Alternatives);
                  Append_Node_To_List (N, Else_Sts);

                  --  Build the IF statement

                  N := Make_If_Statement
                    (Condition, If_Sts, No_List, Else_Sts);
                  Append_Node_To_List (N, Statements);
               end;

            when K_Structure_Type =>
               declare
                  Component_Node      : Node_Id;
                  Switch_Alternatives : List_Id;
                  Switch_Item         : Node_Id;
                  Variant             : Node_Id;
                  Choices             : List_Id;
                  Member              : Node_Id;
                  Declarator          : Node_Id;
                  Wrap_Node           : Node_Id;
                  Count               : Unsigned_Long_Long := 0;
                  Orig_Type           : Node_Id;
                  T                   : Node_Id;
               begin
                  --  Setting the Mechanism

                  N := Make_Assignment_Statement
                    (Make_Explicit_Dereference (Make_Identifier (PN (P_Mech))),
                     RE (RE_By_Reference));
                  Append_Node_To_List (N, Statements);

                  Switch_Item := Make_Identifier (PN (P_Index));

                  --  For each element, we build a case alternative
                  --  that calls the wrap function of the element.

                  Switch_Alternatives := New_List (K_Variant_List);

                  Member := First_Entity (Members (E));

                  while Present (Member) loop
                     Declarator := First_Entity (Declarators (Member));

                     while Present (Declarator) loop
                        --  Create the variant

                        Variant := New_Node (K_Variant);

                        --  Create the unique switch choice

                        Choices := Make_List_Id
                          (Make_Literal (New_Integer_Value (Count, 1, 10)));
                        Set_Discrete_Choices (Variant, Choices);

                        --  Get the Wrap fonction corresponding to the
                        --  component type.

                        T := Type_Spec (Declaration (Declarator));

                        --  Get the original declarator of T

                        Orig_Type := FEU.Get_Original_Type_Declarator (T);

                        Wrap_Node := Get_Wrap_Node (Orig_Type);

                        --  Get the full name of the component

                        N := Make_Selected_Component (PN (P_ACC), CN (C_V));
                        Component_Node := Make_Selected_Component
                          (N,
                           Make_Identifier
                           (To_Ada_Name
                            (FEN.IDL_Name
                             (Identifier
                              (Declarator)))));

                        --  Cast the component node when necessary

                        Cast_When_Necessary
                          (Component_Node, T, Orig_Type, True);

                        --  Call the Wrap function

                        N := Make_Attribute_Reference
                          (Component_Node, A_Unrestricted_Access);
                        N := Make_Subprogram_Call
                          (Wrap_Node, Make_List_Id (N));

                        --  Return the result

                        N := Make_Return_Statement (N);

                        --  Build the variant

                        Set_Component (Variant, N);
                        Append_Node_To_List (Variant, Switch_Alternatives);

                        --  Update the counter of the structure fields

                        Count := Count + 1;

                        Declarator := Next_Entity (Declarator);
                     end loop;

                     Member := Next_Entity (Member);
                  end loop;

                  --  Add a 'others' clause to the alternatives

                  --  Create the variant

                  Variant := New_Node (K_Variant);

                  --  Create an empty switch choice

                  Choices := Make_List_Id (Make_Literal (No_Value));
                  Set_Discrete_Choices (Variant, Choices);

                  --  Build the exception raising

                  N := Make_Raise_Statement
                    (Make_Identifier
                     (EN (E_Constraint_Error)));

                  --  Build the variant

                  Set_Component (Variant, N);
                  Append_Node_To_List (Variant, Switch_Alternatives);

                  --  Build the switch case

                  N := Make_Variant_Part (Switch_Item, Switch_Alternatives);
                  Append_Node_To_List (N, Statements);
               end;

            when others =>
               declare
                  Msg : constant String :=
                    "Cannot generate Get_aggregate_Element for a "
                    & FEN.Node_Kind'Image (FEN.Kind (E));
               begin
                  raise Program_Error with Msg;
               end;
         end case;

         --  Adding a pragma Unreferenced statement (if necessary)

         if not Is_Empty (Unref_Params) then
            N := Make_Pragma (Pragma_Unreferenced, Unref_Params);
            Append_Node_To_List (N, Dcl_Part);
         end if;

         N := Make_Subprogram_Body (Spec, Dcl_Part, Statements);
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
         --  Add a use clause for PolyORB.Types.Unsigned_Long

         N := Make_Used_Type (RE (RE_Unsigned_Long_1));
         Append_Node_To_List (N, Dcl_Part);

         --  Generate the rest of the code depending on the node kind.

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               declare
                  Left_Oprand   : Node_Id;
                  Right_Operand : Node_Id;
               begin
                  --  Declarative part

                  --  Add the unreferenced entities

                  Append_Node_To_List (Make_Identifier (PN (P_TC)),
                                       Unref_Params);

                  --  Add a pragma assertion on the Index parameter

                  N := Make_Expression
                    (Make_Identifier (PN (P_Index)),
                     Op_Equal,
                     Make_Literal (Int0_Val));
                  N := Make_Pragma (Pragma_Assert, Make_List_Id (N));
                  Append_Node_To_List (N, Dcl_Part);

                  --  Statements

                  --  Left operand of the assignment statement

                  Left_Oprand := Make_Explicit_Dereference
                    (Make_Selected_Component (PN (P_ACC), CN (C_V)));

                  --  Right operand of the assignment statement

                  Right_Operand := Make_Subprogram_Call
                    (RE (RE_From_Any_3),
                     Make_List_Id (Make_Identifier (PN (P_From_C))));
                  Right_Operand := Make_Qualified_Expression
                    (RE (RE_Unsigned_Long_1), Right_Operand);
                  N := Make_Attribute_Reference
                    (Get_Type_Definition_Node (E), A_Val);
                  Right_Operand := Make_Subprogram_Call
                    (N, Make_List_Id (Right_Operand));

                  --  The assignment_statement

                  N := Make_Assignment_Statement (Left_Oprand, Right_Operand);
                  Append_Node_To_List (N, Statements);
               end;

            when K_Union_Type =>
               declare
                  T : constant Node_Id := Switch_Type_Spec (E);
                  O : constant Node_Id := FEU.Get_Original_Type_Specifier (T);
               begin
                  --  Declarative part

                  --  Add the unreferenced entities

                  Append_Node_To_List (Make_Identifier (PN (P_TC)),
                                       Unref_Params);

                  --  Add a pragma assertion on the Index parameter

                  N := Make_Expression
                    (Make_Identifier (PN (P_Index)),
                     Op_Equal,
                     Make_Literal (Int0_Val));
                  N := Make_Pragma (Pragma_Assert, Make_List_Id (N));
                  Append_Node_To_List (N, Dcl_Part);

                  --  Declare the New_Switch constant

                  --  1 - Get the expression

                  N := Make_Subprogram_Call
                    (Get_From_Any_Container_Node (O),
                     Make_List_Id (Make_Identifier (PN (P_From_C))));

                  --  2 - Cast the expression when necessary

                  if FEN.Kind (T) = K_Scoped_Name
                    and then FEN.Kind (Reference (T)) = K_Simple_Declarator
                  then
                     N := Make_Qualified_Expression
                       (Get_Type_Definition_Node (O), N);
                     N := Make_Type_Conversion
                       (Get_Type_Definition_Node (T), N);
                  end if;

                  --  3 - Declare...

                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (PN (P_New_Switch)),
                     Constant_Present    => True,
                     Object_Definition   => Get_Type_Definition_Node (T),
                     Expression          => N);
                  Append_Node_To_List (N, Dcl_Part);

                  --  Declare the New_Union variable

                  N := Make_Component_Association
                    (Make_Identifier (CN (C_Switch)),
                     Make_Identifier (PN (P_New_Switch)));
                  N := Make_Type_Conversion (Get_Type_Definition_Node (E), N);
                  N := Make_Object_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                       (PN (P_New_Union)),
                     Object_Definition   => N);
                  Append_Node_To_List (N, Dcl_Part);

                  --  Disable warning on New_Union non-initialization

                  Set_Str_To_Name_Buffer ("Use default initialization");
                  N := Make_Ada_Comment (Name_Find);
                  Append_Node_To_List (N, Dcl_Part);

                  N := Make_Pragma
                    (Pragma_Warnings,
                     Make_List_Id (RE (RE_Off),
                                   Make_Identifier (PN (P_New_Union))));
                  Append_Node_To_List (N, Dcl_Part);

                  --  Disable discriminent check

                  N := Make_Pragma
                    (Pragma_Suppress,
                     Make_List_Id (RE (RE_Discriminant_Check)));
                  Append_Node_To_List (N, Dcl_Part);

                  --  Statements

                  --  The assignment statement

                  N := Make_Assignment_Statement
                    (Make_Explicit_Dereference
                       (Make_Selected_Component
                          (PN (P_ACC), CN (C_V))),
                     Make_Identifier (PN (P_New_Union)));
                  Append_Node_To_List (N, Statements);
               end;

            when others =>
               --  FIXME: To be removed once all types are implemented

               Append_Node_To_List (Make_Identifier (PN (P_ACC)),
                                    Unref_Params);
               Append_Node_To_List (Make_Identifier (PN (P_TC)),
                                    Unref_Params);
               Append_Node_To_List (Make_Identifier (PN (P_Index)),
                                    Unref_Params);
               Append_Node_To_List (Make_Identifier (PN (P_From_C)),
                                    Unref_Params);
         end case;

         --  Adding a pragma Unreferenced statement (if necessary)

         if not Is_Empty (Unref_Params) then
            N := Make_Pragma (Pragma_Unreferenced, Unref_Params);
            Append_Node_To_List (N, Dcl_Part);
         end if;

         N := Make_Subprogram_Body (Spec, Dcl_Part, Statements);
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
               --  corresponding element.

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
                       (Make_Identifier (Map_Lengths_Name (E)),
                        Make_List_Id (Make_Literal (Int1_Val)));
                  else
                     N := Make_Subprogram_Call
                       (Make_Identifier (Map_Lengths_Name (E)),
                        Make_List_Id
                        (Make_Selected_Component (PN (P_ACC), CN (C_Dimen))));
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

            when K_Sequence_Type
              | K_String_Type
              | K_Wide_String_Type =>
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
         if FEN.Kind (E) = K_Fixed_Point_Type
           or else FEN.Kind (E) = K_Sequence_Type
           or else FEN.Kind (E) = K_String_Type
           or else FEN.Kind (E) = K_Wide_String_Type
         then
            Spec := Initialize_Node (BE_Node (E));
         else
            Spec := Initialize_Node (BE_Node (Identifier (E)));
         end if;

         --  Declare the boolean flag global variable that indicates
         --  whether the TypeCode has been initialized or not. There
         --  is no harm this variable is global, because the
         --  initialization is done only once at the beginning of the
         --  application and it is not supposed to be done by more
         --  than one task.

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

         N := Make_Subprogram_Body
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
           (TC_Name   : Name_Id;
            Expr_Node : Node_Id;
            To_Any    : Node_Id := RE (RE_To_Any_0)) return Node_Id;
         --  Build a call to:
         --    Add_Parameter (TC_Name, To_Any (Expr_Node))
         --  If To_Any is not provided, it defaults to the (overloaded)
         --  CORBA.To_Any.

         function Declare_Name
           (Var_Name : Name_Id; Value : Value_Id) return Node_Id;
         --  Makes a variable declaration using the given parameters

         function TypeCode_Initialization return Node_Id;
         --  Initialization for the TypeCode variable
         --  declated in the Helper spec.

         -------------------
         -- Add_Parameter --
         -------------------

         function Add_Parameter
           (TC_Name   : Name_Id;
            Expr_Node : Node_Id;
            To_Any    : Node_Id := RE (RE_To_Any_0))
           return Node_Id
         is
            N : Node_Id;
         begin
            N := Make_Subprogram_Call (To_Any, Make_List_Id (Expr_Node));
            N := Make_Subprogram_Call
              (RE (RE_Add_Parameter),
               Make_List_Id (Make_Identifier (TC_Name), N));

            return N;
         end Add_Parameter;

         ------------------
         -- Declare_Name --
         ------------------

         function Declare_Name
           (Var_Name : Name_Id;
            Value    : Value_Id) return Node_Id
         is
            N : Node_Id;
         begin
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (Var_Name),
               Constant_Present    => True,
               Object_Definition   => RE (RE_String_0),
               Expression          => Make_Subprogram_Call
                 (RE (RE_To_CORBA_String),
                  Make_List_Id (Make_Literal (Value))));

            return N;
         end Declare_Name;

         -----------------------------
         -- TypeCode_Initialization --
         -----------------------------

         function TypeCode_Initialization return Node_Id is
            Expr : Node_Id;
         begin
            case FEN.Kind (E) is
               when K_Enumeration_Type =>
                  Expr := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Object),
                     Make_List_Id (RE (RE_TC_Enum)));

               when
                 K_Forward_Interface_Declaration |
                 K_Interface_Declaration         =>
                  Expr := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Object),
                     Make_List_Id (RE (RE_TC_Object_1)));

               when K_Fixed_Point_Type =>
                  Expr := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Object),
                     Make_List_Id (RE (RE_TC_Fixed)));

               when K_Complex_Declarator =>
                  Expr := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Object),
                     Make_List_Id (RE (RE_TC_Array)));

               when K_Structure_Type =>
                  Expr := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Object),
                     Make_List_Id (RE (RE_TC_Struct)));

               when K_Union_Type =>
                  Expr := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Object),
                     Make_List_Id (RE (RE_TC_Union)));

               when K_Exception_Declaration =>
                  Expr := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Object),
                     Make_List_Id (RE (RE_TC_Except)));

               when K_Simple_Declarator =>
                  Expr := Make_Subprogram_Call
                    (RE (RE_Build_Alias_TC),
                     Make_List_Id
                     (Make_Parameter_Association
                      (Make_Defining_Identifier (PN (P_Name)),
                       Make_Defining_Identifier (VN (V_Name))),
                      Make_Parameter_Association
                      (Make_Defining_Identifier (PN (P_Id)),
                       Make_Defining_Identifier (VN (V_Id))),
                      Make_Parameter_Association
                      (Make_Defining_Identifier (PN (P_Parent)),
                       Get_TC_Node (Type_Spec (Declaration (E))))));

               when K_Sequence_Type =>
                  declare
                     Max_Size_Literal : Node_Id;
                     TC_Element       : Node_Id;
                  begin
                     --  Unbounded sequences are identified by a maximum
                     --  length of 0.

                     if Present (Max_Size (E)) then
                        Max_Size_Literal := Make_Literal
                          (FEN.Value (Max_Size (E)));

                     else
                        Max_Size_Literal := Make_Literal
                          (New_Integer_Value (0, 1, 10));
                     end if;

                     TC_Element := Get_TC_Node (Type_Spec (E));

                     Expr := Make_Subprogram_Call
                       (RE (RE_Build_Sequence_TC),
                        Make_List_Id
                        (TC_Element,
                         Max_Size_Literal));
                  end;

               when K_String_Type | K_Wide_String_Type =>
                  declare
                     Build_Spg : Node_Id;
                  begin
                     if FEN.Kind (E) = K_String_Type then
                        Build_Spg := RE (RE_Build_String_TC);
                     else
                        Build_Spg := RE (RE_Build_Wstring_TC);
                     end if;

                     Expr := Make_Subprogram_Call
                       (Build_Spg,
                        Make_List_Id
                          (Make_Literal (FEN.Value (Max_Size (E)))));
                  end;

               when others =>
                  raise Program_Error with
                    "Cannot initialize TypeCode for frontend node "
                      & FEN.Node_Kind'Image (FEN.Kind (E));
            end case;

            return Make_Assignment_Statement
              (Get_TC_Node (T => E, Resolve_Forward => False),
               Expr);
         end TypeCode_Initialization;

         --  Local variables

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

         --  Start of processing for Initialize_Routine
      begin
         --  Initialize the TypeCode variable

         N := TypeCode_Initialization;
         Append_Node_To_List (N, Statements);

         --  Extract from polyorb-any.ads concerning the Encoding of
         --  TypeCodes:

         --  9.  For string and wide_string, the only parameter will
         --      be the length of the string. Its value will be 0 for
         --      unbounded strings or wide strings.

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
           and then FEN.Kind (E) /= K_String_Type
           and then FEN.Kind (E) /= K_Wide_String_Type
         then
            --  For the forward interfaces, we use the name and the
            --  Rep_Id of the forwarded interface. The Repository_Id
            --  is declared just after the type definition

            if FEN.Kind (E) = K_Forward_Interface_Declaration then
               Stub := Type_Def_Node (BE_Node (Identifier (Forward (E))));
            else
               Stub := Type_Def_Node (BE_Node (Identifier (E)));
            end if;

            Entity_Rep_Id_V := Get_Value (BEN.Expression (Next_Node (Stub)));
         end if;

         Entity_TC_Name := Get_Name
           (Get_Base_Identifier
            (Get_TC_Node
             (E, False)));

         case FEN.Kind (E) is
            when K_Interface_Declaration
              | K_Forward_Interface_Declaration =>
               Stub := Package_Declaration (BEN.Parent (Stub));

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
                     --  Multi-dimension array

                     --  First of all, we create a new list which
                     --  contains the elements of the list Sizes. All
                     --  manipulations on this list will not affect
                     --  the Sizes list because we create new nodes.

                     From_N := First_Node (Sizes);

                     while Present (From_N) loop
                        To_N := Make_Range_Constraint
                          (First (From_N), Last (From_N));
                        Append_Node_To_List
                          (To_N,
                           Sizes_Reverse);
                        From_N := Next_Node (From_N);
                     end loop;

                     --  The TC_Dimension_X variables used here are
                     --  the ones declared in the Helper spec

                     TC := TC_Node (BE_Node (Identifier (E)));
                     Constraint := Last_Node (Sizes_Reverse);

                     for Index in 1 .. Dimension - 1 loop
                        TC_Dim := Next_N_Node (TC, Dimension - Index);
                        TC_Previous_Name := TC_Name;
                        TC_Name := BEN.Name (BEN.Defining_Identifier (TC_Dim));

                        --  Initialize the TC_Dimension variable

                        N := Make_Assignment_Statement
                          (Make_Defining_Identifier (TC_Name),
                           Make_Subprogram_Call
                           (RE (RE_To_CORBA_Object),
                            Make_List_Id (RE (RE_TC_Array))));
                        Append_Node_To_List (N, Statements);

                        --  For multi-dimensional arrays, we fill each
                        --  TC_Dimention_X TypeCode with the
                        --  TC_Dimension_(X+1). The last TC_Dimension
                        --  is filled withe the array element TypeCode.

                        V := Values.Value (Get_Value (Last (Constraint)));
                        V.IVal := V.IVal + 1;
                        Param1 := Make_Type_Conversion
                          (RE (RE_Unsigned_Long),
                           (Make_Literal (New_Value (V))));

                        if TC_Previous_Name = No_Name then

                           --  The deepest dimension

                           T := Type_Spec (Declaration (E));
                           Param2 := Get_TC_Node (T);

                           Handle_Dependency (T, Statements);
                           Add_Dependency
                             (Get_Parent_Unit_Name (Param2),
                              Dependencies,
                              D_Helper);
                        else

                           --  Not the deepest dimension

                           Param2 := Make_Identifier (TC_Previous_Name);
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

                     V := Values.Value (Get_Value (Last (Constraint)));
                     V.IVal := V.IVal + 1;
                     Param1 := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id
                        (Make_Literal (New_Value (V))));
                     Param2 := Make_Identifier (TC_Name);

                  else
                     --  1 dimension array

                     V := Values.Value (Get_Value (Last (First_Node (Sizes))));
                     V.IVal := V.IVal + 1;
                     Param1 := Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        Make_List_Id
                        (Make_Literal (New_Value (V))));

                     T := Type_Spec (Declaration (E));
                     Param2 := Get_TC_Node (T);

                     Handle_Dependency (T, Statements);
                     Add_Dependency
                       (Get_Parent_Unit_Name (Param2),
                        Dependencies,
                        D_Helper);
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
                  TC_Sequence      : Node_Id;
                  TC_Element       : Node_Id;
                  Seq_Package      : Node_Id;
               begin
                  --  Disable the reference counting on the TypeCode
                  --  variable.

                  N := Make_Subprogram_Call
                    (RE (RE_Disable_Reference_Counting),
                     Make_List_Id (Get_TC_Node (E)));
                  Append_Node_To_List (N, Statements);

                  --  If the sequence does not contain local element,
                  --  initialize the instantiated package.

                  if not FEU.Has_Local_Component (E) then
                     Handle_Dependency (Type_Spec (E), Statements);

                     TC_Element := Get_TC_Node (Type_Spec (E));
                     TC_Sequence := Get_TC_Node (E);
                     Seq_Package := Make_Defining_Identifier
                       (Map_Sequence_Pkg_Helper_Name (E));

                     N := Make_Selected_Component
                       (Seq_Package,
                        Make_Defining_Identifier (SN (S_Initialize)));

                     N := Make_Subprogram_Call
                       (N,
                        Make_List_Id
                        (Make_Parameter_Association
                         (RE (RE_Element_TC), TC_Element),
                         Make_Parameter_Association
                         (RE (RE_Sequence_TC), TC_Sequence)));
                     Append_Node_To_List (N, Statements);
                  end if;
               end;

            when K_String_Type
              | K_Wide_String_Type
              | K_Simple_Declarator
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
           and then FEN.Kind (E) /= K_String_Type
           and then FEN.Kind (E) /= K_Wide_String_Type
         then
            Param1 := Make_Identifier (VN (V_Name));
            Param2 := Make_Identifier (VN (V_Id));

            --  Name_U declaration

            Entity_Name_V := New_String_Value
              (Get_Name (Get_Base_Identifier (Stub)), False);
            N := Declare_Name (VN (V_Name), Entity_Name_V);
            Append_Node_To_List (N, Declaration_List);

            --  Id_U declaration

            N := Declare_Name (VN (V_Id), Entity_Rep_Id_V);
            Append_Node_To_List (N, Declaration_List);
         end if;

         --  Add the two parameters

         if FEN.Kind (E) /= K_Sequence_Type
           and then FEN.Kind (E) /= K_String_Type
           and then FEN.Kind (E) /= K_Wide_String_Type
         then
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
                     Param1 := Make_Identifier (Var_Name);
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
                  Switch_To_Any       : Node_Id;
                  TC_Helper           : Node_Id;
                  Declarator          : Node_Id;
                  Designator          : Node_Id;
                  Arg_Name            : Name_Id;
                  Switch_Type         : Node_Id;
                  Literal_Parent      : Node_Id := No_Node;
                  Orig_Type           : constant Node_Id :=
                    FEU.Get_Original_Type_Specifier
                    (Switch_Type_Spec (E));
                  Statement_List     : constant List_Id :=
                    New_List (K_List_Id);
                  Default_Index       : Value_Id :=
                    New_Integer_Value (0, 1, 10); -- (0)
                  --  Index of the default case of the union. It is
                  --  initialized to 0 and incremented each time a
                  --  non-default case is met. It is set to -1 of no
                  --  default case exists in the union.

                  Default_Present     : Boolean;
                  T                   : Node_Id;
               begin
                  --  Getting the discriminator type and the To_Any
                  --  node corresponding to it

                  TC_Helper := Get_TC_Node (Switch_Type_Spec (E));

                  Handle_Dependency (Switch_Type_Spec (E), Statements);
                  Add_Dependency
                    (Get_Parent_Unit_Name (TC_Helper),
                     Dependencies,
                     D_Helper);

                  Switch_To_Any := Get_To_Any_Node (Switch_Type_Spec (E));

                  if Is_Base_Type (Switch_Type_Spec (E)) then
                     Switch_Type :=
                       RE (Convert (FEN.Kind (Switch_Type_Spec (E))));

                  elsif FEN.Kind (Orig_Type) = K_Enumeration_Type then
                     Switch_Type := Map_Expanded_Name (Switch_Type_Spec (E));
                     Literal_Parent := Map_Expanded_Name
                       (Scope_Entity
                        (Identifier
                         (Orig_Type)));
                  else
                     Switch_Type := Map_Expanded_Name (Switch_Type_Spec (E));
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
                  Default_Present := False;
                  while Present (Switch_Alternative) loop
                     Choices := New_List (K_List_Id);
                     Label   := First_Entity (Labels (Switch_Alternative));
                     while Present (Label) loop

                        Choice := Make_Literal
                          (Value  => FEN.Value (Label),
                           Parent => Literal_Parent);

                        --  If this is not a case statement, then we
                        --  increment the default case index. The
                        --  value of Default_Index will be correctly
                        --  set up after the end of the two loops

                        if Get_Value (Choice) /= No_Value then
                           Set_Value
                             (Default_Index,
                              Value (Default_Index) + Value (Int1_Val));
                        else
                           Default_Present := True;
                        end if;

                        Append_Node_To_List (Choice, Choices);
                        Label := Next_Entity (Label);
                     end loop;

                     --  Declaring the argument name "Element" string

                     Declarator := FEN.Declarator
                       (Element (Switch_Alternative));

                     --  Getting the TC_XXX constant corresponding to
                     --  the element type.

                     T := Type_Spec (Element (Switch_Alternative));
                     Handle_Dependency (T, Statements);
                     TC_Helper := Get_TC_Node (T);

                     Add_Dependency
                       (Get_Parent_Unit_Name (TC_Helper),
                        Dependencies,
                        D_Helper);

                     Designator := Map_Expanded_Name (Declarator);
                     Get_Name_String (VN (V_Argument_Name));
                     Add_Char_To_Name_Buffer ('_');
                     Get_Name_String_And_Append
                       (Get_Name (Get_Base_Identifier (Designator)));
                     Arg_Name := Name_Find;
                     N := Make_Literal
                       (New_String_Value
                        (Get_Name (Get_Base_Identifier (Designator)),
                         False));
                     N := Make_Subprogram_Call
                       (RE (RE_To_CORBA_String),
                        Make_List_Id (N));
                     N := Make_Object_Declaration
                       (Defining_Identifier =>
                          Make_Defining_Identifier (Arg_Name),
                        Constant_Present    => True,
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
                        if Get_Value (Choice) /= No_Value then

                           --  Copy Choice value to avoid adding the next nodes
                           --  of Choice to the argument list.

                           N := Copy_Node (Choice);

                           N := Make_Qualified_Expression
                             (Subtype_Mark => Switch_Type,
                              Operand      => N);

                           N := Add_Parameter
                                  (Entity_TC_Name, N, To_Any => Switch_To_Any);
                           Append_Node_To_List (N, Statement_List);

                           N := Add_Parameter (Entity_TC_Name, TC_Helper);
                           Append_Node_To_List (N, Statement_List);

                           N := Add_Parameter
                             (Entity_TC_Name,
                              Make_Defining_Identifier (Arg_Name));
                           Append_Node_To_List (N, Statement_List);

                        else
                           --  Case of a default alternative

                           N := Make_Attribute_Reference
                             (Switch_Type, A_First);

                           N := Add_Parameter
                                  (Entity_TC_Name, N, To_Any => Switch_To_Any);
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

                  if not Default_Present then
                     Default_Index := New_Integer_Value (1, -1, 10); --  (-1)
                  end if;

                  --  Fourth parameter: The index of the "default"
                  --  alternative as a CORBA.Long.

                  N := Make_Type_Conversion
                    (RE (RE_Long),
                     Make_Literal (Default_Index));
                  N := Add_Parameter (Entity_TC_Name, N);
                  Append_Node_To_List (N, Statements);

                  --  Append the Statement_List list to the end of the
                  --  Statements list (we only append the first node,
                  --  the others are appended automatically).

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
                        Designator := Map_Expanded_Name (Declarator);
                        Get_Name_String (VN (V_Argument_Name));
                        Add_Char_To_Name_Buffer ('_');
                        Get_Name_String_And_Append
                          (Get_Name (Get_Base_Identifier (Designator)));
                        Arg_Name := Name_Find;
                        N := Make_Literal
                          (New_String_Value
                           (Get_Name (Get_Base_Identifier (Designator)),
                            False));
                        N := Make_Subprogram_Call
                          (RE (RE_To_CORBA_String),
                           Make_List_Id (N));
                        N := Make_Object_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier (Arg_Name),
                           Constant_Present    => True,
                           Object_Definition   => RE (RE_String_0),
                           Expression          => N);
                        Append_Node_To_List (N, Declaration_List);

                        T := Type_Spec (Declaration (Declarator));
                        Handle_Dependency (T, Statements);
                        Param1 := Get_TC_Node (T);

                        Add_Dependency
                          (Get_Parent_Unit_Name (Param1),
                           Dependencies,
                           D_Helper);

                        Param2 := Make_Identifier (Arg_Name);
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
                  Declarator                 : Node_Id;
                  Dcl_Name                   : Name_Id;
                  Arg_Name_Node              : Node_Id;
                  Register_Excp_Node         : constant Node_Id :=
                    RE (RE_Register_Exception);
               begin
                  --  In case where the exception has members, we add two
                  --  parameters for each member.

                  Member := First_Entity (Members (E));

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

                        --  Get a string literal of the member name

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
                           Constant_Present    => True,
                           Object_Definition   => RE (RE_String_0),
                           Expression          => N);
                        Append_Node_To_List (N, Declaration_List);

                        --  Add the two additional parameters

                        N := Get_TC_Node (Type_Spec (Member));

                        Handle_Dependency (Type_Spec (Member), Statements);
                        Add_Dependency
                          (Get_Parent_Unit_Name (N),
                           Dependencies,
                           D_Helper);

                        N := Add_Parameter (Entity_TC_Name, N);
                        Append_Node_To_List (N, Statements);
                        N := Add_Parameter (Entity_TC_Name, Arg_Name_Node);
                        Append_Node_To_List (N, Statements);

                        Declarator := Next_Entity (Declarator);
                     end loop;

                     Member := Next_Entity (Member);
                  end loop;

                  --  Register the exception (in case of no local
                  --  interface members in the exception).

                  if not FEU.Has_Local_Component (E) then
                     --  Add a dependency to initialize correctly the
                     --  modules.

                     Add_Dependency
                       (Get_Parent_Unit_Name (Register_Excp_Node),
                        Dependencies,
                        D_Helper);

                     Raise_From_Any_Access_Node := Make_Identifier
                       (Map_Raise_From_Any_Name (E));
                     Raise_From_Any_Access_Node := Make_Attribute_Reference
                       (Raise_From_Any_Access_Node, A_Access);
                     N := Make_Subprogram_Call
                       (RE (RE_To_PolyORB_Object),
                        Make_List_Id
                        (Make_Identifier
                           (Entity_TC_Name)));

                     --  Register raiser

                     --  This has to be done in deferred initialization,
                     --  after the TypeCode has been constructed.

                     N := Make_Subprogram_Call
                       (Register_Excp_Node,
                        Make_List_Id
                        (N, Raise_From_Any_Access_Node));
                     Append_Node_To_List (N, Statements);
                  end if;
               end;

            when K_Simple_Declarator =>
               declare
                  T : Node_Id;
               begin
                  T := Type_Spec (Declaration (E));

                  --  Handle the dependancy between `Helper' packages

                  Handle_Dependency (T, Statements);

                  --  Get the TypeCode corresponding to the typespec
                  --  of the type declaration od the simple
                  --  declarator.

                  N := Get_TC_Node (T);

                  Add_Dependency
                    (Get_Parent_Unit_Name (N),
                     Dependencies,
                     D_Helper);

                  N := Add_Parameter (Entity_TC_Name, N);
                  Append_Node_To_List (N, Statements);
               end;

            when others =>
               null;
         end case;

         --  Disable reference counting on the TypeCode variable for
         --  types who are different from sequences. For sequences,
         --  this has been done earlier.

         if FEN.Kind (E) /= K_Sequence_Type then
            N := Make_Subprogram_Call
              (RE (RE_Disable_Reference_Counting),
               Make_List_Id (Get_TC_Node (E)));
            Append_Node_To_List (N, Statements);
         end if;
      end Initialize_Routine;

      -----------------------------
      -- From_Any_Container_Body --
      -----------------------------

      function From_Any_Container_Body (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Enumeration_Type);

         Spec : constant Node_Id :=
           From_Any_Container_Node (BE_Node (Identifier (E)));
         D    : constant List_Id := New_List (K_List_Id);
         S    : constant List_Id := New_List (K_List_Id);
         N    : Node_Id;
      begin
         N := Make_Subprogram_Call
           (RE (RE_Get_Aggregate_Element_2),
            Make_List_Id
            (Make_Identifier (PN (P_C)),
             Make_Literal (Int0_Val)));

         N := Make_Qualified_Expression (RE (RE_Unsigned_Long_1), N);

         N := Make_Subprogram_Call
           (Make_Attribute_Reference (Get_Type_Definition_Node (E), A_Val),
            Make_List_Id (N));
         N := Make_Return_Statement (N);
         Append_Node_To_List (N, S);

         --  Make the subprogram body

         N := Make_Subprogram_Body (Spec, D, S);

         return N;
      end From_Any_Container_Body;

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
         --  Declarations

         --  Get the node corresponding to the declaration of the
         --  "Excp_Name"_Members type.

         Excp_Members := Get_Type_Definition_Node (E);

         --  Prepare the call to From_Any

         N := Make_Type_Conversion
           (RE (RE_Any),
            Make_Defining_Identifier (PN (P_Item)));

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
            Object_Definition   => Excp_Members,
            Expression          => N);
         Append_Node_To_List (N, Declarations);

         --  Statements

         N := Make_Defining_Identifier
           (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         N := Make_Attribute_Reference (N, A_Identity);
         N := Make_Subprogram_Call
           (RE (RE_User_Raise_Exception),
            Make_List_Id
            (N,
             Make_Defining_Identifier (PN (P_Members)),
             Make_Defining_Identifier (PN (P_Message))));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Body (Spec, Declarations, Statements);

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

         --  The enumeration types are a special case. They require an
         --  additional `From_Any' function that converts containers
         --  to a value from the enumeration type.

         N := From_Any_Container_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

         --  The aggregate container routines

         Aggregate_Container_Routines (E);

         --  The Wrap function body

         N := Wrap_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

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
         if not Map_Particular_CORBA_Parts (E, PK_Helper_Internals_Body) then
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
      begin
         Set_Internals_Body;

         --  The aggregate container routines

         Aggregate_Container_Routines (E);

         --  The Wrap function body

         N := Wrap_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

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
               --  The Wrap function body

               N := Wrap_Body (T);
               Append_Node_To_List (N, Statements (Current_Package));

               --  The Initialize body

               N := Initialize_Body (T);
               Append_Node_To_List (N, Statements (Current_Package));

            when K_Sequence_Type =>
               if not FEU.Has_Local_Component (T) then
                  --  The element wrap function

                  N := Element_Wrap_Body (T);
                  Append_Node_To_List (N, Statements (Current_Package));

                  --  The Wrap function body

                  N := Wrap_Body (T);
                  Append_Node_To_List (N, Statements (Current_Package));
               end if;

               --  The Initialize body

               N := Initialize_Body (T);
               Append_Node_To_List (N, Statements (Current_Package));

            when K_String_Type | K_Wide_String_Type =>
               --  The Wrap function body

               N := Wrap_Body (T);
               Append_Node_To_List (N, Statements (Current_Package));

               --  The Initialize body

               N := Initialize_Body (T);
               Append_Node_To_List (N, Statements (Current_Package));

            when others =>
               null;
         end case;

         D := First_Entity (Declarators (E));
         while Present (D) loop
            if FEN.Kind (D) = K_Complex_Declarator then
               --  The aggregate container routines

               Aggregate_Container_Routines (D);
            end if;

            --  We do not generate the `Wrap' spec if the defined type is:
            --  1 - derived from an object type
            --  2 - a fixed point type
            --  3 - a bounded string type
            --  4 - a sequence type

            if not (((FEN.Kind (T) = K_Scoped_Name
                      or else FEN.Kind (T) = K_Object)
                     and then FEN.Kind (D) = K_Simple_Declarator)
                    or else FEN.Kind (T) = K_String_Type
                    or else FEN.Kind (T) = K_Wide_String_Type
                    or else FEN.Kind (T) = K_Fixed_Point_Type
                    or else FEN.Kind (T) = K_Sequence_Type)
            then
               --  The Wrap function body

               N := Wrap_Body (D);
               Append_Node_To_List (N, Statements (Current_Package));
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
      begin
         Set_Internals_Body;

         --  The aggregate container routines

         Aggregate_Container_Routines (E);

         --  The Wrap function body

         N := Wrap_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));

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

         --  Do not generate the raise exception from any in case the
         --  exception contains local interface members.

         if not FEU.Has_Local_Component (E) then
            --  Generation of the Raise_"Exception_Name"_From_Any spec

            Raise_Node := Make_Defining_Identifier
              (Map_Raise_From_Any_Name (E));
            N := Raise_Excp_From_Any_Spec (Raise_Node);
            Append_Node_To_List (N, Statements (Current_Package));

            --  Addition of the pragma No_Return. The argument of the
            --  pragma No_Return must be a local name

            N := Make_Pragma
              (Pragma_No_Return,
               Make_List_Id (Make_Identifier (BEN.Name (Raise_Node))));
            Append_Node_To_List (N, Statements (Current_Package));

            --  Generation of the Raise_"Exception_Name"_From_Any body

            N := Raise_Excp_From_Any_Body (E, Raise_Node);
            Append_Node_To_List (N, Statements (Current_Package));
         end if;

         --  The body of the Initialize routine

         N := Initialize_Body (E);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Exception_Declaration;

   end Package_Body;

end Backend.BE_CORBA_Ada.Helpers_Internals;
