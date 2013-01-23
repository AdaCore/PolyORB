------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         B A C K E N D . B E _ C O R B A _ A D A . H E L P E R S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Namet;    use Namet;
with Values;   use Values;

with Frontend.Nodes;   use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

package body Backend.BE_CORBA_Ada.Helpers is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package BEU renames Backend.BE_CORBA_Ada.Nutils;

   package body Package_Spec is

      function From_Any_Spec (E : Node_Id) return Node_Id;
      --  Return the `From_Any' function spec corresponding to the IDL
      --  node E.

      function To_Any_Spec (E : Node_Id) return Node_Id;
      --  Return the `To_Any' function spec corresponding to the IDL
      --  node E.

      function U_To_Ref_Spec (E : Node_Id) return Node_Id;
      --  Return the `Unchecked_To_Ref' function spec corresponding to
      --  the IDL node E.

      function To_Ref_Spec (E : Node_Id) return Node_Id;
      --  Return the `To_Ref' function spec corresponding to the IDL
      --  node E.

      function Raise_Excp_Spec
        (Excp_Members : Node_Id;
         Raise_Node   : Node_Id)
        return Node_Id;
      --  Return the spec of the Raise_"Exception_Name" procedure

      function TypeCode_Spec (E : Node_Id) return Node_Id;
      --  Return a TypeCode variable for a given type (E).

      function TypeCode_Dimension_Spec
        (Declarator : Node_Id;
         Dim        : Natural)
        return Node_Id;
      --  returns a TypeCode constant for a dimension of an array
      --  other than the last dimension

      function TypeCode_Dimension_Declarations
        (Declarator : Node_Id) return List_Id;
      --  Multidimensional arrays: when they are converted to the Any
      --  type, the multidimensional arrays are seen as nested
      --  arrays. So, for each dimension from the first until the
      --  before last dimension we declare a type code constant. This
      --  function returns a list of TC_Dimensions_'N' constant
      --  declarations

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

      function From_Any_Spec (E : Node_Id) return Node_Id is
         Profile   : constant List_Id := New_List;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)),
            RE (RE_Any));
         Append_To (Profile, Parameter);

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_From_Any)),
            Profile,
            Get_Type_Definition_Node (E));

         return N;
      end From_Any_Spec;

      -------------------
      -- U_To_Ref_Spec --
      -------------------

      function U_To_Ref_Spec (E : Node_Id) return Node_Id is
         Profile   : constant List_Id := New_List;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_The_Ref)),
            Make_Attribute_Reference
            (Map_Ref_Type_Ancestor (E),
             A_Class));
         Append_To (Profile, Parameter);

         N := Make_Subprogram_Specification
           (Map_Narrowing_Designator (E, True),
            Profile, Expand_Designator
            (Type_Def_Node (BE_Node (Identifier (E)))));

         return N;
      end U_To_Ref_Spec;

      -----------------
      -- To_Any_Spec --
      -----------------

      function To_Any_Spec (E : Node_Id) return Node_Id is
         Profile   : constant List_Id := New_List;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)),
            Get_Type_Definition_Node (E));
         Append_To (Profile, Parameter);

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Any)),
            Profile, RE (RE_Any));

         return N;
      end To_Any_Spec;

      -----------------
      -- To_Ref_Spec --
      -----------------

      function To_Ref_Spec (E : Node_Id) return Node_Id is
         Profile   : constant List_Id := New_List;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_The_Ref)),
            Make_Attribute_Reference
            (Map_Ref_Type_Ancestor (E),
             A_Class));
         Append_To (Profile, Parameter);

         N := Make_Subprogram_Specification
           (Map_Narrowing_Designator (E, False),
            Profile,
            Expand_Designator
            (Type_Def_Node (BE_Node (Identifier (E)))));

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
         Profile   : constant List_Id := New_List;
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Members)),
            Excp_Members);
         Append_To (Profile, Parameter);

         N := Make_Subprogram_Specification
           (Raise_Node,
            Profile);

         return N;
      end Raise_Excp_Spec;

      -------------------
      -- TypeCode_Spec --
      -------------------

      function TypeCode_Spec (E : Node_Id) return Node_Id is
         N  : Node_Id := E;
         TC : Name_Id;
      begin
         --  The name of the TypeCode variable is generally mapped
         --  from the name of the Ada type mapped from the IDL
         --  type. For forward interface, the name is mapped from the
         --  forwarded interface. For sequences and bounded strings,
         --  the name is mapped from the pacjahe instantiation name.

         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               N := Get_Type_Definition_Node (E);

            when K_Forward_Interface_Declaration =>
               N := Package_Declaration
                 (BEN.Parent
                  (Type_Def_Node
                   (BE_Node
                    (Identifier
                     (Forward
                      (E))))));

            when K_Fixed_Point_Type =>
               N := Get_Type_Definition_Node (E);

            when K_Interface_Declaration =>
               N := Package_Declaration
                 (BEN.Parent (Type_Def_Node (BE_Node (Identifier (E)))));

            when K_Sequence_Type
              | K_String_Type
              | K_Wide_String_Type =>
               N := Expand_Designator (Instantiation_Node (BE_Node (E)));

            when  K_Simple_Declarator =>
               N := Get_Type_Definition_Node (E);

            when K_Complex_Declarator =>
               N := Get_Type_Definition_Node (E);

            when K_Structure_Type =>
               N := Get_Type_Definition_Node (E);

            when K_Union_Type =>
               N := Get_Type_Definition_Node (E);

            when K_Exception_Declaration =>
               N := Make_Identifier (To_Ada_Name (IDL_Name (Identifier (E))));

            when others =>
               declare
                  Msg : constant String :=
                    "Cannot generate TypeCode for the frontend node "
                    & FEN.Node_Kind'Image (FEN.Kind (E));
               begin
                  raise Program_Error with Msg;
               end;
         end case;

         TC := Add_Prefix_To_Name ("TC_", Get_Name (Get_Base_Identifier (N)));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (TC),
            Object_Definition   => RE (RE_Object));

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
         --  The varible name is mapped as follows:
         --  TC_<Ada_Type_Name>_TC_Dimension_<Dimension_Index>

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

         --  Declare the variable

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (TC_Name),
            Object_Definition   => RE (RE_Object));

         return N;
      end TypeCode_Dimension_Spec;

      -------------------------------------
      -- TypeCode_Dimension_Declarations --
      -------------------------------------

      function TypeCode_Dimension_Declarations
        (Declarator : Node_Id) return List_Id
      is
         pragma Assert (FEN.Kind (Declarator) = K_Complex_Declarator);

         Dim : constant Natural := FEU.Length (FEN.Array_Sizes (Declarator));
         L   : List_Id;
         N   : Node_Id;
      begin
         pragma Assert (Dim > 1);

         L := New_List;

         for I in 1 .. Dim - 1 loop
            N := TypeCode_Dimension_Spec (Declarator, I);
            Append_To (L, N);
         end loop;

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
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         N := From_Any_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_From_Any);

         N := To_Any_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_To_Any);
      end Visit_Enumeration_Type;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         N        : Node_Id;
         Is_Local : constant Boolean := Is_Local_Interface (E);
      begin
         Set_Helper_Spec;

         N := TypeCode_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Local interfaces don't have Any conversion methods
         --  because local references cannot be transferred through
         --  the network.

         if not Is_Local then
            N := From_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;

         N := U_To_Ref_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_U_To_Ref);

         N := To_Ref_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_To_Ref);
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
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Local interfaces don't have Any conversion methods
         --  because local references cannot be transferred through
         --  the network.

         if not Is_Local then
            N := From_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;

         N := U_To_Ref_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_U_To_Ref);

         N := To_Ref_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_To_Ref);

         --  Visit the entities declared inside the interface.

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritance, generate the mappings
         --  for the operations and attributes of the parents other
         --  than the first one.

         Map_Inherited_Entities_Specs
           (Current_Interface    => E,
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
      begin
         Set_Helper_Spec;

         N := TypeCode_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component because local references cannot be transferred
         --  through the network.

         if not FEU.Has_Local_Component (E) then
            N := From_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         L       : List_Id;
         D       : Node_Id;
         N       : Node_Id;
         T       : Node_Id;
      begin
         Set_Helper_Spec;
         L := Declarators (E);
         T := Type_Spec (E);
         D := First_Entity (L);

         --  Handling the case of fixed point type definitions,
         --  sequence type defitions and bounded [wide] string type
         --  definitions. We create Any conversion routines for the
         --  extra entities declared in the stub spec.

         if FEN.Kind (T) = K_Fixed_Point_Type
           or else FEN.Kind (T) = K_Sequence_Type
           or else FEN.Kind (T) = K_String_Type
           or else FEN.Kind (T) = K_Wide_String_Type
         then
            begin
               N := TypeCode_Spec (T);
               Bind_FE_To_BE (T, N, B_TC);
               Append_To (Visible_Part (Current_Package), N);

               --  Do not generate the Any converters in case one of
               --  the component is a local interface or has a local
               --  interface component because local references cannot
               --  be transferred through the network.

               if not FEU.Has_Local_Component (T) then
                  N := From_Any_Spec (T);
                  Bind_FE_To_BE (T, N, B_From_Any);
                  Append_To (Visible_Part (Current_Package), N);

                  N := To_Any_Spec (T);
                  Bind_FE_To_BE (T, N, B_To_Any);
                  Append_To (Visible_Part (Current_Package), N);
               end if;
            end;
         end if;

         --  Handling the Ada type mapped from the IDL type (general
         --  case).

         while Present (D) loop
            N := TypeCode_Spec (D);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (D), N, B_TC);

            --  Multi-dimensional array types need extra TypeCode variables
            --  for each dimension, which are used for the initialization of
            --  the outermost array typecode.

            if FEN.Kind (D) = K_Complex_Declarator
               and then FEU.Length (FEN.Array_Sizes (D)) > 1
            then
               Append_To (Visible_Part (Current_Package),
                 First_Node (TypeCode_Dimension_Declarations (D)));
            end if;

            --  If the new type is defined basing on an interface type
            --  (through Ada SUBtyping), and then if this is not an
            --  array type, then we don't generate From_Any nor
            --  To_Any. We use those of the original type. Otherwise,
            --  the conversion routines whould have exactly the same
            --  signature and name clashing would occur in the helper
            --  name space.

            if Is_Object_Type (T)
              and then FEN.Kind (D) = K_Simple_Declarator
            then
               --  For local interface, we generate nothing because
               --  local references cannot be transferred through
               --  network.

               if not FEU.Has_Local_Component (T) then
                  N := Get_From_Any_Node (T, False);
                  Bind_FE_To_BE (Identifier (D), N, B_From_Any);

                  N := Get_To_Any_Node (T, False);
                  Bind_FE_To_BE (Identifier (D), N, B_To_Any);
               end if;
            else
               --  Do not generate the Any converters in case one of
               --  the component is a local interface or has a local
               --  interface component because local references cannot
               --  be transferred through network.

               if not FEU.Has_Local_Component (T) then
                  N := From_Any_Spec (D);
                  Append_To (Visible_Part (Current_Package), N);
                  Bind_FE_To_BE (Identifier (D), N, B_From_Any);

                  N := To_Any_Spec (D);
                  Append_To (Visible_Part (Current_Package), N);
                  Bind_FE_To_BE (Identifier (D), N, B_To_Any);
               end if;
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
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Get the node corresponding to the declaration of the
         --  "Excp_Name"_Members type.

         Excp_Members := Get_Type_Definition_Node (E);

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component because local refernces cannot be transferred
         --  through network..

         if not FEU.Has_Local_Component (E) then
            N := From_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;

         --  Generation of the Raise_"Exception_Name" spec

         Excp_Name := To_Ada_Name (IDL_Name (FEN.Identifier (E)));
         Raise_Node := Make_Defining_Identifier
           (Add_Prefix_To_Name ("Raise_", Excp_Name));
         N := Raise_Excp_Spec (Excp_Members, Raise_Node);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_Raise_Excp);

         --  A call to Raise_<Exception_Name> does not return

         N := Make_Pragma
           (Pragma_No_Return,
            New_List (Make_Identifier (BEN.Name (Raise_Node))));
         Append_To (Visible_Part (Current_Package), N);
      end Visit_Exception_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N            : Node_Id;
      begin
         Set_Helper_Spec;

         N := TypeCode_Spec (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component because local refernces cannot be transferred
         --  through network..

         if not FEU.Has_Local_Component (E) then
            N := From_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_To (Visible_Part (Current_Package), N);
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;
      end Visit_Union_Type;

   end Package_Spec;

   package body Package_Body is

      function Deferred_Initialization_Block (E : Node_Id) return Node_Id;
      --  Returns the Initialize routine corresponding to the IDL
      --  entity E.

      function Declare_Any_Array
        (A_Name  : Name_Id;
         A_First : Natural;
         A_Last  : Natural)
        return Node_Id;
      --  Declare an `Any' array declaration

      procedure Helper_Initialization (L : List_Id);
      --  Create the initialization block for the Helper package

      function Nth_Element (A_Name : Name_Id; Nth : Nat) return Node_Id;
      --  Create an Ada construct to get the Nth element of array
      --  `A_Name'.

      function From_Any_Body (E : Node_Id) return Node_Id;
      --  Create the body of the `From_Any' function relative to the
      --  IDL entity E.

      function To_Any_Body (E : Node_Id) return Node_Id;
      --  Create the body of the `To_Any' function relative to the IDL
      --  entity E.

      function U_To_Ref_Body (E : Node_Id) return Node_Id;
      --  Create the body of the `Unckecked_To_Ref' function relative
      --  to the IDL entity E.

      function To_Ref_Body (E : Node_Id) return Node_Id;
      --  Create the body of the `To_Ref' function relative to the IDL
      --  entity E.

      function Raise_Excp_Body (E : Node_Id) return Node_Id;
      --  Create the body of the `Raise_Exception' function relative
      --  to the IDL entity E.

      procedure Visit_Enumeration_Type (E : Node_Id);
      procedure Visit_Forward_Interface_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Exception_Declaration (E : Node_Id);

      -----------------------------------
      -- Deferred_Initialization_Block --
      -----------------------------------

      function Deferred_Initialization_Block (E : Node_Id) return Node_Id is
         Frontend_Node : Node_Id := E;
         N             : Node_Id;
      begin
         if FEN.Kind (E) /= K_Fixed_Point_Type
           and then FEN.Kind (E) /= K_Sequence_Type
           and then FEN.Kind (E) /= K_String_Type
           and then FEN.Kind (E) /= K_Wide_String_Type
         then
            Frontend_Node := Identifier (Frontend_Node);
         end if;

         --  Call the Initialize routine generated in the nested Internals
         --  package.

         N := Expand_Designator (Initialize_Node (BE_Node (Frontend_Node)));
         return N;
      end Deferred_Initialization_Block;

      -----------------------
      -- Declare_Any_Array --
      -----------------------

      function Declare_Any_Array
        (A_Name  : Name_Id;
         A_First : Natural;
         A_Last  : Natural)
        return Node_Id
      is
         N     : Node_Id;
         R     : Node_Id;
         L     : List_Id;
         First : Value_Id;
         Last  : Value_Id;
      begin
         First := New_Integer_Value
           (Unsigned_Long_Long (A_First), 1, 10);
         Last  := New_Integer_Value
           (Unsigned_Long_Long (A_Last), 1, 10);

         R := Make_Range_Constraint
           (Make_Literal (First), Make_Literal (Last));

         L := New_List;
         Append_To (L, R);

         N := Make_Object_Declaration
                (Defining_Identifier => Make_Defining_Identifier (A_Name),
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

         N := Make_Indexed_Component
           (Make_Defining_Identifier (A_Name),
            New_List (Make_Literal (Nth_Value)));
         return N;
      end Nth_Element;

      -------------------
      -- From_Any_Body --
      -------------------

      function From_Any_Body (E : Node_Id) return Node_Id is
         N    : Node_Id;
         M    : Node_Id;
         Spec : Node_Id;
         D    : constant List_Id := New_List;
         S    : constant List_Id := New_List;

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
                                       (Type_Definition
                                        (Type_Def_Node
                                         (BE_Node (Identifier (E)))));
            Dimension            : constant Natural := BEU.Length (Sizes);
            Dim                  : Node_Id;
            Loop_Statements      : List_Id := No_List;
            Enclosing_Statements : List_Id;
            Index_List           : constant List_Id := New_List;
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
                 Copy_Expanded_Name (Return_Type (Spec)));
            Append_To (D, N);

            N := Declare_Any_Array (PN (P_Aux), 0, Dimension - 1);
            Append_To (D, N);

            Dim := First_Node (Sizes);
            TC := TC_Node (BE_Node (Identifier (E)));
            loop
               Set_Str_To_Name_Buffer ("I");
               Add_Nat_To_Name_Buffer (I);
               Prev_Index_Node := Index_Node;
               Index_Node := Make_Defining_Identifier
                 (Add_Suffix_To_Name (Unique_Suffix, Name_Find));
               Append_To (Index_List, Index_Node);
               Enclosing_Statements := Loop_Statements;
               Loop_Statements := New_List;
               N := Make_For_Statement
                 (Index_Node, Dim, Loop_Statements);

               if I > 0 then
                  Aux_Node := Nth_Element (PN (P_Aux), I);
                  Aux_Node := Make_Assignment_Statement
                    (Aux_Node,
                     Make_Subprogram_Call
                     (RE (RE_Get_Aggregate_Element),
                      New_List
                      (Nth_Element (PN (P_Aux), I - 1),
                       Expand_Designator (TC),
                       Make_Subprogram_Call
                       (RE (RE_Unsigned_Long),
                        New_List (Copy_Node (Prev_Index_Node))))));
                  Append_To (Enclosing_Statements, Aux_Node);
                  Append_To (Enclosing_Statements, N);
               else
                  Aux_Node := Nth_Element (PN (P_Aux), I);
                  Aux_Node := Make_Assignment_Statement
                    (Aux_Node,
                     Make_Defining_Identifier (PN (P_Item)));
                  Append_To (S, Aux_Node);
                  Append_To (S, N);
               end if;

               I := I + 1;

               --  Although we use only TC_XXX_TC_Dimension_N in the enclosing
               --  loops, the assignment above must be done at the end, and not
               --  at the beginning, of the loop. This is due to the fact that
               --  the statements of a For loop are computed in the iteration
               --  which comes after the one in which the for loop is created.

               TC := Next_Node (TC);
               Dim := Next_Node (Dim);
               exit when No (Dim);
            end loop;

            TC := Get_TC_Node (Type_Spec (Declaration (E)));
            Helper := Get_From_Any_Node (Type_Spec (Declaration (E)));

            N := Make_Indexed_Component
                   (Make_Defining_Identifier (PN (P_Result)),
                    Index_List);

            M := Make_Subprogram_Call
                   (RE (RE_Get_Aggregate_Element),
                    New_List
                      (Nth_Element (PN (P_Aux), I - 1),
                       TC,
                       Make_Type_Conversion
                         (RE (RE_Unsigned_Long),
                          Copy_Node (Index_Node))));

            M := Make_Subprogram_Call
              (Helper,
               New_List (M));

            N := Make_Assignment_Statement (N, M);
            Append_To (Loop_Statements, N);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_To (S, N);

            N := Make_Subprogram_Body (Spec, D, S);
            return N;
         end Complex_Declarator_Body;

         ---------------------------
         -- Enumeration_Type_Body --
         ---------------------------

         function Enumeration_Type_Body (E : Node_Id) return Node_Id is
         begin
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            --  Return statement

            N := Make_Subprogram_Call
              (RE (RE_Get_Container_1),
               New_List (Make_Identifier (PN (P_Item))));
            N := Make_Explicit_Dereference (N);
            N := Make_Subprogram_Call
              (Get_From_Any_Container_Node (E),
               New_List (N));
            N := Make_Return_Statement (N);
            Append_To (S, N);

            --  Build the subprogram body

            N := Make_Subprogram_Body (Spec, D, S);
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
               New_List
               (Make_Subprogram_Call
                (RE (RE_From_Any_1),
                 New_List (Make_Defining_Identifier (PN (P_Item))))));
            N := Make_Return_Statement (N);
            Append_To (S, N);
            N := Make_Subprogram_Body (Spec, D, S);
            return N;
         end Interface_Declaration_Body;

         ----------------------------
         -- Simple_Declarator_Body --
         ----------------------------

         function Simple_Declarator_Body (E : Node_Id) return Node_Id is
         begin
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            --  Get the typespec of the type declaration of the simple
            --  declarator.

            N := Get_Type_Definition_Node (Type_Spec (Declaration (E)));

            --  Get the `From_Any' Spec of typespec of the type
            --  declaration of the simple declarator.

            M := Get_From_Any_Node (Type_Spec (Declaration (E)));

            M := Make_Subprogram_Call
              (M,
               New_List (Make_Defining_Identifier (PN (P_Item))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Constant_Present    => True,
               Object_Definition   => N,
               Expression          => M);
            Append_To (D, N);

            N := Make_Subprogram_Call
              (Return_Type (Spec),
               New_List (Make_Defining_Identifier (PN (P_Result))));

            N := Make_Return_Statement (N);
            Append_To (S, N);

            N := Make_Subprogram_Body (Spec, D, S);

            return N;
         end Simple_Declarator_Body;

         -------------------------
         -- Structure_Type_Body --
         -------------------------

         function Structure_Type_Body (E : Node_Id) return Node_Id is
            Result_Struct_Aggregate  : Node_Id;
            L                        : constant List_Id := New_List;
            Member                   : Node_Id;
            Declarator               : Node_Id;
            Designator               : Node_Id;
            V                        : Value_Type := Value (Int0_Val);
            TC                       : Node_Id;
            Helper                   : Node_Id;
         begin
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            Member := First_Entity (Members (E));

            while Present (Member) loop
               Declarator := First_Entity (Declarators (Member));

               while Present (Declarator) loop
                  TC := Get_TC_Node (Type_Spec (Declaration (Declarator)));

                  Helper := Get_From_Any_Node
                    (Type_Spec
                     (Declaration
                      (Declarator)));

                  N := Make_Subprogram_Call
                    (RE (RE_Get_Aggregate_Element),
                     New_List
                     (Make_Defining_Identifier (PN (P_Item)),
                      TC,
                      Make_Literal (New_Value (V))));
                  N := Make_Subprogram_Call (Helper, New_List (N));

                  Designator := Map_Expanded_Name (Declarator);
                  N := Make_Component_Association (Designator, N);
                  Append_To (L, N);
                  V.IVal := V.IVal + 1;
                  Declarator := Next_Entity (Declarator);
               end loop;

               Member := Next_Entity (Member);
            end loop;

            Result_Struct_Aggregate := Make_Record_Aggregate (L);

            N := Make_Return_Statement (Result_Struct_Aggregate);
            Append_To (S, N);

            N := Make_Subprogram_Body (Spec, D, S);

            return N;
         end Structure_Type_Body;

         ---------------------
         -- Union_Type_Body --
         ---------------------

         function Union_Type_Body (E : Node_Id) return Node_Id is
            Alternative_Name    : Name_Id;
            Switch_Case         : Node_Id;
            Switch_Alternatives : List_Id;
            Switch_Alternative  : Node_Id;
            Switch_Statements   : List_Id;
            Has_Default         : Boolean := False;
            Choices             : List_Id;
            From_Any_Helper     : Node_Id;
            TC_Helper           : Node_Id;
            Switch_Type         : Node_Id;
            Literal_Parent      : Node_Id := No_Node;
            Orig_Type           : constant Node_Id :=
              FEU.Get_Original_Type_Specifier (Switch_Type_Spec (E));
         begin
            Spec := From_Any_Node (BE_Node (Identifier (E)));

            --  Declarative Part

            --  Getting the From_Any function the TC_XXX constant and
            --  the Ada type nodes corresponding to the discriminant
            --  type.

            TC_Helper := Get_TC_Node (Switch_Type_Spec (E));
            From_Any_Helper := Get_From_Any_Node (Switch_Type_Spec (E));

            if Is_Base_Type (Switch_Type_Spec (E)) then
               Switch_Type := RE (Convert (FEN.Kind (Switch_Type_Spec (E))));
            elsif FEN.Kind (Orig_Type) =  K_Enumeration_Type then
               Switch_Type := Map_Expanded_Name (Switch_Type_Spec (E));
               Literal_Parent := Map_Expanded_Name
                 (Scope_Entity
                  (Identifier
                   (Orig_Type)));
            else
               Switch_Type := Map_Expanded_Name (Switch_Type_Spec (E));
            end if;

            --  Declaration of the "Label_Any" Variable.

            N := Make_Subprogram_Call
              (RE (RE_Get_Aggregate_Element),
               New_List
               (Make_Identifier (PN (P_Item)),
                TC_Helper,
                Make_Type_Conversion
                (RE (RE_Unsigned_Long),
                 Make_Literal (Int0_Val))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Label_Any)),
               Constant_Present    => True,
               Object_Definition   => RE (RE_Any),
               Expression          => N);
            Append_To (D, N);

            --  Converting the "Label_Value" to to the discriminant type

            N := Make_Subprogram_Call
              (From_Any_Helper,
               New_List
               (Make_Defining_Identifier (VN (V_Label_Any))));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Label)),
               Constant_Present    => True,
               Object_Definition   => Switch_Type,
               Expression          => N);
            Append_To (D, N);

            --  Declaring the "Result" variable

            N := Make_Type_Conversion
              (Copy_Expanded_Name (Return_Type (Spec)),
               Make_Defining_Identifier (VN (V_Label)));

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => N);
            Append_To (D, N);

            --  Declaring the "Index" variable

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Index)),
               Object_Definition   => RE (RE_Any));
            Append_To (D, N);

            --  Statements

            Switch_Alternatives := New_List;
            Switch_Case := First_Entity (Switch_Type_Body (E));

            while Present (Switch_Case) loop
               Switch_Statements := New_List;

               Map_Choice_List
                 (Labels (Switch_Case),
                  Literal_Parent,
                  Choices,
                  Has_Default);

               --  Getting the field full name

               Alternative_Name := BEU.To_Ada_Name
                 (FEN.IDL_Name
                  (Identifier
                   (Declarator
                    (Element
                     (Switch_Case)))));
               Get_Name_String (PN (P_Result));
               Add_Char_To_Name_Buffer ('.');
               Get_Name_String_And_Append (Alternative_Name);
               Alternative_Name := Name_Find;

               --  Getting the From_Any function the TC_XXX constant and the
               --  Ada type nodes corresponding to the element type.

               TC_Helper := Get_TC_Node
                 (Type_Spec
                  (Element
                   (Switch_Case)));
               From_Any_Helper := Get_From_Any_Node
                 (Type_Spec
                  (Element
                   (Switch_Case)));

               if Is_Base_Type (Type_Spec (Element (Switch_Case))) then
                  Switch_Type := RE
                    (Convert
                     (FEN.Kind
                      (Type_Spec
                       (Element
                        (Switch_Case)))));
               elsif FEN.Kind (Type_Spec (Element (Switch_Case))) =
                 K_Scoped_Name
               then
                  Switch_Type := Map_Expanded_Name
                    (Type_Spec
                     (Element
                      (Switch_Case)));
               else
                  declare
                     Msg : constant String := "Could not fetch From_Any "
                       & "spec and TypeCode of: "
                       & FEN.Node_Kind'Image (Kind
                                              (Type_Spec
                                               (Element
                                                (Switch_Case))));
                  begin
                     raise Program_Error with Msg;
                  end;
               end if;

               --  Assigning the value to the "Index" variable

               N := Make_Subprogram_Call
                 (RE (RE_Get_Aggregate_Element),
                  New_List
                  (Make_Identifier (PN (P_Item)),
                   TC_Helper,
                   Make_Type_Conversion
                   (RE (RE_Unsigned_Long),
                    Make_Literal (Int1_Val))));
               N := Make_Assignment_Statement
                 (Make_Defining_Identifier (VN (V_Index)),
                  N);
               Append_To (Switch_Statements, N);

               --  Converting the Any value

               N := Make_Subprogram_Call
                 (From_Any_Helper,
                  New_List
                  (Make_Defining_Identifier (VN (V_Index))));
               N := Make_Assignment_Statement
                 (Make_Defining_Identifier (Alternative_Name),
                  N);

               Append_To (Switch_Statements, N);

               Switch_Alternative :=  Make_Case_Statement_Alternative
                 (Choices, Switch_Statements);
               Append_To (Switch_Alternatives, Switch_Alternative);

               Switch_Case := Next_Entity (Switch_Case);
            end loop;

            --  Add an empty when others clause to keep the compiler
            --  happy.

            if not Has_Default then
               Append_To (Switch_Alternatives,
                 Make_Case_Statement_Alternative (No_List, No_List));
            end if;

            N := Make_Case_Statement
            (Make_Defining_Identifier (VN (V_Label)),
             Switch_Alternatives);
            Append_To (S, N);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_To (S, N);

            N := Make_Subprogram_Body (Spec, D, S);

            return N;
         end Union_Type_Body;

         --------------------------------
         -- Exception_Declaration_Body --
         --------------------------------

         function Exception_Declaration_Body (E : Node_Id) return Node_Id is
            Members         : List_Id;
            Member          : Node_Id;
            Member_Type     : Node_Id;
            Declarator      : Node_Id;
            Dcl_Name        : Name_Id;
            Index           : Unsigned_Long_Long;
            Param_List      : List_Id;
            Return_List     : List_Id;
            From_Any_Helper : Node_Id;
         begin
            --  Get the "From_Any" spec node from the helper spec

            Spec := From_Any_Node (BE_Node (Identifier (E)));

            Members := FEN.Members (E);

            --  The generated code is totally different depending on
            --  the existence or not of members in the exception.

            if FEU.Is_Empty (Members) then
               --  We declare a dummy empty structure corresponding to
               --  the exception members and we return it.

               --  Declarations

               --  Get the node corresponding to the declaration of
               --  the "Excp_Name"_Members type.

               N := Get_Type_Definition_Node (E);
               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Result)),
                  Object_Definition   => N);
               Append_To (D, N);

               --  Adding the necessary pragmas because the parameter
               --  of the function is unreferenced.

               N := Make_Pragma
                 (Pragma_Warnings, New_List (RE (RE_Off)));
               Append_To (D, N);

               N := Make_Pragma
                 (Pragma_Unreferenced,
                  New_List (Make_Identifier (PN (P_Item))));
               Append_To (D, N);

               N := Make_Pragma
                 (Pragma_Warnings, New_List (RE (RE_On)));
               Append_To (D, N);

               --  Statements

               N := Make_Return_Statement (Make_Identifier (VN (V_Result)));
               Append_To (S, N);
            else
               --  Declarations

               Return_List := New_List;
               Member := First_Entity (Members);
               Index := 0;

               while Present (Member) loop
                  Declarator := First_Entity (Declarators (Member));

                  while Present (Declarator) loop
                     Dcl_Name := To_Ada_Name
                       (IDL_Name (FEN.Identifier (Declarator)));

                     Member_Type := Type_Spec (Member);
                     Param_List := New_List (
                       Make_Identifier (PN (P_Item)),
                       Get_TC_Node (Member_Type),
                       Make_Literal (New_Integer_Value (Index, 1, 10)));

                     From_Any_Helper := Get_From_Any_Node (Member_Type);

                     N := Make_Subprogram_Call
                            (RE (RE_Get_Aggregate_Element), Param_List);

                     N := Make_Subprogram_Call (From_Any_Helper, New_List (N));

                     --  Adding the element to the return list

                     Append_To (Return_List,
                       Make_Component_Association
                         (Make_Identifier (Dcl_Name), N));

                     Declarator := Next_Entity (Declarator);
                     Index := Index + 1;
                  end loop;

                  Member := Next_Entity (Member);
               end loop;

               N := Make_Return_Statement
                      (Make_Record_Aggregate (Return_List));
               Append_To (S, N);
            end if;

            N := Make_Subprogram_Body (Spec, D, S);
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
               declare
                  Msg : constant String := "Cannot not generate From_Any "
                    & "body for a: "
                    & FEN.Node_Kind'Image (Kind (E));
               begin
                  raise Program_Error with Msg;
               end;
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
         Declarative_Part : constant List_Id := New_List;
         Statements       : constant List_Id := New_List;
         Dependency_List  : constant List_Id := Get_GList
           (Package_Declaration (Current_Package), GL_Dependencies);
      begin
         --  Declarative part
         --  Adding 'use' clauses to make the code more readable

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings));
         Append_To (Declarative_Part, N);

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings_Lists));
         Append_To (Declarative_Part, N);

         --  Statements

         --  The package name

         Aggregates := New_List;
         N := Defining_Identifier (Package_Declaration (Current_Package));
         V := New_String_Value (Fully_Qualified_Name (N), False);
         N := Make_Expression (Make_Literal (V), Op_Plus);
         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Name)),
            Expression    => N);
         Append_To (Aggregates, N);

         --  The conflicts

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Conflicts)),
            Expression    => RE (RE_Empty));
         Append_To (Aggregates, N);

         --  Building the dependency list of the package. By default,
         --  all Helper packages have to be initialized after the Any
         --  type initialization.

         Set_Str_To_Name_Buffer ("any");

         N := Make_Expression
           (Make_Literal
            (New_String_Value
             (Name_Find, False)),
            Op_Plus);

         --  Dependencies

         if not Is_Empty (Dependency_List) then
            Dep := First_Node (Dependency_List);

            while Present (Dep) loop
               N := Make_Expression (N, Op_And_Symbol, Dep);
               Dep := Next_Node (Dep);
            end loop;
         end if;

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Depends)),
            Expression    => N);
         Append_To (Aggregates, N);

         --  Provides

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Provides)),
            Expression    => RE (RE_Empty));
         Append_To (Aggregates, N);

         --  Implicit

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Implicit)),
            Expression    => RE (RE_False));
         Append_To (Aggregates, N);

         --  Init procedure

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Init)),
            Expression    => Make_Attribute_Reference
              (Make_Identifier (SN (S_Deferred_Initialization)),
               A_Access));
         Append_To (Aggregates, N);

         --  Shutdown procedure

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Shutdown)),
            Expression     => Make_Null_Statement);
         Append_To (Aggregates, N);

         --  Registering the module

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Operand      => Make_Record_Aggregate (Aggregates));

         N := Make_Subprogram_Call
           (RE (RE_Register_Module), New_List (N));
         Append_To (Statements, N);

         --  Building the initialization block statement

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         Append_To (L, N);
      end Helper_Initialization;

      -----------------
      -- To_Any_Body --
      -----------------

      function To_Any_Body (E : Node_Id) return Node_Id is
         Spec        : Node_Id;
         D           : constant List_Id := New_List;
         S           : constant List_Id := New_List;
         N           : Node_Id;
         M           : Node_Id;
         Helper_Name : Name_Id;

         function Complex_Declarator_Body (E : Node_Id) return Node_Id;
         function Forward_Interface_Declaration_Body
           (E : Node_Id)
           return Node_Id;
         function Interface_Declaration_Body (E : Node_Id) return Node_Id;
         function Simple_Declarator_Body (E : Node_Id) return Node_Id;
         function Exception_Declaration_Body (E : Node_Id) return Node_Id;
         function Generic_Aggregate_Body (E : Node_Id) return Node_Id;

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
            Append_To (D, N);

            L := New_List;
            TC := TC_Node (BE_Node (Identifier (E)));
            Dim := First_Node (Sizes);

            loop
               Set_Str_To_Name_Buffer ("I");
               Add_Nat_To_Name_Buffer (I);
               M := Make_Defining_Identifier
                 (Add_Suffix_To_Name (Unique_Suffix, Name_Find));
               Append_To (L, M);
               Enclosing_Statements := Loop_Statements;
               Loop_Statements := New_List;
               N := Make_For_Statement (M, Dim, Loop_Statements);

               if I > 0 then
                  Result_Node := Nth_Element (PN (P_Result), I);
                  Result_Node := Make_Assignment_Statement
                    (Result_Node,
                     Make_Subprogram_Call
                     (RE (RE_Get_Empty_Any_Aggregate),
                      New_List
                      (Expand_Designator (TC))));
                  Append_To (Enclosing_Statements, Result_Node);

                  Append_To (Enclosing_Statements, N);

                  Result_Node := Make_Subprogram_Call
                    (RE (RE_Add_Aggregate_Element),
                     New_List
                     (Nth_Element (PN (P_Result), I - 1),
                      Nth_Element (PN (P_Result), I)));
                  Append_To (Enclosing_Statements, Result_Node);
               else
                  Result_Node := Nth_Element (PN (P_Result), I);
                  Result_Node := Make_Assignment_Statement
                    (Result_Node,
                     Make_Subprogram_Call
                     (RE (RE_Get_Empty_Any_Aggregate),
                      New_List
                      (Expand_Designator (TC))));
                  Append_To (S, Result_Node);

                  Append_To (S, N);
               end if;

               I := I + 1;
               TC := Next_Node (TC);
               Dim := Next_Node (Dim);

               exit when No (Dim);
            end loop;

            Helper := Get_To_Any_Node (Type_Spec (Declaration (E)));

            N := Make_Indexed_Component
              (Make_Defining_Identifier (PN (P_Item)), L);
            N := Make_Subprogram_Call
              (Helper,
               New_List (N));
            N := Make_Subprogram_Call
              (RE (RE_Add_Aggregate_Element),
               New_List
               (Nth_Element (PN (P_Result), I - 1), N));
            Append_To (Loop_Statements, N);
            N := Make_Return_Statement
              (Nth_Element (PN (P_Result), 0));
            Append_To (S, N);
            N := Make_Subprogram_Body (Spec, D, S);
            return N;
         end Complex_Declarator_Body;

         ----------------------------------------
         -- Forward_Interface_Declaration_Body --
         ----------------------------------------

         function Forward_Interface_Declaration_Body
           (E : Node_Id)
           return Node_Id
         is
         begin
            Spec := To_Any_Node (BE_Node (Identifier (E)));
            N := Make_Type_Conversion
              (RE (RE_Ref_2),
               Make_Defining_Identifier (PN (P_Item)));
            N := Make_Subprogram_Call
              (RE (RE_To_Any_3),
               New_List (N));
            N := Make_Return_Statement (N);
            Append_To (S, N);
            N := Make_Subprogram_Body (Spec, No_List, S);
            return N;
         end Forward_Interface_Declaration_Body;

         --------------------------------
         -- Interface_Declaration_Body --
         --------------------------------

         function Interface_Declaration_Body (E : Node_Id) return Node_Id is
         begin
            Spec := TC_Node (BE_Node (Identifier (E)));

            --  Getting the identifier of the TC_"Interface_name"
            --  variable declared in the Helper spec.

            Helper_Name := BEN.Name (Defining_Identifier (Spec));

            --  Getting the node of the To_Any method spec node

            Spec := To_Any_Node (BE_Node (Identifier (E)));

            N := Make_Type_Conversion
              (RE (RE_Ref_2),
               Make_Defining_Identifier (PN (P_Item)));
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_A)),
               Object_Definition => RE (RE_Any),
               Expression => Make_Subprogram_Call
               (RE (RE_To_Any_3), New_List (N)));
            Append_To (D, N);
            N := Make_Subprogram_Call
              (RE (RE_Set_Type),
               New_List (Make_Defining_Identifier (PN (P_A)),
                             Make_Identifier (Helper_Name)));
            Append_To (S, N);
            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_A)));
            Append_To (S, N);
            N := Make_Subprogram_Body (Spec, D, S);
            return N;
         end Interface_Declaration_Body;

         ----------------------------
         -- Simple_Declarator_Body --
         ----------------------------

         function Simple_Declarator_Body (E : Node_Id) return Node_Id is
         begin
            Spec := To_Any_Node (BE_Node (Identifier (E)));

            --  Get the typespec of the type declaration of the
            --  simple declarator.

            N := Get_Type_Definition_Node (Type_Spec (Declaration (E)));

            --  Get the `To_Any' spec of the typespec of the type
            --  declaration of the simple declarator.

            M := Get_To_Any_Node (Type_Spec (Declaration (E)));

            Helper_Name := BEN.Name
              (Defining_Identifier (TC_Node (BE_Node (Identifier (E)))));
            N := Make_Type_Conversion
              (N,
               Make_Defining_Identifier (PN (P_Item)));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition   => RE (RE_Any),
               Expression          => Make_Subprogram_Call
               (M, New_List (N)));
            Append_To (D, N);

            N := Make_Subprogram_Call
              (RE (RE_Set_Type),
               New_List (Make_Defining_Identifier (PN (P_Result)),
                             Make_Defining_Identifier (Helper_Name)));
            Append_To (S, N);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_To (S, N);

            N := Make_Subprogram_Body (Spec, D, S);
            return N;
         end Simple_Declarator_Body;

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

            --  Declarations

            --  Get the node corresponding to the declaration of the
            --  TC_"Excp_Name" constant.

            N := TC_Node (BE_Node (Identifier (E)));
            N := Expand_Designator (N);
            N := Make_Subprogram_Call
              (RE (RE_Get_Empty_Any_Aggregate),
               New_List (N));

            Members := FEN.Members (E);

            --  If the structure has no members, Result won't ever be modified
            --  and may be declared a constant.

            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (VN (V_Result)),
               Constant_Present    => FEU.Is_Empty (Members),
               Object_Definition   => RE (RE_Any),
               Expression          => N);
            Append_To (D, N);

            --  Also add Unreferenced pragmas in that case, since the Item
            --  formal is never referenced if there are no members.

            if FEU.Is_Empty (Members) then
               N := Make_Pragma
                 (Pragma_Warnings, New_List (RE (RE_Off)));
               Append_To (D, N);

               N := Make_Pragma
                 (Pragma_Unreferenced,
                  New_List (Make_Identifier (PN (P_Item))));
               Append_To (D, N);

               N := Make_Pragma
                 (Pragma_Warnings, New_List (RE (RE_On)));
               Append_To (D, N);
            else
               --  Statements

               Member := First_Entity (Members);

               while Present (Member) loop
                  Declarator := First_Entity (Declarators (Member));
                  Member_Type := Type_Spec (Member);
                  while Present (Declarator) loop

                     To_Any_Helper := Get_To_Any_Node (Member_Type);

                     N := Make_Selected_Component
                       (PN (P_Item),
                        To_Ada_Name
                        (IDL_Name
                         (FEN.Identifier
                          (Declarator))));

                     N := Make_Subprogram_Call
                       (To_Any_Helper,
                        New_List (N));

                     N := Make_Subprogram_Call
                       (RE (RE_Add_Aggregate_Element),
                        New_List
                        (Make_Defining_Identifier (VN (V_Result)),
                         N));
                     Append_To (S, N);

                     Declarator := Next_Entity (Declarator);
                  end loop;

                  Member := Next_Entity (Member);
               end loop;
            end if;

            N := Make_Return_Statement (Make_Identifier (VN (V_Result)));
            Append_To (S, N);

            N := Make_Subprogram_Body (Spec, D, S);
            return N;
         end Exception_Declaration_Body;

         -----------------------------
         --  Generic_Aggregate_Body --
         -----------------------------

         function Generic_Aggregate_Body (E : Node_Id) return Node_Id is
         begin
            Spec := To_Any_Node (BE_Node (Identifier (E)));

            --  Obtain Any with null value

            N := RE (RE_Get_Empty_Any);
            Helper_Name := BEN.Name
              (Defining_Identifier (TC_Node (BE_Node (Identifier (E)))));
            N := Make_Subprogram_Call
              (N,
               New_List (Make_Defining_Identifier (Helper_Name)));
            N := Make_Object_Declaration
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Result)),
               Object_Definition => RE (RE_Any),
               Expression        => N,
               Constant_Present  => True);
            Append_To (D, N);

            --  Generate:

            --  Set_Value
            --    (Get_Container (Result).all,
            --     new Internals.T_Content'(Wrap (new T'(Item))),
            --     Foreign          => False);

            N := Make_Subprogram_Call (
                   RE (RE_Set_Value),
                   New_List (
                     Make_Explicit_Dereference
                       (Make_Subprogram_Call
                         (RE (RE_Get_Container_1),
                          New_List (Make_Identifier (PN (P_Result))))),

                     Make_Object_Instantiation
                       (Make_Qualified_Expression
                          (Subtype_Mark =>
                             Make_Attribute_Reference
                               (RE (RE_Content),
                                A_Class),
                           Operand =>
                             Make_Subprogram_Call
                               (Make_Selected_Component
                                  (Defining_Identifier
                                     (Internals_Package (Current_Entity)),
                                      Make_Identifier (SN (S_Wrap))),

                                New_List
                                  (Make_Object_Instantiation
                                     (Make_Qualified_Expression
                                       (Subtype_Mark =>
                                          Get_Type_Definition_Node (E),
                                        Operand      =>
                                          Make_Identifier (PN (P_Item)))))))),

                     RE (RE_False)));
            Append_To (S, N);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_To (S, N);

            N := Make_Subprogram_Body (Spec, D, S);
            return N;
         end Generic_Aggregate_Body;

      --  Start of processing for To_Any_Body

      begin
         case FEN.Kind (E) is
            when K_Complex_Declarator =>
               N := Complex_Declarator_Body (E);

            when K_Enumeration_Type |
                 K_Structure_Type   |
                 K_Union_Type       =>
               N := Generic_Aggregate_Body (E);

            when K_Forward_Interface_Declaration =>
               N := Forward_Interface_Declaration_Body (E);

            when K_Interface_Declaration =>
               N := Interface_Declaration_Body (E);

            when K_Simple_Declarator =>
               N := Simple_Declarator_Body (E);

            when K_Exception_Declaration =>
               N := Exception_Declaration_Body (E);

            when others =>
               declare
                  Msg : constant String := "Cannot not generate To_Any "
                    & "body for a: "
                    & FEN.Node_Kind'Image (Kind (E));
               begin
                  raise Program_Error with Msg;
               end;
         end case;
         return N;
      end To_Any_Body;

      -------------------
      -- U_To_Ref_Body --
      -------------------

      function U_To_Ref_Body (E : Node_Id) return Node_Id is
         Spec         : Node_Id;
         Declarations : List_Id;
         Statements   : List_Id;
         Param        : Node_Id;
         L            : List_Id;
         S_Set_Node   : Node_Id;
      begin
         --  The spec of the Unchecked_To_Ref function

         Spec := U_To_Ref_Node (BE_Node (Identifier (E)));

         --  Declarative Part

         Declarations := New_List;
         Param := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Result)),
            Object_Definition =>
              Expand_Designator (Type_Def_Node (BE_Node (Identifier (E)))));
         Append_To (Declarations, Param);

         --  Statements Part

         S_Set_Node := Make_Defining_Identifier (SN (S_Set));

         --  Depending on the nature of node E:

         --  * If E is an Interface declaration, we use the Set
         --  function inherited from CORBA.Object.Ref

         --  * If E is a forward Interface declaration, we use the Set
         --  function defined in the instantiated package.

         if FEN.Kind (E) = K_Forward_Interface_Declaration then
            S_Set_Node := Make_Selected_Component
              (Expand_Designator
                (Instantiation_Node (BE_Node (Identifier (E)))),
               S_Set_Node);
         end if;

         Statements := New_List;
         L := New_List;

         Append_To (L, Make_Defining_Identifier (PN (P_Result)));

         Append_To (L,
           Make_Subprogram_Call
             (RE (RE_Object_Of),
              New_List (Make_Defining_Identifier (PN (P_The_Ref)))));

         Append_To (Statements,
           Make_Subprogram_Call
             (Defining_Identifier   => S_Set_Node,
              Actual_Parameter_Part => L));

         Append_To (Statements,
           Make_Return_Statement (Make_Defining_Identifier (PN (P_Result))));

         return Make_Subprogram_Body (Spec, Declarations, Statements);
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
         --    The standard mandates type checking during narrowing
         --    (4.6.2 Narrowing Object References).
         --
         --    Doing the check properly implies either
         --       1. querying the interface repository (not implemented yet);
         --    or 2. calling Is_A (Repository_Id) on an object reference whose
         --          type maps the actual (i. e. most derived) interface of
         --          The_Ref (which is impossible if that type is not
         --          known on the partition where To_Ref is called);
         --    or 3. a remote invocation of an Is_A method of the designated
         --          object.
         --
         --    The most general and correct solution to this problem is 3. When
         --    a remote call is not desired, the user should use
         --    Unchecked_To_Ref, whose purpose is precisely that.
         --
         --    This solution is implemented as a dispatching call to Is_A on
         --    the source object reference. The remote Is_A operation will be
         --    invoked if necessary.

         --  The spec of the To_Ref function

         Spec := To_Ref_Node (BE_Node (Identifier (E)));

         --  The value of the Rep_Id depends on the nature of E node:

         --  * K_Interface_Declaration: we use the variable
         --  Repository_Id declared in the stub.

         --  * K_Forward_Interface_Declaration: we cannot use the
         --  Repository_Id variable because it designates another
         --  entity. So, we build a literal string value that
         --  designates the forwarded interface.

         if FEN.Kind (E) = K_Interface_Declaration then
            Rep_Id := Make_Defining_Identifier (PN (P_Repository_Id));
         elsif FEN.Kind (E) = K_Forward_Interface_Declaration then
            Rep_Id := Make_Literal
              (Get_Value
               (BEN.Expression
                (Next_Node
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (Forward
                     (E))))))));
         else
            declare
               Msg : constant String :=
                 "Could not fetch the Repository_Id of a "
                 & FEN.Node_Kind'Image (Kind (E));
            begin
               raise Program_Error with Msg;
            end;
         end if;

         Statements := New_List;
         N := Make_Expression
           (Left_Expr => Make_Subprogram_Call
            (RE (RE_Is_Nil),
             New_List (Make_Defining_Identifier (PN (P_The_Ref)))),
            Operator   => Op_Or_Else,
            Right_Expr => Make_Subprogram_Call
            (RE (RE_Is_A),
             New_List
             (Make_Defining_Identifier (PN (P_The_Ref)),
              Rep_Id)));
         M := Make_Subprogram_Call
           (Map_Narrowing_Designator (E, True),
            New_List (Make_Defining_Identifier (PN (P_The_Ref))));
         M := Make_Return_Statement (M);
         N := Make_If_Statement
           (Condition => N,
            Then_Statements => New_List (M),
            Else_Statements => No_List);
         Append_To (Statements, N);

         N := Make_Subprogram_Call
           (RE (RE_Raise_Bad_Param),
            New_List (RE (RE_Default_Sys_Member)));
         Append_To (Statements, N);

         N := Make_Subprogram_Body (Spec, No_List, Statements);
         return N;
      end To_Ref_Body;

      ---------------------
      -- Raise_Excp_Body --
      ---------------------

      function Raise_Excp_Body (E : Node_Id) return Node_Id is
         Spec       : Node_Id;
         Statements : constant List_Id := New_List;
         N          : Node_Id;
      begin
         --  The spec was declared at the forth position in the helper
         --  spec.

         Spec := Raise_Excp_Node (BE_Node (Identifier (E)));

         --  Statements

         N := Make_Defining_Identifier
           (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         N := Make_Attribute_Reference (N, A_Identity);
         N := Make_Subprogram_Call
           (RE (RE_User_Raise_Exception),
            New_List
            (N,
             Make_Defining_Identifier (PN (P_Members))));
         Append_To (Statements, N);

         N := Make_Subprogram_Body (Spec, No_List, Statements);
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
      begin
         Set_Helper_Body;

         Append_To (Statements (Current_Package), From_Any_Body (E));
         Append_To (Statements (Current_Package), To_Any_Body (E));

         Append_To
           (Get_GList (Package_Declaration (Current_Package),
                       GL_Deferred_Initialization),
            Deferred_Initialization_Block (E));
      end Visit_Enumeration_Type;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         Is_Local : constant Boolean := Is_Local_Interface (E);
      begin
         Set_Helper_Body;

         if not Is_Local then
            Append_To (Statements (Current_Package), From_Any_Body (E));
            Append_To (Statements (Current_Package), To_Any_Body (E));
         end if;

         Append_To (Statements (Current_Package), U_To_Ref_Body (E));
         Append_To (Statements (Current_Package), To_Ref_Body (E));

         Append_To
           (Get_GList (Package_Declaration (Current_Package),
                       GL_Deferred_Initialization),
            Deferred_Initialization_Block (E));
      end Visit_Forward_Interface_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N             : Node_Id;
         Is_Local      : constant Boolean := Is_Local_Interface (E);
         DI_Statements : List_Id;
      begin
         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Helper_Body;

         --  Initialize Global lists

         Initialize_GList (Package_Declaration (Current_Package),
                           GL_Deferred_Initialization);
         Initialize_GList (Package_Declaration (Current_Package),
                           GL_Initialization_Block);
         Initialize_GList (Package_Declaration (Current_Package),
                           GL_Dependencies);

         if not Is_Local then
            Append_To (Statements (Current_Package), From_Any_Body (E));
            Append_To (Statements (Current_Package), To_Any_Body (E));
         end if;

         Append_To (Statements (Current_Package), U_To_Ref_Body (E));
         Append_To (Statements (Current_Package), To_Ref_Body (E));

         Append_To
           (Get_GList (Package_Declaration (Current_Package),
                       GL_Deferred_Initialization),
            Deferred_Initialization_Block (E));

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritance, generate the mappings
         --  for the operations and attributes of the parents except
         --  the first one.

         Map_Inherited_Entities_Bodies
           (Current_Interface    => E,
            Visit_Operation_Subp => null,
            Helper               => True);

         --  Get the statament list of the Deferred_Initialization procedure

         DI_Statements := Get_GList
           (Package_Declaration (Current_Package),
            GL_Deferred_Initialization);

         --  If the statement list of Deferred_Initialization is empty, then
         --  the Helper package is also empty. So, we do not create the
         --  Deferred_Initialization to keep the statament list of the Helper
         --  empty and avoid generating it at the source file creation phase.

         if not BEU.Is_Empty (DI_Statements) then
            N := Make_Subprogram_Body
              (Make_Subprogram_Specification
               (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
                No_List),
               No_List,
               DI_Statements);
            Append_To (Statements (Current_Package), N);

            declare
               Package_Init_List : constant List_Id
                 := Get_GList (Package_Declaration (Current_Package),
                               GL_Initialization_Block);
            begin
               Helper_Initialization (Package_Init_List);
               Set_Package_Initialization (Current_Package, Package_Init_List);
            end;
         end if;

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D             : Node_Id;
         N             : Node_Id;
         DI_Statements : List_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Helper_Body) then
            D := Stub_Node (BE_Node (Identifier (E)));
            Push_Entity (D);

            Set_Helper_Body;

            --  Initialize Global lists

            Initialize_GList (Package_Declaration (Current_Package),
                              GL_Deferred_Initialization);
            Initialize_GList (Package_Declaration (Current_Package),
                              GL_Initialization_Block);
            Initialize_GList (Package_Declaration (Current_Package),
                              GL_Dependencies);

            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            --  Get the statament slit of the Deferred_Initialization
            --  procedure.

            DI_Statements := Get_GList
              (Package_Declaration (Current_Package),
               GL_Deferred_Initialization);

            --  If the statement list of Deferred_Initialization is
            --  empty, this means that the Helper package is also
            --  empty. So, we do not create the
            --  Deferred_Initialization to keep the statament list of
            --  the Helper empty and avoid generating it at the source
            --  file creation phase.

            --  If no statement have been added to the package before
            --  the deferred initialization subprogram, the body is
            --  kept empty and is not generated.

            if not BEU.Is_Empty (DI_Statements) then
               N := Make_Subprogram_Body
                 (Make_Subprogram_Specification
                  (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
                   No_List),
                  No_List,
                  DI_Statements);
               Append_To (Statements (Current_Package), N);

               declare
                  Package_Init_List : constant List_Id
                    := Get_GList (Package_Declaration (Current_Package),
                                  GL_Initialization_Block);
               begin
                  Helper_Initialization (Package_Init_List);
                  Set_Package_Initialization (Current_Package,
                                              Package_Init_List);
               end;
            end if;

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

         --  Initialize Global lists

         Initialize_GList (Package_Declaration (Current_Package),
                           GL_Deferred_Initialization);
         Initialize_GList (Package_Declaration (Current_Package),
                           GL_Initialization_Block);
         Initialize_GList (Package_Declaration (Current_Package),
                           GL_Dependencies);

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
      begin
         Set_Helper_Body;

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component.

         if not FEU.Has_Local_Component (E) then
            Append_To (Statements (Current_Package), From_Any_Body (E));
            Append_To (Statements (Current_Package), To_Any_Body (E));
         end if;

         Append_To
           (Get_GList (Package_Declaration (Current_Package),
                       GL_Deferred_Initialization),
            Deferred_Initialization_Block (E));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         L : List_Id;
         D : Node_Id;
         N : Node_Id;
         T : Node_Id;

         --  The three procedures below generate special code for
         --  fixed point types, sequence types and bounded [wide]
         --  string types.

         procedure Visit_Fixed_Type_Declaration (Type_Node : Node_Id);
         procedure Visit_Sequence_Type_Declaration (Type_Node : Node_Id);
         procedure Visit_String_Type_Declaration (Type_Node : Node_Id);

         ----------------------------------
         -- Visit_Fixed_Type_Declaration --
         ----------------------------------

         procedure Visit_Fixed_Type_Declaration (Type_Node : Node_Id) is
            Package_Node : Node_Id;
            Spec_Node     : Node_Id;
            Renamed_Subp : Node_Id;
         begin
            --  Getting the name of the package instantiation in the
            --  Internals package.

            Package_Node := Make_Selected_Component
              (Defining_Identifier (Internals_Package (Current_Entity)),
               Make_Defining_Identifier
               (Map_Fixed_Type_Helper_Name (Type_Node)));

            --  The From_Any and To_Any functions for the fixed point
            --  type are homonyms of those of the instantiated
            --  package. We just create a copy of the corresponding
            --  spec and we add a renaming field.

            --  From_Any

            Spec_Node := From_Any_Node (BE_Node (Type_Node));

            --  The renamed subprogram

            Renamed_Subp := Make_Selected_Component
              (Package_Node,
               Make_Defining_Identifier (SN (S_From_Any)));

            N :=  Make_Subprogram_Specification
              (Defining_Identifier => Defining_Identifier (Spec_Node),
               Parameter_Profile   => Parameter_Profile (Spec_Node),
               Return_Type         => Return_Type (Spec_Node),
               Renamed_Subprogram  => Renamed_Subp);
            Append_To (Statements (Current_Package), N);

            --  To_Any

            Spec_Node := To_Any_Node (BE_Node (Type_Node));

            --  The renamed subprogram

            Renamed_Subp := Make_Selected_Component
              (Package_Node,
               Make_Defining_Identifier (SN (S_To_Any)));

            N := Make_Subprogram_Specification
                   (Defining_Identifier => Defining_Identifier (Spec_Node),
                    Parameter_Profile   => Parameter_Profile (Spec_Node),
                    Return_Type         => Return_Type (Spec_Node),
                    Renamed_Subprogram  => Renamed_Subp);
            Append_To (Statements (Current_Package), N);

            --  Deferred initialization

            Append_To
              (Get_GList (Package_Declaration (Current_Package),
                          GL_Deferred_Initialization),
               Deferred_Initialization_Block (Type_Node));
         end Visit_Fixed_Type_Declaration;

         -------------------------------------
         -- Visit_Sequence_Type_Declaration --
         -------------------------------------

         procedure Visit_Sequence_Type_Declaration (Type_Node : Node_Id) is
            Spec_Node    : Node_Id;
            Package_Node : Node_Id;
            Renamed_Subp : Node_Id;
         begin
            --  Do not generate the Any converters in case one of the
            --  component is a local interface or has a local
            --  interface component because local references cannot be
            --  transferred through the network.

            if not FEU.Has_Local_Component (Type_Node) then
               --  Getting the name of the package instantiation in
               --  the Internals package.

               Package_Node := Make_Selected_Component
                 (Defining_Identifier (Internals_Package (Current_Entity)),
                  Make_Defining_Identifier
                  (Map_Sequence_Pkg_Helper_Name
                   (Type_Node)));

               --  The From_Any and To_Any functions for the fixed
               --  point type are homonyms of those of the
               --  instantiated package. We just create a copy of the
               --  corresponding spec and we add a renaming field.

               --  From_Any

               Spec_Node := From_Any_Node (BE_Node (Type_Node));

               --  The renamed subprogram

               Renamed_Subp := Make_Selected_Component
                 (Package_Node,
                  Make_Defining_Identifier (SN (S_From_Any)));

               N :=  Make_Subprogram_Specification
                 (Defining_Identifier => Defining_Identifier (Spec_Node),
                  Parameter_Profile   => Parameter_Profile (Spec_Node),
                  Return_Type         => Return_Type (Spec_Node),
                  Renamed_Subprogram  => Renamed_Subp);
               Append_To (Statements (Current_Package), N);

               --  To_Any

               Spec_Node := To_Any_Node (BE_Node (Type_Node));

               --  The renamed subprogram

               Renamed_Subp := Make_Selected_Component
                 (Package_Node,
                  Make_Defining_Identifier (SN (S_To_Any)));

               N :=  Make_Subprogram_Specification
                 (Defining_Identifier => Defining_Identifier (Spec_Node),
                  Parameter_Profile   => Parameter_Profile (Spec_Node),
                  Return_Type         => Return_Type (Spec_Node),
                  Renamed_Subprogram  => Renamed_Subp);
               Append_To (Statements (Current_Package), N);
            end if;

            --  Deferred Initialization

            Append_To
              (Get_GList (Package_Declaration (Current_Package),
                          GL_Deferred_Initialization),
               Deferred_Initialization_Block (Type_Node));
         end Visit_Sequence_Type_Declaration;

         -----------------------------------
         -- Visit_String_Type_Declaration --
         -----------------------------------

         procedure Visit_String_Type_Declaration (Type_Node : Node_Id) is
            Spec_Node    : Node_Id;
            Package_Node : Node_Id;
            Renamed_Subp : Node_Id;
         begin
            --  Getting the name of the package instantiation in the
            --  Stub spec.

            Package_Node := Expand_Designator
              (Instantiation_Node
               (BE_Node
                (Type_Node)));

            --  The From_Any and To_Any functions for the fixed point
            --  type are homonyms of those of the instantiated
            --  package. We just create a copy of the corresponding
            --  spec and we add a renaming field.

            --  From_Any

            Spec_Node := From_Any_Node (BE_Node (Type_Node));

            --  The renamed subprogram

            Renamed_Subp := Make_Selected_Component
              (Package_Node,
               Make_Defining_Identifier (SN (S_From_Any)));

            N :=  Make_Subprogram_Specification
              (Defining_Identifier => Defining_Identifier (Spec_Node),
               Parameter_Profile   => Parameter_Profile (Spec_Node),
               Return_Type         => Return_Type (Spec_Node),
               Renamed_Subprogram  => Renamed_Subp);
            Append_To (Statements (Current_Package), N);

            --  To_Any

            Spec_Node := To_Any_Node (BE_Node (Type_Node));

            --  The renamed subprogram

            Renamed_Subp := Make_Selected_Component
              (Package_Node,
               Make_Defining_Identifier (SN (S_To_Any)));

            N :=  Make_Subprogram_Specification
              (Defining_Identifier => Defining_Identifier (Spec_Node),
               Parameter_Profile   => Parameter_Profile (Spec_Node),
               Return_Type         => Return_Type (Spec_Node),
               Renamed_Subprogram  => Renamed_Subp);
            Append_To (Statements (Current_Package), N);

            --  Deferred Initialization

            Append_To
              (Get_GList (Package_Declaration (Current_Package),
                          GL_Deferred_Initialization),
               Deferred_Initialization_Block (Type_Node));
         end Visit_String_Type_Declaration;

      begin
         Set_Helper_Body;

         --  Handling the particular cases such as fixed point types
         --  definition and sequence types definitions

         T := Type_Spec (E);

         case (FEN.Kind (T)) is
            when  K_Fixed_Point_Type =>
               Visit_Fixed_Type_Declaration (T);

            when K_Sequence_Type =>
               Visit_Sequence_Type_Declaration (T);

            when K_String_Type
              | K_Wide_String_Type =>
               Visit_String_Type_Declaration (T);

            when others =>
               null;
         end case;

         L := Declarators (E);
         D := First_Entity (L);

         while Present (D) loop

            --  If the new type is defined basing on an interface
            --  type, then we don't generate From_Any nor To_Any. We
            --  use those of the original type. If the type has a
            --  local interface componenet then we do not generate the
            --  Any converters because local references cannot be
            --  transferred through the network.

            if not ((Is_Object_Type (T)
                     and then FEN.Kind (D) = K_Simple_Declarator)
                    or else FEU.Has_Local_Component (T))
            then
               Append_To (Statements (Current_Package), From_Any_Body (D));
               Append_To (Statements (Current_Package), To_Any_Body (D));
            end if;

            Append_To
              (Get_GList (Package_Declaration (Current_Package),
                          GL_Deferred_Initialization),
               Deferred_Initialization_Block (D));

            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
      begin
         Set_Helper_Body;

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component because local references cannot be transferred
         --  through the network .

         if not FEU.Has_Local_Component (E) then
            Append_To (Statements (Current_Package), From_Any_Body (E));
            Append_To (Statements (Current_Package), To_Any_Body (E));
         end if;

         Append_To
           (Get_GList (Package_Declaration (Current_Package),
                       GL_Deferred_Initialization),
            Deferred_Initialization_Block (E));
      end Visit_Union_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         Subp_Body_Node : Node_Id;
      begin
         Set_Helper_Body;

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component because local references cannot be transferred
         --  through the network.

         if not FEU.Has_Local_Component (E) then
            Subp_Body_Node := From_Any_Body (E);
            Append_To (Statements (Current_Package), Subp_Body_Node);

            Subp_Body_Node := To_Any_Body (E);
            Append_To (Statements (Current_Package), Subp_Body_Node);
         end if;

         --  Generation of the Raise_"Exception_Name" body

         Subp_Body_Node := Raise_Excp_Body (E);
         Append_To (Statements (Current_Package), Subp_Body_Node);

         --  Generation of the corresponding instructions in the
         --  Deferred_initialisation procedure.

         Append_To
           (Get_GList (Package_Declaration (Current_Package),
                       GL_Deferred_Initialization),
            Deferred_Initialization_Block (E));
      end Visit_Exception_Declaration;
   end Package_Body;
end Backend.BE_CORBA_Ada.Helpers;
