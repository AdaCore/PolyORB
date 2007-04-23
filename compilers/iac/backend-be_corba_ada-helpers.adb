------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         B A C K E N D . B E _ C O R B A _ A D A . H E L P E R S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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
with Values;   use Values;
with Charset;  use Charset;

with Frontend.Nodes;   use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Expand;      use Backend.BE_CORBA_Ada.Expand;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

package body Backend.BE_CORBA_Ada.Helpers is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;

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
        (Declarator : Node_Id)
        return List_Id;
      --  Multidimensional arrays : when they are converted to the Any
      --  type, the multidimensional arrays are seen as nested
      --  arrays. So, for each dimension from the first until the
      --  before last dimension we declare a type code constant.  This
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
         Profile   : constant List_Id := New_List (K_Parameter_Profile);
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)),
            RE (RE_Any));
         Append_Node_To_List (Parameter, Profile);
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
         Profile   : constant List_Id := New_List (K_Parameter_Profile);
         Parameter : Node_Id;
         N         : Node_Id;
      begin
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

         return N;
      end U_To_Ref_Spec;

      -----------------
      -- To_Any_Spec --
      -----------------

      function To_Any_Spec (E : Node_Id) return Node_Id is
         Profile   : constant List_Id := New_List (K_Parameter_Profile);
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Item)),
            Get_Type_Definition_Node (E));
         Append_Node_To_List (Parameter, Profile);

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_To_Any)),
            Profile, RE (RE_Any));

         return N;
      end To_Any_Spec;

      -----------------
      -- To_Ref_Spec --
      -----------------

      function To_Ref_Spec (E : Node_Id) return Node_Id is
         Profile   : constant List_Id := New_List (K_Parameter_Profile);
         Parameter : Node_Id;
         N         : Node_Id;
      begin
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
         Profile   : constant List_Id := New_List (K_Parameter_Profile);
         Parameter : Node_Id;
         N         : Node_Id;
      begin
         Parameter := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Members)),
            Defining_Identifier (Excp_Members));
         Append_Node_To_List (Parameter, Profile);

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
         case FEN.Kind (E) is
            when K_Enumeration_Type =>
               N := Get_Type_Definition_Node (E);

            when K_Forward_Interface_Declaration =>

               --  Resolve the forward declaration

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
               N := Make_Designator (To_Ada_Name (IDL_Name (Identifier (E))));

            when others =>
               declare
                  Msg : constant String :=
                    "Cannot generate TypeCode for the frontend node "
                    & FEN.Node_Kind'Image (FEN.Kind (E));
               begin
                  raise Program_Error with Msg;
               end;
         end case;

         TC := Add_Prefix_To_Name ("TC_", BEN.Name (Defining_Identifier (N)));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (TC),
            Constant_Present    => False,
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

         --  Declaration of the variable

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (TC_Name),
            Constant_Present    => False,
            Object_Definition   => RE (RE_Object));

         return N;
      end TypeCode_Dimension_Spec;

      -------------------------------------
      -- TypeCode_Dimension_Declarations --
      -------------------------------------

      function TypeCode_Dimension_Declarations
        (Declarator : Node_Id)
        return List_Id
      is
         pragma Assert (FEN.Kind (Declarator) = K_Complex_Declarator);

         Dim : constant Natural := FEU.Length (FEN.Array_Sizes (Declarator));
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
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         N := From_Any_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_From_Any);

         N := To_Any_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
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
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Local interfaces don't have Any conversion methods

         if not Is_Local then
            N := From_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;

         N := U_To_Ref_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_U_To_Ref);

         N := To_Ref_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
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
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Local interfaces don't have Any conversion methods

         if not Is_Local then
            N := From_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;

         N := U_To_Ref_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_U_To_Ref);

         N := To_Ref_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_To_Ref);

         --  Visit the entities declared inside the interface.

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritance, generate the mappings
         --  for the operations and attributes of the parents except
         --  the first one.

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
      begin
         Set_Helper_Spec;

         N := TypeCode_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component.

         if not FEU.Has_Local_Component (E) then
            N := From_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
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
         TC_Dims : List_Id; --  For array types
      begin
         Set_Helper_Spec;
         L := Declarators (E);
         T := Type_Spec (E);
         D := First_Entity (L);

         --  Handling the case of fixed point type definitions,
         --  sequence type defitions and bounded [wide] string type
         --  definitions. We create routines for the extra entities
         --  declared in the stub spec.

         if FEN.Kind (T) = K_Fixed_Point_Type
           or else FEN.Kind (T) = K_Sequence_Type
           or else FEN.Kind (T) = K_String_Type
           or else FEN.Kind (T) = K_Wide_String_Type
         then
            begin
               N := TypeCode_Spec (T);
               Bind_FE_To_BE (T, N, B_TC);
               Append_Node_To_List (N, Visible_Part (Current_Package));

               --  Do not generate the Any converters in case one of
               --  the component is a local interface or has a local
               --  interface component.

               if not FEU.Has_Local_Component (T) then
                  N := From_Any_Spec (T);
                  Bind_FE_To_BE (T, N, B_From_Any);
                  Append_Node_To_List (N, Visible_Part (Current_Package));

                  N := To_Any_Spec (T);
                  Bind_FE_To_BE (T, N, B_To_Any);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
               end if;
            end;
         end if;

         --  General case

         while Present (D) loop
            N := TypeCode_Spec (D);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (D), N, B_TC);

            if FEN.Kind (D) = K_Complex_Declarator then
               TC_Dims := TypeCode_Dimension_Declarations (D);
               Append_Node_To_List
                 (First_Node (TC_Dims), Visible_Part (Current_Package));
            end if;

            --  If the new type is defined basing on an interface
            --  type, and then if this is not an array type, then we
            --  don't generate From_Any nor To_Any. We use those of
            --  the original type.

            if Is_Object_Type (T)
              and then FEN.Kind (D) = K_Simple_Declarator
            then

               --  For local interface, we generate nothing

               if not FEU.Has_Local_Component (T) then
                  N := Get_From_Any_Node (T, False);
                  Bind_FE_To_BE (Identifier (D), N, B_From_Any);

                  N := Get_To_Any_Node (T, False);
                  Bind_FE_To_BE (Identifier (D), N, B_To_Any);
               end if;
            else
               --  Do not generate the Any converters in case one of
               --  the component is a local interface or has a local
               --  interface component.

               if not FEU.Has_Local_Component (T) then
                  N := From_Any_Spec (D);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
                  Bind_FE_To_BE (Identifier (D), N, B_From_Any);

                  N := To_Any_Spec (D);
                  Append_Node_To_List (N, Visible_Part (Current_Package));
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
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Obtaining the node corresponding to the declaration of
         --  the "Excp_Name"_Members type.

         Excp_Members := Type_Def_Node (BE_Node (Identifier (E)));

         --  Do not generate the Any converters in case one of
         --  the component is a local interface or has a local
         --  interface component.

         if not FEU.Has_Local_Component (E) then
            N := From_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;

         --  Generation of the Raise_"Exception_Name" spec

         Excp_Name := To_Ada_Name (IDL_Name (FEN.Identifier (E)));
         Raise_Node := Make_Defining_Identifier
           (Add_Prefix_To_Name ("Raise_", Excp_Name));
         N := Raise_Excp_Spec (Excp_Members, Raise_Node);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_Raise_Excp);

         --  Addition of the pragma No_Return. The Argument of the
         --  pragma No_Return must be a local name

         N := Make_Pragma
           (Pragma_No_Return,
            Make_List_Id (Make_Designator (BEN.Name (Raise_Node))));
         Append_Node_To_List (N, Visible_Part (Current_Package));

      end Visit_Exception_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N            : Node_Id;
      begin
         Set_Helper_Spec;

         N := TypeCode_Spec (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_BE (Identifier (E), N, B_TC);

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component.

         if not FEU.Has_Local_Component (E) then
            N := From_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_From_Any);

            N := To_Any_Spec (E);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_BE (Identifier (E), N, B_To_Any);
         end if;
      end Visit_Union_Type;

   end Package_Spec;

   package body Package_Body is

      package BEU renames Backend.BE_CORBA_Ada.Nutils;

      function Deferred_Initialization_Block (E : Node_Id) return Node_Id;
      --  Returns the Initialize routine corresponding to the IDL
      --  entity E.

      function Declare_Any_Array
        (A_Name         : Name_Id;
         A_First        : Natural;
         A_Last         : Natural)
        return Node_Id;
      --  Declare an `Any' array declaration

      procedure Helper_Initialization (L : List_Id);
      --  Create the initialization block for the Helper package

      function Nth_Element (A_Name : Name_Id; Nth : Nat) return Node_Id;
      --  Create an Ada constrauct to get the Nth element of array
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

      --------------------
      -- Add_Dependency --
      --------------------

      procedure Add_Dependency (Dep : Node_Id; Dependency_List : List_Id) is

         function "=" (Name : Name_Id; Node : Node_Id) return Boolean;
         --  Shortcut to compare `Name' to the full name of `Node'

         function Is_Internal_Unit (Unit : Node_Id) return Boolean;
         --  Return True if `Unit' is an internal Ada unit

         ---------
         -- "=" --
         ---------

         function "=" (Name : Name_Id; Node : Node_Id) return Boolean is
         begin
            return Name = Fully_Qualified_Name (Node);
         end "=";

         ----------------------
         -- Is_Internal_Unit --
         ----------------------

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
         Append   : Boolean := False;
      begin
         if Is_Internal_Unit (Dep) then
            return;
         end if;

         Dep_Name := BEU.Fully_Qualified_Name (Dep);

         --  Particular case : We don't add dependencies on:

         --  * The Helper package itself

         if Dep_Name = Defining_Identifier
           (Helper_Package (Current_Entity))
         then
            return;

         --  First case : A dependency on CORBA.Object.Helper
         --  implies a dependency on CORBA.Object

         elsif Dep_Name = RU (RU_CORBA_Object_Helper, False) then
            Add_Dependency (RU (RU_CORBA_Object, False), Dependency_List);
            return;

         --  Second case : We lower the case of these entities
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
            Append := True;

         --  Third case: Some PolyORB units have a customized
         --  initialization name

         elsif Dep_Name = RU (RU_PolyORB_Exceptions, False) then
            Set_Str_To_Name_Buffer ("exceptions");
            Dep_Name := Name_Find;
            Append := True;
         end if;

         --  Check whether the dependency is already added

         M := First_Node (Dependency_List);
         while Present (M) loop
            if Values.Value (BEN.Value (M)).SVal = Dep_Name then
               Append := False;
            end if;
            M := Next_Node (M);
         end loop;

         --  Add the dependency if it belongs to the cases above

         if Append then
            V := New_String_Value (Dep_Name, False);
            N := Make_Literal (V);
            Append_Node_To_List (N, Dependency_List);
         end if;
      end Add_Dependency;

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

         N := Expand_Designator (Initialize_Node (BE_Node (Frontend_Node)));
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

         R := Make_Range_Constraint
           (Make_Literal (First), Make_Literal (Last));

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

      function From_Any_Body (E : Node_Id) return Node_Id is
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
                                       (Type_Definition
                                        (Type_Def_Node
                                         (BE_Node (Identifier (E)))));
            Dimension            : constant Natural := BEU.Length (Sizes);
            Dim                  : Node_Id;
            Loop_Statements      : List_Id := No_List;
            Enclosing_Statements : List_Id;
            Index_List           : constant List_Id := New_List (K_List_Id);
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

            N := Make_Assignment_Statement (N, M);
            Append_Node_To_List (N, Loop_Statements);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);

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
               Make_List_Id (Make_Designator (PN (P_Item))));
            N := Make_Explicit_Dereference (N);
            N := Make_Subprogram_Call
              (Get_From_Any_Container_Node (E),
               Make_List_Id (N));
            N := Make_Return_Statement (N);
            Append_Node_To_List (N, S);

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
               Make_List_Id
               (Make_Subprogram_Call
                (RE (RE_From_Any_1),
                 Make_List_Id (Make_Defining_Identifier (PN (P_Item))))));
            N := Make_Return_Statement (N);
            Append_Node_To_List (N, S);
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

            N := Make_Subprogram_Body (Spec, D, S);

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

            N := Make_Return_Statement (Result_Struct_Aggregate);
            Append_Node_To_List (N, S);

            N := Make_Subprogram_Body (Spec, D, S);

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

            --  Converting the "Label_Value" to to the discriminant
            --  type.

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

            --  According to the IDL grammar, each "case" alternative
            --  contains exactly 1 element. So, there is no need to
            --  declare the "I" variable like it is done by Idlac.

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

               --  Getting the field full name

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

               --  Getting the From_Any function the TC_XXX constant
               --  and the Ada type nodes corresponding to the element
               --  type.

               TC_Helper := Get_TC_Node
                 (Type_Spec
                  (Element
                   (Switch_Alternative)));
               From_Any_Helper := Get_From_Any_Node
                 (Type_Spec
                  (Element
                   (Switch_Alternative)));

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
                  declare
                     Msg : constant String := "Could not fetch From_Any "
                       & "spec and TypeCode of: "
                       & FEN.Node_Kind'Image (Kind
                                              (Type_Spec
                                               (Element
                                                (Switch_Alternative))));
                  begin
                     raise Program_Error with Msg;
                  end;
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

            N := Make_Subprogram_Body (Spec, D, S);

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
            --  Obtaining the "From_Any" spec node from the helper
            --  spec.

            Spec := From_Any_Node (BE_Node (Identifier (E)));

            Members := FEN.Members (E);

            --  The generated code is fundamentally different
            --  depending on the existence or not of members in the
            --  exception.

            if FEU.Is_Empty (Members) then

               --  Begin Declarations

               --  Obtaining the node corresponding to the declaration
               --  of the "Excp_Name"_Members type.

               N := Type_Def_Node (BE_Node (Identifier (E)));
               N := Defining_Identifier (N);
               N := Make_Object_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Result)),
                  Object_Definition   => N);
               Append_Node_To_List (N, D);

               --  Adding the necessary pragmas because the parameter
               --  of the function is unreferenced.

               N := Make_Pragma
                 (Pragma_Warnings, Make_List_Id (RE (RE_Off)));
               Append_Node_To_List (N, D);

               N := Make_Pragma
                 (Pragma_Unreferenced,
                  Make_List_Id (Make_Designator (PN (P_Item))));
               Append_Node_To_List (N, D);

               N := Make_Pragma
                 (Pragma_Warnings, Make_List_Id (RE (RE_On)));
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
               --  Result_"member" which has the same type.  In
               --  parallel to the declaration, we built a list for
               --  the returned value.

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

            N := Make_Subprogram_Body
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
         Declarative_Part : constant List_Id := New_List (K_List_Id);
         Statements       : constant List_Id := New_List (K_List_Id);
         Dependency_List  : constant List_Id := Get_GList
           (Package_Declaration (Current_Package), GL_Dependencies);
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
         N := Defining_Identifier (Package_Declaration (Current_Package));
         V := New_String_Value (Fully_Qualified_Name (N), False);
         N := Make_Expression (Make_Literal (V), Op_Plus);
         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Name)),
            Expression    => N);
         Append_Node_To_List (N, Aggregates);

         --  The conflicts

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Conflicts)),
            Expression    => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

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
         Append_Node_To_List (N, Aggregates);

         --  Provides

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Provides)),
            Expression    => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Implicit

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Implicit)),
            Expression    => RE (RE_False));
         Append_Node_To_List (N, Aggregates);

         --  Init procedure

         N := Make_Component_Association
           (Selector_Name => Make_Defining_Identifier (PN (P_Init)),
            Expression    => Make_Type_Attribute
              (Make_Designator (SN (S_Deferred_Initialization)),
               A_Access));
         Append_Node_To_List (N, Aggregates);

         --  Shutdown procedure

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Shutdown)),
            Expression     => Make_Null_Statement);
         Append_Node_To_List (N, Aggregates);

         --  Registering the module

         N := Make_Record_Aggregate (Aggregates);

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Aggregate    => N);

         N := Make_Subprogram_Call
           (RE (RE_Register_Module), Make_List_Id (N));
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

      function To_Any_Body (E : Node_Id) return Node_Id
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
            N := Make_Subprogram_Body
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
            N := Make_Subprogram_Body
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
            N := Make_Subprogram_Body
              (Spec, No_List, S);
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

            --  Getting the node of the To_Any method spec declared at
            --  the 3rd place of the helper spec.

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
            N := Make_Subprogram_Body
              (Spec, D, S);
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

            N := Make_Subprogram_Body (Spec, D, S);

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
                  Set_Homogeneous_Parent_Unit_Name
                    (Designator, Item_Designator);

                  --  Getting the declarator type in order to call the
                  --  right To_Any function

                  To_Any_Helper := Get_To_Any_Node
                    (Type_Spec
                     (Declaration
                      (Declarator)));

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
            N := Make_Subprogram_Body (Spec, D, S);

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
              FEU.Get_Original_Type_Specifier (Switch_Type_Spec (E));
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

               --  Getting the field full name

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

               --  Getting the To_Any function node corresponding to
               --  the element type.

               To_Any_Helper := Get_To_Any_Node
                 (Type_Spec
                  (Element
                   (Switch_Alternative)));

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
            (Make_Defining_Identifier (Switch_Item), Switch_Alternatives);
            Append_Node_To_List (N, S);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, S);

            N := Make_Subprogram_Body (Spec, D, S);

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

            --  Obtaining the node corresponding to the declaration of
            --  the TC_"Excp_Name" constant.

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

            --  Adding the necessary pragmas because the parameter of
            --  the function is unreferenced in case of nonexistence
            --  of exception members.

            Members := FEN.Members (E);

            if FEU.Is_Empty (Members) then
               N := Make_Pragma
                 (Pragma_Warnings, Make_List_Id (RE (RE_Off)));
               Append_Node_To_List (N, D);

               N := Make_Pragma
                 (Pragma_Unreferenced,
                  Make_List_Id (Make_Designator (PN (P_Item))));
               Append_Node_To_List (N, D);

               N := Make_Pragma
                 (Pragma_Warnings, Make_List_Id (RE (RE_On)));
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

            N := Make_Subprogram_Body (Spec, D, S);

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
         N            : Node_Id;
         L            : List_Id;
         S_Set_Node   : Node_Id;
      begin
         --  The spec of the Unchecked_To_Ref function

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

         --  * If E is an Interface declaration, we use the Set
         --  function inherited from CORBA.Object.Ref

         --  * If E is a forward Interface declaration, we use the Set
         --  function defined in the instantiated package.

         if FEN.Kind (E) = K_Forward_Interface_Declaration then
            Set_Homogeneous_Parent_Unit_Name
              (S_Set_Node,
               Defining_Identifier
               (Instantiation_Node
                (BE_Node
                 (Identifier
                  (E)))));
         end if;

         Statements := New_List (K_List_Id);
         L := New_List (K_List_Id);

         Append_Node_To_List (Make_Defining_Identifier (PN (P_Result)), L);

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

         N := Make_Subprogram_Body (Spec, Declarations, Statements);

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

         --  * K_Interface_Declaration : we use the variable
         --  Repository_Id declared in the stub.

         --  * K_Forward_Interface_Declaration : we cannot use the
         --  Repository_Id variable because it designates another
         --  entity.  So, we build a literal string value.

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
            declare
               Msg : constant String := "Could not the Repository_Id of a:"
                 & FEN.Node_Kind'Image (Kind (E));
            begin
               raise Program_Error with Msg;
            end;
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

         N := Make_Subprogram_Body (Spec, No_List, Statements);

         return N;
      end To_Ref_Body;

      ---------------------
      -- Raise_Excp_Body --
      ---------------------

      function Raise_Excp_Body (E : Node_Id) return Node_Id is
         Spec         : Node_Id;
         Statements   : constant List_Id := New_List (K_List_Id);
         N            : Node_Id;
      begin
         --  The spec was declared at the forth position in the helper
         --  spec

         Spec := Raise_Excp_Node (BE_Node (Identifier (E)));

         --  Begin Statements

         N := Make_Defining_Identifier
           (To_Ada_Name (IDL_Name (FEN.Identifier (E))));
         N := Make_Attribute_Designator (N, A_Identity);
         N := Make_Subprogram_Call
           (RE (RE_User_Raise_Exception),
            Make_List_Id
            (N,
             Make_Defining_Identifier (PN (P_Members))));
         Append_Node_To_List (N, Statements);

         --  End Statements

         N := Make_Subprogram_Body
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
         Append_Node_To_List (N, Get_GList
                              (Package_Declaration (Current_Package),
                               GL_Deferred_Initialization));
      end Visit_Enumeration_Type;

      -----------------------------------------
      -- Visit_Forward_Interface_Declaration --
      -----------------------------------------

      procedure Visit_Forward_Interface_Declaration (E : Node_Id) is
         N        : Node_Id;
         Is_Local : constant Boolean := Is_Local_Interface (E);
      begin
         Set_Helper_Body;

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
         Append_Node_To_List (N, Get_GList
                              (Package_Declaration (Current_Package),
                               GL_Deferred_Initialization));
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
         Append_Node_To_List (N, Get_GList
                              (Package_Declaration (Current_Package),
                               GL_Deferred_Initialization));

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         --  In case of multiple inheritance, generate the mappings
         --  for the operations and attributes of the parents except
         --  the first one.

         Map_Inherited_Entities_Bodies
           (Current_interface    => E,
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
            Append_Node_To_List (N, Statements (Current_Package));

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
            --  empty. So, we do not create the Deferred_Initialization
            --  to keep the statament list of the Helper empty and avoid
            --  generating it at the source file creation phase.

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
               Append_Node_To_List (N, Statements (Current_Package));

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
         N          : Node_Id;
      begin
         Set_Helper_Body;

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component.

         if not FEU.Has_Local_Component (E) then
            Append_Node_To_List
              (From_Any_Body (E), Statements (Current_Package));
            Append_Node_To_List
              (To_Any_Body (E), Statements (Current_Package));
         end if;

         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Get_GList
                              (Package_Declaration (Current_Package),
                               GL_Deferred_Initialization));
      end Visit_Structure_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         L : List_Id;
         D : Node_Id;
         N : Node_Id;
         T : Node_Id;

         --  The three procedure below generate special code for fixed
         --  point types, sequence types and [wide] string types.

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
            --  Internals package

            Package_Node := Make_Defining_Identifier
              (Map_Fixed_Type_Helper_Name (Type_Node));
            Set_Homogeneous_Parent_Unit_Name
              (Package_Node,
               Defining_Identifier (Internals_Package (Current_Entity)));

            --  The From_Any and To_Any functions for the fixed point
            --  type are homonyms of those of the instantiated
            --  package. We just create a copy of the corresponding
            --  spec and we add a renaming field.

            --  From_Any

            Spec_Node := From_Any_Node (BE_Node (Type_Node));

            --  The renamed subprogram

            Renamed_Subp := Make_Defining_Identifier (SN (S_From_Any));
            Set_Homogeneous_Parent_Unit_Name (Renamed_Subp, Package_Node);

            N :=  Make_Subprogram_Specification
              (Defining_Identifier => Defining_Identifier (Spec_Node),
               Parameter_Profile   => Parameter_Profile (Spec_Node),
               Return_Type         => Return_Type (Spec_Node),
               Renamed_Subprogram  => Renamed_Subp);
            Append_Node_To_List (N, Statements (Current_Package));

            --  To_Any

            Spec_Node := To_Any_Node (BE_Node (Type_Node));

            --  The renamed subprogram

            Renamed_Subp := Make_Defining_Identifier (SN (S_To_Any));
            Set_Homogeneous_Parent_Unit_Name (Renamed_Subp, Package_Node);

            N :=  Make_Subprogram_Specification
              (Defining_Identifier => Defining_Identifier (Spec_Node),
               Parameter_Profile   => Parameter_Profile (Spec_Node),
               Return_Type         => Return_Type (Spec_Node),
               Renamed_Subprogram  => Renamed_Subp);
            Append_Node_To_List (N, Statements (Current_Package));

            --  Deferred initialization

            N := Deferred_Initialization_Block (Type_Node);
            Append_Node_To_List
              (N, Get_GList (Package_Declaration (Current_Package),
                             GL_Deferred_Initialization));
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
            --  component is a local interface or has a local interface
            --  component.

            if not FEU.Has_Local_Component (Type_Node) then
               --  Getting the name of the package instantiation in
               --  the Internals package

               Package_Node := Make_Defining_Identifier
                 (Map_Sequence_Pkg_Helper_Name
                  (Type_Node));
               Set_Homogeneous_Parent_Unit_Name
                 (Package_Node,
                  Defining_Identifier (Internals_Package (Current_Entity)));

               --  The From_Any and To_Any functions for the fixed
               --  point type are homonyms of those of the
               --  instantiated package. We just create a copy of the
               --  corresponding spec and we add a renaming field.

               --  From_Any

               Spec_Node := From_Any_Node (BE_Node (Type_Node));

               --  The renamed subprogram

               Renamed_Subp := Make_Defining_Identifier (SN (S_From_Any));
               Set_Homogeneous_Parent_Unit_Name (Renamed_Subp, Package_Node);

               N :=  Make_Subprogram_Specification
                 (Defining_Identifier => Defining_Identifier (Spec_Node),
                  Parameter_Profile   => Parameter_Profile (Spec_Node),
                  Return_Type         => Return_Type (Spec_Node),
                  Renamed_Subprogram  => Renamed_Subp);
               Append_Node_To_List (N, Statements (Current_Package));

               --  To_Any

               Spec_Node := To_Any_Node (BE_Node (Type_Node));

               --  The renamed subprogram

               Renamed_Subp := Make_Defining_Identifier (SN (S_To_Any));
               Set_Homogeneous_Parent_Unit_Name (Renamed_Subp, Package_Node);

               N :=  Make_Subprogram_Specification
                 (Defining_Identifier => Defining_Identifier (Spec_Node),
                  Parameter_Profile   => Parameter_Profile (Spec_Node),
                  Return_Type         => Return_Type (Spec_Node),
                  Renamed_Subprogram  => Renamed_Subp);
               Append_Node_To_List (N, Statements (Current_Package));
            end if;

            --  Deferred Initialization

            N := Deferred_Initialization_Block (Type_Node);
            Append_Node_To_List
              (N, Get_GList (Package_Declaration (Current_Package),
                             GL_Deferred_Initialization));
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
            --  Stub spec

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

            Renamed_Subp := Make_Defining_Identifier (SN (S_From_Any));
            Set_Homogeneous_Parent_Unit_Name (Renamed_Subp, Package_Node);

            N :=  Make_Subprogram_Specification
              (Defining_Identifier => Defining_Identifier (Spec_Node),
               Parameter_Profile   => Parameter_Profile (Spec_Node),
               Return_Type         => Return_Type (Spec_Node),
               Renamed_Subprogram  => Renamed_Subp);
            Append_Node_To_List (N, Statements (Current_Package));

            --  To_Any

            Spec_Node := To_Any_Node (BE_Node (Type_Node));

            --  The renamed subprogram

            Renamed_Subp := Make_Defining_Identifier (SN (S_To_Any));
            Set_Homogeneous_Parent_Unit_Name (Renamed_Subp, Package_Node);

            N :=  Make_Subprogram_Specification
              (Defining_Identifier => Defining_Identifier (Spec_Node),
               Parameter_Profile   => Parameter_Profile (Spec_Node),
               Return_Type         => Return_Type (Spec_Node),
               Renamed_Subprogram  => Renamed_Subp);
            Append_Node_To_List (N, Statements (Current_Package));

            --  Deferred Initialization

            N := Deferred_Initialization_Block (Type_Node);
            Append_Node_To_List
              (N, Get_GList (Package_Declaration (Current_Package),
                             GL_Deferred_Initialization));
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
            --  Any converters.

            if not ((Is_Object_Type (T)
                     and then FEN.Kind (D) = K_Simple_Declarator)
                    or else FEU.Has_Local_Component (T))
            then
               Append_Node_To_List
                 (From_Any_Body (D), Statements (Current_Package));
               Append_Node_To_List
                 (To_Any_Body (D), Statements (Current_Package));
            end if;

            N := Deferred_Initialization_Block (D);
            Append_Node_To_List (N, Get_GList
                                 (Package_Declaration (Current_Package),
                                  GL_Deferred_Initialization));

            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N            : Node_Id;
      begin
         Set_Helper_Body;

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component.

         if not FEU.Has_Local_Component (E) then
            Append_Node_To_List (From_Any_Body (E),
                                 Statements (Current_Package));
            Append_Node_To_List (To_Any_Body (E),
                                 Statements (Current_Package));
         end if;

         N := Deferred_Initialization_Block (E);
         Append_Node_To_List (N, Get_GList
                              (Package_Declaration (Current_Package),
                               GL_Deferred_Initialization));
      end Visit_Union_Type;

      ---------------------------------
      -- Visit_Exception_Declaration --
      ---------------------------------

      procedure Visit_Exception_Declaration (E : Node_Id) is
         Subp_Body_Node : Node_Id;
         Deferred_Init  : Node_Id;
      begin
         Set_Helper_Body;

         --  Do not generate the Any converters in case one of the
         --  component is a local interface or has a local interface
         --  component.

         if not FEU.Has_Local_Component (E) then
            Subp_Body_Node := From_Any_Body (E);
            Append_Node_To_List (Subp_Body_Node, Statements (Current_Package));

            Subp_Body_Node := To_Any_Body (E);
            Append_Node_To_List (Subp_Body_Node, Statements (Current_Package));
         end if;

         --  Generation of the Raise_"Exception_Name" body

         Subp_Body_Node := Raise_Excp_Body (E);
         Append_Node_To_List (Subp_Body_Node, Statements (Current_Package));

         --  Generation of the corresponding instructions in the
         --  Deferred_initialisation procedure.

         Deferred_Init := Deferred_Initialization_Block (E);
         Append_Node_To_List (Deferred_Init, Get_GList
                              (Package_Declaration (Current_Package),
                               GL_Deferred_Initialization));
      end Visit_Exception_Declaration;
   end Package_Body;
end Backend.BE_CORBA_Ada.Helpers;
