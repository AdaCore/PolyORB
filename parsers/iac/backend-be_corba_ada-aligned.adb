------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--            B A C K E N D . B E _ C O R B A _ A D A . A L I G N E D       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
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
with Namet;     use Namet;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

with Backend.BE_CORBA_Ada.Common; use Backend.BE_CORBA_Ada.Common;

package body Backend.BE_CORBA_Ada.Aligned is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package BEU renames Backend.BE_CORBA_Ada.Nutils;

   package body Package_Spec is

      --  For the parshalling of parameter with use of Ada
      --  representation clauses

      function Args_Type_Record_In (E : Node_Id) return Node_Id;
      function Args_Type_Record_Out (E : Node_Id) return Node_Id;

      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Enumeration_Type (E : Node_Id);

      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);

      function Make_Type_Designator (N : Node_Id) return Node_Id;

      Variable_Parameter : Boolean := False;
      Discriminants      : List_Id := No_List;

      ----------------------
      -- Args_Type_Record --
      ----------------------

      function Args_Type_Record_In (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);
         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         P          : constant List_Id := Parameter_Profile (Spec);
         Components : List_Id;
         Component  : Node_Id;
         Parameter  : Node_Id;
         Args_Type  : Node_Id := No_Node;
         Par_Type   : Node_Id;
         N          : Node_Id;
      begin
         Set_Aligned_Spec;
         Components := New_List (K_Component_List);

         --  For each subprogram we generate a record containing the
         --  In parameter with the aligned type

         if not BEU.Is_Empty (P) then

            --  Skip the first parameter corresponding to 'Self'

            Parameter := Next_Node (First_Node (P));
            while Present (Parameter) loop

               if Is_In (BEN.Parameter_Mode (Parameter)) then
                  --  If the parameter type is a class-wide type, we remove the
                  --  "'Class" attribute from the type name

                  Par_Type := Parameter_Type (Parameter);

                  if BEN.Kind (Par_Type) = K_Attribute_Designator then
                     Par_Type := Prefix (Par_Type);
                  end if;

                  Par_Type := Make_Type_Designator (FE_Node (Par_Type));

                  Component := Make_Component_Declaration
                    (Defining_Identifier => Defining_Identifier (Parameter),
                     Subtype_Indication  => Par_Type);
                  Append_Node_To_List (Component, Components);
               end if;
               Parameter := Next_Node (Parameter);
            end loop;
         end if;

         Get_Name_String (BEN.Name (Defining_Identifier (Spec)));
         Add_Str_To_Name_Buffer ("_Args_Type_In");
         N := Make_Defining_Identifier (Name_Find);

         --  Type Declaration

         Args_Type := Make_Full_Type_Declaration
           (Defining_Identifier => N,
            Type_Definition     => Make_Derived_Type_Definition
            (Subtype_Indication    => RE (RE_Request_Args),
             Record_Extension_Part => Make_Record_Definition
             (Components)));

         Set_Homogeneous_Parent_Unit_Name
           (Defining_Identifier (Args_Type),
            Defining_Identifier (Aligned_Package (Current_Entity)));

         return Args_Type;
      end Args_Type_Record_In;

      function Args_Type_Record_Out (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);
         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         P          : constant List_Id := Parameter_Profile (Spec);
         T          : constant Node_Id := Return_Type (Spec);
         Components : List_Id;
         Component  : Node_Id;
         Parameter  : Node_Id;
         Args_Type  : Node_Id := No_Node;
         Par_Type   : Node_Id;
         N          : Node_Id;
      begin
         Set_Aligned_Spec;
         Components := New_List (K_Component_List);

         --  For each subprogram we generate a record containing the
         --  In parameter with the aligned type

         if not BEU.Is_Empty (P) then

            --  Skip the first parameter corresponding to 'Self'

            Parameter := Next_Node (First_Node (P));
            while Present (Parameter) loop

               if Is_Out (BEN.Parameter_Mode (Parameter)) then

                  --  If the parameter type is a class-wide type, we remove the
                  --  "'Class" attribute from the type name

                  Par_Type := Parameter_Type (Parameter);

                  if BEN.Kind (Par_Type) = K_Attribute_Designator then
                     Par_Type := Prefix (Par_Type);
                  end if;

                  Par_Type := Make_Type_Designator (FE_Node (Par_Type));

                  Component := Make_Component_Declaration
                    (Defining_Identifier => Defining_Identifier (Parameter),
                     Subtype_Indication  => Par_Type);
                  Append_Node_To_List (Component, Components);
               end if;
               Parameter := Next_Node (Parameter);
            end loop;
         end if;

         --  If the subprogram is a function, we add an additional member
         --  corresponding to the result of the function.

         if Present (T) then

            --  If the return type is a class-wide type, we remove the
            --  "'Class" attribute from the type name

            Par_Type := T;

            if BEN.Kind (Par_Type) = K_Attribute_Designator then
               Par_Type := Prefix (Par_Type);
            end if;

            Par_Type := Make_Type_Designator (FE_Node (Par_Type));

            Component := Make_Component_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Returns)),
               Subtype_Indication  => Par_Type);
            Append_Node_To_List (Component, Components);
         end if;

         Get_Name_String (BEN.Name (Defining_Identifier (Spec)));
         Add_Str_To_Name_Buffer ("_Args_Type_Out");
         N := Make_Defining_Identifier (Name_Find);

         --  Type Declaration

         Args_Type := Make_Full_Type_Declaration
           (Defining_Identifier => N,
            Type_Definition     => Make_Derived_Type_Definition
            (Subtype_Indication    => RE (RE_Request_Args),
             Record_Extension_Part => Make_Record_Definition
             (Components)));

         Set_Homogeneous_Parent_Unit_Name
           (Defining_Identifier (Args_Type),
            Defining_Identifier (Aligned_Package (Current_Entity)));

         return Args_Type;
      end Args_Type_Record_Out;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is
            when K_Module =>
               Visit_Module (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Structure_Type =>
               Visit_Structure_Type (E);

            when K_Union_Type =>
               Visit_Union_Type (E);

            when K_Type_Declaration =>
               Visit_Type_Declaration (E);

            when K_Specification =>
               Visit_Specification (E);

            when K_Enumeration_Type =>
               Visit_Enumeration_Type (E);

            when others =>
               null;
         end case;
      end Visit;

      function Make_Type_Designator (N : Node_Id)
                                    return Node_Id
      is
         Rewinded_Type : Node_Id;
         M             : Node_Id;
      begin
         Rewinded_Type := FEU.Get_Original_Type (N);
         Set_Aligned_Spec;

         case FEN.Kind (Rewinded_Type) is
            when K_String =>
               Variable_Parameter := True;
               return RE (RE_String_10);

            when K_Wide_String =>
               Variable_Parameter := True;
               return RE (RE_Wide_String_10);

            when K_String_Type =>
               M := Make_Designator (IDL_Name (Identifier (N)));
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Aligned_Package (Current_Entity)));
               return M;

            when K_Wide_String_Type =>
               M := Make_Designator (IDL_Name (Identifier (N)));
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Aligned_Package (Current_Entity)));
               return M;

            when K_Complex_Declarator =>
               M := Make_Designator (IDL_Name (Identifier (N)));
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Aligned_Package (Current_Entity)));
               return M;

            when K_Long =>
               return RE (RE_Long_10);

            when K_Short =>
               return RE (RE_Short_10);

            when K_Boolean =>
               return RE (RE_Boolean_10);

            when K_Octet =>
               return RE (RE_Octet_10);

            when K_Char =>
               return RE (RE_Char_10);

            when K_Wide_Char =>
               return RE (RE_Wide_Char_10);

            when K_Unsigned_Short =>
               return RE (RE_Unsigned_Short_10);

            when K_Unsigned_Long =>
               return RE (RE_Unsigned_Long_10);

            when K_Unsigned_Long_Long =>
               return RE (RE_Unsigned_Long_Long_10);

            when K_Long_Double =>
               return RE (RE_Long_Double_10);

            when K_Float =>
               return RE (RE_Float_10);

            when K_Double =>
               return RE (RE_Double_10);

            when K_Enumeration_Type =>
               return Make_Designator (IDL_Name (Identifier (Rewinded_Type)));

            when K_Sequence_Type =>
               Variable_Parameter := True;
               M := RE (RE_Sequence_10);
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Aligned_Package (Current_Entity)));
               return M;

            when K_Union_Type =>
               M := Make_Designator (IDL_Name (Identifier (Rewinded_Type)));
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Aligned_Package (Current_Entity)));
               return M;

            when K_Structure_Type =>
               M := Make_Designator (IDL_Name (Identifier (Rewinded_Type)));
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Aligned_Package (Current_Entity)));
               return M;

            when K_Fixed_Point_Type =>
               M := RE (RE_Fixed_Point_10);
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Aligned_Package (Current_Entity)));
               return M;

            when K_Object =>
               --  XXX is it right ?

               M := Make_Designator (FEN.Image (Base_Type (Rewinded_Type)));
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Main_Package (Current_Entity)));
               return M;

            when K_Interface_Declaration =>
               --  XXX is it right ?

               M := Make_Designator (IDL_Name (Identifier (Rewinded_Type)));
               Set_Homogeneous_Parent_Unit_Name
                 (M, Defining_Identifier (Main_Package (Current_Entity)));
               return M;

            when others =>
               --  If any problem print the node kind here

               raise Program_Error;
         end case;
      end Make_Type_Designator;

      ----------------------------
      -- Visit_Enumeration_Type --
      ----------------------------

      procedure Visit_Enumeration_Type (E : Node_Id) is
         Enumerator     : Node_Id;
         Enum_Literals  : List_Id;
         Enum_Literal   : Node_Id;
         Enum_Type_Decl : Node_Id;

      begin
         Set_Aligned_Spec;
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

         Append_Node_To_List
           (Enum_Type_Decl,
            Visible_Part (Current_Package));
      end Visit_Enumeration_Type;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         D                : Node_Id;
         T                : Node_Id;
         N                : Node_Id;
         Is_Subtype       : Boolean := False;
         Type_Spec_Node   : Node_Id;
      begin
         Set_Aligned_Spec;
         Type_Spec_Node := Type_Spec (E);

         --  The case of fixed point numbers is a special case :

         --  * The fixed type shall be mapped to an equivalent Ada
         --  decimal type

         --  * For each declarator, a type definition shall be
         --  generated.

         if FEN.Kind (Type_Spec_Node) = K_Fixed_Point_Type then
            declare
               Fixed_Type_Node : Node_Id;
               Fixed_Name      : constant Name_Id
                 := Map_Fixed_Type_Name (Type_Spec_Node);
            begin
               --  XXX it is certainly false. TODO : make a package
               --  instanciation at the marshalling time

               T := Make_Defining_Identifier (Fixed_Name);
               Set_Homogeneous_Parent_Unit_Name
                 (T, Defining_Identifier (Aligned_Package (Current_Entity)));

               Fixed_Type_Node := Make_Full_Type_Declaration
                 (Defining_Identifier => T,
                  Type_Definition     => Make_Decimal_Type_Definition
                  (Type_Spec_Node));

               Set_Homogeneous_Parent_Unit_Name
                 (Defining_Identifier (Fixed_Type_Node),
                  Defining_Identifier (Aligned_Package (Current_Entity)));

               Append_Node_To_List (Fixed_Type_Node,
                                    Visible_Part (Current_Package));

            end;
         elsif FEN.Kind (Type_Spec_Node) = K_Sequence_Type then
            declare
               Seq_Package_Inst : Node_Id;
               Bounded          : constant Boolean :=
                 Present (Max_Size (Type_Spec_Node));
               Seq        : Node_Id;
               Seq_Package_Name : Name_Id;
               Seq_Package_Node : Node_Id;
               Type_Node        : Node_Id;
            begin
               --  XXX : To be implemented in PolyORB.Aligned_Types
               --  We create an instanciation of the generic package
               --  PolyORB.Aligned_Types.Sequences.Bounded or
               --  PolyORB.Aligned_Types.Sequences.Unbounded.  Then,
               --  the sequence type is derived from the "Sequence"
               --  Type of the instanciated package.

               Seq_Package_Name := Map_Sequence_Pkg_Name (Type_Spec_Node);
               if Bounded then
                  Seq := RU (RU_PolyORB_Aligned_Types_Sequences_Bounded);
               else
                  Seq := RU (RU_PolyORB_Aligned_Types_Sequences_Unbounded);
               end if;

               --  Building the sequence package node

               Type_Node := Make_Type_Designator (Type_Spec (Type_Spec_Node));

               Seq_Package_Node := Make_Defining_Identifier
                 (Seq_Package_Name);
               Set_Homogeneous_Parent_Unit_Name
                 (Seq_Package_Node,
                  Defining_Identifier
                  (Aligned_Package (Current_Entity)));

               if Bounded then
                  Seq_Package_Inst := Make_Package_Instantiation
                    (Defining_Identifier => Seq_Package_Node,
                     Generic_Package     => Seq,
                     Parameter_List      => Make_List_Id
                     (Type_Node,
                      Make_Literal
                      (FEN.Value
                       (Max_Size
                        (Type_Spec_Node)))));
               else
                  Seq_Package_Inst := Make_Package_Instantiation
                    (Defining_Identifier => Seq_Package_Node,
                     Generic_Package     => Seq,
                     Parameter_List      => Make_List_Id (Type_Node));
               end if;

               Set_Homogeneous_Parent_Unit_Name
                 (Defining_Identifier (Seq_Package_Inst),
                  Defining_Identifier (Aligned_Package (Current_Entity)));

               Append_Node_To_List (Seq_Package_Inst,
                                    Visible_Part (Current_Package));

               T := Make_Defining_Identifier (TN (T_Sequence));
               Set_Homogeneous_Parent_Unit_Name (T, Seq_Package_Node);
            end;

         elsif FEN.Kind (Type_Spec_Node) = K_String_Type or else
           FEN.Kind (Type_Spec_Node) = K_Wide_String_Type then
            declare
               Str_Package_Inst : Node_Id;
               Pkg_Name         : Name_Id;
               Pkg_Node         : Node_Id;
               String_Pkg       : Node_Id;
            begin
               --  XXX : To be implemented in PolyORB.Aligned_Types
               --  We create an instanciation of the generic package
               --  PolyORB.Aligned_Types.Bounded_Strings (or
               --  PolyORB.Aligned_Types.Bounded_Wide_Strings).  Then,
               --  the string type is derived from the
               --  'Bounded_String' type (or the 'Bounded_Wide_String'
               --  type of the instanciated package.

               Pkg_Name := Map_String_Pkg_Name (Type_Spec_Node);

               if FEN.Kind (Type_Spec_Node) = K_Wide_String_Type then
                  String_Pkg :=
                    RU (RU_PolyORB_Aligned_Types_Bounded_Wide_Strings);
                  T := Make_Defining_Identifier (TN (T_Bounded_Wide_String));
               else
                  String_Pkg :=
                    RU (RU_PolyORB_Aligned_Types_Bounded_Strings);
                  T := Make_Defining_Identifier (TN (T_Bounded_String));
               end if;

               --  Building the string package node

               Pkg_Node := Make_Defining_Identifier
                 (Pkg_Name);

               Set_Homogeneous_Parent_Unit_Name
                 (Pkg_Node,
                  Defining_Identifier
                  (Aligned_Package (Current_Entity)));

               Str_Package_Inst := Make_Package_Instantiation
                 (Defining_Identifier => Pkg_Node,
                  Generic_Package     => String_Pkg,
                  Parameter_List      => Make_List_Id
                  (Make_Literal (FEN.Value (Max_Size (Type_Spec_Node)))));

               Set_Homogeneous_Parent_Unit_Name
                 (Defining_Identifier (Str_Package_Inst),
                  Defining_Identifier (Aligned_Package (Current_Entity)));

               Append_Node_To_List (Str_Package_Inst,
                                    Visible_Part (Current_Package));

               Set_Homogeneous_Parent_Unit_Name (T, Pkg_Node);
            end;
         else
            --  General case

            T := Make_Type_Designator (Type_Spec_Node);
         end if;

         --  According to the Ada mapping specification. Most of the
         --  type definitions in an IDL file should be mapped to :
         --  "type ... is new ...;". However, there are exception to
         --  this rule : "interface Base {...}; typedef Base Root;"
         --  sould be mapped : "subtype Root is Base.Ref;"

         --  Determining wether we map the type definition to a "type
         --  ... is new ...;" or a "subtype ... is ...;" statement.

         Is_Subtype := Is_Object_Type (Type_Spec (E));

         D := First_Entity (Declarators (E));
         while Present (D) loop
            if Kind (D) = K_Complex_Declarator then
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Map_Defining_Identifier (D),
                  Type_Definition     =>
                    Make_Array_Type_Definition
                  (Map_Range_Constraints
                   (FEN.Array_Sizes (D))
                   , T));
            else
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Map_Defining_Identifier (D),
                  Type_Definition     => Make_Derived_Type_Definition
                  (Subtype_Indication    => T,
                   Record_Extension_Part => No_Node,
                   Is_Subtype => Is_Subtype),
                  Is_Subtype => Is_Subtype);
            end if;

            Set_Homogeneous_Parent_Unit_Name
              (Defining_Identifier (N),
               Defining_Identifier (Aligned_Package (Current_Entity)));

            Append_Node_To_List (N, Visible_Part (Current_Package));
            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Aligned_Spec;

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         Pop_Entity;
      end Visit_Interface_Declaration;

      --------------------------
      -- Visit_Structure_Type --
      --------------------------

      procedure Visit_Structure_Type (E : Node_Id) is
         Components : constant List_Id := New_List (K_Component_List);
         N          : Node_Id;
         M          : Node_Id;
         Member     : Node_Id;
         Desc       : Node_Id;
         Range_Constraint : Node_Id;
      begin
         Discriminants := New_List (K_Component_List);
         Variable_Parameter := False;
         Set_Aligned_Spec;
         Member := First_Entity (Members (E));
         while Present (Member) loop
            N := Map_Defining_Identifier
              (Identifier (First_Entity (Declarators (Member))));

            M := Make_Type_Designator (Type_Spec (Member));

            if Variable_Parameter then
               --  If there is any unbounded type we make a
               --  descriminant for the generated type

               Get_Name_String
                 (IDL_Name (Identifier (First_Entity (Declarators (Member)))));
               Add_Str_To_Name_Buffer ("_Size");
               Desc := Make_Defining_Identifier (Name_Find);

               --  Add a new descriminant in the discrilinant list

               Append_Node_To_List
                 (Make_Component_Declaration (Desc, RE (RE_Natural)),
                  Discriminants);

               Range_Constraint := New_Node (K_Range_Constraint);
               Set_First (Range_Constraint, Make_Literal (Int1_Val));
               Set_Last (Range_Constraint, Desc);

               --  XXX : temporary just for testing.

               M := Make_String_Type_Definition
                 (M, Range_Constraint);
               Variable_Parameter := False;
            end if;

            N := Make_Component_Declaration
              (Defining_Identifier => N,
               Subtype_Indication  => M);

            Append_Node_To_List (N, Components);
            Member := Next_Entity (Member);
         end loop;

         N := Make_Defining_Identifier (FEN.IDL_Name (FEN.Identifier (E)));

         N := Make_Full_Type_Declaration
           (Defining_Identifier   => N,
            Type_Definition       => Make_Record_Definition (Components),
            Discriminant_Spec     => Discriminants);

         Set_Homogeneous_Parent_Unit_Name
           (Defining_Identifier (N),
            Defining_Identifier (Aligned_Package (Current_Entity)));

         Append_Node_To_List (N, Visible_Part (Current_Package));
         Discriminants := No_List;
      end Visit_Structure_Type;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N              : Node_Id;
         S              : constant Node_Id := Switch_Type_Spec (E);
         Orig_Type      : constant Node_Id := FEU.Get_Original_Type (S);
         T              : Node_Id;
         L              : List_Id;
         Literal_Parent : Node_Id := No_Node;
         Member         : Node_Id;
         M              : Node_Id;
         Variants       : List_Id;
         Variant        : Node_Id;
         Choices        : List_Id;
         Label          : Node_Id;
         Choice         : Node_Id;
      begin
         Set_Aligned_Spec;
         T := Make_Type_Designator (S);

         --  If the discriminator is an enumeration type, we must put
         --  the full names of literals

         if FEN.Kind (Orig_Type) = K_Enumeration_Type then
            Literal_Parent := Map_Designator
              (Scope_Entity
               (Identifier
                (Orig_Type)));
         end if;

         Variants := New_List (K_Variant_List);
         Member := First_Entity (Switch_Type_Body (E));
         while Present (Member) loop
            Variant := New_Node (K_Variant);
            Choices := New_List (K_Discrete_Choice_List);
            Set_Discrete_Choices (Variant, Choices);
            Label   := First_Entity (Labels (Member));
            while Present (Label) loop
               Choice := Make_Literal
                 (Value             => FEN.Value (Label),
                  Parent_Designator => Literal_Parent);
               Append_Node_To_List (Choice, Choices);
               Label := Next_Entity (Label);
            end loop;

            N := Map_Defining_Identifier
              (Identifier (Declarator (Element (Member))));

            M := Make_Type_Designator (Type_Spec (Element (Member)));

            Set_Component (Variant, Make_Component_Declaration (N, M));

            Append_Node_To_List (Variant, Variants);
            Member := Next_Entity (Member);
         end loop;

         L := New_List (K_Component_List);
         Append_Node_To_List
           (Make_Variant_Part
            (Make_Defining_Identifier (CN (C_Switch)),
             Variants),
            L);
         N := Make_Defining_Identifier (FEN.IDL_Name (FEN.Identifier (E)));
         N := Make_Full_Type_Declaration
           (N,
            Make_Record_Type_Definition
            (Make_Record_Definition (L)),
            Make_List_Id
            (Make_Component_Declaration
             (Make_Defining_Identifier (CN (C_Switch)), T,
              Make_Type_Attribute (T, A_First))));
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Union_Type;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Aligned_Spec) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
      end  Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N     : Node_Id;
      begin
         Set_Aligned_Spec;

         --  Explaining comment

         Set_Str_To_Name_Buffer
           ("Operation : ");

         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         --  Generating the 'Operation_Name'_Args_Type declaration

         N := Args_Type_Record_In (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         N := Args_Type_Record_Out (E);
         Append_Node_To_List (N, Visible_Part (Current_Package));
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
   end Package_Spec;
end Backend.BE_CORBA_Ada.Aligned;
