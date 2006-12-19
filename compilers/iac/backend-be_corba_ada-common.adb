------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--          B A C K E N D . B E _ C O R B A _ A D A . C O M M O N           --
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
with Values;
with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Expand;      use Backend.BE_CORBA_Ada.Expand;

package body Backend.BE_CORBA_Ada.Common is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;

   -------------------------------------
   -- Cast_Variable_From_PolyORB_Type --
   -------------------------------------

   function Cast_Variable_From_PolyORB_Type
     (Var_Name : Name_Id; Var_Type : Node_Id)
     return Node_Id
   is
      N                : Node_Id;
      Orig_Type        : Node_Id;
      Direct_Type_Node : Node_Id;
   begin
      N := Make_Designator (Var_Name);

      Orig_Type := FEU.Get_Original_Type (Var_Type);

      if FEN.Kind (Var_Type) = K_Simple_Declarator
        or else FEN.Kind (Var_Type) = K_Complex_Declarator
      then
         Direct_Type_Node := Type_Spec (Declaration (Var_Type));
      else
         Direct_Type_Node := Var_Type;
      end if;

      case FEN.Kind (Orig_Type) is

         when K_String =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String_1),
                  Make_List_Id (N));
               N := Make_Subprogram_Call
                 (RE (RE_To_CORBA_String),
                  Make_List_Id (N));
               if FEN.Kind (Direct_Type_Node) /= K_String then
                  N := Make_Subprogram_Call
                    (Map_Designator (Direct_Type_Node),
                     Make_List_Id (N));
               end if;
            end;

         when K_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Convert_Subp := Make_Designator
                 (SN (S_To_Bounded_String));
               Set_Homogeneous_Parent_Unit_Name
                 (Str_Convert_Subp, Str_Package_Node);

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String_1),
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (Map_Designator (Direct_Type_Node),
                  Make_List_Id (N));
            end;

         when K_Wide_String =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_Wide_String_1),
                  Make_List_Id (N));
               N := Make_Subprogram_Call
                 (RE (RE_To_CORBA_Wide_String),
                  Make_List_Id (N));
               if FEN.Kind (Direct_Type_Node) /= K_Wide_String then
                  N := Make_Subprogram_Call
                    (Map_Designator (Direct_Type_Node),
                     Make_List_Id (N));
               end if;
            end;

         when K_Wide_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Convert_Subp := Make_Designator
                 (SN (S_To_Bounded_Wide_String));
               Set_Homogeneous_Parent_Unit_Name
                 (Str_Convert_Subp, Str_Package_Node);

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_Wide_String_1),
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (Map_Designator (Direct_Type_Node),
                  Make_List_Id (N));
            end;

         when K_Long
           | K_Long_Long
           | K_Unsigned_Long
           | K_Unsigned_Long_Long
           | K_Float
           | K_Double
           | K_Long_Double
           | K_Char
           | K_Wide_Char
           | K_Octet
           | K_Sequence_Type
           | K_Short
           | K_Unsigned_Short
           | K_Boolean
           | K_Fixed_Point_Type =>
            declare
               CORBA_Type : constant Node_Id := Map_Designator
                 (Direct_Type_Node);
            begin
               N := Make_Subprogram_Call
                 (CORBA_Type,
                  Make_List_Id (N));
            end;

            --  For Objects and interfaces, there is no need to cast
            --  to the original type because the type definition is
            --  done by means of 'subtype' and not 'type ... is new
            --  ...'

         when K_Object =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_CORBA_Ref),
                  Make_List_Id (N));
            end;

         when K_Interface_Declaration =>
            declare
               To_Ref_Node : constant Node_Id := Expand_Designator
                 (BEN.To_Ref_Node
                  (BE_Node
                   (Identifier
                    (Orig_Type))));
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_CORBA_Ref),
                  Make_List_Id (N));
               N := Make_Subprogram_Call
                 (To_Ref_Node,
                  Make_List_Id (N));
            end;

         when K_Enumeration_Type =>
            declare
               CORBA_Type : constant Node_Id := Map_Designator
                 (Direct_Type_Node);
               M : Node_Id;
            begin
               --  Even if the type is not directly an enumeration and
               --  is defined basing on an enumeration, we still have
               --  access to the 'Val attribute. So there is no need
               --  to cast the variable to the original enumeration
               --  type.

               M := Make_Type_Attribute (CORBA_Type, A_Val);
               N := Make_Subprogram_Call (M, Make_List_Id (N));
            end;

         when others =>
            null;
      end case;

      return N;
   end Cast_Variable_From_PolyORB_Type;

   -----------------------------------
   -- Cast_Variable_To_PolyORB_Type --
   -----------------------------------

   function Cast_Variable_To_PolyORB_Type
     (Var_Node : Node_Id; Var_Type : Node_Id)
     return Node_Id
   is
      N         : Node_Id;
      Orig_Type : Node_Id;
   begin
      N := Var_Node;

      Orig_Type := FEU.Get_Original_Type (Var_Type);

      case FEN.Kind (Orig_Type) is

         when K_Long =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Long_1), Make_List_Id (N));
            end;

         when K_Long_Long =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Long_Long_1), Make_List_Id (N));
            end;

         when K_Unsigned_Long =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Long_1), Make_List_Id (N));
            end;

         when K_Unsigned_Long_Long =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Long_Long_1), Make_List_Id (N));
            end;

         when K_Short =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Short_1), Make_List_Id (N));
            end;

         when K_Unsigned_Short =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Short_1), Make_List_Id (N));
            end;

         when K_Float =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Float_1), Make_List_Id (N));
            end;

         when K_Double =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Double_1), Make_List_Id (N));
            end;

         when K_Long_Double =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Long_Double_1), Make_List_Id (N));
            end;

         when K_Char =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Char_1), Make_List_Id (N));
            end;

         when K_Wide_Char =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Wchar_1), Make_List_Id (N));
            end;

         when K_Octet =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Octet_1), Make_List_Id (N));
            end;

         when K_Boolean =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Boolean_1), Make_List_Id (N));
            end;

         when K_Fixed_Point_Type =>
            declare
               FP_Type_Node     : Node_Id;
            begin

               --  Getting the fixed point type

               FP_Type_Node := Expand_Designator
                 (Type_Def_Node (BE_Node (Orig_Type)));

               N := Make_Subprogram_Call
                 (FP_Type_Node, Make_List_Id (N));
            end;

         when K_Object =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Ref), Make_List_Id (N));
            end;

         when K_Interface_Declaration =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Ref_2), Make_List_Id (N));
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Ref), Make_List_Id (N));
            end;

         when K_Enumeration_Type =>
            declare
               Ada_Enum_Type : constant Node_Id := Expand_Designator
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (Orig_Type))));
               M : Node_Id;
            begin
               if FEN.Kind (Var_Type) = K_Scoped_Name
                 and then FEN.Kind (Reference (Var_Type))
                 /= K_Enumeration_Type
               then
                  N := Make_Subprogram_Call
                    (Ada_Enum_Type,
                     Make_List_Id (N));
               end if;

               --  Even if the type is not directly an enumeration and
               --  is defined basing on an enumeration, we still have
               --  access to the 'Pos' attribute. So there is no need
               --  to cast the variable to the original enumeration
               --  type.

               M := Make_Type_Attribute (Ada_Enum_Type, A_Pos);
               M := Make_Subprogram_Call (M, Make_List_Id (N));
               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Long_1),
                  Make_List_Id (M));
            end;

         when K_String =>
            begin
               if FEN.Kind (Var_Type) /= K_String then
                  N := Make_Subprogram_Call
                    (RE (RE_String_0),
                     Make_List_Id (N));
               end if;

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String),
                  Make_List_Id (N));
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String),
                  Make_List_Id (N));
            end;

         when K_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Type         : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Type := Make_Designator (TN (T_Bounded_String));
               Set_Homogeneous_Parent_Unit_Name (Str_Type, Str_Package_Node);

               Str_Convert_Subp := Make_Designator (SN (S_To_String));
               Set_Homogeneous_Parent_Unit_Name
                 (Str_Convert_Subp, Str_Package_Node);

               N := Make_Subprogram_Call
                 (Str_Type,
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String),
                  Make_List_Id (N));
            end;

         when K_Wide_String =>
            begin
               if FEN.Kind (Var_Type) /= K_Wide_String then
                  N := Make_Subprogram_Call
                    (RE (RE_Wide_String),
                     Make_List_Id (N));
               end if;

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_Wide_String),
                  Make_List_Id (N));
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Wide_String),
                  Make_List_Id (N));
            end;

         when K_Wide_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Type         : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Type := Make_Designator (TN (T_Bounded_Wide_String));
               Set_Homogeneous_Parent_Unit_Name (Str_Type, Str_Package_Node);

               Str_Convert_Subp := Make_Designator
                 (SN (S_To_Wide_String));
               Set_Homogeneous_Parent_Unit_Name
                 (Str_Convert_Subp, Str_Package_Node);

               N := Make_Subprogram_Call
                 (Str_Type,
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Wide_String),
                  Make_List_Id (N));
            end;

         when K_Sequence_Type =>
            declare
               Seq_Package_Node : Node_Id;
               Seq_Type         : Node_Id;
            begin

               --  Getting the instantiated package node

               Seq_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Sequence type

               Seq_Type := Make_Designator (TN (T_Sequence));
               Set_Homogeneous_Parent_Unit_Name (Seq_Type, Seq_Package_Node);

               N := Make_Subprogram_Call
                 (Seq_Type,
                  Make_List_Id (N));
            end;

         when others =>
            null;
      end case;

      return N;
   end Cast_Variable_To_PolyORB_Type;

   -----------
   -- Is_In --
   -----------

   function Is_In (Par_Mode : Mode_Id) return Boolean is
   begin
      return Par_Mode = Mode_In or else Par_Mode = Mode_Inout;
   end Is_In;

   ------------
   -- Is_Out --
   ------------

   function Is_Out (Par_Mode : Mode_Id) return Boolean is
   begin
      return Par_Mode = Mode_Out or else Par_Mode = Mode_Inout;
   end Is_Out;

   ----------------------------
   -- Contains_In_Parameters --
   ----------------------------

   function Contains_In_Parameters (E : Node_Id) return Boolean is
      pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

      Parameter : Node_Id;
      Result    : Boolean := False;
   begin
      Parameter := First_Entity (Parameters (E));

      while Present (Parameter) loop
         if Is_In (FEN.Parameter_Mode (Parameter)) then
            Result := True;
            exit;
         end if;

         Parameter := Next_Entity (Parameter);
      end loop;

      return Result;
   end Contains_In_Parameters;

   -----------------------------
   -- Contains_Out_Parameters --
   -----------------------------

   function Contains_Out_Parameters (E : Node_Id) return Boolean is
      pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

      Parameter : Node_Id;
      Result    : Boolean := False;
   begin
      Parameter := First_Entity (Parameters (E));

      while Present (Parameter) loop
         if Is_Out (FEN.Parameter_Mode (Parameter)) then
            Result := True;
            exit;
         end if;

         Parameter := Next_Entity (Parameter);
      end loop;

      return Result;
   end Contains_Out_Parameters;

   --------------------------
   -- Make_type_Designator --
   --------------------------

   function Make_Type_Designator (N          : Node_Id;
                                  Declarator : Node_Id := No_Node)
                                 return Node_Id
   is
      Rewinded_Type : Node_Id;
      M             : Node_Id;
   begin
      Set_Aligned_Spec;
      Rewinded_Type := FEU.Get_Original_Type (N);

      if Present (Declarator) and then
        FEN.Kind (Declarator) = K_Complex_Declarator then
         declare
            Designator : Node_Id;
            Decl_Name  : Name_Id;
            Type_Node  : Node_Id;
         begin
            Decl_Name := To_Ada_Name
              (IDL_Name (FEN.Identifier (Declarator)));
            Designator := Make_Type_Designator (N);

            Get_Name_String (Decl_Name);
            Add_Str_To_Name_Buffer ("_Array");
            Decl_Name := Name_Find;

            Type_Node := Make_Full_Type_Declaration
              (Defining_Identifier => Make_Defining_Identifier (Decl_Name),
               Type_Definition     => Make_Array_Type_Definition
               (Map_Range_Constraints
                (FEN.Array_Sizes (Declarator)), Designator));

            Set_Homogeneous_Parent_Unit_Name
              (Defining_Identifier (Type_Node),
               (Defining_Identifier (Aligned_Package (Current_Entity))));

            --  We make a link between the identifier and the type
            --  declaration.  This link is useful for the generation
            --  of the From_Any and To_Any functions and the TC_XXX
            --  constant necessary for user defined types.

            Append_Node_To_List
              (Type_Node,
               Visible_Part (Current_Package));

            Designator := New_Node (K_Designator);
            Set_Defining_Identifier
              (Designator, Defining_Identifier (Type_Node));
            Set_Homogeneous_Parent_Unit_Name
              (Designator,
               (Defining_Identifier (Main_Package (Current_Entity))));

            return Designator;
         end;
      end if;

      case FEN.Kind (Rewinded_Type) is

         when K_String =>
            return RE (RE_String_10);

         when K_Sequence_Type =>
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
            return RE (RE_Wchar_10);

         when K_Unsigned_Short =>
            return RE (RE_Unsigned_Short_10);

         when K_Unsigned_Long
           | K_Enumeration_Type =>
            return RE (RE_Unsigned_Long_10);

         when K_Unsigned_Long_Long =>
            return RE (RE_Unsigned_Long_Long_10);

         when K_Long_Double =>
            return RE (RE_Long_Double_10);

         when K_Float =>
            return RE (RE_Float_10);

         when K_Double =>
            return RE (RE_Double_10);

         when K_Complex_Declarator =>
            M := Make_Designator (IDL_Name (Identifier (N)));
            Set_Homogeneous_Parent_Unit_Name
              (M, Defining_Identifier (Aligned_Package (Current_Entity)));
            return M;

         when K_String_Type
           | K_Wide_String_Type
           | K_Structure_Type
           | K_Union_Type
           | K_Fixed_Point_Type =>

            M := Make_Designator (IDL_Name (Identifier (N)));
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

   -------------------------------------------
   -- Cast_Variable_To_PolyORB_Aligned_Type --
   -------------------------------------------

   function Cast_Variable_To_PolyORB_Aligned_Type
     (Var_Node : Node_Id; Var_Type : Node_Id)
     return Node_Id
   is
      N         : Node_Id;
      Orig_Type : Node_Id;
   begin
      N := Var_Node;

      Orig_Type := FEU.Get_Original_Type (Var_Type);

      case FEN.Kind (Orig_Type) is

         when K_Long =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Long_10), Make_List_Id (N));
            end;

         when K_Long_Long =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Long_Long_10), Make_List_Id (N));
            end;

         when K_Unsigned_Long =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Long_10), Make_List_Id (N));
            end;

         when K_Unsigned_Long_Long =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Long_Long_10), Make_List_Id (N));
            end;

         when K_Short =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Short_10), Make_List_Id (N));
            end;

         when K_Unsigned_Short =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Short_10), Make_List_Id (N));
            end;

         when K_Float =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Float_10), Make_List_Id (N));
            end;

         when K_Double =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Double_10), Make_List_Id (N));
            end;

         when K_Long_Double =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Long_Double_10), Make_List_Id (N));
            end;

         when K_Char =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Char_10), Make_List_Id (N));
            end;

         when K_Octet =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Octet_10), Make_List_Id (N));
            end;

         when K_Boolean =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_Boolean_10), Make_List_Id (N));
            end;

         when K_Fixed_Point_Type =>
            declare
               FP_Type_Node     : Node_Id;
            begin
               --  Getting the fixed point type

               FP_Type_Node := Expand_Designator
                 (Type_Def_Node (BE_Node (Orig_Type)));

               N := Make_Subprogram_Call
                 (FP_Type_Node, Make_List_Id (N));
            end;

         when K_Enumeration_Type =>
            declare
               Ada_Enum_Type : constant Node_Id := Expand_Designator
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (Orig_Type))));
               M : Node_Id;
            begin
               if FEN.Kind (Var_Type) = K_Scoped_Name
                 and then FEN.Kind (Reference (Var_Type))
                 /= K_Enumeration_Type
               then
                  N := Make_Subprogram_Call
                    (Ada_Enum_Type,
                     Make_List_Id (N));
               end if;

               --  Even if the type is not directly an enumeration and
               --  is defined basing on an enumeration, we still have
               --  access to the 'Pos' attribute. So there is no need
               --  to cast the variable to the original enumeration
               --  type.

               M := Make_Type_Attribute (Ada_Enum_Type, A_Pos);
               M := Make_Subprogram_Call (M, Make_List_Id (N));
               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Long_10),
                  Make_List_Id (M));
            end;

         when K_String =>
            begin
               if FEN.Kind (Var_Type) /= K_String then
                  N := Make_Subprogram_Call
                    (RE (RE_String_0),
                     Make_List_Id (N));
               end if;

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String),
                  Make_List_Id (N));
            end;

         when K_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Type         : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Type := Make_Designator (TN (T_Bounded_String));
               Set_Homogeneous_Parent_Unit_Name (Str_Type, Str_Package_Node);

               Str_Convert_Subp := Make_Designator (SN (S_To_String));
               Set_Homogeneous_Parent_Unit_Name
                 (Str_Convert_Subp, Str_Package_Node);

               N := Make_Subprogram_Call
                 (Str_Type,
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  Make_List_Id (N));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String),
                  Make_List_Id (N));
            end;

         when K_Sequence_Type =>
            declare
               Seq_Package_Node : Node_Id;
               Seq_Type         : Node_Id;
            begin
               --  Getting the instantiated package node in aligned
               --  backend

               Seq_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Sequence type

               Seq_Type := Make_Designator (TN (T_Sequence));
               Set_Homogeneous_Parent_Unit_Name (Seq_Type, Seq_Package_Node);

               N := Make_Subprogram_Call
                 (Seq_Type,
                  Make_List_Id (N));
            end;

         when others =>
            null;
      end case;

      return N;
   end Cast_Variable_To_PolyORB_Aligned_Type;

   -------------------
   -- Marshall_Args --
   -------------------

   procedure Marshall_Args
     (Stat     : List_Id;
      Var_Type : Node_Id;
      Var      : Node_Id;
      Var_Exp  : Node_Id := No_Node)
   is
      Rewinded_Type : Node_Id;
      C             : Node_Id;
      N             : Node_Id;
      M             : Node_Id;
   begin
      Rewinded_Type := FEU.Get_Original_Type (Var_Type);

      case FEN.Kind (Rewinded_Type) is
         when K_Structure_Type =>
            declare
               Member : Node_Id;
            begin
               Member := First_Entity (Members (Rewinded_Type));
               while Present (Member) loop
                  C := Make_Designator
                    (IDL_Name
                     (Identifier
                      (First_Entity (Declarators (Member)))));
                  Set_Homogeneous_Parent_Unit_Name (C, Var);

                  if Var_Exp /= No_Node then
                     M := Make_Designator
                       (IDL_Name
                        (Identifier
                         (First_Entity (Declarators (Member)))));
                     Set_Homogeneous_Parent_Unit_Name (M, Var_Exp);
                     Marshall_Args (Stat, Type_Spec (Member), C, M);
                  else
                     Marshall_Args (Stat, Type_Spec (Member), C);
                  end if;
                  Member := Next_Entity (Member);
               end loop;
               return;
            end;

         when K_Union_Type =>
            declare
               Member  : Node_Id;
               L       : List_Id;
               Variant : Node_Id;
               Choice  : Node_Id;
               Literal_Parent : Node_Id := No_Node;
               Label   : Node_Id;
               Choices : List_Id;
               Switch_Alternatives : List_Id;
               Switch_Node : Node_Id;
            begin
               Switch_Node := Make_Designator (CN (C_Switch));
               if Var_Exp /= No_Node then
                  Set_Homogeneous_Parent_Unit_Name (Switch_Node, Var_Exp);
               else
                  Set_Homogeneous_Parent_Unit_Name (Switch_Node, Var);
               end if;

               C := FEU.Get_Original_Type
                 (Switch_Type_Spec (Rewinded_Type));

               if FEN.Kind (C) = K_Enumeration_Type then
                  Literal_Parent := Map_Designator
                    (Scope_Entity
                     (Identifier
                      (C)));
               end if;

               Switch_Alternatives := New_List (K_Variant_List);
               Member := First_Entity
                 (Switch_Type_Body (Rewinded_Type));

               while Present (Member) loop
                  Variant := New_Node (K_Variant);
                  Choices := New_List (K_Discrete_Choice_List);
                  Label   := First_Entity (Labels (Member));

                  while Present (Label) loop
                     Choice := Make_Literal
                       (Value             => FEN.Value (Label),
                        Parent_Designator => Literal_Parent);
                     Append_Node_To_List (Choice, Choices);
                     Label := Next_Entity (Label);
                  end loop;

                  L := New_List (K_List_Id);
                  C := Make_Designator
                    (IDL_Name
                     (Identifier
                      (Declarator (Element (Member)))));
                  Set_Homogeneous_Parent_Unit_Name (C, Var);

                  if Var_Exp /= No_Node then
                     M := Make_Designator
                       (IDL_Name
                        (Identifier
                         (Declarator (Element (Member)))));
                     Set_Homogeneous_Parent_Unit_Name (M, Var_Exp);
                     Marshall_Args (L, Type_Spec (Element (Member)), C, M);
                  else
                     Marshall_Args (L, Type_Spec (Element (Member)), C);
                  end if;

                  --  Building the switch alternative

                  N := Make_Block_Statement
                    (Declarative_Part => No_List,
                     Statements       => L);

                  Set_Component (Variant, N);
                  Set_Discrete_Choices (Variant, Choices);
                  Append_Node_To_List (Variant, Switch_Alternatives);

                  Member := Next_Entity (Member);
               end loop;

               N := Make_Variant_Part
                 (Switch_Node,
                  Switch_Alternatives);
               Append_Node_To_List (N, Stat);
               return;
            end;

         when K_String =>
            if Var_Exp /= No_Node then
               M := Cast_Variable_To_PolyORB_Aligned_Type (Var_Exp, Var_Type);
            else
               M := Cast_Variable_To_PolyORB_Aligned_Type (Var, Var_Type);
            end if;

            C := RE (RE_Nul);
            M := Make_Expression (M, Op_And_Symbol, C);
            C := Cast_Variable_To_PolyORB_Aligned_Type (Var, Var_Type);
            N := Make_Designator
              (Designator => PN (P_Content),
               Parent     => Fully_Qualified_Name (Var));

            N := Make_Designator (Fully_Qualified_Name (N));
            if Var_Exp /= No_Node then
               Set_Homogeneous_Parent_Unit_Name
                 (N, Make_Designator (VN (V_Args_Out)));
            else
               Set_Homogeneous_Parent_Unit_Name
                 (N, Make_Designator (VN (V_Args_In)));
            end if;

            N := Make_Assignment_Statement (N, M);
            Append_Node_To_List (N, Stat);
            return;

         when K_Sequence_Type =>
            declare
               Range_Constraint : Node_Id;
               Index_Node       : Node_Id;
               K                : Node_Id;
            begin
               Set_Str_To_Name_Buffer ("J");
               Index_Node := Make_Defining_Identifier (Name_Find);

               if Var_Exp /= No_Node then
                  N := Make_Subprogram_Call
                    (RE (RE_Length_2), Make_List_Id (Var_Exp));
               else
                  N := Make_Subprogram_Call
                    (RE (RE_Length_2), Make_List_Id (Var));
               end if;

               N := Make_Subprogram_Call
                 (RE (RE_Unsigned_Long_10), Make_List_Id (N));

               Range_Constraint := Make_Range_Constraint
                 (Make_Literal (Int1_Val), N);

               N := Make_Designator
                 (Designator => PN (P_Content),
                  Parent     => Fully_Qualified_Name (Var));

               N := Make_Designator (Fully_Qualified_Name (N));
               if Var_Exp /= No_Node then
                  Set_Homogeneous_Parent_Unit_Name
                    (N, Make_Designator (VN (V_Args_Out)));
                  N := Make_Designator (Fully_Qualified_Name (N));
                  N := Make_Subprogram_Call
                    (N, Make_List_Id (Index_Node));

                  M := Make_Designator (Fully_Qualified_Name (Var_Exp));
                  K := Make_Subprogram_Call
                    (RE (RE_Integer), Make_List_Id (Index_Node));
                  M := Make_Subprogram_Call
                    (RE (RE_Get_Element), Make_List_Id (M, K));

                  M := Cast_Variable_To_PolyORB_Aligned_Type
                    (M,
                     Type_Spec
                     (Type_Spec
                      (Declaration (Reference (Var_Type)))));

                  N := Make_Assignment_Statement (N, M);
               else
                  Set_Homogeneous_Parent_Unit_Name
                    (N, Make_Designator (VN (V_Args_In)));
                  N := Make_Designator (Fully_Qualified_Name (N));
                  N := Make_Subprogram_Call
                    (N, Make_List_Id (Index_Node));

                  M := Make_Designator (Fully_Qualified_Name (Var));
                  K := Make_Subprogram_Call
                    (RE (RE_Integer), Make_List_Id (Index_Node));
                  M := Make_Subprogram_Call
                    (RE (RE_Get_Element), Make_List_Id (M, K));

                  M := Cast_Variable_To_PolyORB_Aligned_Type
                    (M,
                     Type_Spec
                     (Type_Spec
                      (Declaration (Reference (Var_Type)))));

                  N := Make_Assignment_Statement (N, M);
               end if;

               N := Make_For_Statement
                 (Index_Node, Range_Constraint, Make_List_Id (N));
               Append_Node_To_List (N, Stat);

               return;
            end;

         when K_Complex_Declarator =>
            M := Make_Designator (IDL_Name (Identifier (Var_Type)));
            Set_Homogeneous_Parent_Unit_Name
              (M, Defining_Identifier (Aligned_Package (Current_Entity)));
            M := Make_Subprogram_Call (M, Make_List_Id (Var));

         when others =>
            if Var_Exp /= No_Node then
               M := Cast_Variable_To_PolyORB_Aligned_Type (Var_Exp, Var_Type);
            else
               M := Cast_Variable_To_PolyORB_Aligned_Type (Var, Var_Type);
            end if;
            C := Cast_Variable_To_PolyORB_Aligned_Type (Var, Var_Type);
      end case;

      N := Make_Designator (Fully_Qualified_Name (Var));

      if Var_Exp /= No_Node then
         Set_Homogeneous_Parent_Unit_Name
           (N, Make_Designator (VN (V_Args_Out)));
      else
         Set_Homogeneous_Parent_Unit_Name
           (N, Make_Designator (VN (V_Args_In)));
      end if;

      N := Make_Assignment_Statement (N, M);
      Append_Node_To_List (N, Stat);
   end Marshall_Args;

   -----------------------------
   -- Get_Discriminants_Value --
   -----------------------------

   procedure Get_Discriminants_Value (P      : Node_Id;
                                      N      : Node_Id;
                                      L      : List_Id;
                                      Ret    : Boolean := False)
   is
      Rewinded_Type : Node_Id;
      Var           : Node_Id;
      M             : Node_Id;
      C             : Node_Id;
   begin
      --  Handle the case of non void operation having OUT parameters

      if FEN.Kind (P) = K_Parameter_Declaration then
         Var := Map_Defining_Identifier (Declarator (P));
      else
         Var := Make_Defining_Identifier (PN (P_Returns));
      end if;

      Rewinded_Type := FEU.Get_Original_Type (N);

      case FEN.Kind (Rewinded_Type) is
         when K_Union_Type =>
            declare
               Member : Node_Id;
            begin
               Member := First_Entity (Switch_Type_Body (Rewinded_Type));

               while Present (Member) loop
                  M := Make_Defining_Identifier
                    (IDL_Name
                     (Identifier
                      (Declarator (Element (Member)))));
                  Set_Homogeneous_Parent_Unit_Name
                    (M, Var);
                  Get_Discriminants_Value
                    (M, Type_Spec (Element (Member)), L);
                  Member := Next_Entity (Member);
               end loop;

               M := Make_Designator (CN (C_Switch));
               Set_Homogeneous_Parent_Unit_Name (M, Var);

               C := Switch_Type_Spec (Rewinded_Type);
               M := Cast_Variable_To_PolyORB_Aligned_Type (M, C);
               Append_Node_To_List (M, L);
            end;

         when K_Structure_Type =>
            declare
               Member : Node_Id;
            begin
               Member := First_Entity (Members (Rewinded_Type));

               while Present (Member) loop
                  M := Make_Defining_Identifier
                    (IDL_Name
                     (Identifier
                      (First_Entity
                       (Declarators (Member)))));
                  Set_Homogeneous_Parent_Unit_Name
                    (M, Var);
                  Get_Discriminants_Value (M, Type_Spec (Member), L, Ret);
                  Member := Next_Entity (Member);
               end loop;
            end;

         when K_String
           | K_Wide_String =>
            C := Make_Attribute_Designator
              (Make_Subprogram_Call
               (RE (RE_To_Standard_String), Make_List_Id (Var)),
               A_Length);
            C := Make_Expression
              (C, Op_Plus, Make_Literal (Values.New_Integer_Value (1, 1, 10)));
            Append_Node_To_List (C, L);

         when K_Sequence_Type =>

            if not Present (Max_Size (Rewinded_Type)) then
               C := Make_Subprogram_Call
                 (RE (RE_Length_2), Make_List_Id (Var));
               C := Make_Subprogram_Call
                 (RE (RE_Unsigned_Long_10), Make_List_Id (C));
               Append_Node_To_List (C, L);
            end if;

         when others =>
            null;
      end case;
   end Get_Discriminants_Value;
end Backend.BE_CORBA_Ada.Common;
