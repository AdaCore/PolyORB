------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--            B A C K E N D . B E _ C O R B A _ A D A . C O M M O N         --
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

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nutils;       use Backend.BE_CORBA_Ada.Nutils;
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
               Declaration      : Node_Id;
               Declarator       : Node_Id;
               Str_Package_Node : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instanciated package node

               Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
               Declarator := First_Entity (Declarators (Declaration));
               Str_Package_Node := Defining_Identifier
                 (Stub_Package_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (Declarator)))));

               --  Getting the conversion subprogram

               Str_Convert_Subp := Make_Designator
                 (SN (S_To_Bounded_String));
               Set_Correct_Parent_Unit_Name
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
               Declaration      : Node_Id;
               Declarator       : Node_Id;
               Str_Package_Node : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instanciated package node

               Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
               Declarator := First_Entity (Declarators (Declaration));
               Str_Package_Node := Defining_Identifier
                 (Stub_Package_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (Declarator)))));

               --  Getting the conversion subprogram

               Str_Convert_Subp := Make_Designator
                 (SN (S_To_Bounded_Wide_String));
               Set_Correct_Parent_Unit_Name
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
            --  to the original type because the type definition is done
            --  by means of 'subtype' and not 'type ... is new ...'

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
               --  access to the 'Val attribute. So there is
               --  no need to cast the variable to  the original
               --  enumeration type.

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
               Declaration      : Node_Id;
               Declarator       : Node_Id;
               FP_Type_Node     : Node_Id;
            begin

               --  Getting the fixed point type

               Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
               Declarator := First_Entity (Declarators (Declaration));
               FP_Type_Node := Expand_Designator
                 (Stub_Type_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (Declarator)))));

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
               --  access to the 'Pos attribute. So there is
               --  no need to cast the variable to  the original
               --  enumeration type.

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
               Declaration      : Node_Id;
               Declarator       : Node_Id;
               Str_Package_Node : Node_Id;
               Str_Type         : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instanciated package node

               Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
               Declarator := First_Entity (Declarators (Declaration));
               Str_Package_Node := Defining_Identifier
                 (Stub_Package_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (Declarator)))));

               --  Getting the conversion subprogram

               Str_Type := Make_Designator (TN (T_Bounded_String));
               Set_Correct_Parent_Unit_Name (Str_Type, Str_Package_Node);

               Str_Convert_Subp := Make_Designator (SN (S_To_String));
               Set_Correct_Parent_Unit_Name
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
               Declaration      : Node_Id;
               Declarator       : Node_Id;
               Str_Package_Node : Node_Id;
               Str_Type         : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instanciated package node

               Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
               Declarator := First_Entity (Declarators (Declaration));
               Str_Package_Node := Defining_Identifier
                 (Stub_Package_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (Declarator)))));

               --  Getting the conversion subprogram

               Str_Type := Make_Designator (TN (T_Bounded_Wide_String));
               Set_Correct_Parent_Unit_Name (Str_Type, Str_Package_Node);

               Str_Convert_Subp := Make_Designator
                 (SN (S_To_Wide_String));
               Set_Correct_Parent_Unit_Name
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
               Declaration      : Node_Id;
               Declarator       : Node_Id;
               Seq_Package_Node : Node_Id;
               Seq_Type         : Node_Id;
            begin

               --  Getting the instanciated package node

               Declaration := FEU.Get_Original_Type_Declaration (Var_Type);
               Declarator := First_Entity (Declarators (Declaration));
               Seq_Package_Node := Defining_Identifier
                 (Stub_Package_Node
                  (BE_Ada_Instanciations
                   (BE_Node
                    (Identifier
                     (Declarator)))));

               --  Sequence type

               Seq_Type := Make_Designator (TN (T_Sequence));
               Set_Correct_Parent_Unit_Name (Seq_Type, Seq_Package_Node);

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

end Backend.BE_CORBA_Ada.Common;
