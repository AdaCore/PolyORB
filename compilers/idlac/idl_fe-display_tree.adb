------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                  I D L _ F E . D I S P L A Y _ T R E E                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Wide_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

with Utils; use Utils;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree, Idl_Fe.Tree.Synthetic;

package body Idl_Fe.Display_Tree is
   Blanks : constant String (1 .. 80) := (others => ' ');

   procedure Disp_Tree (N : Node_Id; Indent : Natural; Full : Boolean);

   procedure Disp_Indent (Indent : Natural; S : String := "") is
      N : Natural;
   begin
      N := Indent;
      while N > 0 loop
         if N < Blanks'Length then
            Put (Blanks (1 .. N));
            N := 0;
         else
            Put (Blanks);
            N := N - Blanks'Length;
         end if;
      end loop;
      if S'Length > 0 then
         Put_Line (S);
      end if;
   end Disp_Indent;

   procedure Disp_List (List : Node_List; Indent : Natural; Full : Boolean) is
      It : Node_Iterator;
      N : Node_Id;
   begin
      Init (It, List);
      while not Is_End (It) loop
         Get_Next_Node (It, N);

         if N /= No_Node then
            Disp_Tree (N, Indent, Full);
         else
            Disp_Indent (Indent, "*null*");
         end if;
      end loop;
   end Disp_List;

   procedure Disp_Binary (N : Node_Id;
                          Indent : Natural;
                          Full : Boolean;
                          Op : String) is
   begin
      Put_Line ("binary operator : " & Op);
      Disp_Const_Value (Expr_Value (N), Indent);
      Disp_Indent (Indent, "left:");
      Disp_Tree (Left (N), Indent, Full);
      Disp_Indent (Indent, "right:");
      Disp_Tree (Right (N), Indent, Full);
   end Disp_Binary;

   procedure Disp_Unary (N : Node_Id;
                         Indent : Natural;
                         Full : Boolean;
                         Op : String) is
   begin
      Put_Line ("unary operator : " & Op);
      Disp_Const_Value (Expr_Value (N), Indent);
      Disp_Indent (Indent, "operand:");
      Disp_Tree (Operand (N), Indent, Full);
   end Disp_Unary;

   procedure Disp_Const_Value (Expr : Constant_Value_Ptr;
                               Indent : Natural) is
   begin
      Disp_Indent (Indent);
      Put ("value : ");
      if Expr /= null then
         case Expr.Kind is
            when C_Octet =>
               Put_Line ("octet " &
                         Idl_Integer'Image (Expr.Integer_Value));
            when C_Short =>
               Put_Line ("short " &
                         Idl_Integer'Image (Expr.Integer_Value));
            when C_Long =>
               Put_Line ("long " &
                         Idl_Integer'Image (Expr.Integer_Value));
            when C_LongLong =>
               Put_Line ("long long " &
                         Idl_Integer'Image (Expr.Integer_Value));
            when C_UShort =>
               Put_Line ("unsigned short " &
                         Idl_Integer'Image (Expr.Integer_Value));
            when C_ULong =>
               Put_Line ("unsigned long " &
                         Idl_Integer'Image (Expr.Integer_Value));
            when C_ULongLong =>
               Put_Line ("unsigned long long " &
                         Idl_Integer'Image (Expr.Integer_Value));
            when C_General_Integer =>
               Put_Line ("general integer " &
                         Idl_Integer'Image (Expr.Integer_Value));
            when C_String =>
               Put_Line ("string " &
                         Ada.Characters.Latin_1.Quotation &
                         Expr.String_Value.all &
                         Ada.Characters.Latin_1.Quotation);
            when C_WString =>
               Put ("wide string literal: " &
                         Ada.Characters.Latin_1.Quotation);
               Ada.Wide_Text_IO.Put (Expr.WString_Value.all);
               Put_Line ("" & Ada.Characters.Latin_1.Quotation);
            when C_Char =>
               Put_Line ("character " &
                         Idl_Character'Image
                         (Expr.Char_Value));
            when C_WChar =>
               Put_Line ("wide character " &
                         Idl_Wide_Character'Image
                         (Expr.WChar_Value));
            when C_Fixed =>
               Put_Line ("fixed point " &
                         "<" &
                         Idl_Integer'Image
                         (Expr.Digits_Nb) &
                         "," &
                         Idl_Integer'Image
                         (Expr.Scale) &
                         "> " &
                         Idl_Integer'Image (Expr.Fixed_Value));
            when C_General_Fixed =>
               Put_Line ("general fixed point " &
                         "<" &
                         Idl_Integer'Image
                         (Expr.Digits_Nb) &
                         "," &
                         Idl_Integer'Image
                         (Expr.Scale) &
                         "> " &
                         Idl_Integer'Image (Expr.Fixed_Value));
            when C_Float =>
               Put_Line ("float " &
                    Idl_Float'Image (Expr.Float_Value));
            when C_Double =>
               Put_Line ("double " &
                    Idl_Float'Image (Expr.Float_Value));
            when C_LongDouble =>
               Put_Line ("long double " &
                    Idl_Float'Image (Expr.Float_Value));
            when C_General_Float =>
               Put_Line ("general float " &
                    Idl_Float'Image (Expr.Float_Value));
            when C_Boolean =>
               Put_Line ("boolean " &
                         Idl_Boolean'Image (Expr.Boolean_Value));
            when C_Enum =>
               Put_Line ("enum " &
                         Name (Expr.Enum_Name) &
                         ": " &
                         Name (Expr.Enum_Value));
            when C_No_Kind =>
               Put_Line ("no_kind");
         end case;
      else
         Put_Line ("no correct value.");
      end if;
   end Disp_Const_Value;

   --  Disp tree procedure
   procedure Disp_Tree (N : Node_Id; Indent : Natural; Full : Boolean) is
      N_Indent : Natural := Indent + Offset;
   begin
      Disp_Indent (Indent);
      if N = No_Node then
         Put_Line ("node not properly defined");
         return;
      end if;
      Put ("[" & Img (N));
      if Is_Named (N) then
         Put ("/" & Img (Parent_Scope (N)));
      end if;
      Put ("] ");
      case Kind (N) is
         when K_Scoped_Name =>
            Put ("scoped name: -> " & Img (Value (N))
                 & " " & Name (Value (N)));
            if S_Type (N) /= No_Node then
               Put_Line (" (type: "
                         & Img (S_Type (N)) & " "
                         & Node_Kind'Image (Kind (S_Type (N))) &
                         ")");
            else
               Put_Line ("");
            end if;

         when K_Repository =>
            Put_Line ("repository");
            Disp_List (Contents (N), N_Indent, Full);

         when K_Module =>
            Put_Line ("module " & Name (N));
            Disp_Indent (N_Indent, "content:");
            Disp_List (Contents (N), N_Indent + Offset, Full);

         when K_Interface =>
            if Abst (N) then
               Put ("abstract ");
            end if;
            Put_Line ("interface " & Name (N));
            if Repository_Id (N) /= No_Node then
               Disp_Indent (Indent + 2);
               Put_Line
                 ("repository id: """
                  & String_Value (Repository_Id (N))
                  & """");
            end if;
            if Full then
               if Parents (N) /= Nil_List then
                  Disp_Indent (N_Indent);
                  Put_Line ("parents:");
                  Disp_List (Parents (N),
                             N_Indent + Offset,
                             False);
               end if;
            end if;
            Disp_List (Contents (N), N_Indent, Full);

         when K_Forward_Interface =>
            if Abst (N) then
               Put ("abstract ");
            end if;
            if Forward (N) /= No_Node then
               Put_Line ("forward interface "
                         & Name (Forward (N))
                         );
            else
               Put_Line ("forward interface (never declared!!) "
                         & Name (N));
            end if;

         when K_ValueType =>
            if Abst (N) then
               Put ("abstract ");
            end if;
            if Custom (N) then
               Put ("custom ");
            end if;
            Put_Line ("valuetype " & Name (N));
            if Full then
               if Parents (N) /= Nil_List then
                  Disp_Indent (N_Indent);
                  if Truncatable (N) then
                     Put_Line ("parents (truncatable):");
                  else
                     Put_Line ("parents:");
                  end if;
                  Disp_List (Parents (N),
                             N_Indent + Offset,
                             False);
               end if;
               if Supports (N) /= Nil_List then
                  Disp_Indent (N_Indent);
                  Put_Line ("supports:");
                  Disp_List (Supports (N),
                             N_Indent + Offset,
                             False);
               end if;
            end if;
            Disp_List (Contents (N), N_Indent, Full);

         when K_Forward_ValueType =>
            if Abst (N) then
               Put ("abstract ");
            end if;
            if Forward (N) /= No_Node then
               Put_Line ("forward valuetype "
                         & Name (Forward (N))
                         );
            else
               Put_Line ("forward valuetype (never declared!!) "
                         & Name (N));
            end if;

         when K_Boxed_ValueType =>
            Put_Line ("boxed valuetype " & Name (N));
            Disp_Tree (Boxed_Type (N),
                       N_Indent + Offset,
                       Full);

         when K_State_Member =>
            if Is_Public (N) then
               Put ("public");
            else
               Put ("private");
            end if;
            Put_Line (" statemember");
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (State_Type (N),
                       N_Indent + Offset,
                       Full);
            Disp_Indent (N_Indent, "declarators:");
            Disp_List (State_Declarators (N),
                       N_Indent + Offset,
                       Full);


         when K_Initializer =>
            Put_Line ("initializer " & Name (N));
            if Param_Decls (N) /= Nil_List then
               Disp_Indent (N_Indent, "parameters:");
               Disp_List (Param_Decls (N), N_Indent + Offset, Full);
            end if;

         when K_Operation =>
            begin
               Put ("operation ");
               if Is_Oneway (N) then
                  Put ("oneway ");
               end if;
               Put (Name (N));
               if Is_Implicit_Inherited (N) then
                  Put (" (implicit inherited)");
               end if;
               New_Line;
               Disp_Indent (N_Indent, "type:");
               Disp_Tree (Operation_Type (N), N_Indent + Offset, Full);
               if Parameters (N) /= Nil_List then
                  Disp_Indent (N_Indent, "parameters:");
                  Disp_List (Parameters (N), N_Indent + Offset, Full);
               end if;
               if Raises (N) /= Nil_List then
                  Disp_Indent (N_Indent, "raises:");
                  Disp_List (Raises (N), N_Indent + Offset, Full);
               end if;
               if Contexts (N) /= Nil_List then
                  Disp_Indent (N_Indent, "contexts:");
                  Disp_List (Contexts (N), N_Indent + Offset, Full);
               end if;
            end;

         when K_Attribute =>
            Put ("attribute ");
            if Is_Readonly (N) then
               Put ("readonly ");
            end if;
            Put_Line ("");
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (A_Type (N), N_Indent + Offset, Full);
            Disp_Indent (N_Indent, "declarators:");
            Disp_List (Declarators (N),
                       N_Indent + Offset, Full);

         when K_Void =>
            Put_Line ("void");

         when K_Float =>
            Put_Line ("float");

         when K_Double =>
            Put_Line ("double");

         when K_Long_Double =>
            Put_Line ("long double");

         when K_Short =>
            Put_Line ("short");

         when K_Long =>
            Put_Line ("long");

         when K_Long_Long =>
            Put_Line ("long long");

         when K_Unsigned_Long =>
            Put_Line ("unsigned long");

         when K_Unsigned_Short =>
            Put_Line ("unsigned short");

         when K_Unsigned_Long_Long =>
            Put_Line ("unsigned long long");

         when K_Char =>
            Put_Line ("char");

         when K_Wide_Char =>
            Put_Line ("wide_char");

         when K_Boolean =>
            Put_Line ("boolean");

         when K_Object =>
            Put_Line ("object");

         when K_Octet =>
            Put_Line ("octet");

         when K_Any =>
            Put_Line ("any");

         when K_String =>
            Put ("string ");
            if Bound (N) = No_Node then
               Put_Line ("(unbounded)");
            else
               Put_Line ("bounds:");
               Disp_Tree (Bound (N), N_Indent, Full);
            end if;

         when K_Wide_String =>
            Put ("wide string ");
            if Bound (N) = No_Node then
               Put_Line ("(unbounded)");
            else
               Put_Line ("bounds:");
               Disp_Tree (Bound (N), N_Indent, Full);
            end if;

         when K_Param =>
            Put ("param ");
            case Mode (N) is
               when Mode_In =>
                  Put_Line ("in");
               when Mode_Out =>
                  Put_Line ("out");
               when Mode_Inout =>
                  Put_Line ("inout");
            end case;
            Disp_Indent (N_Indent, "name:");
            Disp_Tree (Declarator (N), N_Indent + Offset, False);
            if Param_Type (N) /= No_Node then
               case (Kind (Param_Type (N))) is
                  when K_Interface | K_ValueType =>
                     Disp_Indent (N_Indent, "type: "
                                  & Name (Param_Type (N)));
                  when others =>
                     Disp_Indent (N_Indent, "type: ");
                     Disp_Tree (Param_Type (N), N_Indent, Full);
               end case;
            else
               Disp_Indent (N_Indent, "type: no valid type");
            end if;

         when K_Exception =>
            Put ("exception ");
            Put_Line (Name (N));
            if Repository_Id (N) /= No_Node then
               Disp_Indent (Indent + 2);
               Put_Line
                 ("repository id: """
                  & String_Value (Repository_Id (N))
                  & """");
            end if;
            if Full then
               Disp_Indent (N_Indent, "members:");
               Disp_List (Members (N), N_Indent + Offset, Full);
            end if;

         when K_Member =>
            Put_Line ("member");
            Disp_Indent (N_Indent, "declarator:");
            Disp_List (Decl (N), N_Indent + Offset, Full);
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (M_Type (N), N_Indent + Offset, Full);


         when K_Declarator =>
            Put_Line ("declarator " & Name (N));
            if Array_Bounds (N) /= Nil_List then
               Disp_Indent (N_Indent, "bounds:");
               Disp_List (Array_Bounds (N),
                          N_Indent + Offset, True);
            end if;

         when K_Union =>
            Put_Line ("union " & Name (N));
            if Full then
               Disp_Indent (N_Indent, "switch type:");
               Disp_Tree (Switch_Type (N),
                          N_Indent + Offset, True);
               Disp_Indent (N_Indent, "cases:");
               Disp_List (Cases (N), N_Indent + Offset, True);
            end if;

         when K_Case =>
            Put_Line ("case");
            Disp_Indent (N_Indent, "labels (*null* means default):");
            Disp_List (Labels (N), N_Indent + Offset, Full);
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (Case_Type (N), N_Indent + Offset, Full);
            Disp_Indent (N_Indent, "declarator:");
            Disp_Tree (Case_Decl (N), N_Indent + Offset, Full);

         when K_Or_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "or");

         when K_Xor_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "xor");

         when K_And_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "and");

         when K_Shl_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "shl");

         when K_Shr_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "shr");

         when K_Add_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "add");

         when K_Sub_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "sub");

         when K_Mul_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "mul");

         when K_Div_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "div");

         when K_Mod_Expr =>
            Disp_Binary (N, N_Indent + Offset, Full, "mod");

         when K_Not_Expr =>
            Disp_Unary (N, N_Indent + Offset, Full, "not");

         when K_Neg_Expr =>
            Disp_Unary (N, N_Indent + Offset, Full, "neg");

         when K_Id_Expr =>
            Disp_Unary (N, N_Indent + Offset, Full, "id");

         when K_Primary_Expr =>
            Put_Line ("primary expression");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);
            Disp_Tree (Operand (N),
                       N_Indent,
                       Full);

         when K_Lit_Integer =>
            Put_Line ("integer literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Lit_String =>
            Put_Line ("string literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Lit_Wide_String =>
            Put_Line ("wide string literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Lit_Character =>
            Put_Line ("character literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Lit_Wide_Character =>
            Put_Line ("wide character literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Lit_Fixed_Point =>
            Put_Line ("fixed point literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Lit_Floating_Point =>
            Put_Line ("floating point literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Lit_Boolean =>
            Put_Line ("boolean literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Lit_Enum =>
            Put_Line ("enum literal: ");
            Disp_Const_Value (Expr_Value (N), N_Indent + Offset);

         when K_Struct =>
            Put ("struct " & Name (N));
            if Is_Exception_Members (N) then
               Put_Line (" (exception members)");
            else
               New_Line;
            end if;
            if Full then
               Disp_Indent (N_Indent, "members:");
               Disp_List (Members (N), N_Indent + Offset, True);
            end if;

         when K_Enum =>
            Put_Line ("enum " & Name (N));
            if Full then
               Disp_Indent (N_Indent, "enumerators:");
               Disp_List (Enumerators (N), N_Indent + Offset, True);
            end if;

         when K_ValueBase =>
            Put_Line ("ValueBase");

         when K_Enumerator =>
            Put_Line ("enumerator: " & Name (N));

         when K_Type_Declarator =>
            Put_Line ("type declarator:");
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (T_Type (N),
                       N_Indent + Offset, Full);
            Disp_Indent (N_Indent, "declarators:");
            Disp_List (Declarators (N),
                       N_Indent + Offset, Full);

         when K_Sequence =>
            Put_Line ("sequence");
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (Sequence_Type (N),
                       N_Indent + Offset, Full);
            if Bound (N) /= No_Node then
               Disp_Indent (N_Indent, "bound:");
               Disp_Tree (Bound (N),
                         N_Indent + Offset, Full);
            end if;

         when K_Const_Dcl =>
            Put_Line ("const " & Name (N));
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (Constant_Type (N),
                       N_Indent + Offset,
                       Full);
            Disp_Indent (N_Indent, "expr:");
            Disp_Tree (Expression (N),
                       N_Indent + Offset,
                       Full);

         when K_Fixed =>
            Put_Line ("fixed");
            Disp_Tree (Digits_Nb (N), N_Indent, Full);
            Disp_Tree (Scale (N), N_Indent, Full);

         when K_Native =>
            Put_Line ("native:");
            Disp_Tree (Declarator (N), N_Indent + Offset, Full);

         -------------------------------------------------------
         -- The following nodes are generated by the expander --
         -- and have no equivalent in standard IDL syntax.    --
         -------------------------------------------------------

         when K_Ben_Idl_File =>
            Put_Line ("ben_idl_file " & Name (N));
            Disp_List (Contents (N), N_Indent, Full);

         when K_Sequence_Instance =>
            Put_Line ("sequence_instance " & Name (N));
            Disp_Tree (Sequence (N), N_Indent, Full);

         when K_String_Instance =>
            Put ("string_instance " & Name (N));
            if Is_Wide (N) then
               Put_Line (" (wide)");
            else
               New_Line;
            end if;
            Disp_Tree (Bound (N), N_Indent, Full);

         when others =>
            Put_Line ("not implemented yet");
      end case;
   end Disp_Tree;

   procedure Disp_Tree (Tree : Node_Id) is
   begin
      Disp_Tree (Tree, 0, True);
   end Disp_Tree;

end Idl_Fe.Display_Tree;
