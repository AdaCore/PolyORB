--  idlac: IDL to Ada compiler.
--  Copyright (C) 1999 Tristan Gingold.
--
--  emails: gingold@enst.fr
--          adabroker@adabroker.eu.org
--
--  IDLAC is free software;  you can  redistribute it and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Software
--  Foundation;  either version 2,  or (at your option) any later version.
--  IDLAC is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for  more details.  You should have  received  a copy of the GNU General
--  Public License  distributed with IDLAC;  see file COPYING.  If not, write
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston,
--  MA 02111-1307, USA.
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
--  with Errors;

package body Disp is
   Blanks : constant String (1 .. 80) := (others => ' ');

   procedure Disp_Tree (N : N_Root'Class; Indent : Natural; Full : Boolean);

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
      N : N_Root_Acc;
   begin
      Init (It, List);
      while not Is_End (It) loop
         N := Get_Node (It);
         if N /= null then
            Disp_Tree (N.all, Indent, Full);
         else
            Disp_Indent (Indent, "*null*");
         end if;
         Next (It);
      end loop;
   end Disp_List;

   --  Disp tree procedure
   procedure Disp_Tree (N : N_Root'Class; Indent : Natural; Full : Boolean) is
      Offset : constant Natural := 2;
      N_Indent : Natural := Indent + Offset;

--       procedure Disp_Binary (Op : String) is
--       begin
--          Put_Line ("binary operator " & Op);
--          Disp_Indent (N_Indent, "left:");
--          Disp_Tree (N_Binary_Expr (N).Left.all, N_Indent + Offset, Full);
--          Disp_Indent (N_Indent, "right:");
--          Disp_Tree (N_Binary_Expr (N).Right.all, N_Indent + Offset, Full);
--       end Disp_Binary;

--       procedure Disp_Unary (Op : String) is
--       begin
--          Put_Line ("unary operator " & Op);
--          Disp_Indent (N_Indent, "operand:");
--          Disp_Tree (N_Unary_Expr (N).Operand.all, N_Indent + Offset, Full);
--       end Disp_Unary;

   begin
      Disp_Indent (Indent);

      case Get_Kind (N) is
         when K_Scoped_Name =>
--             Put_Line
--               ("scoped name: " & Get_Name (N_Scoped_Name (N).Value.all));
            Put_Line ("Scoped Name");

         when K_Repository =>
            Put_Line ("repository");
            Disp_List (N_Repository (N).Contents, N_Indent, Full);

         when K_Module =>
            Put_Line ("module " & Get_Name (N_Module (N)));
            Disp_Indent (N_Indent, "content:");
            Disp_List (N_Module (N).Contents, N_Indent + Offset, Full);

         when K_Interface =>
            if N_Interface (N).Abst then
               Put ("abstract ");
            end if;
            Put_Line ("interface " & Get_Name (N_Interface (N)));
--             if Full then
--                if N_Interface (N).Parents /= Nil_List then
--                   Disp_Indent (N_Indent);
--                   Put_Line ("base:");
--                   Disp_List (N_Interface (N).Parents, N_Indent, False);
--                end if;
--                Disp_List (N_Interface (N).Contents, N_Indent, Full);
--             end if;
         when K_Forward_Interface =>
            if N_Forward_Interface (N).Abst then
               Put ("abstract ");
            end if;
               if N_Forward_Interface (N).Forward /= null then
                  Put_Line ("forward interface "
                            & Get_Name (N_Forward_Interface (N).Forward.all)
                            );
               else
                  Put_Line ("forward interface (never declared!!) "
                            & Get_Name (N_Forward_Interface (N)));
               end if;

         when K_ValueType =>
            Put_Line ("valuetype " & Get_Name (N_ValueType (N)));
--             if Full then
--                if N_Interface (N).Parents /= Nil_List then
--                   Disp_Indent (N_Indent);
--                   Put_Line ("base:");
--                   Disp_List (N_Interface (N).Parents, N_Indent, False);
--                end if;
--                Disp_List (N_Interface (N).Contents, N_Indent, Full);
--             end if;

         when K_Forward_ValueType =>
            if N_Forward_ValueType (N).Forward /= null then
               Put_Line ("forward interface "
                         & Get_Name (N_Forward_ValueType (N).Forward.all));
            else
               Put_Line ("forward interface (never declared!!) "
                         & Get_Name (N_Forward_ValueType (N)));
            end if;

         when K_Boxed_ValueType =>
            Put_Line ("boxed valuetype " & Get_Name (N_Boxed_ValueType (N)));
            Disp_Tree (N_Boxed_ValueType (N).Boxed_Type.all,
                       N_Indent + Offset,
                       Full);

         when K_State_Member =>
            if N_State_Member (N).Is_Public then
               Put ("public");
            else
               Put ("private");
            end if;
            Put_Line (" statemember");
            Disp_Tree (N_State_Member (N).State_Type.all,
                       N_Indent + Offset,
                       Full);
            Disp_List (N_State_Member (N).State_Declarators,
                       N_Indent + Offset,
                       Full);


         when K_Initializer =>
            Put_Line ("initializer");



         when K_Operation =>
            declare
               Op : N_Operation renames N_Operation (N);
            begin
               Put ("operation ");
               if Op.Is_Oneway then
                  Put ("oneway ");
               end if;
               Put_Line (Get_Name (Op));
               Disp_Indent (N_Indent, "type:");
               Disp_Tree (Op.Operation_Type.all, N_Indent + Offset, Full);
               Disp_Indent (N_Indent, "parameters:");
               Disp_List (Op.Parameters, N_Indent + Offset, Full);
               if Op.Raises /= Nil_List then
                  Disp_Indent (N_Indent, "raises:");
                  Disp_List (Op.Raises, N_Indent + Offset, Full);
               end if;
               if Op.Contexts /= Nil_List then
                  Disp_Indent (N_Indent, "contexts:");
                  Disp_List (Op.Contexts, N_Indent + Offset, Full);
               end if;
            end;

--          when K_Attribute =>
--             Put ("attribute ");
--             if N_Attribute (N).Is_Readonly then
--                Put ("readonly ");
--             end if;
--             Put_Line (Get_Name (N_Attribute (N)));
--             Disp_Indent (N_Indent, "type:");
--             Disp_Tree (N_Attribute (N).A_Type.all, N_Indent, Full);

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
            if N_String (N).Bound = null then
               Put_Line ("string (unbounded)");
            else
               Put_Line ("string bounds:");
               Disp_Tree (N_String (N).Bound.all, N_Indent, Full);
            end if;

         when K_Wide_String =>
            if N_Wide_String (N).Bound = null then
               Put_Line ("string (unbounded)");
            else
               Put_Line ("string bounds:");
               Disp_Tree (N_Wide_String (N).Bound.all, N_Indent, Full);
            end if;

         when K_Param =>
            Put ("param ");
            case N_Param (N).Mode is
               when Mode_In =>
                  Put ("in");
               when Mode_Out =>
                  Put ("out");
               when Mode_Inout =>
                  Put ("inout");
            end case;
            Put (' ');
            Disp_Tree (N_Param (N).Declarator.all, N_Indent + Offset, False);
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (N_Param (N).Param_Type.all, N_Indent + Offset, False);

         when K_Exception =>
            Put ("exception ");
            Put_Line (Get_Name (N_Exception (N)));
            if Full then
               Disp_Indent (N_Indent, "members:");
               Disp_List (N_Exception (N).Members, N_Indent + Offset, Full);
            end if;

         when K_Member =>
            Put_Line ("member");
            Disp_Indent (N_Indent, "declarator:");
            Disp_List (N_Member (N).Decl, N_Indent + Offset, Full);
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (N_Member (N).M_Type.all, N_Indent + Offset, Full);

         when K_Declarator =>
            Put_Line ("declarator " & Get_Name (N_Declarator (N)));
            if N_Declarator (N).Array_Bounds /= Nil_List then
               Disp_Indent (N_Indent, "fixed_array:");
               Disp_List (N_Declarator (N).Array_Bounds,
                          N_Indent + Offset, True);
            end if;

         when K_Union =>
            Put_Line ("union " & Get_Name (N_Union (N)));
            if Full then
               Disp_Indent (N_Indent, "switch type:");
               Disp_Tree (N_Union (N).Switch_Type.all,
                          N_Indent + Offset, True);
               Disp_Indent (N_Indent, "cases:");
               Disp_List (N_Union (N).Cases, N_Indent + Offset, True);
            end if;

         when K_Case =>
            Put_Line ("case");
            Disp_Indent (N_Indent, "labels:");
            Disp_List (N_Case (N).Labels, N_Indent, Full);
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (N_Case (N).Case_Type.all, N_Indent, Full);
            Disp_Indent (N_Indent, "declarator:");
            Disp_Tree (N_Case (N).Case_Decl.all, N_Indent, Full);

--          when K_Or =>
--             Disp_Binary ("or");

--          when K_Xor =>
--             Disp_Binary ("xor");

--          when K_And =>
--             Disp_Binary ("and");

--          when K_Shl =>
--             Disp_Binary ("shl");

--          when K_Shr =>
--             Disp_Binary ("shr");

--          when K_Add =>
--             Disp_Binary ("add");

--          when K_Sub =>
--             Disp_Binary ("sub");

--          when K_Mul =>
--             Disp_Binary ("mul");

--          when K_Div =>
--             Disp_Binary ("div");

--          when K_Mod =>
--             Disp_Binary ("mod");

--          when K_Not =>
--             Disp_Unary ("not");

--          when K_Neg =>
--             Disp_Unary ("neg");

--          when K_Id =>
--             Disp_Unary ("Id");

--          when K_Lit_Char =>
--             raise Errors.Internal_Error;

         when K_Lit_String =>
            Put_Line ("string literal : " &
                      Ada.Characters.Latin_1.Quotation &
                      N_Lit_String (N).Value.all &
                      Ada.Characters.Latin_1.Quotation);

--          when K_Lit_Integer =>
--             Put_Line ("integer literal: " & N_Lit_Integer (N).Lit.all);

--          when K_Lit_Floating_Point =>
--             Put_Line ("floating point: "
--                       & N_Lit_Floating_Point (N).Lit.all);

--          when K_Lit_Fixed_Point =>
--             raise Errors.Internal_Error;

--          when K_Lit_True =>
--             Put_Line ("true");

--          when K_Lit_False =>
--             Put_Line ("false");

--          when K_Lit_Wchar =>
--             raise Errors.Internal_Error;

--          when K_Lit_Wstring =>
--             raise Errors.Internal_Error;

         when K_Struct =>
            Put_Line ("struct " & Get_Name (N_Struct (N)));
            if Full then
               Disp_Indent (N_Indent, "members:");
               Disp_List (N_Struct (N).Members, N_Indent + Offset, True);
            end if;

         when K_Enum =>
            Put_Line ("enum " & Get_Name (N_Enum (N)));
            if Full then
               Disp_Indent (N_Indent, "enumerators:");
               Disp_List (N_Enum (N).Enumerators, N_Indent + Offset, True);
            end if;

         when K_ValueBase =>
            Put_Line ("ValueBase");

         when K_Enumerator =>
            Put_Line ("enumerator: " & Get_Name (N_Enumerator (N)));

         when K_Type_Declarator =>
            Put_Line ("type declarator:");
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (N_Type_Declarator (N).T_Type.all,
                       N_Indent + Offset, Full);
            Disp_Indent (N_Indent, "declarators:");
            Disp_List (N_Type_Declarator (N).Declarators,
                       N_Indent + Offset, Full);

         when K_Sequence =>
            Put_Line ("sequence");
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (N_Sequence (N).Sequence_Type.all,
                       N_Indent + Offset, Full);
            if N_Sequence (N).Bound /= null then
               Disp_Indent (N_Indent, "bound:");
               Disp_Tree (N_Sequence (N).Bound.all,
                         N_Indent + Offset, Full);
            end if;

         when K_Const_Dcl =>
            Put_Line ("const " & Get_Name (N_Const_Dcl (N)));
            Disp_Indent (N_Indent, "type:");
            Disp_Tree (N_Const_Dcl (N).Const_Type.all,
                       N_Indent + Offset,
                       Full);
            Disp_Indent (N_Indent, "expr:");
            Disp_Tree (N_Const_Dcl (N).Expression.all,
                       N_Indent + Offset,
                       Full);

         when K_Fixed =>
            Put_Line ("fixed");
            Disp_Tree (N_Fixed (N).Digits_Nb.all, N_Indent + Offset, Full);
            Disp_Tree (N_Fixed (N).Scale.all, N_Indent + Offset, Full);

         when K_Native =>
            Put_Line ("native:");
            Disp_Tree (N_Native (N).Declarator.all, N_Indent + Offset, Full);

      end case;
   end Disp_Tree;

   procedure Disp_Tree (Tree : N_Root'Class) is
   begin
      Disp_Tree (Tree, 0, True);
   end Disp_Tree;

end Disp;
