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
with Tree; use Tree;
with Errors;

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
      N : Node_Acc;
   begin
      Init (It, List);
      while not Is_End (It) loop
         N := Get_Node (It);
         if N /= null then
            Disp_Tree (N.all, Indent, Full);
         else
            Disp_Indent (indent, "*null*");
         end if;
         Next (It);
      end loop;
   end Disp_List;

   --  Disp tree procedure
   procedure Disp_Tree (N : N_Root'Class; Indent : Natural; Full : Boolean) is
      Offset : constant Natural := 2;
      NIndent : Natural := Indent + Offset;

      procedure Disp_Binary (Op : String) is
      begin
         Put_Line ("binary operator " & Op);
         Disp_Indent (Nindent, "left:");
         Disp_Tree (N_Binary_Expr (N).Left.all, Nindent + Offset, Full);
         Disp_Indent (Nindent, "right:");
         Disp_Tree (N_Binary_Expr (N).Right.all, Nindent + Offset, Full);
      end Disp_Binary;

      procedure Disp_Unary (Op : String) is
      begin
         Put_Line ("unary operator " & Op);
         Disp_Indent (Nindent, "operand:");
         Disp_Tree (N_Unary_Expr (N).Operand.all, Nindent + Offset, Full);
      end Disp_Unary;

   begin
      Disp_Indent (Indent);

      case Get_Kind (N) is
         when K_Scoped_Name =>
            Put_Line
              ("scoped name: " & Get_Name (N_Scoped_Name (N).Value.all));

         when K_Repository =>
            Put_Line ("repository");
            Disp_List (N_Repository (N).Contents, Nindent, Full);

         when K_Interface =>
            Put_Line ("interface " & Get_Name (N_Interface (N)));
            if Full then
               if N_Interface (N).Parents /= Nil_List then
                  Disp_Indent (Nindent);
                  Put_Line ("base:");
                  Disp_List (N_Interface (N).Parents, Nindent, False);
               end if;
               Disp_List (N_Interface (N).Contents, Nindent, Full);
            end if;

         when K_Forward_Interface =>
            if N_Forward_Interface (N).Forward /= null then
               Put_Line ("forward interface "
                         & Get_Name (N_Forward_Interface (N).Forward.all));
            else
               Put_Line ("forward interface (not declared!!) "
                         & Get_Name (N_Forward_Interface (N)));
            end if;

         when K_Operation =>
            declare
               Op : N_Operation renames N_Operation (N);
            begin
               Put ("operation ");
               if Op.Is_Oneway then
                  Put ("oneway ");
               end if;
               Put_Line (Get_Name (Op));
               Disp_Indent (Nindent, "type:");
               Disp_Tree (Op.Op_Type.all, Nindent + Offset, Full);
               Disp_Indent (Nindent, "parameters:");
               Disp_List (Op.Parameters, Nindent + Offset, Full);
               if Op.Raises /= Nil_List then
                  Disp_Indent (Nindent, "raises:");
                  Disp_List (Op.Raises, Nindent + Offset, Full);
               end if;
               if Op.Contexts /= Nil_List then
                  Disp_Indent (Nindent, "contexts:");
                  Disp_List (Op.Contexts, Nindent + Offset, Full);
               end if;
            end;

         when K_Attribute =>
            Put ("attribute ");
            if N_Attribute (N).Is_Readonly then
               Put ("readonly ");
            end if;
            Put_Line (Get_Name (N_Attribute (N)));
            Disp_Indent (Nindent, "type:");
            Disp_Tree (N_Attribute (N).A_Type.all, Nindent, Full);

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

         when K_Wchar =>
            Put_Line ("wchar");

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
               Disp_Tree (N_String (N).Bound.all, Nindent, Full);
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
            Put_Line (Get_Name (N_Param (N)));
            Disp_Indent (Nindent, "type:");
            Disp_Tree (N_Param (N).P_Type.all, Nindent + Offset, False);

         when K_Exception =>
            Put ("exception ");
            Put_Line (Get_Name (N_Exception (N)));
            if Full then
               Disp_Indent (Nindent, "members:");
               Disp_List (N_Exception (N).Members, Nindent + Offset, Full);
            end if;

         when K_Member =>
            Put_Line ("member");
            Disp_Indent (Nindent, "declarator:");
            Disp_List (N_Member (N).Decl, Nindent + Offset, Full);
            Disp_Indent (Nindent, "type:");
            Disp_Tree (N_Member (N).M_Type.all, Nindent + Offset, Full);

         when K_Declarator =>
            Put_Line ("declarator " & Get_Name (N_Declarator (N)));
            if N_Declarator (N).Array_Bounds /= Nil_List then
               Disp_Indent (Nindent, "fixed_array:");
               Disp_List (N_Declarator (N).Array_Bounds,
                          Nindent + Offset, True);
            end if;

         when K_Union =>
            Put_Line ("union " & Get_Name (N_Union (N)));
            if Full then
               Disp_Indent (Nindent, "switch type:");
               Disp_Tree (N_Union (N).Switch_Type.all, Nindent + Offset, True);
               Disp_Indent (Nindent, "cases:");
               Disp_List (N_Union (N).Cases, Nindent + Offset, True);
            end if;

         when K_Case =>
            Put_Line ("case");
            Disp_Indent (Nindent, "labels:");
            Disp_List (N_Case (N).Labels, Nindent, Full);
            Disp_Indent (Nindent, "type:");
            Disp_Tree (N_Case (N).C_Type.all, Nindent, Full);
            Disp_Indent (Nindent, "declarator:");
            Disp_Tree (N_Case (N).C_Decl.all, Nindent, Full);

         when K_Or =>
            Disp_Binary ("or");

         when K_Xor =>
            Disp_Binary ("xor");

         when K_And =>
            Disp_Binary ("and");

         when K_Shl =>
            Disp_Binary ("shl");

         when K_Shr =>
            Disp_Binary ("shr");

         when K_Add =>
            Disp_Binary ("add");

         when K_Sub =>
            Disp_Binary ("sub");

         when K_Mul =>
            Disp_Binary ("mul");

         when K_Div =>
            Disp_Binary ("div");

         when K_Mod =>
            Disp_Binary ("mod");

         when K_Not =>
            Disp_Unary ("not");

         when K_Neg =>
            Disp_Unary ("neg");

         when K_Id =>
            Disp_Unary ("Id");

         when K_Lit_Char =>
            raise Errors.Internal_Error;

         when K_Lit_String =>
            raise Errors.Internal_Error;

         when K_Lit_Integer =>
            Put_Line ("integer literal: " & N_Lit_Integer (N).Lit.all);

         when K_Lit_Floating_Point =>
            Put_Line ("floating point: " & N_Lit_Floating_Point (N).Lit.all);

         when K_Lit_Fixed_Point =>
            raise Errors.Internal_Error;

         when K_Lit_True =>
            Put_Line ("true");

         when K_Lit_False =>
            Put_Line ("false");

         when K_Lit_Wchar =>
            raise Errors.Internal_Error;

         when K_Lit_Wstring =>
            raise Errors.Internal_Error;

         when K_Struct =>
            Put_Line ("struct " & Get_Name (N_Struct (N)));
            if Full then
               Disp_Indent (Nindent, "members:");
               Disp_List (N_Struct (N).Members, Nindent + Offset, True);
            end if;

         when K_Enum =>
            Put_Line ("enum " & Get_Name (N_Enum (N)));
            if Full then
               Disp_Indent (Nindent, "enumerators:");
               Disp_List (N_Enum (N).Enumerators, Nindent + Offset, True);
            end if;

         when K_Enumerator =>
            Put_Line ("enumerator: " & Get_Name (N_Enumerator (N)));

         when K_Type_Declarator =>
            Put_Line ("type declarator:");
            Disp_Indent (Nindent, "type:");
            Disp_Tree (N_Type_Declarator (N).T_Type.all,
                       Nindent + Offset, Full);
            Disp_Indent (Nindent, "declarators:");
            Disp_List (N_Type_Declarator (N).declarators,
                       Nindent + Offset, Full);

         when K_Sequence =>
            Put_Line ("sequence");
            Disp_Indent (Nindent, "type:");
            Disp_Tree (N_Sequence (N).S_Type.all, Nindent + Offset, full);
            if N_Sequence (N).Bound /= null then
               Disp_Indent (Nindent, "bound:");
               Disp_Tree (N_Sequence (N).Bound.all, Nindent + Offset, Full);
            end if;

         when K_Module =>
            Put_Line ("module " & Get_Name (N_Module (N)));
            Disp_Indent (Nindent, "content:");
            Disp_List (N_Module (N).Contents, Nindent + Offset, Full);

         when K_Const =>
            Put_Line ("const " & Get_Name (N_Const (N)));
            Disp_Indent (Nindent, "type:");
            Disp_Tree (N_Const (N).C_Type.all, Nindent + Offset, Full);
            Disp_Indent (Nindent, "expr:");
            Disp_Tree (N_Const (N).Expr.all, Nindent + Offset, Full);

         when K_Native =>
            Put_Line ("native:");
            Disp_Tree (N_Native (N).Decl.all, Nindent, Full);
      end case;
   end Disp_Tree;

   procedure Disp_Tree (Tree : N_Root'Class) is
   begin
      Disp_Tree (Tree, 0, True);
   end Disp_Tree;
end Disp;
