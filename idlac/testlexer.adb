with Ada.Text_IO;
with GNAT.Command_Line;
with Tokens; use Tokens;

procedure testlexer is
   Token : Tokens.Idl_Token;
begin
   Tokens.Initialize (GNAT.Command_Line.Get_Argument,
                      True,
                      True);

   loop
      Token := Get_Next_Token;
      Ada.Text_Io.Put (Idl_Token'Image (Token));
      case Token is
         when T_Lit_Decimal_Integer |
           T_Lit_Octal_Integer |
           T_Lit_Hexa_Integer |
           T_Lit_Simple_Char |
           T_Lit_Escape_Char |
           T_Lit_Octal_Char |
           T_Lit_Hexa_Char |
           T_Lit_Unicode_Char |
           T_Lit_Wide_Simple_Char |
           T_Lit_Wide_Escape_Char |
           T_Lit_Wide_Octal_Char |
           T_Lit_Wide_Hexa_Char |
           T_Lit_Wide_Unicode_Char |
           T_Lit_Simple_Floating_Point |
           T_Lit_Exponent_Floating_Point |
           T_Lit_Pure_Exponent_Floating_Point |
           T_Lit_String |
           T_Lit_Wide_String |
           T_Lit_Simple_Fixed_Point |
           T_Lit_Floating_Fixed_Point =>
            Ada.Text_Io.Put (" : " & Get_Literal);
         when T_Identifier =>
            Ada.Text_Io.Put (" : " & Get_Identifier);
         when T_Pragma =>
            Ada.Text_Io.Put (" : " & Get_Pragma);
         when others =>
            null;
      end case;
      Ada.Text_Io.Put_Line ("");
      exit when Token = T_Eof;
   end loop;
end testlexer;
