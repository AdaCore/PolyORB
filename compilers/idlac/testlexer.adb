------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            T E S T L E X E R                             --
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

with Ada.Text_IO;
with GNAT.Command_Line;
with Idl_Fe.Lexer; use Idl_Fe.Lexer;
with Errors;

procedure testlexer is

   use Idl_Fe.Lexer.Lexer_State;

   Token : Idl_Fe.Lexer.Idl_Token;
begin
   Idl_Fe.Lexer.Initialize (GNAT.Command_Line.Get_Argument,
                            True,
                            True);

   loop
      Token := Get_Next_Token;
      Ada.Text_IO.Put (Idl_Token'Image (Token));
      case Token is
         when T_Lit_Decimal_Integer |
           T_Lit_Octal_Integer |
           T_Lit_Hexa_Integer |
           T_Lit_Char |
           T_Lit_Wide_Char |
           T_Lit_Simple_Floating_Point |
           T_Lit_Exponent_Floating_Point |
           T_Lit_Pure_Exponent_Floating_Point |
           T_Lit_String |
           T_Lit_Wide_String |
           T_Lit_Simple_Fixed_Point |
           T_Lit_Floating_Fixed_Point |
           T_Identifier |
           T_Pragma =>
            Ada.Text_IO.Put (" : " & Get_Lexer_String & ".");
         when others =>
            null;
      end case;
      Ada.Text_IO.Put_Line ("");
      exit when Token = T_Eof;
   end loop;
exception
   when Errors.Fatal_Error =>
      null;
end testlexer;
