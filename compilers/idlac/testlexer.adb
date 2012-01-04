------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            T E S T L E X E R                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;
with GNAT.Command_Line;
with Idl_Fe.Lexer; use Idl_Fe.Lexer;
with Idlac_Errors;

procedure Testlexer is

   use Idl_Fe.Lexer.Lexer_State;

   Token : Idl_Fe.Lexer.Idl_Token;
begin
   Idl_Fe.Lexer.Initialize (GNAT.Command_Line.Get_Argument);

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
   when Idlac_Errors.Fatal_Error =>
      null;
end Testlexer;
