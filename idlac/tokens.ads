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

with Types; use Types;

package Tokens is

   --  All the possible tokens.
   type Idl_Token is
      (
   --  Position 0.
       T_Error,

   --  Keywords.
   --  Must start at position 1.
   --  Must be synchronised with idents.ads, idents.adb and tokens.adb
       T_Any,
       T_Attribute,
       T_Boolean,
       T_Case,
       T_Char,
       T_Const,
       T_Context,
       T_Default,
       T_Double,
       T_Enum,
       T_Exception,
       T_False,
       T_Fixed,
       T_Float,
       T_In,
       T_Inout,
       T_Interface,
       T_Long,
       T_Module,
       T_Native,
       T_Object,
       T_Octet,
       T_Oneway,
       T_Out,
       T_Raises,
       T_Readonly,
       T_Sequence,
       T_Short,
       T_String,
       T_Struct,
       T_Switch,
       T_True,
       T_Typedef,
       T_Unsigned,
       T_Union,
       T_Void,
       T_Wchar,
       T_Wstring,
   --  Punctuation characters
       T_Sharp,                 -- #
       T_Semi_Colon,            -- ;
       T_Left_Cbracket,         -- {
       T_Right_Cbracket,        -- }
       T_Colon,                 -- :
       T_Comma,                 -- ,
       T_Equal,                 -- =
       T_Plus,                  -- +
       T_Minus,                 -- -
       T_Left_Paren,            -- (
       T_Right_Paren,           -- )
       T_Less,                  -- <
       T_Greater,               -- >
       T_Left_Sbracket,         -- [
       T_Right_Sbracket,        -- ]
       T_Apostrophe,            -- '
       T_Quote,                 -- "
       T_Backslash,             -- \
       T_Bar,                   -- |
       T_Circumflex,            -- ^
       T_Ampersand,             -- &
       T_Star,                  -- *
       T_Slash,                 -- /
       T_Percent,               -- %
       T_Tilde,                 -- ~
       T_Colon_Colon,           -- ::
       T_Greater_Greater,       -- >>
       T_Less_Less,             -- <<
   --  Literals
       T_Lit_Integer,
       T_Lit_Char,
       T_Lit_Floating_Point,
       T_Lit_String,
       T_Lit_Fixed_Point,
   --  Identifier
       T_Identifier,
   --  Misc
       T_Eof
       );

   subtype Idl_Keywords is Idl_Token range T_Any .. T_Wstring;

   --  Initialize the lexical analyser.
   --  The lexical analyser use the standard input file.
   --  After this call, no token is available.  Therefore, next_token should
   --  be call.
   procedure Initialize;

   --  Advance the lexical analyse until a new token is found.
   --  An invalid token will make function TOKEN returns t_error.
   procedure Next_Token;

   --  Get the current token.
   function Token return Idl_Token;

   --  Return the location of the current token.
   function Get_Loc return Location;

   --  If the current token is an identifier (t_identifier), then return
   --  its value as a string.
   function Get_Identifier return String;

   --  FIXME:  if the current token is a literal, returns its value as a
   --  string.
   function Get_Literal return String;

   --  Make function TOKEN returns TOK at it next call, without performing
   --  any other action.
   --  The purpose is to handle some errors, such as '>>' instead of '> >'.
   --  TOK cannot be t_error.
   --  This procedure can stack only one token, ie, it must be called after
   --  next_token.
   procedure Set_Replacement_Token (Tok : Idl_Token);

   function Idl_Compare (Left, Right : String) return Boolean;

   --  Compare two IDL identifiers.
   --  Returns Equal if they are equal (case sensitivity).
   --  Returns Case_Differ if they differ only in case.
   --  Returns Differ otherwise.
   type Ident_Equality is (Differ, Case_Differ, Equal);
   function Idl_Identifier_Equal (Left, Right : String) return Ident_Equality;

   --  Return the idl_token TOK as a string.
   --  Format is "`keyword'", "`+'" (for symbols), "identifier `id'"
   function Image (Tok : Idl_Token) return String;
end Tokens;


