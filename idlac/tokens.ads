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
with Errors;

package Tokens is

   -------------------------------
   --  idl keywords and tokens  --
   -------------------------------

   --  All the idl_keywords
   --
   --  IDL Syntax and semantics, CORBA V2.3 § 3.2.4
   --
   --  must be synchronized with the token declarations
   All_Idl_Keywords : array (1 .. 47) of String_Cacc :=
     (new String'("abstract"),
      new String'("any"),
      new String'("attribute"),
      new String'("boolean"),
      new String'("case"),
      new String'("char"),
      new String'("const"),
      new String'("context"),
      new String'("custom"),
      new String'("default"),
      new String'("double"),
      new String'("enum"),
      new String'("exception"),
      new String'("factory"),
      new String'("FALSE"),
      new String'("fixed"),
      new String'("float"),
      new String'("in"),
      new String'("inout"),
      new String'("interface"),
      new String'("long"),
      new String'("module"),
      new String'("native"),
      new String'("Object"),
      new String'("octet"),
      new String'("oneway"),
      new String'("out"),
      new String'("private"),
      new String'("public"),
      new String'("raises"),
      new String'("readonly"),
      new String'("sequence"),
      new String'("short"),
      new String'("string"),
      new String'("struct"),
      new String'("supports"),
      new String'("switch"),
      new String'("TRUE"),
      new String'("truncatable"),
      new String'("typedef"),
      new String'("unsigned"),
      new String'("union"),
      new String'("ValueBase"),
      new String'("valuetype"),
      new String'("void"),
      new String'("wchar"),
      new String'("wstring")
      );



   --  All the idl tokens.
   type Idl_Token is
      (
       T_Error,           --  Position 0.
       T_Abstract,        --  Keywords, synchronised with keywords
       T_Any,
       T_Attribute,
       T_Boolean,
       T_Case,
       T_Char,
       T_Const,
       T_Context,
       T_Custom,
       T_Default,
       T_Double,
       T_Enum,
       T_Exception,
       T_Factory,
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
       T_Private,
       T_Public,
       T_Raises,
       T_Readonly,
       T_Sequence,
       T_Short,
       T_String,
       T_Struct,
       T_Supports,
       T_Switch,
       T_True,
       T_Truncatable,
       T_Typedef,
       T_Unsigned,
       T_Union,
       T_ValueBase,
       T_Valuetype,
       T_Void,
       T_Wchar,
       T_Wstring,
       T_Semi_Colon,          -- ;  --  graphical character tokens
       T_Left_Cbracket,       -- {
       T_Right_Cbracket,      -- }
       T_Colon,               -- :
       T_Comma,               -- ,
       T_Colon_Colon,         -- ::
       T_Left_Paren,          -- (
       T_Right_Paren,         -- )
       T_Equal,               -- =
       T_Bar,                 -- |
       T_Circumflex,          -- ^
       T_Ampersand,           -- &
       T_Greater_Greater,     -- >>
       T_Less_Less,           -- <<
       T_Plus,                -- +
       T_Minus,               -- -
       T_Star,                -- *
       T_Slash,               -- /
       T_Percent,             -- %
       T_Tilde,               -- ~
       T_Less,                -- <
       T_Greater,             -- >
       T_Left_Sbracket,       -- [
       T_Right_Sbracket,      -- ]
       T_Lit_Decimal_Integer,        --  Literals
       T_Lit_Octal_Integer,
       T_Lit_Hexa_Integer,
       T_Lit_Simple_Char,
       T_Lit_Escape_Char,
       T_Lit_Octal_Char,
       T_Lit_Hexa_Char,
       T_Lit_Unicode_Char,
       T_Lit_Wide_Simple_Char,
       T_Lit_Wide_Escape_Char,
       T_Lit_Wide_Octal_Char,
       T_Lit_Wide_Hexa_Char,
       T_Lit_Wide_Unicode_Char,
       T_Lit_Simple_Floating_Point,
       T_Lit_Exponent_Floating_Point,
       T_Lit_Pure_Exponent_Floating_Point,
       T_Lit_String,
       T_Lit_Wide_String,
       T_Lit_Simple_Fixed_Point,
       T_Lit_Floating_Fixed_Point,
       T_Identifier,                 --  Identifier
       T_Eof,                        --  Misc
       T_Pragma,
       T_Line
       );


   ----------------------------------------------------
   --  The main methods : initialize and next_token  --
   ----------------------------------------------------

   --  initializes the lexer by opening the file to process
   --  and by preprocessing it if necessary
   procedure Initialize (Filename : in String;
                         Preprocess : in Boolean;
                         Keep_Temporary_Files : in Boolean);

   --  Analyses further and returns the next token.
   --  Returns t_error if the entry is invalid.
   function Get_Next_Token return Idl_Token;


   -------------------------------------
   --  methods useful for the parser  --
   -------------------------------------

   --  Returns the location of the current_token
   function Get_Lexer_Location return Errors.Location;

   --  If the current token is an identifier, a literal or a pragma,
   --  returns its string value
   function Get_Lexer_string return String;


   -----------------------------
   --  idl string processing  --
   -----------------------------

   --  Compare two IDL identifiers.
   --  Returns Equal if they are equal (case sensitivity).
   --  Returns Case_Differ if they differ only in case.
   --  Returns Differ otherwise.
   type Ident_Equality is (Differ, Case_Differ, Equal);
   function Idl_Identifier_Equal (Left, Right : String) return Ident_Equality;



   -------------------------
   --  Maybe useless ???  --
   -------------------------

--    subtype Idl_Keywords is Idl_Token range T_Any .. T_Wstring;

--    function Idl_Compare (Left, Right : String) return Boolean;


--    --  Return the idl_token TOK as a string.
--    --  Format is "`keyword'", "`+'" (for symbols), "identifier `id'"
--    function Image (Tok : Idl_Token) return String;

end Tokens;


