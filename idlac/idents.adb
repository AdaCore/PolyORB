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

with types; use Types;

package body Idents is
   type Idents_Array is array (Uniq_Id range <>) of String_Cacc;
   All_Idents : Idents_Array (Id_First .. Id_Last) :=
     (new String'("any"),
      new String'("attribute"),
      new String'("boolean"),
      new String'("case"),
      new String'("char"),
      new String'("const"),
      new String'("context"),
      new String'("default"),
      new String'("double"),
      new String'("enum"),
      new String'("exception"),
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
      new String'("raises"),
      new String'("readonly"),
      new String'("sequence"),
      new String'("short"),
      new String'("string"),
      new String'("struct"),
      new String'("switch"),
      new String'("TRUE"),
      new String'("typedef"),
      new String'("unsigned"),
      new String'("union"),
      new String'("void"),
      new String'("wchar"),
      new String'("wstring"),

      new String'("pragma"),
      new String'("ID"),
      new String'("prefix"),
      new String'("version"),
      new String'("subsystem"),

      new String'("abort"),
      new String'("abs"),
      new String'("abstract"),
      new String'("accept"),
      new String'("access"),
      new String'("aliased"),
      new String'("all"),
      new String'("and"),
      new String'("array"),
      new String'("at"),
      new String'("begin"),
      new String'("body"),
      new String'("constant"),
      new String'("declare"),
      new String'("delay"),
      new String'("delta"),
      new String'("digits"),
      new String'("do"),
      new String'("else"),
      new String'("elsif"),
      new String'("end"),
      new String'("entry"),
      new String'("exit"),
      new String'("for"),
      new String'("function"),
      new String'("generic"),
      new String'("goto"),
      new String'("if"),
      new String'("is"),
      new String'("limited"),
      new String'("loop"),
      new String'("mod"),
      new String'("new"),
      new String'("not"),
      new String'("null"),
      new String'("of"),
      new String'("or"),
      new String'("others"),
      new String'("package"),
      new String'("private"),
      new String'("procedure"),
      new String'("protected"),
      new String'("raise"),
      new String'("range"),
      new String'("record"),
      new String'("rem"),
      new String'("renames"),
      new String'("requeue"),
      new String'("return"),
      new String'("reverse"),
      new String'("select"),
      new String'("separate"),
      new String'("subtype"),
      new String'("tagged"),
      new String'("task"),
      new String'("terminate"),
      new String'("then"),
      new String'("type"),
      new String'("until"),
      new String'("use"),
      new String'("when"),
      new String'("while"),
      new String'("with"),
      new String'("xor")
     );

   procedure Initialize is
   begin
      for I in All_Idents'Range loop
         if Add_Identifier (All_Idents (I)) /= I then
            raise Internal_Error;
         end if;
      end loop;
   end Initialize;
end Idents;
