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

package Idents is
   --  Be careful when modifying this file.
   --  Order of identifiers must be the same as the order in idents.adb.
   --  Furthermore, order of IDL keywords must be the same as tokens, defined
   --  in tokens.ads.
   --  FIXME:  use the same Ada identifier, to avoid errors ?
   --        anyway, number of IDL tokens does not change a lot!

   --  IDL tokens.
   Id_First :     constant Uniq_Id := 1;
   Id_First_tok : constant Uniq_Id := Id_First;
   Id_Any :       constant Uniq_Id := 1;
   Id_Attribute : constant Uniq_Id := 2;
   Id_Boolean :   constant Uniq_Id := 3;
   Id_Case :      constant Uniq_Id := 4;
   Id_Char :      constant Uniq_Id := 5;
   Id_Const :     constant Uniq_Id := 6;
   Id_Context :   constant Uniq_Id := 7;
   Id_Default :   constant Uniq_Id := 8;
   Id_Double :    constant Uniq_Id := 9;
   Id_Enum :      constant Uniq_Id := 10;
   Id_Exception : constant Uniq_Id := 11;
   Id_False :     constant Uniq_Id := 12;
   Id_Fixed :     constant Uniq_Id := 13;
   Id_Float :     constant Uniq_Id := 14;
   Id_In :        constant Uniq_Id := 15;
   Id_Inout :     constant Uniq_Id := 16;
   Id_Interface : constant Uniq_Id := 17;
   Id_Long :      constant Uniq_Id := 18;
   Id_Module :    constant Uniq_Id := 19;
   Id_Native :    constant Uniq_Id := 20;
   Id_Object :    constant Uniq_Id := 21;
   Id_Octet :     constant Uniq_Id := 22;
   Id_Oneway :    constant Uniq_Id := 23;
   Id_Out :       constant Uniq_Id := 24;
   Id_Raises :    constant Uniq_Id := 25;
   Id_Readonly :  constant Uniq_Id := 26;
   Id_Sequence :  constant Uniq_Id := 27;
   Id_Short :     constant Uniq_Id := 28;
   Id_String :    constant Uniq_Id := 29;
   Id_Struct :    constant Uniq_Id := 30;
   Id_Switch :    constant Uniq_Id := 31;
   Id_True :      constant Uniq_Id := 32;
   Id_Typedef :   constant Uniq_Id := 33;
   Id_Unsigned :  constant Uniq_Id := 34;
   Id_Union :     constant Uniq_Id := 35;
   Id_Void :      constant Uniq_Id := 36;
   Id_Wchar :     constant Uniq_Id := 37;
   Id_Wstring :   constant Uniq_Id := 38;
   Id_Last_Tok :  constant Uniq_Id := 38;

   --  Well known identifiers.
   --  Preprocessor
   Id_Pragma :    constant Uniq_Id := Id_Last_Tok + 1;
   Id_ID :        constant Uniq_Id := Id_Last_Tok + 2;
   Id_Prefix :    constant Uniq_Id := Id_Last_Tok + 3;
   Id_Version :   constant Uniq_Id := Id_Last_Tok + 4;
   Id_Subsystem : constant Uniq_Id := Id_Last_Tok + 5;

   --  Ada reserved words, not previouly defined.
   --  Note: case, exception, in, out are IDL keywords too.
   --  Note: pragma was already defined.
   Id_First_Ada_Word : constant Uniq_Id := Id_Last_Tok + 6;
   Id_Abort :      constant Uniq_Id := Id_First_Ada_Word + 0;
   Id_Abs :        constant Uniq_Id := Id_First_Ada_Word + 1;
   Id_Abstract :   constant Uniq_Id := Id_First_Ada_Word + 2;
   Id_Accept :     constant Uniq_Id := Id_First_Ada_Word + 3;
   Id_Access :     constant Uniq_Id := Id_First_Ada_Word + 4;
   Id_Aliased :    constant Uniq_Id := Id_First_Ada_Word + 5;
   Id_All :        constant Uniq_Id := Id_First_Ada_Word + 6;
   Id_And :        constant Uniq_Id := Id_First_Ada_Word + 7;
   Id_Array :      constant Uniq_Id := Id_First_Ada_Word + 8;
   Id_At :         constant Uniq_Id := Id_First_Ada_Word + 9;
   Id_Begin :      constant Uniq_Id := Id_First_Ada_Word + 10;
   Id_Body :       constant Uniq_Id := Id_First_Ada_Word + 11;
   Id_Constant :   constant Uniq_Id := Id_First_Ada_Word + 12;
   Id_Declare :    constant Uniq_Id := Id_First_Ada_Word + 13;
   Id_Delay :      constant Uniq_Id := Id_First_Ada_Word + 14;
   Id_Delta :      constant Uniq_Id := Id_First_Ada_Word + 15;
   Id_Digits :     constant Uniq_Id := Id_First_Ada_Word + 16;
   Id_Do :         constant Uniq_Id := Id_First_Ada_Word + 17;
   Id_Else :       constant Uniq_Id := Id_First_Ada_Word + 18;
   Id_Elsif :      constant Uniq_Id := Id_First_Ada_Word + 19;
   Id_End :        constant Uniq_Id := Id_First_Ada_Word + 20;
   Id_Entry :      constant Uniq_Id := Id_First_Ada_Word + 21;
   Id_Exit :       constant Uniq_Id := Id_First_Ada_Word + 22;
   Id_For :        constant Uniq_Id := Id_First_Ada_Word + 23;
   Id_Function :   constant Uniq_Id := Id_First_Ada_Word + 24;
   Id_Generic :    constant Uniq_Id := Id_First_Ada_Word + 25;
   Id_Goto :       constant Uniq_Id := Id_First_Ada_Word + 26;
   Id_If :         constant Uniq_Id := Id_First_Ada_Word + 27;
   Id_Is :         constant Uniq_Id := Id_First_Ada_Word + 28;
   Id_Limited :    constant Uniq_Id := Id_First_Ada_Word + 29;
   Id_Loop :       constant Uniq_Id := Id_First_Ada_Word + 30;
   Id_Mod :        constant Uniq_Id := Id_First_Ada_Word + 31;
   Id_New :        constant Uniq_Id := Id_First_Ada_Word + 32;
   Id_Not :        constant Uniq_Id := Id_First_Ada_Word + 33;
   Id_Null :       constant Uniq_Id := Id_First_Ada_Word + 34;
   Id_Of :         constant Uniq_Id := Id_First_Ada_Word + 35;
   Id_Or :         constant Uniq_Id := Id_First_Ada_Word + 36;
   Id_Others :     constant Uniq_Id := Id_First_Ada_Word + 37;
   Id_Package :    constant Uniq_Id := Id_First_Ada_Word + 38;
   Id_Private :    constant Uniq_Id := Id_First_Ada_Word + 39;
   Id_Procedure :  constant Uniq_Id := Id_First_Ada_Word + 40;
   Id_Protected :  constant Uniq_Id := Id_First_Ada_Word + 41;
   Id_Raise :      constant Uniq_Id := Id_First_Ada_Word + 42;
   Id_Range :      constant Uniq_Id := Id_First_Ada_Word + 43;
   Id_Record :     constant Uniq_Id := Id_First_Ada_Word + 44;
   Id_Rem :        constant Uniq_Id := Id_First_Ada_Word + 45;
   Id_Renames :    constant Uniq_Id := Id_First_Ada_Word + 46;
   Id_Requeue :    constant Uniq_Id := Id_First_Ada_Word + 47;
   Id_Return :     constant Uniq_Id := Id_First_Ada_Word + 48;
   Id_Reverse :    constant Uniq_Id := Id_First_Ada_Word + 49;
   Id_Select :     constant Uniq_Id := Id_First_Ada_Word + 50;
   Id_Separate :   constant Uniq_Id := Id_First_Ada_Word + 51;
   Id_Subtype :    constant Uniq_Id := Id_First_Ada_Word + 52;
   Id_Tagged :     constant Uniq_Id := Id_First_Ada_Word + 53;
   Id_Task :       constant Uniq_Id := Id_First_Ada_Word + 54;
   Id_Terminate :  constant Uniq_Id := Id_First_Ada_Word + 55;
   Id_Then :       constant Uniq_Id := Id_First_Ada_Word + 56;
   Id_Type :       constant Uniq_Id := Id_First_Ada_Word + 57;
   Id_Until :      constant Uniq_Id := Id_First_Ada_Word + 58;
   Id_Use :        constant Uniq_Id := Id_First_Ada_Word + 59;
   Id_When :       constant Uniq_Id := Id_First_Ada_Word + 60;
   Id_While :      constant Uniq_Id := Id_First_Ada_Word + 61;
   Id_With :       constant Uniq_Id := Id_First_Ada_Word + 62;
   Id_Xor :        constant Uniq_Id := Id_First_Ada_Word + 63;
   Id_Last_Ada_Word : constant Uniq_Id := Id_Xor;

   Id_Last :       constant Uniq_Id := Id_Last_Ada_Word;

   procedure Initialize;
end Idents;
