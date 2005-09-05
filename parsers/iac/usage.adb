------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                                U S A G E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                           Copyright (c) 2005                             --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Backend; use Backend;
with Output;  use Output;

procedure Usage is
begin
   Set_Standard_Error;
   Write_Str  ("Usage: ");
   Write_Str  (Command_Name);
   Write_Line (" opts file [-cppargs args]");
   Write_Eol;
   Write_Line ("  name is a file from which you can omit the .idl suffix");
   Write_Eol;
   Write_Line ("  -c       Compile only");
   Write_Line ("  -E       Preprocess only");
   Write_Line ("  -k       Keep temporary files");
   Write_Line ("  -p       Produce source on standard output");
   Write_Eol;
   Write_Line ("  -gd      Generate delegation package");
   Write_Line ("  -gi      Generate implementation template");
   Write_Line ("  -gD      Generate dynamic invocation code");
   Write_Line ("  -gI      Generate interface repository code");
   Write_Eol;
   Write_Line
     ("  -<lang>  Generate code for a supported language");
   Write_Eol;
   Write_Languages (4, 12);
   Write_Line ("  -d?      Compiler debug option");
   Write_Eol;
   Write_Line ("  -cppargs Pass arguments to the C++ preprocessor");
   --  Write_Line ("  -I <dir> Shortcut -cppargs -I directory");
   Write_Eol;
   OS_Exit (1);
end Usage;
