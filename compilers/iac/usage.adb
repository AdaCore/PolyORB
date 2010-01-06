------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U S A G E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Backend; use Backend;
with Output;  use Output;
with Platform;

procedure Usage is
begin
   Set_Standard_Error;
   Write_Line ("IAC from PolyORB " & Platform.Version);
   Write_Str  ("Usage: ");
   Write_Str  (Command_Name);
   Write_Line (" opts file [-cppargs args]");
   Write_Eol;
   Write_Line ("  name is a file from which you can omit the .idl suffix");
   Write_Eol;
   Write_Line ("  -E       Preprocess only");
   Write_Line ("  -k       Keep temporary files");
   Write_Line ("  -o DIR   Specify output directory (DIR must exist)");
   Write_Line ("  -p       Produce source on standard output");
   Write_Line ("  -q       Quiet mode");
   Write_Eol;
   Write_Line ("  -dm      Generate debug messages when analyzing scopes");
   Write_Eol;
   Write_Line ("  -df      Dump the frontend tree (the IDL tree)");
   Write_Eol;
   Write_Line ("  -cppargs Pass arguments to the C++ preprocessor");
   Write_Line ("  -I <dir> Shortcut -cppargs -I directory. Use this flag");
   Write_Line ("           for the imported entities");
   Write_Line ("  -nocpp   Do not preprocess input");
   Write_Eol;
   Write_Line ("  -<lang>  Generate code for a supported language");
   Write_Eol;
   Write_Languages (4, 12);
   OS_Exit (1);
end Usage;
