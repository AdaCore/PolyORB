------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U S A G E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Ada.Command_Line; use Ada.Command_Line;

with Backend; use Backend;
with Output;  use Output;
with Platform;

procedure Usage is
begin
   --  Note: The following text needs to be kept in sync with the documentation
   --  in polyorb_ug.texi.

   Set_Standard_Error;
   Write_Line ("IAC from PolyORB " & Platform.Version);
   Write_Str  ("Usage: ");
   Write_Str  (Command_Name);
   Write_Line (" [options] file [-cppargs args...]");
   Write_Eol;
   Write_Line ("  -h       Print this help message, and do nothing else");
   Write_Eol;
   Write_Line ("  file is the name of the .idl file (.idl suffix optional)");
   Write_Eol;
   Write_Line ("  -E       Preprocess only");
   Write_Line ("  -k       Keep temporary files");
   Write_Line ("  -o DIR   Output directory (DIR must exist)");
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
   Write_Line ("  -gnatW8  Use UTF-8 character encoding in Ada output.");
   Write_Line ("           (Default is Latin-1.)");
   Write_Eol;
   Write_Line ("  -<lang>  Generate code for one of the following languages:");
   Write_Eol;
   Write_Languages (4, 12);
end Usage;
