------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . F I L E S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

--  This package contains different utilities for file manipulation:
--   - seaching included/imported files with substituting IDL file extensions;
--   - preprocessing files;

--  Most of this functionality moved from idlac driver and idl_fe-lexer.

package Idl_Fe.Files is

   procedure Add_Search_Path
    (Path    : String;
     Success : out Boolean);
   --  Add IDL files search path. If path don't exists or not a direcotry
   --  then Success is False.

   function Locate_IDL_File (File_Name : String) return String;
   --  Search file in search paths and return file path. Return empty
   --  string if file not found. Append IDL file suffix if it absent.

   function Locate_IDL_Specification (Scoped_Name : String) return String;
   --  Convert Scoped_Name to file name and search file in search paths.
   --  Return file path if found and empty string else.

   function Preprocess_File (File_Name : String) return String;
   --  Call the C++ preprocessor for processing defined file. Return
   --  file name of temporary file or empty string if something failed.

end Idl_Fe.Files;
