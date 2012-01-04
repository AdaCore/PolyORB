------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . F I L E S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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
