------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         S O U R C E _ I N P U T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2011-2012, Free Software Foundation, Inc.          --
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

--  This package contains routines for reading in the "source" files, including
--  the source file named on the command line, included and imported source
--  files, and the temp files used to store the preprocessor output (even
--  though those temp files are not true "source", strictly speaking).
--
--  When we read a source file, we read the entire contents into a buffer, and
--  it stays in memory for the entire run of IAC. The heap-allocated data
--  structures in this package are never reclaimed.

with Types; use Types;

package Source_Input is

   pragma Elaborate_Body (Source_Input);

   type Source_Kind is (True_Source, Preprocessed_Source);

   type Source_File is limited
      record
         Name   : Name_Id;
         Buffer : Text_Buffer_Ptr;
         --  Content of the file, followed by EOF
         Kind   : Source_Kind;
      end record;
   type Source_File_Ptr is access constant Source_File;

   function Open_Source
     (Name : Name_Id; Kind : Source_Kind) return Source_File_Ptr;
   --  Read the named source file, and return the data in a Source_File.
   --  Gives a fatal error if the file is not found.

   function Named_File (Name : Name_Id) return Source_File_Ptr;
   --  The named file must already have been created by Open_Source; this
   --  returns it, and must be a True_Source. (???Currently not used; see
   --  below.)

   procedure Iterate_Source_Files
     (Process : not null access procedure (Source : Source_File));
   --  Iterates through all Source_Files, in the order they were created by
   --  Open_Source.

   procedure Iterate_Lines
     (Source : Source_File;
      Process : not null access procedure (Line : String));
   --  Iterates through the lines of the source file, calling Process on each
   --  line. Line terminators (LF, CR, CRLF) are not included in Line.

   procedure Copy_To_Standard_Output (Source : Source_File);
   --  Copy the file contents to standard output.

   --  ???Part of the purpose of this package is to allow us to put the IDL
   --  source code in the Ada stubs package spec. The IDL should be commented
   --  out, and interspersed with the Ada, according to the source
   --  locations. That feature is not yet complete. Currently, we put the
   --  source at the top of the package (not interspersed). And we don't bother
   --  with #include files. Named_File is intended to support the interspersal;
   --  it is currently not used.

end Source_Input;
