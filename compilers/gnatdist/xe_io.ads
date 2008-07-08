------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                                X E _ I O                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1995-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains all the routines needed to handle input and
--  output operations.

with GNAT.OS_Lib; use GNAT.OS_Lib;
with XE_Types;    use XE_Types;
with XE_Units;    use XE_Units;

package XE_IO is

   ---------------------------------
   -- File and Directory Handling --
   ---------------------------------

   procedure Copy_File (Source, Target : File_Name_Type);
   --  Copy source file into target file (preserves file stamps)

   procedure Rename_File (Source, Target : File_Name_Type);
   --  Rename source file into target file

   procedure Create_File
     (File  : out File_Descriptor;
      Fname : File_Name_Type;
      Exec  : Boolean := False);
   --  Create file Fname and make it executable when required

   procedure Delete_File (Fname : File_Name_Type);
   --  Delete Fname, fail silently if the file does not exists but
   --  raise Fatal Error if it file exists and cannot be deleted.

   procedure Create_Dir (Dname : Directory_Name_Type);
   --  Create a directory Dname. This function creates all the
   --  subdirectories (separated by a Directory_Separator) one by one
   --  and then creates the final directory.

   function Is_Directory    (Fname : File_Name_Type) return Boolean;
   function Is_Regular_File (Fname : File_Name_Type) return Boolean;

   function To_Absolute_File (Fname : File_Name_Type) return File_Name_Type;
   --  When the directory corresponding to Fname is already an
   --  absolute directory, return Fname. Otherwise, prefix Fname by
   --  the current directory.

   function Dir
     (D1   : Directory_Name_Type;
      D2   : Directory_Name_Type)
      return Directory_Name_Type;
   function Dir
     (D1   : String_Access;
      D2   : Directory_Name_Type)
      return Directory_Name_Type;
   --  Concatenate several names and insert a directory separator
   --  between them.

   function Strip_Directory (Fname : String) return String;
   function Strip_Directory (Fname : File_Name_Type) return File_Name_Type;
   --  Strips the prefix directory name (if any) from Name. Returns the
   --  stripped name. Name cannot end with a directory separator.

   function Strip_Suffix (Fname : File_Name_Type) return File_Name_Type;
   --  Strips the suffix (the last '.' and whatever comes after it) from Name.
   --  Returns the stripped name.

   function Strip_Exec_Suffix (Fname : File_Name_Type) return File_Name_Type;
   --  When suffix is an executable suffix, strip it

   function Normalize_CWD (F : File_Name_Type) return File_Name_Type;
   --  Remove any leading CWD (./)

   function To_Afile (Fname : File_Name_Type) return File_Name_Type;
   function To_Ofile (Fname : File_Name_Type) return File_Name_Type;
   --  Strip suffix and add resp. ALI suffix or object suffix

   function Is_Predefined_File (Fname : File_Name_Type) return Boolean;
   --  Return True when Fname belongs to Ada, GNAT, Interfaces or
   --  System hierarchy.

   function File_Time_Stamp (Fname : File_Name_Type) return Time_Stamp_Type;
   --  Return image of file Fname time stamp.

   ------------------------------
   -- Temporary Files Handling --
   ------------------------------

   procedure Register_Temp_File
     (File  : out File_Descriptor;
      Fname : in out File_Name_Type);
   --  When Fname is null, create a new temporary file. Otherwise, consider
   --  Fname as a temporary file to be removed when Remove_All_Temporary_Files
   --  is invoked.

   procedure Register_Temp_File (Fname : File_Name_Type);
   --  Insert Fname in the list of temporary files

   procedure Remove_Temp_File (Fname : File_Name_Type);
   --  Remove Fname file and remove it from the temporary files list

   procedure Remove_All_Temp_Files;
   --  Remove all the files registered as temporary files

   ----------------------
   -- Message Handling --
   ----------------------

   procedure Message
     (S1 : String  := No_Str;
      S2 : Name_Id := No_Name;
      S3 : String  := No_Str;
      S4 : Name_Id := No_Name;
      S5 : String  := No_Str);
   --  Display a message to the standard output. The message is the
   --  concatenation of S1 to S5. Parameters with default values are not
   --  displayed.

   procedure Write_Stamp_Comparison (Newer, Older   : File_Name_Type);
   procedure Write_Program_Name;

   ---------------------
   -- Output Handling --
   ---------------------

   procedure Set_Output (New_Output : GNAT.OS_Lib.File_Descriptor);
   --  Sets subsequent output to appear on the given file

   procedure Set_Standard_Error;
   --  Sets subsequent output to appear on the standard error file
   --  (whatever that might mean for the host operating system, if
   --  anything).

   procedure Set_Standard_Output;
   --  Sets subsequent output to appear on the standard output file
   --  (whatever that might mean for the host operating system, if
   --  anything). Output to standard output is the default mode before
   --  any call to either of the Set procedures.

   procedure Write_Char (C : Character);
   --  Write one character to the standard output file. Note that the
   --  character should not be LF or CR (use Write_Eol for end of line)

   procedure Write_Eol (N : Natural := 1);
   --  Write an end of line (whatever is required by the system in use,
   --  e.g. CR/LF for DOS, or LF for Unix) to the standard output file.
   --  This routine also empties the line buffer, actually writing it
   --  to the file. Note that Write_Eol is the only routine that causes
   --  any actual output to be written.

   procedure Write_Int (Val : Int);
   --  Write an integer value with no leading blanks or zeroes. Negative
   --  values are preceded by a minus sign).

   procedure Write_Str (S : String);
   --  Write a string of characters to the standard output file. Note that
   --  end of line is handled separately using WRITE_EOL, so the string
   --  should not contain either of the characters LF or CR, but it may
   --  contain horizontal tab characters.

   procedure Write_Line (S : String);
   --  Equivalent to Write_Str (S) followed by Write_Eol;

   Space_Increment : Natural := 2;
   N_Space         : Natural := 0;

   procedure Decrement_Indentation;
   procedure Increment_Indentation;

   procedure Set_Space_Increment (Value : Natural);
   procedure Write_Indentation (Offset : Integer := 0);
   procedure Write_Space;

   --------------------
   -- Input Handling --
   --------------------

   procedure Read_File
     (Fname  : File_Name_Type;
      First  : out Text_Ptr;
      Last   : out Text_Ptr;
      Buffer : out Text_Buffer_Ptr);

end XE_IO;
