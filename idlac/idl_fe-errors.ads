------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        I D L _ F E . E R R O R S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package Idl_Fe.Errors is

   -------------------------
   --  Types Definitions  --
   -------------------------

   --  type for the file names
   type File_Name_Ptr is access String;

   --  type for the directory names
   type Dir_Name_Ptr is access String;

   --  How to deallocate a file name
   procedure Free is new Ada.Unchecked_Deallocation (String, File_Name_Ptr);

   --  How to deallocate a directory name
   procedure Free is new Ada.Unchecked_Deallocation (String, Dir_Name_Ptr);

   --  defines a place in one of the parsed files
   type Location is record
      Filename : File_Name_Ptr;
      Dirname : Dir_Name_Ptr;
      Line : Natural;
      Col : Natural;
   end record;

   No_Location : constant Location :=
     (Filename => null,
      Dirname => null,
      Line => 0,
      Col => 0);

   --  this exception is for internal use,
   --  it is raised when idlac reaches an inconsistent state
   Internal_Error : exception;

   --  this exception is raised when the compiler cannot parse
   --  its entry
   Fatal_Error : exception;

   --  defines three levels of error
   --  Fatal causes the immediate stop of the parsing, Error displays
   --  an error, try to resume the parsing but does not generate any
   --  code, Warning only informs the user of a mistake but generates
   --  code normally
   type Error_Kind is (Fatal, Error, Warning);



   -------------------------
   --  Location handling  --
   -------------------------

   --  returns a string with the following format :
   --  file : name_of_file, line : line_nb, column : column_nb
   function Display_Location (Loc : in Location) return String;



   ----------------------
   --  Error handling  --
   ----------------------

   --  deals with a Lexer error, raise it if level is fatal
   procedure Lexer_Error (Message : in String;
                          Level : in Error_Kind;
                          Loc : Location);

   --  deals with a Parser error, raise it if level is fatal
   procedure Parser_Error (Message : in String;
                           Level : in Error_Kind;
                           Loc : in Location);

   --  was there any errors ?
   function Is_Error return Boolean;

   --  was there any warning ?
   function Is_Warning return Boolean;

   --  returns the number of errors
   function Error_Number return Natural;

   --  returns the number of warnings
   function Warning_Number return Natural;


private

   --  nice display of a natural
   function Nat_To_String (Val : Natural) return String;

   --  display an error
   procedure Display_Error (Message : in String;
                            Level : in Error_Kind;
                            Loc : Location);

end Idl_Fe.Errors;
