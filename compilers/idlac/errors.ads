------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               E R R O R S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2001 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package Errors is

   -------------------------
   --  Types Definitions  --
   -------------------------

   --  A point in a source file

   type String_Ptr is access String;
   type Location is record
      Filename : String_Ptr;
      Dirname  : String_Ptr;
      Line     : Natural;
      Col      : Natural;
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

   function Location_To_String
     (Loc   : in Location;
      Short : in Boolean := False)
     return String;
   --  Return a string with the following format if Short is False:
   --  file : name_of_file, line : line_nb, column : column_nb
   --  or, if Short is True,
   --  name_of_file:line_nb:column_nb

   ----------------------
   --  Error handling  --
   ----------------------

   procedure Error
     (Message : in String;
      Level : in Error_Kind;
      Loc : in Location);
   --  Produce an error message. Fatal_Error is raised if
   --  Level is Fatal.

   function Is_Error return Boolean;
   --  was there any errors ?

   function Is_Warning return Boolean;
   --  was there any warning ?

   function Error_Number return Natural;
   --  returns the number of errors

   function Warning_Number return Natural;
   --  returns the number of warnings

end Errors;