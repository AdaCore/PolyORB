------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L A C _ E R R O R S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
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

package Idlac_Errors is

   -----------------------
   -- Types definitions --
   -----------------------

   type String_Ptr is access String;

   --  Source file locations

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

   Internal_Error : exception;
   --  Internal use: raised when the parser detects that it is in an
   --  inconsistent state.

   Fatal_Error : exception;
   --  Exception raised when input cannot be processed

   --  Error levels

   type Error_Kind is (Fatal, Error, Warning);
   --  Fatal causes the immediate stop of the parsing, Error displays an
   --  error message, try to resume the parsing but does not generate any
   --  code. Warning only informs the user of a mistake but generates
   --  code normally.

   -----------------------
   -- Location handling --
   -----------------------

   function Location_To_String
     (Loc   : Location;
      Short : Boolean := False)
     return String;
   --  Return a string with the following format if Short is False:
   --  file : name_of_file, line : line_nb, column : column_nb
   --  or, if Short is True,
   --  name_of_file:line_nb:column_nb

   procedure Set_Path (Loc : in out Location; Filename : String);
   --  Set Loc.Dirname and Loc.Filename to the appropriate parts of Filename.

   --------------------
   -- Error handling --
   --------------------

   procedure Error (Message : String; Level : Error_Kind; Loc : Location);
   --  Produce an error message. Fatal_Error is raised if Level is Fatal

   function Is_Error return Boolean;
   --  True if an error occurred

   function Is_Warning return Boolean;
   --  True if a warning was emitted

   function Error_Number return Natural;
   --  Returns the number of errors

   function Warning_Number return Natural;
   --  Returns the number of warnings

end Idlac_Errors;
