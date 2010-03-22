------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               E R R O R S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2010, Free Software Foundation, Inc.          --
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

with Locations; use Locations;
with Types;     use Types;

package Errors is

   Not_Yet_Implemented : exception;
   --  Raised when code is not ready yet

   Internal_Error : exception;
   --  Raised when idlac reaches an internal inconsistent state

   Fatal_Error : exception;
   --  Raised when idlac has detected an external inconsistent state; that is,
   --  a user error like source-file-not-found. Whenever you raise Fatal_Error,
   --  you should print an error message first.

   procedure Display_Error (S : String);
   procedure DE (S : String) renames Display_Error;
   --  Display a warning or error message S. The following special characters
   --  may appear in the message:
   --    % (Percent):      insert Error_Name (N)
   --    # (Hash):         insert Error_Name (N) within quotes
   --    ! (Exclamantion): insert Error_Loc (L)
   --    $ (Dollar):       insert Error_Int (I)
   --    ? (Question):     make message a warning
   --    \ (Backslash):    make message a continuation (does not count toward
   --                      errors or warnings counter)
   --  Where N, L and I are indices incremented after each insertion character
   --  (note that Error_Loc (1) is always inserted ahead of the message, and
   --  L starts at 2 as far as explicit '!' insertions are concerned).

   procedure Initialize;

   Error_Name : array (1 .. 2) of Name_Id;
   Error_Loc  : array (1 .. 2) of Location;
   Error_Int  : array (1 .. 2) of Int;

   --  Count of errors and warnings displayed so far

   N_Errors   : Int := 0;
   N_Warnings : Int := 0;

end Errors;
