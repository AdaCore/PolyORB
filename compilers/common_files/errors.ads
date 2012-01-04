------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               E R R O R S                                --
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

with Locations; use Locations;
with Types;     use Types;

package Errors is

   pragma Elaborate_Body;
   --  Because the body initializes the global variables below, so the compiler
   --  otherwise warns that they could be accessed by elaboration code in other
   --  packages.

   Not_Yet_Implemented : exception;
   --  Raised when code is not ready yet

   Internal_Error : exception;
   --  Raised when idlac reaches an internal inconsistent state

   Fatal_Error : exception;
   --  Raised when idlac has detected an external inconsistent state; that is,
   --  a user error like source-file-not-found. Whenever you raise Fatal_Error,
   --  you should print an error message first.

   type Message_Template is new String;
   --  Type of message templates used by Display_Error. We don't use type
   --  String, because we want to avoid using input data (tainted data) as part
   --  of the template. For example, if File_Name is a String that came from
   --  the command line, then:
   --
   --      Display_Error ("file not found: %", File_Name); -- Correct
   --      Display_Error ("file not found: " & File_Name); -- WRONG!
   --
   --  The second example will get a compilation error. If we allowed that,
   --  then we would crash if File_Name = "%".

   procedure Display_Error (Template : Message_Template);
   procedure DE (Template : Message_Template) renames Display_Error;
   --  Display a warning or error message based on the Template. The following
   --  special characters may appear in the Template:
   --
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

   procedure Display_Error (Template : Message_Template; S : String);
   procedure DE (Template : Message_Template; S : String)
     renames Display_Error;
   --  Same as previous Display_Error, except first put S into Error_Name (1).
   --  Template must contain a % special character, which will be replaced by
   --  S.

   Error_Name : array (1 .. 2) of Name_Id;
   Error_Loc  : array (1 .. 2) of Location;
   Error_Int  : array (1 .. 2) of Int;

   --  Count of errors and warnings displayed so far

   N_Errors   : Int := 0;
   N_Warnings : Int := 0;

end Errors;
