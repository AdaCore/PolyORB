------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

package Utils is

   --  This package contains some utilities that will be used throughout
   --  the whole system.

   function Integer_To_String (I : Integer) return String;
   --  Return the image of an integer

   function String_To_Integer (S : String) return Integer;
   --  Return the integer corresponding to a string, or raise
   --  Constraint_Error if we have a malformed integer.

   function Get_Line (Prompt : String := "") return String;
   --  Input a line on standard input, using Prompt as a prompt if not empty.
   --  Will raise Ada.IO_Exceptions.End_Error if control-D is pressed.

end Utils;
