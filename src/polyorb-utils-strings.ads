------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S T R I N G S                 --
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

--  General-purpose string pointer and related functions

with Ada.Unchecked_Deallocation;

package PolyORB.Utils.Strings is

   pragma Preelaborate;

   -----------------
   -- Conversions --
   -----------------

   function To_Boolean (V : String) return Boolean;
   --  Convert a String value to a Boolean value according to the following
   --  interpretation rules:
   --  * True if the value starts with '1' or 'Y' or 'y',
   --    or is "on" or "enable" or "true"
   --  * False if the value starts with '0' or 'n' or 'N',
   --    or is "off" or "disable" or "false" or empty.
   --  Constraint_Error is raised if the value is set to anything else.

   --------------------
   -- String Pointer --
   --------------------

   type String_Ptr is access all Standard.String;

   function "+"
     (S : Standard.String)
     return String_Ptr;
   pragma Inline ("+");

   procedure Free is new Ada.Unchecked_Deallocation
     (Standard.String, String_Ptr);

end PolyORB.Utils.Strings;
