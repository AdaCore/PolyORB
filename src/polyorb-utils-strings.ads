------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S T R I N G S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

--  General-purpose string pointer and related functions

with PolyORB.Utils.Unchecked_Deallocation;

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

   ---------------------
   -- String accesses --
   ---------------------

   type String_Ptr is access all Standard.String;

   function "+" (S : Standard.String) return String_Ptr;
   pragma Inline ("+");
   --  Return new String('S)

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Standard.String,
      Name => String_Ptr);

end PolyORB.Utils.Strings;
