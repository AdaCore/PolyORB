------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

--  Utilities for the IDLAC compiler.

with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with System;

package Utils is

   function Img (N : Character) return String;
   function Img (N : Integer) return String;
   function Img (N : Idl_Integer) return String;
   --  Return the image of an integer without the leading space

   function Long_Integer_Img
     (N : Long_Integer) return String;
   --  Same as above. This function cannot be named Img because this would
   --  conflict with the one for Idl_Integer in environments where 64-bit
   --  integers are not supported (eg on Apex). In such environments,
   --  Idl_Integer is a subtype of Long_Integer.

   function Img (N : Idl_Float) return String;
   --  Return the image of a Long_Long_Float

   function Img (N : Node_Id) return String;
   --  Return the image of a Node_Id

   function Img (N : Node_Kind) return String;
   --  Return the image of a Node_Kind

   function Img (B : Boolean) return String;
   --  Return "True" or "False", cased that way

   function Img (A : System.Address) return String;
   --  Return the image of an Address

   pragma Inline (Img);
   --  All versions of Img are covered by this pragma

   function Img (A : Constant_Value_Ptr) return String;
   --  Return the image of a constant

end Utils;
