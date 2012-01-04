------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          I D L A C _ U T I L S                           --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Utilities for the IDLAC compiler.

with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with System;

package Idlac_Utils is

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

end Idlac_Utils;
