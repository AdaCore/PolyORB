------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                U T I L S                                 --
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

with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with System;

package Utils is

   function Img (N : Integer) return String;
   function Img (N : Long_Long_Integer) return String;
   --  Return the image of an integer without the leading space

   function Img (N : Long_Long_Float) return String;
   --  Return the image of a Long_Long_Float.

   function Img (N : Node_Id) return String;
   --  Return the image of a Node_Id.

   function Img (N : Node_Kind) return String;
   --  Return the image of a Node_Kind.

   function Img (B : Boolean) return String;
   --  Return "True" or "False", cased that way.

   function Img (A : System.Address) return String;
   --  Return the image of an Address.

   pragma Inline (Img);
   --  All versions of Img are covered by this pragma

end Utils;
