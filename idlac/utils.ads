with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with System;

package Utils is

   function Img (N : Natural) return String;
   --  Return the image of a non-negative integer
   --  without the leading space

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
