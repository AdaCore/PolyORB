with Idl_Fe.Types; use Idl_Fe.Types;

package Utils is

   function Img (N : Natural) return String;
   --  Return the image of a non-negative integer
   --  without the leading space

   function Img (N : Node_Id) return String;
   --  Return the image of a Node_Id.

   function Img (B : Boolean) return String;
   --  Return "True" or "False", cased that way.

   pragma Inline (Img);
   --  All versions of Img are covered by this pragma

end Utils;
