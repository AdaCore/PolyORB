package Utils is

   pragma Pure;

   function Img (N : Natural) return String;
   --  Return the image of a non-negative integer without the leading space

   function Img (B : Boolean) return String;
   --  Return "True" or "False", cased as is

   pragma Inline (Img);
   --  All versions of Img are covered by this pragma

end Utils;
