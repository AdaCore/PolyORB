package body Utils is

   ---------
   -- Img --
   ---------

   function Img (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Node_Id) return String is
   begin
      if N = No_Node then
         return "No_Node";
      else
         return Img (Natural (N));
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (B : Boolean) return String is
   begin
      if B then
         return "True";
      else
         return "False";
      end if;
   end Img;

end Utils;
