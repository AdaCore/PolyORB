package body PolyORB.Opaque is

   function "+" (P : Opaque_Pointer; Ofs : Stream_Element_Offset)
                return Opaque_Pointer is
   begin
      return Opaque_Pointer'(P.Zone, P.Offset + Ofs);
   end "+";

end PolyORB.Opaque;
