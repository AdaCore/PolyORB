--  Storage of opaque data.

--  $Id: //droopi/main/src/droopi-opaque.ads#3 $

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

package Droopi.Opaque is

   pragma Preelaborate;

   type Zone_Access is access all Stream_Element_Array;
   --  A storage zone: an array of bytes.

   procedure Free is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Zone_Access);

   type Opaque_Pointer is record
      Zone : Zone_Access;
      --  The storage zone wherein the data resides.

      Offset : Stream_Element_Offset;
      --  The position of the first data element within the zone.

   end record;

   function "+" (P : Opaque_Pointer; Ofs : Stream_Element_Offset)
                return Opaque_Pointer;
   pragma Inline ("+");
   --  Add Ofs to P.Offset.

   subtype Alignment_Type is Stream_Element_Offset range 1 .. 8;

end Droopi.Opaque;
