--  Storage of opaque data.

--  $Id: //droopi/main/src/droopi-opaque.ads#1 $

with Ada.Streams;

package Droopi.Opaque is

   pragma Preelaborate;

   type Zone_Access is access all Ada.Streams.Stream_Element_Array;
   --  A storage zone: an array of bytes.

   type Opaque_Pointer is record
      Zone : Zone_Access;
      --  The storage chunk within which the data resides.

      First, Last : Ada.Streams.Stream_Element_Offset;
      --  The beginning and end offset of the data within
      --  the chunk.
   end record;

   subtype Alignment_Type is Ada.Streams.Stream_Element_Offset range 1 .. 8;

end Droopi.Opaque;
