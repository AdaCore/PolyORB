with Ada.Unchecked_Deallocation;
with CORBA;

package Broca.Types is
   --  Buffer to/from network before unmarshalling or after marshalling.
   --  Must start at 0, according to CORBA V2.2 13.3
   type Buffer_Index_Type is new Natural;
   type Buffer_Type is array (Buffer_Index_Type range <>) of CORBA.Octet;
   type Buffer_Access is access Buffer_Type;

   --  A buffer with the current position, as needed by unmarshall and
   --  endianness.
   type Buffer_Descriptor is
      record
         Buffer : Buffer_Access := null;
         Pos : Buffer_Index_Type := 0;
         Little_Endian : CORBA.Boolean := False;
      end record;

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Name => Buffer_Access, Object => Buffer_Type);
end Broca.Types;
