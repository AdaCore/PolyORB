with CORBA;
with Broca.Types; use Broca.Types;

package Broca.Marshalling is
   --  True if this machine use little endian byte order.
   Is_Little_Endian : Boolean;

   procedure Dump (Buffer : Buffer_Type);

   --  Alignment
   procedure Marshall_Align_2 (Buffer : in out Buffer_Descriptor);
   procedure Marshall_Align_4 (Buffer : in out Buffer_Descriptor);
   procedure Marshall_Align_8 (Buffer : in out Buffer_Descriptor);
   procedure Marshall_Align_16 (Buffer : in out Buffer_Descriptor);
   pragma Inline (Marshall_Align_2);
   pragma Inline (Marshall_Align_4);
   pragma Inline (Marshall_Align_8);
   pragma Inline (Marshall_Align_16);

   --  Append the contents of SOURCE to TARGET.
   --  TARGET size must be big enough.
   procedure Marshall_Append (Target : in out Buffer_Descriptor;
                              Source : in Buffer_Descriptor);
   procedure Marshall_Size_Append (Target : in out Buffer_Descriptor;
                                   Source : in Buffer_Descriptor);

   --  Extract LENGTH octets from SOURCE, update position of SOURCE and copy
   --  it to TARGET.
   --  TARGET must be big enough, however, position is reset before copying
   --  and not updated, and little_endian flag is copied from SOURCE.
   procedure Unmarshall_Extract (Target : in out Buffer_Descriptor;
                                 Source : in out Buffer_Descriptor;
                                 Length : Buffer_Index_Type);

   --  Return true if BUFFER can be interpreted as STR.
   --  The string is not unmarshalled.
   function Marshall_Compare (Stream : in Buffer_Descriptor;
                              Str : in String) return Boolean;

   --  Skip a string in a buffer.
   procedure Unmarshall_Skip_String (Buffer : in out Buffer_Descriptor);

   --  Unmarshall procedure
   --  Extract a type from BUFFER starting at POS, which maybe aligned before,
   --  and update the position POS.
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Octet);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Char);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Boolean);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Unsigned_Short);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Unsigned_Long);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Short);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Long);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.String);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Float);
   procedure Unmarshall
     (Stream : in out Buffer_Descriptor; Res : out CORBA.Double);

   --  Size procedure
   --  Only adjust stream.pos according to alignment.

   --  For a sequence of a primitive type.
   procedure Marshall_Size_Primitive_Sequence
     (Stream : in out Buffer_Descriptor;
      Element_Size : Natural;
      Nbr_Elements : Natural);

   procedure Marshall_Size_Octet (Stream : in out Buffer_Descriptor);
   procedure Marshall_Size_Unsigned_Short (Stream : in out Buffer_Descriptor);
   procedure Marshall_Size_Unsigned_Long (Stream : in out Buffer_Descriptor);

   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.String);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : String);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Octet);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Char);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Boolean);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Long);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Short);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Long);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Short);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Float);
   procedure Marshall_Size
     (Stream : in out Buffer_Descriptor; Val : CORBA.Double);

   --  For an outcoming stream, allocate the buffer and clear pos.
   procedure Allocate_Buffer (Stream : in out Buffer_Descriptor);

   --  Be sure STREAM.Buffer is at least of length SIZE.
   --  Set STREAM.Pos to SIZE.
   procedure Increase_Buffer_And_Set_Pos
     (Stream : in out Buffer_Descriptor; Size : Buffer_Index_Type);
   --  Be sure STREAM.Buffer is at least of length SIZE.
   --  Set STREAM.Pos to 0.
   procedure Increase_Buffer_And_Clear_Pos
     (Stream : in out Buffer_Descriptor; Size : Buffer_Index_Type);

   --  Marshall
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Octet);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Char);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Boolean);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Short);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Unsigned_Long);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Short);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Long);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.String);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Float);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : CORBA.Double);
   procedure Marshall
     (Stream : in out Buffer_Descriptor; Val : String);

end Broca.Marshalling;
