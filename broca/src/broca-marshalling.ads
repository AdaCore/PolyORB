with CORBA;
with Broca.Buffers; use Broca.Buffers;

package Broca.Marshalling is

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

   function Compare (Buffer  : in Buffer_Descriptor;
                     Pattern : in String) return Boolean;
   --  Return true if Buffer can be interpreted as Pattern.  The
   --  string is not unmarshalled.

   procedure Skip_String (Buffer : in out Buffer_Descriptor);
   --  Unmarshall String and ignore result.

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
