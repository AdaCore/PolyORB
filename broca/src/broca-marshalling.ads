with CORBA;
with Broca.Buffers; use Broca.Buffers;

package Broca.Marshalling is

   O_Size  : constant Buffer_Index_Type := 1;
   C_Size  : constant Buffer_Index_Type := 1;
   B_Size  : constant Buffer_Index_Type := 1;
   S_Size  : constant Buffer_Index_Type := 2;
   US_Size : constant Buffer_Index_Type := 2;
   L_Size  : constant Buffer_Index_Type := 4;
   UL_Size : constant Buffer_Index_Type := 4;
   F_Size  : constant Buffer_Index_Type := 4;
   D_Size  : constant Buffer_Index_Type := 8;

   -- Compute_Size, Marshall and Unmarshall predefined types.

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Octet);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Char);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Boolean);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Unsigned_Short);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Short);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Unsigned_Long);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Long);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.String);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Float);

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Result : out CORBA.Double);


   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.String);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Octet);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Char);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Boolean);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Short);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Short);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Long);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Long);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Float);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Double);

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in String);

   procedure Compute_New_Size
     (Buffer        : in out Buffer_Descriptor;
      Length_Size   : in Buffer_Index_Type;
      Element_Size  : in Buffer_Index_Type;
      Array_Length  : in Natural);

   procedure Skip_String (Buffer : in out Buffer_Descriptor);
   --  Unmarshall String and ignore result.

   --  Marshall
   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Octet);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Char);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Boolean);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Short);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Short);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Unsigned_Long);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Long);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Float);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.Double);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in String);

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in CORBA.String);

end Broca.Marshalling;
