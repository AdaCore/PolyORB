with Ada.Finalization;
with CORBA;

package Broca.CDR is

   --  This package takes care of CDR and GIOP encoding. It has not been
   --  tuned for efficiency yet, but should be easy to use.

   type Buffer_Type is private;

   type Endianess_Type is (Little_Endian, Big_Endian);

   type Index_Type is mod 2 ** 32;

   type Octet_Array is array (Index_Type range <>) of CORBA.Octet;
   type Octet_Array_Access is access Octet_Array;

   procedure Free (Octets : in out Octet_Array_Access);

   type Alignment_Type is range 1 .. 8;

   procedure Initialize
     (Buffer  : access Buffer_Type;
      Content : in Octet_Array);
   --  Read endianess from content and prepare the buffer to be worked on

   function Get_Content (Buffer : access Buffer_Type)
     return Octet_Array;
   --  Return an octet array representing the stream to be sent

   procedure Marshall_Opaque
     (Buffer    : access Buffer_Type;
      Octets    : in Octet_Array;
      Alignment : in Alignment_Type := 1);

   function Unmarshall_Opaque
     (Buffer    : access Buffer_Type;
      Size      : Index_Type;
      Alignment : Alignment_Type := 1)
     return Octet_Array;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Inner  : in Buffer_Type);

   function Unmarshall (Buffer : access Buffer_Type) return Buffer_Type;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Octet);

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.Octet;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Char);

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.Char;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Boolean);

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.Boolean;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Unsigned_Short);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Short;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Unsigned_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Short);

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.Short;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.Long);

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Octets : in CORBA.String);

   function Unmarshall (Buffer : access Buffer_Type) return CORBA.String;

private

   type Buffer_Type is new Ada.Finalization.Controlled with record
      Endianess : Endianess_Type;
      Content   : Octet_Array_Access;
      Index     : Index_Type;
   end record;

   procedure Initialize (Buffer : in out Buffer_Type);
   procedure Adjust     (Buffer : in out Buffer_Type);
   procedure Finalize   (Buffer : in out Buffer_Type);

end Broca.CDR;
