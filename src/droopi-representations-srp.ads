--  A representation for our own Simple Request Protocol (SRP).

--  $Id$

with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Utils.SRP; use Droopi.Utils.SRP;

package Droopi.Representations.SRP is

   pragma Elaborate_Body;

   type Rep_SRP is new Representation with private;
   type Rep_SRP_Access is access all Rep_SRP;
   type String_Ptr is access all String;

   ------------------------------------------
   -- Part taken from AWS (Ada Web Server) --
   ------------------------------------------

   function Decode_URL (Str : in String) return String;
   --  The translations are:
   --     +     should be changed to a space
   --     %xy   should be replaced by the character whose code is xy

   function Base64_Encode (Data : Ada.Streams.Stream_Element_Array)
                          return String;
   --  Encode Data using the base64 algorithm

   function Base64_Encode (Data : in String) return String;
   --  Same as above but takes a string as input

   function Base64_Decode (B64_Data : in String)
                          return Ada.Streams.Stream_Element_Array;
   --  Decode B64_Data using the base64 algorithm

   -----------------------------------

   --  Only encodes the parameters' values
   --  Warning, Str must be a well-formed SRP string, otherwise
   --  Constraint_Error may be raised
   function Encode_URL (Str : in String) return String;

   --  Only encodes the parameters' values
   --  Is less error prone than the previous function
   procedure Encode_URL (SRP_Info : in out Split_SRP);

   --  Encodes the entire string
   function Encode_String (Str : in String) return String;

   procedure Marshall_From_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : CORBA.Any);

   procedure Unmarshall_To_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out CORBA.Any);

   --  The following methods are specific to Rep_SRP and are
   --  here only to facilitate testing of other parts of the ORB.

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character);
   --  Marshall one character.

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character;
   --  Unmarshall one character.

   procedure Marshall_String
     (R : access Rep_SRP;
      B : access Buffer_Type;
      S : String);
   --  Marshall a string.

   function Unmarshall_String
     (R : Rep_SRP;
      B : access Buffer_Type)
     return String;
   --  Unmarshall a string terminated by a CR/LF sequence.

   function Unmarshall_To_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type) return CORBA.Any;

   --  Temporary procedure. Should be replaces by Marshall_From_Any when
   --  we will be able to [un]marshall Split_SRP [from] to Any
   procedure Marshall_From_Split_SRP
     (R       : Rep_SRP;
      Buffer  : access Buffers.Buffer_Type;
      SRP_Info : Split_SRP);

private

   type Rep_SRP is new Representation with null record;

end Droopi.Representations.SRP;
