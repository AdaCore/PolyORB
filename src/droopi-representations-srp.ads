--  A representation for our own Simple Request Protocol (SRP).

--  $Id$

with Ada.Streams;

with Droopi.Any;
with Droopi.Buffers;
with Droopi.Types;
with Droopi.Utils.SRP;

package Droopi.Representations.SRP is

   pragma Elaborate_Body;

   use Ada.Streams;

   use Droopi.Any;
   use Droopi.Buffers;
   use Droopi.Types;
   use Droopi.Utils.SRP;

   type Rep_SRP is new Representation with private;
   type Rep_SRP_Access is access all Rep_SRP;

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

   function Encode_URL (SRP_Info : Split_SRP) return Types.String;

   --  Only encodes the parameters' values
   --  Is less error prone than the previous function
   procedure Encode_URL (SRP_Info : in out Split_SRP);

   --  Encodes the entire string
   function Encode_String (Str : in String) return String;

   -------------------
   -- UNMARSHALLING --
   -------------------

   procedure Unmarshall (Buffer : access Buffer_Type;
                         NV     : in out NamedValue);

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Types.Boolean;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Types.Octet;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Types.Char;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Types.Unsigned_Short;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Types.Unsigned_Long;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Types.Short;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Types.Long;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Stream_Element_Array;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Standard.String;

--    function Unmarshall (Buffer : access Buffer_Type)
--                        return Types.String;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Types.String;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Any.TypeCode.Object;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Droopi.Any.Any;

   -----------------
   -- MARSHALLING --
   -----------------

   procedure Marshall
     (Buffer   : access Buffer_Type;
      Info_SRP : in Split_SRP);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Boolean);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Char);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Wchar);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        Droopi.Types.Octet);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Unsigned_Short);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Unsigned_Long);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Short);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Long);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Standard.String);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.String);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Stream_Element_Array);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.Any);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.TypeCode.Object);

   procedure Marshall_From_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any);

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : Droopi.Any.Any);

   procedure Unmarshall_To_Any
     (R      : Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any);

   procedure Unmarshall_To_Any
     (Buffer : access Buffer_Type;
      Result : in out Droopi.Any.Any);

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
      Buffer : access Buffers.Buffer_Type) return Any.Any;

   --  Temporary procedure. Should be replaces by Marshall_From_Any when
   --  we will be able to [un]marshall Split_SRP [from] to Any
   procedure Marshall_From_Split_SRP
     (R       : Rep_SRP;
      Buffer  : access Buffers.Buffer_Type;
      SRP_Info : Split_SRP);

private

   type Rep_SRP is new Representation with null record;

end Droopi.Representations.SRP;
