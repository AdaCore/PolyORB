--  A data representation implementing the CORBA Common Data Representation.
--  For reference on CDR see:
--    The Common Object Request Broker: Architecture and Specification,
--    revision 2.2", Open Management Group
--    (http://www.omg.org/).

--  $Id$

with Ada.Streams; use Ada.Streams;

with CORBA;

with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Any;
with Droopi.Types;

package Droopi.Representations.CDR is

   pragma Elaborate_Body;

   ----------------------------------------
   -- The Encapsulation view of a buffer --
   ----------------------------------------

   --  A buffer is a sequence of bytes that can be
   --  turned into an opaque Encapsulation object
   --  and back.

   subtype Encapsulation is Stream_Element_Array;

   function Encapsulate
     (Buffer   : access Buffer_Type)
     return Encapsulation;
   --  Create an Octet_Array corresponding to Buffer
   --  as an encapsulation.

   procedure Start_Encapsulation
     (Buffer : access Buffer_Type);
   --  Prepare Buffer to receive marshalled data
   --  that will be turned into an Encapsulation.

   procedure Decapsulate
     (Octets : access Encapsulation;
      Buffer : access Buffer_Type);
   --  Initialize a buffer with an Octet_Array
   --  corresponding to an Encapsulation.
   --  Buffer must be a fresh, empty buffer.
   --  The lifespan of the actual Octets array
   --  shall be no less than that of Buffer.


   ---------------------------------------------
   -- Marshaling and unmarshaling subprograms --
   ---------------------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Octet);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Octet);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Octet;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Char);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Char);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Char;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Wchar);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Wchar);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Wchar;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Boolean);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Boolean);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Boolean;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Unsigned_Short);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Unsigned_Short);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Unsigned_Short;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Unsigned_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Unsigned_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Unsigned_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Unsigned_Long_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Unsigned_Long_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Unsigned_Long_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Short);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Short);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Short;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Long_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Long_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Long_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Float);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Float);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Float;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Double);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Double);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Double;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Long_Double);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Long_Double);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Long_Double;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Standard.String);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Standard.String);

   function Unmarshall (Buffer : access Buffer_Type)
     return Standard.String;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.String);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.String);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.String;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Wide_String);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Wide_String);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Wide_String;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.Identifier);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.Identifier);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.Identifier;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.ScopedName);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.ScopedName);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.ScopedName;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Types.RepositoryId);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Types.RepositoryId);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Types.RepositoryId;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Any.ValueModifier);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.ValueModifier);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Any.ValueModifier;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Any.Visibility);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.Visibility);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Any.Visibility;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Any.Any);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.Any);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Any.Any;

   --  The next three marshall or unmarshall the value of the any and
   --  not the any type itself.

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Any.Any);
   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.Any);

   --  This procedure unmarshalls an Any in Result.
   --  If Result already has a value, then its memory location
   --  will be reused. Otherwise, a new location will be created
   procedure Unmarshall_To_Any (Buffer : access Buffer_Type;
                                Result : in out Droopi.Any.Any);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Any.TypeCode.Object);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.TypeCode.Object);

   function Unmarshall (Buffer : access Buffer_Type)
     return Droopi.Any.TypeCode.Object;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Droopi.Any.NamedValue);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Droopi.Any.NamedValue);

   procedure Unmarshall (Buffer : access Buffer_Type;
                         NV : in out Droopi.Any.NamedValue);

   --  Marshalling and unmashalling of object references
   --  (but not valuetypes)

--   procedure Marshall
--     (Buffer : access Buffer_Type;
--      Data   : in Droopi.Types.AbstractBase.Ref'Class);

--   procedure Unmarshall
--     (Buffer : access Buffer_Type;
--      Data : in out Droopi.Types.AbstractBase.Ref'Class);

--   function Unmarshall
--     (Buffer : access Buffer_Type)
--     return Droopi.Types.Object.Ref;

   --  Marshalling and unmarshalling of system exceptions

   procedure Marshall
     (Buffer : access Buffer_Type;
      Excpt  : in CORBA.Exception_Occurrence);

   procedure Unmarshall_And_Raise
     (Buffer : access Buffer_Type);
   pragma No_Return (Unmarshall_And_Raise);


   --- Marshalling of Octets sequences

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Stream_Element_Array);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Stream_Element_Array);

   function Unmarshall (Buffer : access Buffer_Type)
     return Stream_Element_Array;

   generic
      type F is delta <> digits <>;
   package Fixed_Point is

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : access F);
      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : in F);

      function Unmarshall (Buffer : access Buffer_Type)
                           return F;
   end Fixed_Point;

end  Droopi.Representations.CDR;

