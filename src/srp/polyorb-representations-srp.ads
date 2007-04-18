------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . S R P           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  A representation for our own Simple Request Protocol (SRP).

with Ada.Streams;

with PolyORB.Any;
with PolyORB.Buffers;
with PolyORB.Types;
with PolyORB.Utils.SRP;

package PolyORB.Representations.SRP is

   pragma Elaborate_Body;

   use Ada.Streams;

   use PolyORB.Any;
   use PolyORB.Buffers;
   use PolyORB.Types;
   use PolyORB.Utils.SRP;

   type Rep_SRP is new Representation with private;
   type Rep_SRP_Access is access all Rep_SRP;

   ------------------------------------------
   -- Part taken from AWS (Ada Web Server) --
   ------------------------------------------

   function Decode_URL (Str : String) return String;
   --  The translations are:
   --     +     should be changed to a space
   --     %xy   should be replaced by the character whose code is xy

   function Base64_Encode (Data : Ada.Streams.Stream_Element_Array)
                          return String;
   --  Encode Data using the base64 algorithm

   function Base64_Encode (Data : String) return String;
   --  Same as above but takes a string as input

   function Base64_Decode (B64_Data : String)
                          return Ada.Streams.Stream_Element_Array;
   --  Decode B64_Data using the base64 algorithm

   function Encode_URL (SRP_Info : Split_SRP) return Types.String;

   --  Only encodes the parameters' values
   --  Is less error prone than the previous function
   procedure Encode_URL (SRP_Info : in out Split_SRP);

   --  Encodes the entire string
   function Encode_String
     (Str : String; Also_Escape : String := "/") return String;

   -------------------
   -- UNMARSHALLING --
   -------------------

   procedure Unmarshall (Buffer : access Buffer_Type;
                         NV     : in out NamedValue);

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Types.Boolean;

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Types.Octet;

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Types.Char;

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Types.Unsigned_Short;

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Types.Unsigned_Long;

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Types.Short;

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Types.Long;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Stream_Element_Array;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Standard.String;

--    function Unmarshall (Buffer : access Buffer_Type)
--                        return Types.String;

   function Unmarshall (Buffer : access Buffer_Type)
                       return Types.String;

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Any.TypeCode.Local_Ref;

   function Unmarshall (Buffer : access Buffer_Type)
                       return PolyORB.Any.Any;

   -----------------
   -- MARSHALLING --
   -----------------

   procedure Marshall
     (Buffer   : access Buffer_Type;
      Info_SRP : Split_SRP);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Boolean);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Char);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Wchar);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :        PolyORB.Types.Octet);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Unsigned_Short);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Unsigned_Long);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Short);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Long);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : Standard.String);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.String);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : Stream_Element_Array);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Any.Any);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Any.TypeCode.Local_Ref);

   procedure Marshall_From_Any
     (R      : access Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container);

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Any.Any_Container'Class);

   procedure Unmarshall_To_Any
     (R      : access Rep_SRP;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any_Container'Class;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall_To_Any
     (Buffer : access Buffer_Type;
      Result : in out PolyORB.Any.Any_Container'Class);

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
     (R      : access Rep_SRP;
      Buffer : access Buffers.Buffer_Type) return Any.Any;

   --  Temporary procedure. Should be replaces by Marshall_From_Any when
   --  we will be able to [un]marshall Split_SRP [from] to Any
   procedure Marshall_From_Split_SRP
     (R       : Rep_SRP;
      Buffer  : access Buffers.Buffer_Type;
      SRP_Info : Split_SRP);

private

   type Rep_SRP is new Representation with null record;

end PolyORB.Representations.SRP;
