------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . R E P R E S E N T A T I O N S . C D R . C O M M O N    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

--  This package containts marshalling/unmarshalling subprograms for base
--  types, which representation is GIOP version independent.

--  XXX Also this package contains marshalling/unmarshalling subprogram
--  for object references, which must be removed.

with Ada.Streams;

with PolyORB.Any;
with PolyORB.Buffers;
with PolyORB.References;
with PolyORB.Types;

package PolyORB.Representations.CDR.Common is

   pragma Elaborate_Body;

   use Ada.Streams;
   use PolyORB.Buffers;

   -------------------------------------------------
   -- The Encapsulation view of a CDR data stream --
   -------------------------------------------------

   --  A CDR data stream is a sequence of bytes that can be
   --  turned into an opaque Encapsulation object and back.

   subtype Encapsulation is Stream_Element_Array;

   function Encapsulate (Buffer : access Buffer_Type) return Encapsulation;
   --  Create an Octet_Array corresponding to Buffer
   --  as an encapsulation.

   procedure Start_Encapsulation (Buffer : access Buffer_Type);
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

   -----------------------------------------------
   -- Marshalling and unmarshalling subprograms --
   -----------------------------------------------

   --  'Octet' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Octet);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Octet);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Octet;

   --  'Char' type as defined by GIOP 1.0 (in ISO-8859-1 character set)

   procedure Marshall_Latin_1_Char
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Char);

   function Unmarshall_Latin_1_Char
     (Buffer : access Buffer_Type)
      return PolyORB.Types.Char;

   --  'Boolean' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Boolean);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Boolean);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Boolean;

   --  'Unsigned_Short' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Short);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Unsigned_Short);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Short;

   --  'Unsigned_Long' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Long);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Unsigned_Long);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Long;

   --  'Unsigned_Long_Long' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Long_Long);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Unsigned_Long_Long);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Long_Long;

   --  'Short' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Short);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Short);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Short;

   --  'Long' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Long);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Long;

   --  'Long_Long' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long_Long);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Long_Long);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Long_Long;

   --  'Float' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Float);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Float);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Float;

   --  'Double' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Double);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Double);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Double;

   --  'Long_Double' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long_Double);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Long_Double);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Long_Double;

   --  'String' type as defined by GIOP 1.0 (in ISO-8859-1 character set)

   procedure Marshall_Latin_1_String
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.String);

   function Unmarshall_Latin_1_String
     (Buffer : access Buffer_Type)
     return PolyORB.Types.String;

   --  'Identifier' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Identifier);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.Identifier);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.Identifier;

   --  'RepositoryId' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.RepositoryId);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Types.RepositoryId);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Types.RepositoryId;

   --  'ValueModifier' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.ValueModifier);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Any.ValueModifier);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.ValueModifier;

   --  'Visibility' type

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.Visibility);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.Any.Visibility);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.Any.Visibility;

   --  Object References (but not valuetypes)

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     PolyORB.References.Ref'Class);

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Data   : in out PolyORB.References.Ref'Class);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.References.Ref;

   --  Octets sequences

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Stream_Element_Array);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in     Stream_Element_Array);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return Stream_Element_Array;

   --  Fixed_Point types

   generic
      type F is delta <> digits <>;
   package Fixed_Point is

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : access F);

      procedure Marshall
        (Buffer : access Buffer_Type;
         Data   : in     F);

      function Unmarshall
        (Buffer : access Buffer_Type)
        return F;

      function Fixed_To_Octets
        (Data : in F)
        return Stream_Element_Array;

      function Octets_To_Fixed
        (Octets : Stream_Element_Array)
        return F;

   end Fixed_Point;

   --  Standard 'String' type

   procedure Marshall_Latin_1_String
     (Buffer : access Buffer_Type;
      Data   : in     Standard.String);

   function Unmarshall_Latin_1_String
     (Buffer : access Buffer_Type)
      return Standard.String;

end PolyORB.Representations.CDR.Common;
