------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . C D R           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A data representation implementing the CORBA Common Data Representation.
--  For reference on CDR see:
--    The Common Object Request Broker: Architecture and Specification,
--    revision 2.2", Open Management Group
--    (http://www.omg.org/).

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Any;
with PolyORB.Buffers; use PolyORB.Buffers;
with PolyORB.References;
with PolyORB.Types;

package PolyORB.Representations.CDR is

   pragma Elaborate_Body;

   -------------------------------------------------
   -- The Encapsulation view of a CDR data stream --
   -------------------------------------------------

   --  A CDR data stream is a sequence of bytes that can be
   --  turned into an opaque Encapsulation object and back.

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

   ------------------------------------------------
   -- Marshaliling and unmarshalling subprograms --
   ------------------------------------------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Octet);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Octet);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Octet;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Char);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Char);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Char;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Wchar);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Wchar);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Wchar;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Boolean);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Boolean);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Boolean;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Short);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Unsigned_Short);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Short;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Unsigned_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Unsigned_Long_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Unsigned_Long_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Unsigned_Long_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Short);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Short);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Short;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Long_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Long_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Float);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Float);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Float;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Double);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Double);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Double;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Long_Double);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Long_Double);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Long_Double;

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
      Data   : access PolyORB.Types.String);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.String);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.String;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Wide_String);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Wide_String);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Wide_String;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.Identifier);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.Identifier);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.Identifier;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.ScopedName);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.ScopedName);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.ScopedName;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Types.RepositoryId);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Types.RepositoryId);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Types.RepositoryId;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.ValueModifier);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Any.ValueModifier);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Any.ValueModifier;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.Visibility);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Any.Visibility);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Any.Visibility;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.Any);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Any.Any);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Any.Any;

   --  The next three subprograms marshall or unmarshall the value of
   --  the Any, not the Any type itself (i.e. they do not marshall Data's
   --  typecode).

   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.Any);
   procedure Marshall_From_Any
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Any.Any);

   procedure Unmarshall_To_Any
     (Buffer : access Buffer_Type;
      Result : in out PolyORB.Any.Any);
   --  Unmarshall the value of Result from Buffer. Result must have
   --  a valid TypeCode, which defines what kind of value is unmarshalled.
   --  If Result already has a value, then its memory location
   --  will be reused. Otherwise, a new location will be allocated.

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.TypeCode.Object);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Any.TypeCode.Object);

   function Unmarshall (Buffer : access Buffer_Type)
     return PolyORB.Any.TypeCode.Object;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.Any.NamedValue);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access PolyORB.Any.NamedValue);

   function  Unmarshall (Buffer : access Buffer_Type)
             return PolyORB.Any.NamedValue;

   --  Marshalling and unmashalling of object references
   --  (but not valuetypes)

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in PolyORB.References.Ref'Class);

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Data : in out PolyORB.References.Ref'Class);

   function Unmarshall
     (Buffer : access Buffer_Type)
     return PolyORB.References.Ref;

   --  Marshalling of octets sequences.

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

end  PolyORB.Representations.CDR;
