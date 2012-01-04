------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . R E P R E S E N T A T I O N S . D N S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  A data representation implementing the DNS Data Representation.

with PolyORB.Types;
with PolyORB.Buffers;
with PolyORB.Protocols.DNS;
with PolyORB.Any;
with PolyORB.Any.NVList;

package PolyORB.Representations.DNS is
   use PolyORB.Types;
   use PolyORB.Buffers;
   use PolyORB.Protocols.DNS;
   use PolyORB.Any;
   use PolyORB.Any.NVList.Internals;
   use PolyORB.Any.NVList.Internals.NV_Lists;

   --  The next two subprograms marshall or unmarshall the value of
   --  the Any, not the Any type itself (i.e. they do not marshall Data's
   --  typecode).

   procedure Marshall_From_Any
     (Buffer : Buffer_Access; Argument : Any.Any;
     Is_Reply : Types.Boolean);

   procedure Unmarshall_To_Any
     (Buffer : Buffer_Access; Arg : Any.Any; Length : Integer;
      Is_Reply : Types.Boolean);

   --  'Boolean' type
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :  PolyORB.Types.Boolean);

   function Unmarshall
     (Buffer : access Buffer_Type)
      return PolyORB.Types.Boolean;

   --  'Octet' type
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :  PolyORB.Types.Octet);

   function Unmarshall
     (Buffer : access Buffer_Type)
      return PolyORB.Types.Octet;

   --  'Unsigned_Long'  type
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :  PolyORB.Types.Unsigned_Long);
   function Unmarshall
     (Buffer : access Buffer_Type)
      return PolyORB.Types.Unsigned_Long;

   --  'Unsigned_Long_Long' type
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :  PolyORB.Types.Unsigned_Long_Long);
   function Unmarshall
     (Buffer : access Buffer_Type)
      return PolyORB.Types.Unsigned_Long_Long;

   --  'Unsigned_Short'  type
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   :  PolyORB.Types.Unsigned_Short);
   function Unmarshall
     (Buffer : access Buffer_Type)
      return PolyORB.Types.Unsigned_Short
   ;
   --  'String' type
   procedure Marshall_DNS_String
     (Buffer : access Buffer_Type;
      Data   : Standard.String);

   procedure Marshall_TXT_String
     (Buffer : access Buffer_Type;
      Data   : Standard.String);

   function Unmarshall_TXT_String
     (Buffer      : access Buffer_Type;
      Data_Length : PolyORB.Types.Unsigned_Short)
      return PolyORB.Types.String;

   procedure Marshall_Latin_1_String
     (Buffer : access Buffer_Type;
      Data   : Standard.String);
   procedure Marshall_Latin_1_String
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.String);

   function Unmarshall_Latin_1_String
     (Buffer : access Buffer_Type)
      return Standard.String;

   function Unmarshall_Latin_1_String
     (Buffer : access Buffer_Type)
      return PolyORB.Types.String;

   function Unmarshall_DNS_String
     (Buffer : access Buffer_Type)
      return PolyORB.Types.String;

   procedure Marshall_Latin_1_Char
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Char);
   function Unmarshall_Latin_1_Char
     (Buffer : access Buffer_Type) return PolyORB.Types.Char;

   --  Identifier type
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : PolyORB.Types.Identifier);

   function Unmarshall
     (Buffer : access Buffer_Type)
      return PolyORB.Types.Identifier;

end PolyORB.Representations.DNS;
