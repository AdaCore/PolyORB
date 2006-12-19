------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . G I O P _ P . C O D E _ S E T S . C O N V E R T E R S   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

--  Code sets converters

--  Each code set converter process conversion between native code set
--  and transmission code set and marshalling/unmarshalling of data.
--  For each pair of native/transmission code sets we define it's own
--  converter.

with PolyORB.Buffers;

package PolyORB.GIOP_P.Code_Sets.Converters is

   type Converter is abstract tagged private;

   type Converter_Access is access all Converter'Class;

   procedure Marshall
     (C      : Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Char;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Marshall
     (C      : Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Unmarshall
     (C      : Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Char;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Unmarshall
     (C      : Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container)
      is abstract;

   type Wide_Converter is abstract tagged private;

   type Wide_Converter_Access is access all Wide_Converter'Class;

   procedure Set_GIOP_1_2_Mode (C : in out Wide_Converter);
   --  Use GIOP 1.2 semantics for wchar types

   procedure Marshall
     (C      : Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Marshall
     (C      : Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Unmarshall
     (C      : Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container)
      is abstract;

   procedure Unmarshall
     (C      : Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container)
      is abstract;

   function Get_Converter
     (Native_Code_Set : Code_Set_Id;
      Target_Code_Set : Code_Set_Id)
      return Converter_Access;
   --  Return converter for processing conversion between
   --  corresponding code sets for char data.

   function Get_Converter
     (Native_Code_Set : Code_Set_Id;
      Target_Code_Set : Code_Set_Id)
      return Wide_Converter_Access;
   --  Return converter for processing conversion between
   --  corresponding code sets for wchar data.

   function Supported_Char_Conversion_Code_Sets
     (Code_Set : Code_Set_Id)
      return Code_Set_Id_List;
   --  Return list of Code_Set_Id supported as conversion code set for
   --  defined native code set of char data.

   function Supported_Wchar_Conversion_Code_Sets
     (Code_Set : Code_Set_Id)
      return Code_Set_Id_List;
   --  Return list of Code_Set_Id supported as conversion code set for
   --  defined native code set of wchar data.

private

   type Converter_Factory is
     access function return Converter_Access;

   type Wide_Converter_Factory is
     access function return Wide_Converter_Access;

   procedure Register_Native_Code_Set
     (Code_Set : Code_Set_Id;
      Native   : Converter_Factory;
      Fallback : Converter_Factory);
   --  Register native code set

   procedure Register_Conversion_Code_Set
     (Native     : Code_Set_Id;
      Conversion : Code_Set_Id;
      Factory    : Converter_Factory);
   --  Register additional conversion code set

   procedure Register_Native_Code_Set
     (Code_Set : Code_Set_Id;
      Native   : Wide_Converter_Factory;
      Fallback : Wide_Converter_Factory);
   --  Register native code set

   procedure Register_Conversion_Code_Set
     (Native     : Code_Set_Id;
      Conversion : Code_Set_Id;
      Factory    : Wide_Converter_Factory);
   --  Register additional conversion code set

   type Converter is abstract tagged null record;

   type Wide_Converter is abstract tagged record
      GIOP_1_2_Mode : Boolean := False;
   end record;

   --  Shared marshalling subprogram (for use in another converters)

   procedure Marshall
     (Buffer    : access Buffers.Buffer_Type;
      Data      : Types.Unsigned_Short;
      Alignment : Buffers.Alignment_Type);
   --  Marshall Unsigned_Short as big endian value with specified Alignment

   function Unmarshall
     (Buffer    : access Buffers.Buffer_Type;
      Alignment : Buffers.Alignment_Type)
      return Types.Unsigned_Short;
   --  Unmarshall Unsigned_Short as big endian value with specified Alignment

   procedure Marshall
     (Buffer    : access Buffers.Buffer_Type;
      Data      : Types.Unsigned_Long;
      Alignment : Buffers.Alignment_Type);
   --  Marshall Unsigned_Long as big endian value with specified Alignment

   function Unmarshall
     (Buffer    : access Buffers.Buffer_Type;
      Alignment : Buffers.Alignment_Type)
      return Types.Unsigned_Long;
   --  Unmarshall Unsigned_Long as big endian value with specified Alignment

   --  Ada95 data converters

   type ISO88591_Native_Converter is new Converter with null record;

   procedure Marshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Char;
      Error  : in out Errors.Error_Container);

   procedure Marshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Char;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container);

   type ISO88591_UTF8_Converter is new Converter with null record;

   procedure Marshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Char;
      Error  : in out Errors.Error_Container);

   procedure Marshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Char;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container);

   type UCS2_Native_Wide_Converter is new Wide_Converter with null record;

   procedure Marshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Marshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container);

   type UCS2_UTF16_Wide_Converter is new Wide_Converter with null record;

   procedure Marshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Marshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container);

end PolyORB.GIOP_P.Code_Sets.Converters;
