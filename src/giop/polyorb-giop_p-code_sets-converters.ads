------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . G I O P _ P . C O D E _ S E T S . C O N V E R T E R S   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

--  Code sets converters

--  Each code set converter process conversion between native code set and
--  transmission code set and marshalling/unmarshalling of data. A converter is
--  defined for each pair of native/transmission code sets.

with GNAT.Byte_Swapping;

with PolyORB.Buffers;
with PolyORB.Utils.Buffers;
pragma Elaborate_All (PolyORB.Utils.Buffers);

package PolyORB.GIOP_P.Code_Sets.Converters is

   ----------------------------------------
   -- Narrow character converter (CCS-C) --
   ----------------------------------------

   type Converter is abstract tagged limited private;
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

   --------------------------------------
   -- Wide character converter (CCS-W) --
   --------------------------------------

   type Wide_Converter is abstract tagged limited private;
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
      Target_Code_Set : Code_Set_Id) return Converter_Access;
   --  Return converter for processing conversion between corresponding code
   --  sets for char data.

   function Get_Converter
     (Native_Code_Set : Code_Set_Id;
      Target_Code_Set : Code_Set_Id) return Wide_Converter_Access;
   --  Return converter for processing conversion between corresponding code
   --  sets for wchar data.

   function Supported_Char_Conversion_Code_Sets
     (Code_Set : Code_Set_Id) return Code_Set_Id_List;
   --  Return list of Code_Set_Id supported as conversion code set for defined
   --  native code set of char data (CCS-C).

   function Supported_Wchar_Conversion_Code_Sets
     (Code_Set : Code_Set_Id)
      return Code_Set_Id_List;
   --  Return list of Code_Set_Id supported as conversion code set for defined
   --  native code set of wchar data (CCS-W).

private

   type Converter_Factory is access function return Converter_Access;
   type Wide_Converter_Factory is access function return Wide_Converter_Access;

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

   type Converter is abstract tagged limited null record;

   type Wide_Converter is abstract tagged limited record
      GIOP_1_2_Mode : Boolean := False;
   end record;

   ----------------------------------------------------
   -- Supporting routines for unaligned unsigned I/O --
   ----------------------------------------------------

   use PolyORB.Utils.Buffers;

   function Swapped is
     new GNAT.Byte_Swapping.Swapped2 (PolyORB.Types.Unsigned_Short);

   package Unaligned_Unsigned_Short is
     new Align_Transfer_Elementary
       (T => PolyORB.Types.Unsigned_Short, With_Alignment => False);

   function Swapped is
     new GNAT.Byte_Swapping.Swapped4 (PolyORB.Types.Unsigned_Long);

   package Unaligned_Unsigned_Long is
     new Align_Transfer_Elementary
       (T => PolyORB.Types.Unsigned_Long, With_Alignment => False);

   --  Ada95 data converters

   type ISO88591_Native_Converter is new Converter with null record;

   overriding procedure Marshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Char;
      Error  : in out Errors.Error_Container);

   overriding procedure Marshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Char;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container);

   type ISO88591_UTF8_Converter is new Converter with null record;

   overriding procedure Marshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Char;
      Error  : in out Errors.Error_Container);

   overriding procedure Marshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Char;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container);

   type UCS2_Native_Wide_Converter is new Wide_Converter with null record;

   overriding procedure Marshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container);

   overriding procedure Marshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container);

   type UCS2_UTF16_Wide_Converter is new Wide_Converter with null record;

   overriding procedure Marshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container);

   overriding procedure Marshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container);

   overriding procedure Unmarshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container);

end PolyORB.GIOP_P.Code_Sets.Converters;
