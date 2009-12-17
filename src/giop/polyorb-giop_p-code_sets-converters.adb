------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . G I O P _ P . C O D E _ S E T S . C O N V E R T E R S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2009, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Code_Sets.Converters is

   use PolyORB.Buffers;
   use PolyORB.Errors;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Types;

   --  Character data converters registry data types

   type Conversion_Record is record
      Code_Set : Code_Set_Id;
      Factory  : Converter_Factory;
   end record;

   package Conversion_Lists is
     new PolyORB.Utils.Chained_Lists (Conversion_Record);

   type Info_Record is record
      Code_Set             : Code_Set_Id;
      Native               : Converter_Factory;
      Fallback             : Converter_Factory;
      Conversions          : Conversion_Lists.List;

      Conversion_Code_Sets : Code_Set_Id_List;
      --  Cache of supported conversion code sets, used to avoid re-creation
      --  of the list for each processed request.
   end record;

   package Info_Lists is new PolyORB.Utils.Chained_Lists (Info_Record);

   --  Wide_Character data converters registry data types

   type Wide_Conversion_Record is record
      Code_Set : Code_Set_Id;
      Factory  : Wide_Converter_Factory;
   end record;

   package Wide_Conversion_Lists is
     new PolyORB.Utils.Chained_Lists (Wide_Conversion_Record);

   type Wide_Info_Record is record
      Code_Set             : Code_Set_Id;
      Native               : Wide_Converter_Factory;
      Fallback             : Wide_Converter_Factory;
      Conversions          : Wide_Conversion_Lists.List;

      Conversion_Code_Sets : Code_Set_Id_List;
      --  Cache of supported conversion code sets, used to avoid re-creation
      --  of the list for each processed request.
   end record;

   package Wide_Info_Lists is
     new PolyORB.Utils.Chained_Lists (Wide_Info_Record);

   subtype Surrogate_Character is Wide_Character
     range Wide_Character'Val (16#D800#) .. Wide_Character'Val (16#DFFF#);

   subtype Invalid_Character is Wide_Character
     range Wide_Character'Val (16#FFFE#) .. Wide_Character'Val (16#FFFF#);

   function Find (Code_Set : Code_Set_Id) return Info_Lists.Element_Access;

   function Find
     (Code_Set : Code_Set_Id)
      return Wide_Info_Lists.Element_Access;

   --  Code set converters factory functions

   function Create_ISO88591_Native_Converter return Converter_Access;

   function Create_ISO88591_UTF8_Converter return Converter_Access;

   function Create_UCS2_Native_Converter return Wide_Converter_Access;

   function Create_UCS2_UTF16_Converter return Wide_Converter_Access;

   --  Code set registry variables

   Info      : Info_Lists.List;
   Wide_Info : Wide_Info_Lists.List;

   --  UTF16 byte order mark

   BOM         : constant Unsigned_Short := 16#FEFF#;
   Reverse_BOM : constant Unsigned_Short := 16#FFFE#;

   --------------------------------------
   -- Create_ISO88591_Native_Converter --
   --------------------------------------

   function Create_ISO88591_Native_Converter return Converter_Access is
   begin
      return new ISO88591_Native_Converter;
   end Create_ISO88591_Native_Converter;

   ------------------------------------
   -- Create_ISO88591_UTF8_Converter --
   ------------------------------------

   function Create_ISO88591_UTF8_Converter return Converter_Access is
   begin
      return new ISO88591_UTF8_Converter;
   end Create_ISO88591_UTF8_Converter;

   ----------------------------------
   -- Create_UCS2_Native_Converter --
   ----------------------------------

   function Create_UCS2_Native_Converter return Wide_Converter_Access is
   begin
      return new UCS2_Native_Wide_Converter;
   end Create_UCS2_Native_Converter;

   ---------------------------------
   -- Create_UCS2_UTF16_Converter --
   ---------------------------------

   function Create_UCS2_UTF16_Converter return Wide_Converter_Access is
   begin
      return new UCS2_UTF16_Wide_Converter;
   end Create_UCS2_UTF16_Converter;

   ----------
   -- Find --
   ----------

   function Find
     (Code_Set : Code_Set_Id) return Info_Lists.Element_Access
   is
      use Info_Lists;
      Iter : Iterator := First (Info);
   begin
      while not Last (Iter) loop
         if Value (Iter).Code_Set = Code_Set then
            return Value (Iter);
         end if;
         Next (Iter);
      end loop;

      return null;
   end Find;

   function Find
     (Code_Set : Code_Set_Id) return Wide_Info_Lists.Element_Access
   is
      use Wide_Info_Lists;
      Iter : Iterator := First (Wide_Info);
   begin
      while not Last (Iter) loop
         if Value (Iter).Code_Set = Code_Set then
            return Value (Iter);
         end if;
         Next (Iter);
      end loop;

      return null;
   end Find;

   -------------------
   -- Get_Converter --
   -------------------

   function Get_Converter
     (Native_Code_Set : Code_Set_Id;
      Target_Code_Set : Code_Set_Id) return Converter_Access
   is
      use Conversion_Lists;
      use type Info_Lists.Element_Access;
      Info : constant Info_Lists.Element_Access := Find (Native_Code_Set);
   begin
      if Info = null then
         return null;

      elsif Target_Code_Set = Native_Code_Set then
         return Info.Native.all;

      elsif Target_Code_Set = Char_Data_Fallback_Code_Set then
         return Info.Fallback.all;

      else
         declare
            Iter : Iterator := First (Info.Conversions);
         begin
            while not Last (Iter) loop
               if Target_Code_Set = Value (Iter).Code_Set then
                  return Value (Iter).Factory.all;
               end if;
               Next (Iter);
            end loop;
         end;
      end if;

      return null;
   end Get_Converter;

   function Get_Converter
     (Native_Code_Set : Code_Set_Id;
      Target_Code_Set : Code_Set_Id) return Wide_Converter_Access
   is
      use Wide_Conversion_Lists;
      use type Wide_Info_Lists.Element_Access;
      Info : constant Wide_Info_Lists.Element_Access := Find (Native_Code_Set);
   begin
      if Info = null then
         return null;

      elsif Target_Code_Set = Native_Code_Set then
         return Info.Native.all;

      elsif Target_Code_Set = Wchar_Data_Fallback_Code_Set then
         return Info.Fallback.all;

      else
         declare
            Iter : Iterator := First (Info.Conversions);
         begin
            while not Last (Iter) loop
               if Target_Code_Set = Value (Iter).Code_Set then
                  return Value (Iter).Factory.all;
               end if;
               Next (Iter);
            end loop;
         end;
      end if;

      return null;
   end Get_Converter;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Char;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);
   begin
      Marshall_Latin_1_Char (Buffer, Data);
   end Marshall;

   procedure Marshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);
   begin
      Marshall_Latin_1_String (Buffer, Data);
   end Marshall;

   procedure Marshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Char;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
   begin
      if Character'Pos (Data) < 16#80# then
         Marshall (Buffer, Octet (Character'Pos (Data)));
      else
         Throw
           (Error,
            Data_Conversion_E,
            System_Exception_Members'
             (Minor     => 1,
              Completed => Completed_No));
      end if;
   end Marshall;

   procedure Marshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);

   begin
      Pad_Align (Buffer, Align_4);

      declare
         Reserv : constant Reservation     := Reserve (Buffer, 4);
         Buf    : Buffer_Access            := new Buffer_Type;
         Length : Unsigned_Long            := 0;
         Equiv  : constant Standard.String :=
                    To_String (Data) & Character'Val (16#00#);

      begin
         for J in Equiv'Range loop
            if Character'Pos (Equiv (J)) < 16#80# then
               Marshall (Buffer, Octet (Character'Pos (Equiv (J))));
               Length := Length + 1;
            else
               Marshall
                 (Buffer,
                  Octet'((Character'Pos (Equiv (J)) and 16#3F#) or 16#80#));
               Marshall
                 (Buffer,
                  Octet'((Character'Pos (Equiv (J)) / 2**6) or 16#C0#));
               Length := Length + 2;
            end if;
         end loop;

         Marshall (Buf, Length);
         Copy_Data (Buf.all, Reserv);
         Release (Buf);
      end;
   end Marshall;

   procedure Marshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (Error);
   begin
      if C.GIOP_1_2_Mode then
         Marshall (Buffer, Types.Octet'(2));
         Unaligned_Unsigned_Short.Marshall
           (Buffer, Unsigned_Short (Wchar'Pos (Data)));
      else
         Marshall (Buffer, Unsigned_Short (Wchar'Pos (Data)));
      end if;
   end Marshall;

   procedure Marshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (Error);
      Equiv : constant Wide_String := PolyORB.Types.To_Wide_String (Data);
   begin
      if C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Long'(Equiv'Length * 2));

      else
         Marshall (Buffer, Unsigned_Long'(Equiv'Length + 1));
      end if;

      for J in Equiv'Range loop
         Marshall
           (Buffer,
            Unsigned_Short'(Wide_Character'Pos (Equiv (J))));
      end loop;

      if not C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Short'(0));
      end if;
   end Marshall;

   procedure Marshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
   begin
      if Data in Surrogate_Character or else Data in Invalid_Character then
         Throw
           (Error,
            Data_Conversion_E,
            System_Exception_Members'
            (Minor     => 1,
             Completed => Completed_No));

         return;
      end if;

      if C.GIOP_1_2_Mode then
         Marshall (Buffer, Octet'(4));
         Unaligned_Unsigned_Short.Marshall (Buffer, BOM);
         Unaligned_Unsigned_Short.Marshall
           (Buffer, Unsigned_Short'(Wchar'Pos (Data)));

      else
         Marshall (Buffer, Unsigned_Short'(Wchar'Pos (Data)));
      end if;
   end Marshall;

   procedure Marshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      Equiv : constant Wide_String := To_Wide_String (Data);
   begin
      if C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Long (Equiv'Length + 1) * 2);
         Marshall (Buffer, BOM);

      else
         Marshall (Buffer, Unsigned_Long (Equiv'Length + 2));
         Marshall (Buffer, BOM);
      end if;

      for J in Equiv'Range loop
         if Equiv (J) in Surrogate_Character
           or else Equiv (J) in Invalid_Character
         then
            Throw
              (Error,
               Data_Conversion_E,
               System_Exception_Members'
               (Minor     => 1,
                Completed => Completed_No));
            return;
         end if;

         Marshall (Buffer, Unsigned_Short'(Wide_Character'Pos (Equiv (J))));
      end loop;

      if not C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Short'(0));
      end if;
   end Marshall;

   ------------------------------
   -- Register_Native_Code_Set --
   ------------------------------

   procedure Register_Native_Code_Set
     (Code_Set : Code_Set_Id;
      Native   : Converter_Factory;
      Fallback : Converter_Factory)
   is
   begin
      Info_Lists.Append
        (Info,
         (Code_Set             => Code_Set,
          Native               => Native,
          Fallback             => Fallback,
          Conversions          => Conversion_Lists.Empty,
          Conversion_Code_Sets => Code_Set_Id_List (Code_Set_Id_Lists.Empty)));
   end Register_Native_Code_Set;

   procedure Register_Native_Code_Set
     (Code_Set : Code_Set_Id;
      Native   : Wide_Converter_Factory;
      Fallback : Wide_Converter_Factory)
   is
   begin
      Wide_Info_Lists.Append
        (Wide_Info,
         (Code_Set             => Code_Set,
          Native               => Native,
          Fallback             => Fallback,
          Conversions          => Wide_Conversion_Lists.Empty,
          Conversion_Code_Sets => Code_Set_Id_List (Code_Set_Id_Lists.Empty)));
   end Register_Native_Code_Set;

   ----------------------------------
   -- Register_Conversion_Code_Set --
   ----------------------------------

   procedure Register_Conversion_Code_Set
     (Native     : Code_Set_Id;
      Conversion : Code_Set_Id;
      Factory    : Converter_Factory)
   is
      Info : constant Info_Lists.Element_Access := Find (Native);

   begin
      Conversion_Lists.Append (Info.Conversions, (Conversion, Factory));
      Append (Info.Conversion_Code_Sets, Conversion);
   end Register_Conversion_Code_Set;

   procedure Register_Conversion_Code_Set
     (Native     : Code_Set_Id;
      Conversion : Code_Set_Id;
      Factory    : Wide_Converter_Factory)
   is
      Info : constant Wide_Info_Lists.Element_Access := Find (Native);

   begin
      Wide_Conversion_Lists.Append (Info.Conversions, (Conversion, Factory));
      Append (Info.Conversion_Code_Sets, Conversion);
   end Register_Conversion_Code_Set;

   -----------------------
   -- Set_GIOP_1_2_Mode --
   -----------------------

   procedure Set_GIOP_1_2_Mode (C : in out Wide_Converter) is
   begin
      C.GIOP_1_2_Mode := True;
   end Set_GIOP_1_2_Mode;

   -----------------------------------------
   -- Supported_Char_Conversion_Code_Sets --
   -----------------------------------------

   function Supported_Char_Conversion_Code_Sets
     (Code_Set : Code_Set_Id)
      return Code_Set_Id_List
   is
      use type Info_Lists.Element_Access;
      Info : constant Info_Lists.Element_Access := Find (Code_Set);
   begin
      if Info /= null then
         return Info.Conversion_Code_Sets;
      else
         return Code_Set_Id_List (Code_Set_Id_Lists.Empty);
      end if;
   end Supported_Char_Conversion_Code_Sets;

   ------------------------------------------
   -- Supported_Wchar_Conversion_Code_Sets --
   ------------------------------------------

   function Supported_Wchar_Conversion_Code_Sets
     (Code_Set : Code_Set_Id) return Code_Set_Id_List
   is
      use type Wide_Info_Lists.Element_Access;
      Info : constant Wide_Info_Lists.Element_Access := Find (Code_Set);
   begin
      if Info /= null then
         return Info.Conversion_Code_Sets;
      else
         return Code_Set_Id_List (Code_Set_Id_Lists.Empty);
      end if;
   end Supported_Wchar_Conversion_Code_Sets;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Char;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);

   begin
      Data := Unmarshall_Latin_1_Char (Buffer);
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (C      : ISO88591_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);
   begin
      Data := Unmarshall_Latin_1_String (Buffer);
   end Unmarshall;

   procedure Unmarshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Char;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);

   begin
      Data := Character'Val (Octet'(Unmarshall (Buffer)));

      if Character'Pos (Data) >= 16#80# then
         Throw
           (Error,
            Data_Conversion_E,
            System_Exception_Members'
             (Minor     => 1,
              Completed => Completed_No));
      end if;
   end Unmarshall;

   procedure Unmarshall
     (C      : ISO88591_UTF8_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);

      Length : Unsigned_Long := Unmarshall (Buffer);
      Result : Standard.String (1 .. Integer (Length));
      Aux    : Octet;
      Last   : Natural := Result'First - 1;
   begin
      while Length > 0 loop
         Last := Last + 1;

         Result (Last) := Character'Val (Octet'(Unmarshall (Buffer)));
         Length := Length - 1;

         if Character'Pos (Result (Last)) >= 16#80# then
            if Length = 0 then
               raise Program_Error;
               --  XXX Raise Marshall exception ?
            end if;

            Aux := Unmarshall (Buffer);
            Result (Last) :=
              Character'Val
               (Octet'(Character'Pos (Result (Last)) and 16#1F#) * 2**6
                  + (Aux and 16#3F#));
            Length := Length - 1;
         end if;
      end loop;

      Data := To_PolyORB_String (Result (Result'First .. Last - 1));
   end Unmarshall;

   procedure Unmarshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (Error);
      Length : Octet;
   begin
      if C.GIOP_1_2_Mode then
         Length := Unmarshall (Buffer);

         if Length /= 2 then
            raise Program_Error;
            --  XXX Raise Marshall exception ?
         else
            Data := Wchar'Val (Unaligned_Unsigned_Short.Unmarshall (Buffer));
         end if;

      else
         Data := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer)));
      end if;
   end Unmarshall;

   procedure Unmarshall
     (C      : UCS2_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (Error);

      Length : constant Unsigned_Long := Unmarshall (Buffer);
      Result : Standard.Wide_String (1 .. Integer (Length));
      Last   : Natural := Result'First - 1;
   begin
      if C.GIOP_1_2_Mode then
         if Length mod 2 /= 0 then
            raise Program_Error;
            --  XXX Raise Marshall exception ?
         end if;
         Last  := Natural (Length / 2);

      else
         Last  := Natural (Length);
      end if;

      for J in Result'First .. Last loop
         Result (J) := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer)));
      end loop;

      if not C.GIOP_1_2_Mode then
         Last := Last - 1;
      end if;

      Data := To_PolyORB_Wide_String (Result (Result'First .. Last));
   end Unmarshall;

   procedure Unmarshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
      Code : Unsigned_Short;

   begin
      if C.GIOP_1_2_Mode then
         declare
            Length : constant Octet := Unmarshall (Buffer);

         begin
            Code := Unaligned_Unsigned_Short.Unmarshall (Buffer);

            if Length = 2 then
               Data := Wchar'Val (Code);

            elsif Length = 4 then
               if Code = Reverse_BOM then
                  raise Program_Error;
                  --  XXX Value marshalled in reverse endian-ness

               elsif Code = BOM then
                  Data := Wchar'Val
                            (Unaligned_Unsigned_Short.Unmarshall (Buffer));

               else
                  raise Program_Error;
               end if;

            else
               raise Program_Error;
            end if;
         end;

      else
         Code := Unmarshall (Buffer);
         if Code = Reverse_BOM then
            raise Program_Error;
            --  XXX Value marshalled in reverse endian-ness

         elsif Code = BOM then
            Data := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer)));

         else
            Data := Wchar'Val (Code);
         end if;
      end if;

      if Data in Surrogate_Character
        or else Data in Invalid_Character
      then
         Throw
           (Error,
            Data_Conversion_E,
            System_Exception_Members'
             (Minor     => 1,
              Completed => Completed_No));
      end if;
   end Unmarshall;

   procedure Unmarshall
     (C      : UCS2_UTF16_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      Length : constant Unsigned_Long := Unmarshall (Buffer);
      Result : Standard.Wide_String (1 .. Integer (Length));
      First  : Positive;
      Last   : Natural;
      Code   : Unsigned_Short;
   begin
      if C.GIOP_1_2_Mode then
         if Length mod 2 = 1 then
            raise Program_Error;
            --  XXX Raise Marshall exception ?

         elsif Length = 0 then
            Data := To_PolyORB_Wide_String (Wide_String'(""));
            return;
         end if;
         Last  := Natural (Length / 2);

      else
         Last  := Natural (Length);
      end if;

      Code := Unmarshall (Buffer);

      if Code = Reverse_BOM then
         raise Program_Error;
         --  Value encoded in reverse endian-ness

      elsif Code = BOM then
         Last  := Last - 1;
         First := Result'First;
      else
         Result (Result'First) := Wchar'Val (Code);
         First := Result'First + 1;
      end if;

      for J in First .. Last loop
         Result (J) := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer)));

         if Result (J) in Surrogate_Character
           or else Result (J) in Invalid_Character
         then
            Throw
              (Error,
               Data_Conversion_E,
               System_Exception_Members'
                (Minor     => 1,
                 Completed => Completed_No));
         end if;
      end loop;

      if not C.GIOP_1_2_Mode then
         Last := Last - 1;
      end if;

      Data := To_PolyORB_Wide_String (Result (Result'First .. Last));
   end Unmarshall;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is

      use PolyORB.Parameters;

      --  These parameters force the registration of additional fallback code
      --  sets for char and wchar data. This is useful for interoperation with
      --  ORBs with broken charsets negotiation support.

      Char_Fallback      : constant Boolean := Get_Conf
                             ("giop", "giop.add_char_fallback_code_set",
                              Default => False);
      Wide_Char_Fallback : constant Boolean := Get_Conf
                             ("giop", "giop.add_wchar_fallback_code_set",
                              Default => False);

   begin
      --  Register supported char code sets (ISO-8859-1)

      Register_Native_Code_Set
        (Ada95_Native_Character_Code_Set,
         Create_ISO88591_Native_Converter'Access,
         Create_ISO88591_UTF8_Converter'Access);

      if Char_Fallback then
         --  Fallback code sets (UTF-8)

         Register_Conversion_Code_Set
           (Ada95_Native_Character_Code_Set,
            Char_Data_Fallback_Code_Set,
            Create_ISO88591_UTF8_Converter'Access);
      end if;

      --  Register supported wchar code sets (UCS-2)

      Register_Native_Code_Set
        (Ada95_Native_Wide_Character_Code_Set,
         Create_UCS2_Native_Converter'Access,
         Create_UCS2_UTF16_Converter'Access);
      Register_Conversion_Code_Set
        (Ada95_Native_Wide_Character_Code_Set,
         UCS_2_Level_2_Code_Set,
         Create_UCS2_Native_Converter'Access);
      Register_Conversion_Code_Set
        (Ada95_Native_Wide_Character_Code_Set,
         UCS_2_Level_3_Code_Set,
         Create_UCS2_Native_Converter'Access);

      if Wide_Char_Fallback then
         --  Fallback code sets (UTF-16)

         Register_Conversion_Code_Set
           (Ada95_Native_Wide_Character_Code_Set,
            Wchar_Data_Fallback_Code_Set,
            Create_UCS2_UTF16_Converter'Access);
      end if;
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"code_sets.converters",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Code_Sets.Converters;
