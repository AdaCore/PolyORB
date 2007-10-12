------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.CODE_SETS.CONVERTERS.UNICODE                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Code_Sets.Converters.Unicode is

   use PolyORB.Buffers;
   use PolyORB.Errors;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Types;

   --  Special ranges of UTF-16 codes

   subtype Surrogate_Character is Wide_Character
     range Wide_Character'Val (16#D800#) .. Wide_Character'Val (16#DFFF#);

   subtype Invalid_Character is Wide_Character
     range Wide_Character'Val (16#FFFE#) .. Wide_Character'Val (16#FFFF#);

   subtype High_Surrogate_Character is Surrogate_Character
     range Wide_Character'Val (16#D800#) ..  Wide_Character'Val (16#DBFF#);

   subtype Low_Surrogate_Character is Surrogate_Character
     range Wide_Character'Val (16#DC00#) ..  Wide_Character'Val (16#DFFF#);

   High_Surrogate_Base : constant Types.Unsigned_Long
     := Wide_Character'Pos (High_Surrogate_Character'First);
   Low_Surrogate_Base  : constant Types.Unsigned_Long
    := Wide_Character'Pos (Low_Surrogate_Character'First);

   --  UTF16 byte order mark

   BOM         : constant Unsigned_Short := 16#FEFF#;
   Reverse_BOM : constant Unsigned_Short := 16#FFFE#;

   --  UTF-8 native converter

   type UTF8_Native_Converter is new Converter with null record;

   procedure Marshall
     (C      : UTF8_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Char;
      Error  : in out Errors.Error_Container);

   procedure Marshall
     (C      : UTF8_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UTF8_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Char;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UTF8_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container);

   --  UTF-16 native converter

   type UTF16_Native_Wide_Converter is new Wide_Converter with null record;

   procedure Marshall
     (C      : UTF16_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Marshall
     (C      : UTF16_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UTF16_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UTF16_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container);

   --  UTF-16 as UCS-2 converter

   type UTF16_UCS2_Wide_Converter is new Wide_Converter with null record;

   procedure Marshall
     (C      : UTF16_UCS2_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Marshall
     (C      : UTF16_UCS2_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UTF16_UCS2_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UTF16_UCS2_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container);

   --  UTF-16 as UCS-4 converter

   type UTF16_UCS4_Wide_Converter is new Wide_Converter with null record;

   procedure Marshall
     (C      : UTF16_UCS4_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Marshall
     (C      : UTF16_UCS4_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UTF16_UCS4_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container);

   procedure Unmarshall
     (C      : UTF16_UCS4_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container);

   --  Converter factories

   function Create_UTF8_Native_Converter return Converter_Access;

   function Create_UTF16_Native_Converter return Wide_Converter_Access;

   function Create_UTF16_UCS2_Converter return Wide_Converter_Access;

   function Create_UTF16_UCS4_Converter return Wide_Converter_Access;

   procedure Initialize;

   -----------------------------------
   -- Create_UTF16_Native_Converter --
   -----------------------------------

   function Create_UTF16_Native_Converter return Wide_Converter_Access is
   begin
      return new UTF16_Native_Wide_Converter;
   end Create_UTF16_Native_Converter;

   ---------------------------------
   -- Create_UTF16_UCS2_Converter --
   ---------------------------------

   function Create_UTF16_UCS2_Converter return Wide_Converter_Access is
   begin
      return new UTF16_UCS2_Wide_Converter;
   end Create_UTF16_UCS2_Converter;

   ---------------------------------
   -- Create_UTF16_UCS4_Converter --
   ---------------------------------

   function Create_UTF16_UCS4_Converter return Wide_Converter_Access is
   begin
      return new UTF16_UCS4_Wide_Converter;
   end Create_UTF16_UCS4_Converter;

   ----------------------------------
   -- Create_UTF8_Native_Converter --
   ----------------------------------

   function Create_UTF8_Native_Converter return Converter_Access is
   begin
      return new UTF8_Native_Converter;
   end Create_UTF8_Native_Converter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      use PolyORB.Parameters;

      --  The following parameters force the registration of
      --  additional "fallback" code sets for char and wchar
      --  data. This is useful for interoperation with ORB
      --  with broken char sets negotiation support.

      Char_Fallback      : constant Boolean
        := Get_Conf ("giop", "giop.add_char_fallback_code_set", False);
      Wide_Char_Fallback : constant Boolean
        := Get_Conf ("giop", "giop.add_wchar_fallback_code_set", False);

   begin
      --  Register supported char code sets (UTF-8)

      Register_Native_Code_Set
        (UTF_8_Code_Set,
         Create_UTF8_Native_Converter'Access,
         Create_UTF8_Native_Converter'Access);

      if Char_Fallback then
         --  Fallback code sets (UTF-8)

         Register_Conversion_Code_Set
           (UTF_8_Code_Set,
            Char_Data_Fallback_Code_Set,
            Create_UTF8_Native_Converter'Access);
      end if;

      --  Register supported wchar code sets (UTF-16)

      Register_Native_Code_Set
        (UTF_16_Code_Set,
         Create_UTF16_Native_Converter'Access,
         Create_UTF16_Native_Converter'Access);
      Register_Conversion_Code_Set
        (UTF_16_Code_Set,
         UCS_2_Level_1_Code_Set,
         Create_UTF16_UCS2_Converter'Access);
      Register_Conversion_Code_Set
        (UTF_16_Code_Set,
         UCS_2_Level_2_Code_Set,
         Create_UTF16_UCS2_Converter'Access);
      Register_Conversion_Code_Set
        (UTF_16_Code_Set,
         UCS_2_Level_3_Code_Set,
         Create_UTF16_UCS2_Converter'Access);
      Register_Conversion_Code_Set
        (UTF_16_Code_Set,
         UCS_4_Level_1_Code_Set,
         Create_UTF16_UCS4_Converter'Access);
      Register_Conversion_Code_Set
        (UTF_16_Code_Set,
         UCS_4_Level_2_Code_Set,
         Create_UTF16_UCS4_Converter'Access);
      Register_Conversion_Code_Set
        (UTF_16_Code_Set,
         UCS_4_Level_3_Code_Set,
         Create_UTF16_UCS4_Converter'Access);

      if Wide_Char_Fallback then
         --  Fallback code sets (UTF-16)

         Register_Conversion_Code_Set
           (UTF_16_Code_Set,
            Wchar_Data_Fallback_Code_Set,
            Create_UTF16_Native_Converter'Access);
      end if;
   end Initialize;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (C      : UTF16_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
   begin
      if Data in Surrogate_Character
        or else Data in Invalid_Character
      then
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
         Marshall (Buffer, BOM, 1);
         Marshall (Buffer, Unsigned_Short (Wchar'Pos (Data)), 1);

      else
         Marshall (Buffer, Unsigned_Short (Wchar'Pos (Data)), 2);
      end if;
   end Marshall;

   procedure Marshall
     (C      : UTF16_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      Equiv : constant Wide_String := To_Wide_String (Data);
      Align : Alignment_Type;

   begin
      if C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Long (Equiv'Length + 1) * 2);
         Marshall (Buffer, BOM, 1);
         Align := 1;

      else
         Marshall (Buffer, Unsigned_Long (Equiv'Length + 2));
         Marshall (Buffer, BOM, 2);
         Align := 2;
      end if;

      for J in Equiv'Range loop
         if Equiv (J) in Invalid_Character then
            Throw
              (Error,
               Data_Conversion_E,
               System_Exception_Members'
               (Minor     => 1,
                Completed => Completed_No));

            return;
         end if;

         Marshall
           (Buffer,
            Unsigned_Short (Wide_Character'Pos (Equiv (J))),
            Align);
      end loop;

      if not C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Short (0), 2);
      end if;
   end Marshall;

   procedure Marshall
     (C      : UTF16_UCS2_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
   begin
      if Data in Surrogate_Character
        or else Data in Invalid_Character
      then
         Throw
           (Error,
            Data_Conversion_E,
            System_Exception_Members'
             (Minor     => 1,
              Completed => Completed_No));

         return;
      end if;

      if C.GIOP_1_2_Mode then
         Marshall (Buffer, Types.Octet'(2));
         Marshall (Buffer, Unsigned_Short (Wchar'Pos (Data)), 1);

      else
         Marshall (Buffer, Unsigned_Short (Wchar'Pos (Data)), 2);
      end if;
   end Marshall;

   procedure Marshall
     (C      : UTF16_UCS2_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      Equiv : constant Wide_String := PolyORB.Types.To_Wide_String (Data);
      Align : Alignment_Type;

   begin
      if C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Long'(Equiv'Length * 2));
         Align := 1;

      else
         Marshall (Buffer, Unsigned_Long'(Equiv'Length + 1));
         Align := 2;
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

         Marshall
           (Buffer,
            Unsigned_Short (Wide_Character'Pos (Equiv (J))),
            Align);
      end loop;

      if not C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Short'(0), Align);
      end if;
   end Marshall;

   procedure Marshall
     (C      : UTF16_UCS4_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
   begin
      if Data in Surrogate_Character
        or else Data in Invalid_Character
      then
         Throw
           (Error,
            Data_Conversion_E,
            System_Exception_Members'
             (Minor     => 1,
              Completed => Completed_No));

         return;
      end if;

      if C.GIOP_1_2_Mode then
         Marshall (Buffer, Types.Octet'(4));
         Marshall (Buffer, Unsigned_Long (Wchar'Pos (Data)), 1);

      else
         Marshall (Buffer, Unsigned_Long (Wchar'Pos (Data)), 4);
      end if;
   end Marshall;

   procedure Marshall
     (C      : UTF16_UCS4_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      Equiv : constant Wide_String := PolyORB.Types.To_Wide_String (Data);
      Space : constant Reservation := Reserve (Buffer, 4);
      J     : Positive := Equiv'First;
      L     : Natural := 0;
      Align : Alignment_Type;

   begin
      if C.GIOP_1_2_Mode then
         Align := 1;

      else
         Align := 4;
      end if;

      while J <= Equiv'Last loop
         if Equiv (J) in High_Surrogate_Character then
            if J < Equiv'Last
              and then Equiv (J + 1) in Low_Surrogate_Character
            then
               Marshall
                 (Buffer,
                  Unsigned_Long
                  ((Wide_Character'Pos (Equiv (J)) - High_Surrogate_Base)
                    * 16#400#
                    + (Wide_Character'Pos (Equiv (J + 1)) - Low_Surrogate_Base)
                    + 16#10000#),
                  Align);
               J := J + 2;
               L := L + 1;

            else
               Throw
                 (Error,
                  Data_Conversion_E,
                  System_Exception_Members'
                  (Minor     => 1,
                   Completed => Completed_No));

               return;
            end if;

         elsif Equiv (J) in Invalid_Character then
            Throw
              (Error,
               Data_Conversion_E,
               System_Exception_Members'
               (Minor     => 1,
                Completed => Completed_No));

            return;

         else
            Marshall
              (Buffer,
               Unsigned_Long (Wide_Character'Pos (Equiv (J))),
               Align);
            J := J + 1;
            L := L + 1;
         end if;
      end loop;

      if not C.GIOP_1_2_Mode then
         Marshall (Buffer, Unsigned_Long'(0), Align);
      end if;

      declare
         Length_Buffer : Buffer_Access := new Buffer_Type;

      begin
         if C.GIOP_1_2_Mode then
            Marshall (Buffer, Unsigned_Long'(Unsigned_Long (L) * 4));

         else
            Marshall (Buffer, Unsigned_Long'(Unsigned_Long (L) + 1));
         end if;

         Copy_Data (Length_Buffer.all, Space);
         Release (Length_Buffer);
      end;
   end Marshall;

   procedure Marshall
     (C      : UTF8_Native_Converter;
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
     (C      : UTF8_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   : Types.String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);

   begin
      Marshall_Latin_1_String (Buffer, Data);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (C      : UTF16_Native_Wide_Converter;
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
            Code := Unmarshall (Buffer, 1);

            if Length = 2 then
               Data := Wchar'Val (Code);

            elsif Length = 4 then
               if Code = Reverse_BOM then
                  raise Program_Error;
                  --  XXX Value marshalled in reverse endian-ness

               elsif Code = BOM then
                  Data := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer, 1)));

               else
                  raise Program_Error;
               end if;

            else
               raise Program_Error;
            end if;
         end;

      else
         Code := Unmarshall (Buffer, 2);

         if Code = Reverse_BOM then
            raise Program_Error;
            --  XXX Value marshalled in reverse endian-ness

         elsif Code = BOM then
            Data := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer, 2)));

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
     (C      : UTF16_Native_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      Length : constant Unsigned_Long := Unmarshall (Buffer);
      Result : Standard.Wide_String (1 .. Integer (Length));
      First  : Positive;
      Last   : Natural;
      Align  : Alignment_Type;
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
         Align := 1;

      else
         Last  := Natural (Length);
         Align := 2;
      end if;

      Code := Unmarshall (Buffer, Align);

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
         Result (J) := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer, Align)));

         if Result (J) in Invalid_Character then
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

   procedure Unmarshall
     (C      : UTF16_UCS2_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
      Length : Octet;

   begin
      if C.GIOP_1_2_Mode then
         Length := Unmarshall (Buffer);

         if Length /= 2 then
            raise Program_Error;
            --  XXX Raise Marshall exception ?

         else
            Data := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer, 1)));
         end if;

      else
         Data := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer, 2)));
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
     (C      : UTF16_UCS2_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      Length : constant Unsigned_Long := Unmarshall (Buffer);
      Result : Standard.Wide_String (1 .. Integer (Length));
      Last   : Natural := Result'First - 1;
      Align  : Alignment_Type;

   begin
      if C.GIOP_1_2_Mode then
         if Length mod 2 = 1 then
            raise Program_Error;
            --  XXX Raise Marshall exception ?
         end if;

         Last  := Natural (Length / 2);
         Align := 1;

      else
         Last  := Natural (Length);
         Align := 2;
      end if;

      for J in Result'First .. Last loop
         Result (J) := Wchar'Val (Unsigned_Short'(Unmarshall (Buffer, Align)));

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

   procedure Unmarshall
     (C      : UTF16_UCS4_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wchar;
      Error  : in out Errors.Error_Container)
   is
      Length : Octet;
      Aux    : Unsigned_Long;

   begin
      if C.GIOP_1_2_Mode then
         Length := Unmarshall (Buffer);

         if Length /= 4 then
            raise Program_Error;
            --  XXX Raise Marshall exception ?

         else
            Aux := Unsigned_Long'(Unmarshall (Buffer, 1));
         end if;

      else
         Aux := Unsigned_Long'(Unmarshall (Buffer, 4));
      end if;

      if Aux > 16#FFFF#
        or else Wchar'Val (Aux) in Surrogate_Character
        or else Wchar'Val (Aux) in Invalid_Character
      then
         Throw
           (Error,
            Data_Conversion_E,
            System_Exception_Members'
             (Minor     => 1,
              Completed => Completed_No));

      else
         Data := Wchar'Val (Aux);
      end if;
   end Unmarshall;

   procedure Unmarshall
     (C      : UTF16_UCS4_Wide_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.Wide_String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (Error);

      Length : Unsigned_Long := Unmarshall (Buffer);
      Align  : Alignment_Type;

   begin
      if C.GIOP_1_2_Mode then
         if Length mod 4 /= 0 then
            raise Program_Error;
            --  XXX Raise Marshall exception ?
         end if;

         Length := Length / 4;
         Align := 1;

      else
         Align := 2;
      end if;

      declare
         Result : Standard.Wide_String (1 .. Integer (Length) * 2);
         Last   : Natural := Result'First - 1;
         Aux    : Unsigned_Long;

      begin
         for J in 1 .. Length loop
            Aux := Unsigned_Long'(Unmarshall (Buffer, Align));

            if Aux <= 16#FFFF# then
               Last := Last + 1;
               Result (Last) := Wchar'Val (Aux);

            else
               Last := Last + 1;
               Result (Last) :=
                 Wide_Character'Val (Aux / 16#400# + High_Surrogate_Base);

               Last := Last + 1;
               Result (Last) :=
                 Wide_Character'Val (Aux mod 16#400# + Low_Surrogate_Base);
            end if;
         end loop;

         if not C.GIOP_1_2_Mode then
            Last := Last - 1;
         end if;

         Data := To_PolyORB_Wide_String (Result (Result'First .. Last));
      end;
   end Unmarshall;

   procedure Unmarshall
     (C      : UTF8_Native_Converter;
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
     (C      : UTF8_Native_Converter;
      Buffer : access Buffers.Buffer_Type;
      Data   :    out Types.String;
      Error  : in out Errors.Error_Container)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Error);

   begin
      Data := Unmarshall_Latin_1_String (Buffer);
   end Unmarshall;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"code_sets.converters.unicode",
          Conflicts => Empty,
          Depends   => +"code_sets.converters",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Code_Sets.Converters.Unicode;
