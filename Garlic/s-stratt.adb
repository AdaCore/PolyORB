------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . S T R E A M _ A T T R I B U T E S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.IO_Exceptions;
with Unchecked_Conversion;

with Ada.Streams; use Ada.Streams;
with Interfaces;  use Interfaces;

package body System.Stream_Attributes is

   pragma Suppress (Range_Check);
   pragma Suppress (Overflow_Check);

   use UST;

   Err : exception renames Ada.IO_Exceptions.Data_Error;
   --  Exception raised if insufficient data read.

   SU : constant := System.Storage_Unit;
   pragma Assert (SU = 8);

   BB : constant := 2 ** SU;           --  Byte base
   BL : constant := 2 ** SU - 1;       --  Byte last
   BS : constant := 2 ** (SU - 1);     --  Byte sign

   US : constant := Unsigned'Size;     --  Unsigned size
   UB : constant := (US - 1) / SU + 1; --  Unsigned byte
   UL : constant := 2 ** US - 1;       --  Unsigned last

   FB : constant := 2.0 ** SU;         --  Float base

   subtype SE  is Ada.Streams.Stream_Element;
   subtype SEA is Ada.Streams.Stream_Element_Array;
   subtype SEO is Ada.Streams.Stream_Element_Offset;

   generic function UC renames Unchecked_Conversion;

   type Field_Type is
      record
         E_Size       : Integer; --  Exponent bit size
         E_Bias       : Integer; --  Exponent bias
         F_Size       : Integer; --  Fraction bit size
         E_Last       : Integer; --  Max exponent value
         F_Mask       : SE;      --  Mask to apply on first fraction byte
         E_Bytes      : SEO;     --  N. of exponent bytes completly used
         F_Bytes      : SEO;     --  N. of fraction bytes completly used
         F_Bits       : Integer; --  N. of bits used on first fraction word
      end record;


   type Precision is (Single, Double, Extended);
   Fields : constant array (Precision) of Field_Type
     := (
         --  Single precision
         (E_Size  => 8,
          E_Bias  => 127,
          F_Size  => 23,
          E_Last  => 2 ** 8 - 1,
          F_Mask  => 2 ** 7 - 1,
          E_Bytes => 2,
          F_Bytes => 3,
          F_Bits  => 23 mod US),
         --  Double precision
         (E_Size  => 11,
          E_Bias  => 1023,
          F_Size  => 52,
          E_Last  => 2 ** 11 - 1,
          F_Mask  => 2 ** 4 - 1,
          E_Bytes => 2,
          F_Bytes => 7,
          F_Bits  => 52 mod US),
         --  Extended precision
         (E_Size  => 15,
          E_Bias  => 16383,
          F_Size  => 63,
          E_Last  => 2 ** 15 - 1,
          F_Mask  => 2 ** 8 - 1,
          E_Bytes => 2,
          F_Bytes => 8,
          F_Bits  => 63 mod US));

   --  The representation of all items requires a multiple of four bytes
   --  (or 32 bits) of data. The bytes are numbered 0 through n-1. The bytes
   --  are read or written to some byte stream such that byte m always
   --  precedes byte m+1. If the n bytes needed to contain the data are not
   --  a multiple of four, then the n bytes are followed by enough (0 to 3)
   --  residual zero bytes, r, to make the total byte count a multiple of 4.

   --  An XDR signed integer is a 32-bit datum that encodes an integer
   --  in the range [-2147483648,2147483647]. The integer is represented
   --  in two's complement notation. The most and least significant bytes
   --  are 0 and 3, respectively. Integers are declared as follows:
   --
   --        (MSB)                   (LSB)
   --      +-------+-------+-------+-------+
   --      |byte 0 |byte 1 |byte 2 |byte 3 |
   --      +-------+-------+-------+-------+
   --      <------------32 bits------------>

   pragma Assert (Long_Long_Integer'Size <= 64);
   pragma Assert (16 <= Integer'Size and Integer'Size <= 32);

   SSI_L : constant := 4;
   SI_L  : constant := 4;
   I_L   : constant := 4;
   LI_L  : constant := 8;
   LLI_L : constant := 8;

   subtype XDR_S_SSI is SEA (1 .. SSI_L);
   subtype XDR_S_SI  is SEA (1 .. SI_L);
   subtype XDR_S_I   is SEA (1 .. I_L);
   subtype XDR_S_LI  is SEA (1 .. LI_L);
   subtype XDR_S_LLI is SEA (1 .. LLI_L);

   type XDR_SSI is range -2 ** (SU * SSI_L - 1) .. 2 ** (SU * SSI_L - 1) - 1;
   type XDR_SI  is range -2 ** (SU * SI_L  - 1) .. 2 ** (SU * SI_L  - 1) - 1;
   type XDR_I   is range -2 ** (SU * I_L   - 1) .. 2 ** (SU * I_L   - 1) - 1;
   type XDR_LI  is range -2 ** (SU * LI_L  - 1) .. 2 ** (SU * LI_L  - 1) - 1;
   type XDR_LLI is range -2 ** (SU * LLI_L - 1) .. 2 ** (SU * LLI_L - 1) - 1;

   --  An XDR unsigned integer is a 32-bit datum that encodes a nonnegative
   --  integer in the range [0,4294967295]. It is represented by an unsigned
   --  binary number whose most and least significant bytes are 0 and 3,
   --  respectively. An unsigned integer is declared as follows:
   --
   --        (MSB)                   (LSB)
   --      +-------+-------+-------+-------+
   --      |byte 0 |byte 1 |byte 2 |byte 3 |
   --      +-------+-------+-------+-------+
   --      <------------32 bits------------>

   pragma Assert (Long_Long_Unsigned'Size <= 64);
   pragma Assert (16 <= Unsigned'Size and Unsigned'Size <= 32);

   SSU_L : constant := 4;
   SU_L  : constant := 4;
   U_L   : constant := 4;
   LU_L  : constant := 8;
   LLU_L : constant := 8;

   subtype XDR_S_SSU is SEA (1 .. SSU_L);
   subtype XDR_S_SU  is SEA (1 .. SU_L);
   subtype XDR_S_U   is SEA (1 .. U_L);
   subtype XDR_S_LU  is SEA (1 .. LU_L);
   subtype XDR_S_LLU is SEA (1 .. LLU_L);

   type XDR_SSU is mod BB ** SSU_L;
   type XDR_SU  is mod BB ** SU_L;
   type XDR_U   is mod BB ** U_L;
   type XDR_LU  is mod BB ** LU_L;
   type XDR_LLU is mod BB ** LLU_L;

   --  The standard defines the floating-point data type "float" (32 bits
   --  or 4 bytes). The encoding used is the IEEE standard for normalized
   --  single-precision floating-point numbers.

   --  The standard defines the encoding for the double-precision
   --  floating-point data type "double" (64 bits or 8 bytes). The
   --  encoding used is the IEEE standard for normalized double-precision
   --  floating-point numbers.

   SF_L  : constant := 4;   --  Single precision
   F_L   : constant := 4;   --  Single precision
   LF_L  : constant := 8;   --  Double precision
   LLF_L : constant := 12;  --  Extended precision

   --  TBD
   TM_L : constant := 8;
   subtype XDR_S_TM is SEA (1 .. TM_L);
   type XDR_TM is mod BB ** TM_L;

   type XDR_SA is mod 2 ** Standard'Address_Size;
   function To_XDR_SA is new UC (System.Address, XDR_SA);
   function To_XDR_SA is new UC (XDR_SA, System.Address);


   --  Enumerations have the same representation as signed integers.
   --  Enumerations are handy for describing subsets of the integers.

   --  Booleans are important enough and occur frequently enough to warrant
   --  their own explicit type in the standard. Booleans are declared as
   --  an enumeration.

   --  enum { FALSE = 0, TRUE = 1 } boolean;

   B_L   : constant := 4;
   subtype XDR_S_B is SEA (1 .. B_L);
   type XDR_B  is mod BB ** B_L;


   --  The standard defines a string of n (numbered 0 through n-1) ASCII
   --  bytes to be the number n encoded as an unsigned integer (as described
   --  above), and followed by the n bytes of the string. Byte m of the string
   --  always precedes byte m+1 of the string, and byte 0 of the string always
   --  follows the string's length. If n is not a multiple of four, then the
   --  n bytes are followed by enough (0 to 3) residual zero bytes, r, to make
   --  the total byte count a multiple of four.

   --  To fit with XDR string, do not consider character as an enumeration
   --  type.

   C_L   : constant := 1;
   subtype XDR_S_C is SEA (1 .. C_L);
   type XDR_C  is mod BB ** C_L;

   --  Consider Wide_Character as an enumeration type.
   WC_L  : constant := 4;
   subtype XDR_S_WC is SEA (1 .. WC_L);
   type XDR_WC is mod BB ** WC_L;

   ----------------
   -- Workaround --
   ----------------

   function Scaling
     (X : Short_Float; A : Integer)
      return Short_Float;
   function Scaling
     (X : Short_Float; A : Integer)
      return Short_Float is
      E : Integer := Short_Float'Exponent (X);
   begin
      return Short_Float'Compose (X, A + E);
   end Scaling;

   function Scaling
     (X : Float; A : Integer)
      return Float;
   function Scaling
     (X : Float; A : Integer)
      return Float is
      E : Integer := Float'Exponent (X);
   begin
      return Float'Compose (X, A + E);
   end Scaling;

   function Scaling
     (X : Long_Float; A : Integer)
      return Long_Float;
   function Scaling
     (X : Long_Float; A : Integer)
      return Long_Float is
      E : Integer := Long_Float'Exponent (X);
   begin
      return Long_Float'Compose (X, A + E);
   end Scaling;

   function Scaling
     (X : Long_Long_Float; A : Integer)
      return Long_Long_Float;
   function Scaling
     (X : Long_Long_Float; A : Integer)
      return Long_Long_Float is
      E : Integer := Long_Long_Float'Exponent (X);
   begin
      return Long_Long_Float'Compose (X, A + E);
   end Scaling;

   ----------
   -- I_AD --
   ----------

   function I_AD (Stream : access RST) return Fat_Pointer is
      FP : Fat_Pointer;

   begin
      FP.P1 := I_AS (Stream).P1;
      FP.P2 := I_AS (Stream).P1;

      return FP;
   end I_AD;

   ----------
   -- I_AS --
   ----------

   function I_AS (Stream : access RST) return Thin_Pointer is
      S : XDR_S_TM;
      L : SEO;
      U : XDR_TM := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_TM (S (N));
         end loop;

         return (P1 => To_XDR_SA (XDR_SA (U)));
      end if;
   end I_AS;

   ---------
   -- I_B --
   ---------

   function I_B (Stream : access RST) return Boolean is
      S : XDR_S_B;
      L : SEO;
      U : XDR_B := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_B (S (N));
         end loop;

         case U is
            when 0      => return False;
            when 1      => return True;
            when others => raise Err;
         end case;
      end if;
   end I_B;

   ---------
   -- I_C --
   ---------

   function I_C (Stream : access RST) return Character is
      S : XDR_S_C;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else

         --  Use Ada requirements on Character representation clause.
         return Character'Val (S (1));
      end if;
   end I_C;

   ----------
   -- I_WC --
   ----------

   function I_WC (Stream : access RST) return Wide_Character is
      S : XDR_S_WC;
      L : SEO;
      U : XDR_WC;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_WC (S (N));
         end loop;

         --  Use Ada requirements on Wide_Character representation clause.
         return Wide_Character'Val (U);
      end if;
   end I_WC;

   ----------
   -- I_SF --
   ----------

   function I_SF (Stream : access RST) return Short_Float is
      I       : constant Precision := Single;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;

      E : Unsigned;
      P : Boolean;
      X : Short_Float;
      S : SEA (1 .. SF_L);
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      end if;

      --  Extract Fraction, Exponent and Sign.
      X := Short_Float (S (SF_L + 1 - F_Bytes) and F_Mask);
      for N in SF_L + 2 - F_Bytes .. SF_L loop
         X := X * FB + Short_Float (S (N));
      end loop;
      X := Scaling (X, -F_Size); --  Short_Float

      if BS <= S (1) then
         P := False;
         E := Unsigned (S (1) - BS);
      else
         P := True;
         E := Unsigned (S (1));
      end if;

      for N in 2 .. E_Bytes loop
         E := E * BB + Unsigned (S (N));
      end loop;
      E := Shift_Right (E, Integer (E_Bytes) * SU - E_Size - 1);

      --  Look for special cases.
      if X = 0.0 then

         --  Signed zeros.
         if E = 0 then
            if P then
               return Short_Float'Copy_Sign (0.0, 1.0);
            else
               return Short_Float'Copy_Sign (0.0, -1.0);
            end if;

         else

            --  Signed infinites.
            if E = Unsigned (E_Last) then
               if P then
                  return Short_Float'Safe_Last;
               else
                  return Short_Float'Safe_First;
               end if;
            end if;
         end if;
      end if;

      --  Denormalized float.
      if E = 0 then

         X := Scaling (X, 1 - E_Bias); --  Short_Float

      --  Normalized float.
      else

         X := Scaling (X + 1.0, Integer (E) - E_Bias); --  Short_Float

      end if;

      if P then
         X := Short_Float'Copy_Sign (X, 1.0);
      else
         X := Short_Float'Copy_Sign (X, -1.0);
      end if;

      return X;
   end I_SF;

   ---------
   -- I_F --
   ---------

   function I_F (Stream : access RST) return Float is
      I       : constant Precision := Single;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;

      E : Unsigned;
      P : Boolean;
      X : Float;
      S : SEA (1 .. F_L);
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      end if;

      --  Extract Fraction, Exponent and Sign.
      X := Float (S (F_L + 1 - F_Bytes) and F_Mask);
      for N in F_L + 2 - F_Bytes .. F_L loop
         X := X * FB + Float (S (N));
      end loop;
      X := Scaling (X, -F_Size); --  Float

      if BS <= S (1) then
         P := False;
         E := Unsigned (S (1) - BS);
      else
         P := True;
         E := Unsigned (S (1));
      end if;

      for N in 2 .. E_Bytes loop
         E := E * BB + Unsigned (S (N));
      end loop;
      E := Shift_Right (E, Integer (E_Bytes) * SU - E_Size - 1);

      --  Look for special cases.
      if X = 0.0 then

         --  Signed zeros.
         if E = 0 then
            if P then
               return Float'Copy_Sign (0.0, 1.0);
            else
               return Float'Copy_Sign (0.0, -1.0);
            end if;

         else

            --  Signed infinites.
            if E = Unsigned (E_Last) then
               if P then
                  return Float'Safe_Last;
               else
                  return Float'Safe_First;
               end if;
            end if;
         end if;
      end if;

      --  Denormalized float.
      if E = 0 then

         X := Scaling (X, 1 - E_Bias); --  Flaot

      --  Normalized float.
      else

         X := Scaling (X + 1.0, Integer (E) - E_Bias); --  Float

      end if;

      if P then
         X := Float'Copy_Sign (X, 1.0);
      else
         X := Float'Copy_Sign (X, -1.0);
      end if;

      return X;
   end I_F;

   ----------
   -- I_LF --
   ----------

   function I_LF (Stream : access RST) return Long_Float is
      I       : constant Precision := Double;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;

      E : Unsigned;
      P : Boolean;
      X : Long_Float;
      S : SEA (1 .. LF_L);
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      end if;

      --  Extract Fraction, Exponent and Sign.
      X := Long_Float (S (LF_L + 1 - F_Bytes) and F_Mask);
      for N in LF_L + 2 - F_Bytes .. LF_L loop
         X := X * FB + Long_Float (S (N));
      end loop;
      X := Scaling (X, -F_Size); --  Long_Float

      if BS <= S (1) then
         P := False;
         E := Unsigned (S (1) - BS);
      else
         P := True;
         E := Unsigned (S (1));
      end if;

      for N in 2 .. E_Bytes loop
         E := E * BB + Unsigned (S (N));
      end loop;
      E := Shift_Right (E, Integer (E_Bytes) * SU - E_Size - 1);

      --  Look for special cases.
      if X = 0.0 then

         --  Signed zeros.
         if E = 0 then
            if P then
               return Long_Float'Copy_Sign (0.0, 1.0);
            else
               return Long_Float'Copy_Sign (0.0, -1.0);
            end if;

         else

            --  Signed infinites.
            if E = Unsigned (E_Last) then
               if P then
                  return Long_Float'Safe_Last;
               else
                  return Long_Float'Safe_First;
               end if;
            end if;
         end if;
      end if;

      --  Denormalized float.
      if E = 0 then

         X := Scaling (X, 1 - E_Bias); --  Long_Float

      --  Normalized float.
      else

         X := Scaling (X + 1.0, Integer (E) - E_Bias); --  Long_Float

      end if;

      if P then
         X := Long_Float'Copy_Sign (X, 1.0);
      else
         X := Long_Float'Copy_Sign (X, -1.0);
      end if;

      return X;
   end I_LF;

   -----------
   -- I_LLF --
   -----------

   function I_LLF (Stream : access RST) return Long_Long_Float is
      I       : constant Precision := Extended;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;

      E : Unsigned;
      P : Boolean;
      X : Long_Long_Float;
      S : SEA (1 .. LLF_L);
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      end if;

      --  Extract Fraction, Exponent and Sign.
      X := Long_Long_Float (S (LLF_L + 1 - F_Bytes) and F_Mask);
      for N in LLF_L + 2 - F_Bytes .. LLF_L loop
         X := X * FB + Long_Long_Float (S (N));
      end loop;
      X := Scaling (X, -F_Size); --  Long_Long_Float

      if BS <= S (1) then
         P := False;
         E := Unsigned (S (1) - BS);
      else
         P := True;
         E := Unsigned (S (1));
      end if;

      for N in 2 .. E_Bytes loop
         E := E * BB + Unsigned (S (N));
      end loop;
      E := Shift_Right (E, Integer (E_Bytes) * SU - E_Size - 1);

      --  Look for special cases.
      if X = 0.0 then

         --  Signed zeros.
         if E = 0 then
            if P then
               return Long_Long_Float'Copy_Sign (0.0, 1.0);
            else
               return Long_Long_Float'Copy_Sign (0.0, -1.0);
            end if;

         else

            --  Signed infinites.
            if E = Unsigned (E_Last) then
               if P then
                  return Long_Long_Float'Safe_Last;
               else
                  return Long_Long_Float'Safe_First;
               end if;
            end if;
         end if;
      end if;

      --  Denormalized float.
      if E = 0 then

         X := Scaling (X, 1 - E_Bias); --  Long_Long_Float

      --  Normalized float.
      else

         X := Scaling (X + 1.0, Integer (E) - E_Bias); --  Long_Long_Float

      end if;

      if P then
         X := Long_Long_Float'Copy_Sign (X, 1.0);
      else
         X := Long_Long_Float'Copy_Sign (X, -1.0);
      end if;

      return X;
   end I_LLF;

   -----------
   -- I_SSI --
   -----------

   function I_SSI (Stream : access RST) return Short_Short_Integer is
      S : XDR_S_SSI;
      L : SEO;
      U : XDR_SSU;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_SSU (S (N));
         end loop;

         --  Test sign and apply two complement's notation.
         if S (1) < BL then
            return Short_Short_Integer (U);
         else
            return Short_Short_Integer (-((XDR_SSU'Last xor U) + 1));
         end if;
      end if;
   end I_SSI;

   ----------
   -- I_SI --
   ----------

   function I_SI (Stream : access RST) return Short_Integer is
      S : XDR_S_SI;
      L : SEO;
      U : XDR_SU;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_SU (S (N));
         end loop;

         --  test sign and apply two complement's notation.
         if S (1) < BL then
            return Short_Integer (U);
         else
            return Short_Integer (-((XDR_SU'Last xor U) + 1));
         end if;
      end if;
   end I_SI;

   ---------
   -- I_I --
   ---------

   function I_I (Stream : access RST) return Integer is
      S : XDR_S_I;
      L : SEO;
      U : XDR_U;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_U (S (N));
         end loop;

         --  Test sign and apply two complement's notation.
         if S (1) < BL then
            return Integer (U);
         else
            return Integer (-((XDR_U'Last xor U) + 1));
         end if;
      end if;
   end I_I;

   ----------
   -- I_LI --
   ----------

   function I_LI (Stream : access RST) return Long_Integer is
      S : XDR_S_LI;
      L : SEO;
      U : Unsigned := 0;
      X : Long_Unsigned := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else

         --  Compute using machine unsigned
         --  rather than long_long_unsigned

         for N in S'Range loop
            U := U * BB + Unsigned (S (N));

            --  We have filled an unsigned.
            if N mod UB = 0 then
               X := Shift_Left (X, US) + Long_Unsigned (U);
               U := 0;
            end if;
         end loop;

         --  Test sign and apply two complement's notation.
         if S (1) < BL then
            return Long_Integer (X);
         else
            return Long_Integer (-((Long_Unsigned'Last xor X) + 1));
         end if;

      end if;
   end I_LI;

   -----------
   -- I_LLI --
   -----------

   function I_LLI (Stream : access RST) return Long_Long_Integer is
      S : XDR_S_LLI;
      L : SEO;
      U : Unsigned := 0;
      X : Long_Long_Unsigned := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else

         --  Compute using machine unsigned for computing
         --  rather than long_long_unsigned.

         for N in S'Range loop
            U := U * BB + Unsigned (S (N));

            --  We have filled an unsigned.
            if N mod UB = 0 then
               X := Shift_Left (X, US) + Long_Long_Unsigned (U);
               U := 0;
            end if;
         end loop;

         --  Test sign and apply two complement's notation.
         if S (1) < BL then
            return Long_Long_Integer (X);
         else
            return Long_Long_Integer (-((Long_Long_Unsigned'Last xor X) + 1));
         end if;
      end if;
   end I_LLI;

   -----------
   -- I_SSU --
   -----------

   function I_SSU (Stream : access RST) return Short_Short_Unsigned is
      S : XDR_S_SSU;
      L : SEO;
      U : XDR_SSU := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_SSU (S (N));
         end loop;

         return Short_Short_Unsigned (U);
      end if;
   end I_SSU;

   ----------
   -- I_SU --
   ----------

   function I_SU (Stream : access RST) return Short_Unsigned is
      S : XDR_S_SU;
      L : SEO;
      U : XDR_SU := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_SU (S (N));
         end loop;

         return Short_Unsigned (U);
      end if;
   end I_SU;

   ---------
   -- I_U --
   ---------

   function I_U (Stream : access RST) return Unsigned is
      S : XDR_S_U;
      L : SEO;
      U : XDR_U := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else
         for N in S'Range loop
            U := U * BB + XDR_U (S (N));
         end loop;

         return Unsigned (U);
      end if;
   end I_U;

   ----------
   -- I_LU --
   ----------

   function I_LU (Stream : access RST) return Long_Unsigned is
      S : XDR_S_LU;
      L : SEO;
      U : Unsigned := 0;
      X : Long_Unsigned := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else

         --  Compute using machine unsigned
         --  rather than long_unsigned.

         for N in S'Range loop
            U := U * BB + Unsigned (S (N));

            --  We have filled an unsigned.
            if N mod UB = 0 then
               X := Shift_Left (X, US) + Long_Unsigned (U);
               U := 0;
            end if;
         end loop;

         return X;
      end if;
   end I_LU;

   -----------
   -- I_LLU --
   -----------

   function I_LLU (Stream : access RST) return Long_Long_Unsigned is
      S : XDR_S_LLU;
      L : SEO;
      U : Unsigned := 0;
      X : Long_Long_Unsigned := 0;

   begin
      Ada.Streams.Read (Stream.all, S, L);

      if L /= S'Last then
         raise Err;
      else

         --  Compute using machine unsigned
         --  rather than long_long_unsigned.

         for N in S'Range loop
            U := U * BB + Unsigned (S (N));

            --  We have filled an unsigned.
            if N mod UB = 0 then
               X := Shift_Left (X, US) + Long_Long_Unsigned (U);
               U := 0;
            end if;
         end loop;

         return X;
      end if;
   end I_LLU;

   ----------
   -- W_AD --
   ----------

   procedure W_AD (Stream : access RST; Item : in Fat_Pointer) is
      S : XDR_S_TM;
      U : XDR_TM;

   begin
      U := XDR_TM (To_XDR_SA (Item.P1));
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      U := XDR_TM (To_XDR_SA (Item.P2));
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_AD;

   ----------
   -- W_AS --
   ----------

   procedure W_AS (Stream : access RST; Item : in Thin_Pointer) is
      S : XDR_S_TM;
      U : XDR_TM := XDR_TM (To_XDR_SA (Item.P1));

   begin
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_AS;

   ---------
   -- W_B --
   ---------

   procedure W_B (Stream : access RST; Item : in Boolean) is
      S : XDR_S_B;
      U : XDR_B;

   begin

      --  False = 0, True = 1.

      if Item then
         U := 1;
      else
         U := 0;
      end if;

      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_B;

   ---------
   -- W_C --
   ---------

   procedure W_C (Stream : access RST; Item : in Character) is
      S : XDR_S_C;

      pragma Assert (C_L = 1);

   begin

      --  Use Ada requirements on Character representation clause.
      S (1) := SE (Character'Pos (Item));

      Ada.Streams.Write (Stream.all, S);
   end W_C;

   ----------
   -- W_WC --
   ----------

   procedure W_WC (Stream : access RST; Item : in Wide_Character) is
      S : XDR_S_WC;
      U : XDR_WC;

   begin

      --  Use Ada requirements on Wide_Character representation clause.
      U := XDR_WC (Wide_Character'Pos (Item));

      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_WC;

   ----------
   -- W_SF --
   ----------

   procedure W_SF (Stream : access RST; Item : in Short_Float) is
      I       : constant Precision := Single;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;

      E : Integer := 0;
      F : Short_Float;
      Q : Short_Float;
      N : SEO;
      U : Unsigned := 0;
      P : Integer;
      S : SEA (1 .. SF_L) := (others => 0);

   begin
      F := abs (Item);

      --  Signed zero.
      if Item = 0.0 then

         U := 0;

      else

         --  Signed infinites.
         if Item <= Short_Float'Safe_First or else
            Short_Float'Safe_Last <= Item then
            E := E_Last;

         else
            E := Short_Float'Exponent (F);

            --  Denormalized float.
            if E <= 1 - E_Bias then
               E := 0;
               F := Scaling (F, E_Bias - 1); --  Short_Float

            --  Signed infinites.
            else
               if E_Last + E_Bias < E then
                  E := E_Last;
                  F := 0.0;

               --  Normalized float.
               else
                  E := E + E_Bias - 1;
                  F := Short_Float'Fraction (F) * 2.0 - 1.0;
               end if;
            end if;

            --  Copy fraction on the stream array.
            --  Compute using machine unsigned rather
            --  than larger unsigned.
            --  N : Number of intermediate unsigned.
            --  F : Float fraction.
            --  P : Bits to shift left.
            --  U : Intermediate unsigned.

            N := (F_Bytes - 1) / UB;
            P := Fields (I).F_Bits;
            loop
               F := Scaling  (F, P); --  Short_Float
               Q := Short_Float'Truncation (F);
               U := Unsigned (Q);
               for I in reverse SF_L - UB * (N + 1) + 1 .. SF_L - UB * N loop
                  S (I) := SE (U mod BB);
                  U := U / BB;
               end loop;
               exit when N = 0;
               N := N - 1;
               F := F - Q;
               P := Unsigned'Size;
            end loop;
         end if;

         --  Store the exponent at the proper bit position.
         U := Shift_Left (Unsigned (E), Integer (E_Bytes) * SU - E_Size - 1);

         --  We intentionnally don't store the first byte
         --  as we have to add the sign bit.
         for N in reverse 2 .. E_Bytes loop
            S (N) := SE (U mod BB) + S (N);
            U := U / BB;
         end loop;

      end if;

      --  Store the sign and the first exponent byte.
      if Short_Float'Copy_Sign (1.0, Item) = -1.0 then
         S (1) := SE (U + BS);
      else
         S (1) := SE (U);
      end if;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_SF;

   ---------
   -- W_F --
   ---------

   procedure W_F (Stream : access RST; Item : in Float) is
      I       : constant Precision := Single;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;

      E : Integer := 0;
      F : Float;
      Q : Float;
      N : SEO;
      U : Unsigned;
      P : Integer;
      S : SEA (1 .. F_L) := (others => 0);

   begin
      F := abs (Item);

      --  Signed zero.
      if Item = 0.0 then

         U := 0;

      else

         --  Signed infinites.
         if Item <= Float'Safe_First or else
            Float'Safe_Last <= Item then
            E := E_Last;

         else
            E := Float'Exponent (F);

            --  Denormalized float.
            if E <= 1 - E_Bias then
               E := 0;
               F := Scaling (F, E_Bias - 1); --  Float

            --  Signed infinites.
            else
               if E_Last + E_Bias < E then
                  E := E_Last;
                  F := 0.0;

               --  Normalized float.
               else
                  E := E + E_Bias - 1;
                  F := Float'Fraction (F) * 2.0 - 1.0;
               end if;
            end if;

            --  Copy fraction on the stream array.
            --  Compute using machine unsigned rather
            --  than larger unsigned.
            --  N : Number of intermediate unsigned.
            --  F : Float fraction.
            --  P : Bits to shift left.
            --  U : Intermediate unsigned.

            N := (F_Bytes - 1) / UB;
            P := Fields (I).F_Bits;
            loop
               F := Scaling  (F, P); --  Float
               Q := Float'Truncation (F);
               U := Unsigned (Q);
               for I in reverse F_L - UB * (N + 1) + 1 .. F_L - UB * N loop
                  S (I) := SE (U mod BB);
                  U := U / BB;
               end loop;
               exit when N = 0;
               N := N - 1;
               F := F - Q;
               P := Unsigned'Size;
            end loop;
         end if;

         --  Store the exponent at the proper bit position.
         U := Shift_Left (Unsigned (E), Integer (E_Bytes) * SU - E_Size - 1);

         --  We intentionnally don't store the first byte
         --  as we have to add the sign bit.
         for N in reverse 2 .. E_Bytes loop
            S (N) := SE (U mod BB) + S (N);
            U := U / BB;
         end loop;

      end if;

      --  Store the sign and the first exponent byte.
      if Float'Copy_Sign (1.0, Item) = -1.0 then
         S (1) := SE (U + BS);
      else
         S (1) := SE (U);
      end if;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_F;

   ----------
   -- W_LF --
   ----------

   procedure W_LF (Stream : access RST; Item : in Long_Float) is
      I       : constant Precision := Double;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;

      E : Integer := 0;
      F : Long_Float;
      Q : Long_Float;
      N : SEO;
      U : Unsigned;
      P : Integer;
      S : SEA (1 .. LF_L) := (others => 0);

   begin
      F := abs (Item);

      --  Signed zero.
      if Item = 0.0 then

         U := 0;

      else

         --  Signed infinites.
         if Item <= Long_Float'Safe_First or else
            Long_Float'Safe_Last <= Item then
            E := E_Last;

         else
            E := Long_Float'Exponent (F);

            --  Denormalized float.
            if E <= 1 - E_Bias then
               E := 0;
               F := Scaling (F, E_Bias - 1); --  Long_Float

            --  Signed infinites.
            else
               if E_Last + E_Bias < E then
                  E := E_Last;
                  F := 0.0;

               --  Normalized float.
               else
                  E := E + E_Bias - 1;
                  F := Long_Float'Fraction (F) * 2.0 - 1.0;
               end if;
            end if;

            --  Copy fraction on the stream array.
            --  Compute using machine unsigned rather
            --  than larger unsigned.
            --  N : Number of intermediate unsigned.
            --  F : Float fraction.
            --  P : Bits to shift left.
            --  U : Intermediate unsigned.

            N := (F_Bytes - 1) / UB;
            P := Fields (I).F_Bits;
            loop
               F := Scaling  (F, P); --  Long_Float
               Q := Long_Float'Truncation (F);
               U := Unsigned (Q);
               for I in reverse LF_L - UB * (N + 1) + 1 .. LF_L - UB * N loop
                  S (I) := SE (U mod BB);
                  U := U / BB;
               end loop;
               exit when N = 0;
               N := N - 1;
               F := F - Q;
               P := Unsigned'Size;
            end loop;
         end if;

         --  Store the exponent at the proper bit position.
         U := Shift_Left (Unsigned (E), Integer (E_Bytes) * SU - E_Size - 1);

         --  We intentionnally don't store the first byte
         --  as we have to add the sign bit.
         for N in reverse 2 .. E_Bytes loop
            S (N) := SE (U mod BB) + S (N);
            U := U / BB;
         end loop;

      end if;

      --  Store the sign and the first exponent byte.
      if Long_Float'Copy_Sign (1.0, Item) = -1.0 then
         S (1) := SE (U + BS);
      else
         S (1) := SE (U);
      end if;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_LF;

   -----------
   -- W_LLF --
   -----------

   procedure W_LLF (Stream : access RST; Item : in Long_Long_Float) is
      I       : constant Precision := Extended;
      E_Size  : Integer  renames Fields (I).E_Size;
      E_Bias  : Integer  renames Fields (I).E_Bias;
      E_Last  : Integer  renames Fields (I).E_Last;
      F_Size  : Integer  renames Fields (I).F_Size;
      F_Mask  : SE       renames Fields (I).F_Mask;
      E_Bytes : SEO      renames Fields (I).E_Bytes;
      F_Bytes : SEO      renames Fields (I).F_Bytes;

      E : Integer := 0;
      F : Long_Long_Float;
      Q : Long_Long_Float;
      N : SEO;
      U : Unsigned;
      P : Integer;
      S : SEA (1 .. LLF_L) := (others => 0);

   begin
      F := abs (Item);

      --  Signed zero.
      if Item = 0.0 then

         U := 0;

      else

         --  Signed infinites.
         if Item <= Long_Long_Float'Safe_First or else
            Long_Long_Float'Safe_Last <= Item then
            E := E_Last;

         else
            E := Long_Long_Float'Exponent (F);

            --  Denormalized float.
            if E <= 1 - E_Bias then
               E := 0;
               F := Scaling (F, E_Bias - 1); --  Long_Long_Float

            --  Signed infinites.
            else
               if E_Last + E_Bias < Integer (E) then
                  E := E_Last;
                  F := 0.0;

               --  Normalized float.
               else
                  E := E + E_Bias - 1;
                  F := Long_Long_Float'Fraction (F) * 2.0 - 1.0;
               end if;
            end if;

            --  Copy fraction on the stream array.
            --  Compute using machine unsigned rather
            --  than larger unsigned.
            --  N : Number of intermediate unsigned.
            --  F : Float fraction.
            --  P : Bits to shift left.
            --  U : Intermediate unsigned.

            N := (F_Bytes - 1) / UB;
            P := Fields (I).F_Bits;
            loop
               F := Scaling  (F, P); --  Long_Long_Float
               Q := Long_Long_Float'Truncation (F);
               U := Unsigned (Q);
               for I in reverse LLF_L - UB * (N + 1) + 1 .. LLF_L - UB * N loop
                  S (I) := SE (U mod BB);
                  U := U / BB;
               end loop;
               exit when N = 0;
               N := N - 1;
               F := F - Q;
               P := Unsigned'Size;
            end loop;
         end if;

         --  Store the exponent at the proper bit position.
         U := Shift_Left (Unsigned (E), Integer (E_Bytes) * SU - E_Size - 1);

         --  We intentionnally don't store the first byte
         --  as we have to add the sign bit.
         for N in reverse 2 .. E_Bytes loop
            S (N) := SE (U mod BB) + S (N);
            U := U / BB;
         end loop;

      end if;

      --  Store the sign and the first exponent byte.
      if Long_Long_Float'Copy_Sign (1.0, Item) = -1.0 then
         S (1) := SE (U + BS);
      else
         S (1) := SE (U);
      end if;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_LLF;

   -----------
   -- W_SSI --
   -----------

   procedure W_SSI (Stream : access RST; Item : in Short_Short_Integer) is
      S : XDR_S_SSI;
      U : XDR_SSU;

   begin

      --  Test sign and apply two complement's notation.
      if Item < 0 then
         U := XDR_SSU'Last xor XDR_SSU (-(Item + 1));
      else
         U := XDR_SSU (Item);
      end if;

      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_SSI;

   ----------
   -- W_SI --
   ----------

   procedure W_SI (Stream : access RST; Item : in Short_Integer) is
      S : XDR_S_SI;
      U : XDR_SU;

   begin

      --  Test sign and apply two complement's notation.
      if Item < 0 then
         U := XDR_SU'Last xor XDR_SU (-(Item + 1));
      else
         U := XDR_SU (Item);
      end if;

      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_SI;

   ---------
   -- W_I --
   ---------

   procedure W_I (Stream : access RST; Item : in Integer) is
      S : XDR_S_I;
      U : XDR_U;

   begin

      --  Test sign and apply two complement's notation.
      if Item < 0 then
         U := XDR_U'Last xor XDR_U (-(Item + 1));
      else
         U := XDR_U (Item);
      end if;

      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_I;

   ----------
   -- W_LI --
   ----------

   procedure W_LI (Stream : access RST; Item : in Long_Integer) is
      S : XDR_S_LI;
      U : Unsigned;
      X : Long_Unsigned;

   begin

      --  Test sign and apply two complement's notation.
      if Item < 0 then
         X := Long_Unsigned'Last xor Long_Unsigned (-(Item + 1));
      else
         X := Long_Unsigned (Item);
      end if;

      --  Compute using machine unsigned
      --  rather than long_unsigned.

      for N in reverse S'Range loop

         --  We have filled an unsinged.
         if (LU_L - N) mod UB = 0 then
            U := Unsigned (X and UL);
            X := Shift_Right (X, US);
         end if;

         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_LI;

   -----------
   -- W_LLI --
   -----------

   procedure W_LLI (Stream : access RST; Item : in Long_Long_Integer) is
      S : XDR_S_LLI;
      U : Unsigned;
      X : Long_Long_Unsigned;

   begin

      --  Test sign and apply two complement's notation.
      if Item < 0 then
         X := Long_Long_Unsigned'Last xor Long_Long_Unsigned (-(Item + 1));
      else
         X := Long_Long_Unsigned (Item);
      end if;

      --  Compute using machine unsigned
      --  rather than long_long_unsigned.

      for N in reverse S'Range loop

         --  We have filled an unsigned.
         if (LLU_L - N) mod UB = 0 then
            U := Unsigned (X and UL);
            X := Shift_Right (X, US);
         end if;

         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_LLI;

   -----------
   -- W_SSU --
   -----------

   procedure W_SSU (Stream : access RST; Item : in Short_Short_Unsigned) is
      S : XDR_S_SSU;
      U : XDR_SSU := XDR_SSU (Item);

   begin
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_SSU;

   ----------
   -- W_SU --
   ----------

   procedure W_SU (Stream : access RST; Item : in Short_Unsigned) is
      S : XDR_S_SU;
      U : XDR_SU := XDR_SU (Item);

   begin
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_SU;

   ---------
   -- W_U --
   ---------

   procedure W_U (Stream : access RST; Item : in Unsigned) is
      S : XDR_S_U;
      U : XDR_U := XDR_U (Item);

   begin
      for N in reverse S'Range loop
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_U;

   ----------
   -- W_LU --
   ----------

   procedure W_LU (Stream : access RST; Item : in Long_Unsigned) is
      S : XDR_S_LU;
      U : Unsigned;
      X : Long_Unsigned := Item;

   begin

      --  Compute using machine unsigned
      --  rather than long_unsigned.

      for N in reverse S'Range loop

         --  We have filled an unsigned.
         if (LU_L - N) mod UB = 0 then
            U := Unsigned (X and UL);
            X := Shift_Right (X, US);
         end if;
         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_LU;

   -----------
   -- W_LLU --
   -----------

   procedure W_LLU (Stream : access RST; Item : in Long_Long_Unsigned) is
      S : XDR_S_LLU;
      U : Unsigned;
      X : Long_Long_Unsigned := Item;

   begin

      --  Compute using machine unsigned
      --  rather than long_long_unsigned.

      for N in reverse S'Range loop

         --  We have filled an unsigned.
         if (LLU_L - N) mod UB = 0 then
            U := Unsigned (X and UL);
            X := Shift_Right (X, US);
         end if;

         S (N) := SE (U mod BB);
         U := U / BB;
      end loop;

      Ada.Streams.Write (Stream.all, S);

      if U /= 0 then
         raise Err;
      end if;
   end W_LLU;

end System.Stream_Attributes;


