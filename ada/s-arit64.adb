------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . A R I T H _ 6 4                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------



with Interfaces; use Interfaces;
with Unchecked_Conversion;

package body System.Arith_64 is

   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);

   subtype Uns64 is Unsigned_64;
   function To_Uns is new Unchecked_Conversion (Int64, Uns64);
   function To_Int is new Unchecked_Conversion (Uns64, Int64);

   subtype Uns32 is Unsigned_32;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function "+" (A, B : Uns32) return Uns64;
   function "+" (A : Uns64; B : Uns32) return Uns64;
   pragma Inline ("+");
   --  Length doubling additions

   function "-" (A : Uns64; B : Uns32) return Uns64;
   pragma Inline ("-");
   --  Length doubling subtraction

   function "*" (A, B : Uns32) return Uns64;
   function "*" (A : Uns64; B : Uns32) return Uns64;
   pragma Inline ("*");
   --  Length doubling multiplications

   function "/" (A : Uns64; B : Uns32) return Uns64;
   pragma Inline ("/");
   --  Length doubling division

   function "rem" (A : Uns64; B : Uns32) return Uns64;
   pragma Inline ("rem");
   --  Length doubling remainder

   function "&" (Hi, Lo : Uns32) return Uns64;
   pragma Inline ("&");
   --  Concatenate hi, lo values to form 64-bit result

   function Lo (A : Uns64) return Uns32;
   pragma Inline (Lo);
   --  Low order half of 64-bit value

   function Hi (A : Uns64) return Uns32;
   pragma Inline (Hi);
   --  High order half of 64 bit value

   function To_Int_With_Ovflo_Check (A : Uns64) return Int64;
   --  Convert argument to signed integer, with check for overflow

   ---------
   -- "+" --
   ---------

   function "+" (A, B : Uns32) return Uns64 is
   begin
      return Uns64 (A) + Uns64 (B);
   end "+";

   function "+" (A : Uns64; B : Uns32) return Uns64 is
   begin
      return A + Uns64 (B);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A : Uns64; B : Uns32) return Uns64 is
   begin
      return A - Uns64 (B);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (A, B : Uns32) return Uns64 is
   begin
      return Uns64 (A) * Uns64 (B);
   end "*";

   function "*" (A : Uns64; B : Uns32) return Uns64 is
   begin
      return A * Uns64 (B);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (A : Uns64; B : Uns32) return Uns64 is
   begin
      return A / Uns64 (B);
   end "/";

   -----------
   -- "rem" --
   -----------

   function "rem" (A : Uns64; B : Uns32) return Uns64 is
   begin
      return A rem Uns64 (B);
   end "rem";

   ---------
   -- "&" --
   ---------

   function "&" (Hi, Lo : Uns32) return Uns64 is
   begin
      return Shift_Left (Uns64 (Hi), 32) or Uns64 (Lo);
   end "&";

   --------
   -- Hi --
   --------

   function Hi (A : Uns64) return Uns32 is
   begin
      return Uns32 (Shift_Right (A, 32));
   end Hi;

   --------
   -- Lo --
   --------

   function Lo (A : Uns64) return Uns32 is
   begin
      return Uns32 (A and 16#FFFF_FFFF#);
   end Lo;

   --------------------------
   -- Add_With_Ovflo_Check --
   --------------------------

   function Add_With_Ovflo_Check (X, Y : Int64) return Int64 is
      R : constant Int64 := To_Int (To_Uns (X) + To_Uns (Y));

   begin
      if X >= 0 then
         if Y < 0 or else R >= 0 then
            return R;
         end if;

      else -- X < 0
         if Y > 0 or else R < 0 then
            return R;
         end if;
      end if;

      raise Constraint_Error;
   end Add_With_Ovflo_Check;

   -------------------
   -- Double_Divide --
   -------------------

   procedure Double_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean)
   is
      Xu  : constant Uns64 := To_Uns (abs X);
      Xhi : constant Uns32 := Hi (Xu);
      Xlo : constant Uns32 := Lo (Xu);

      Yu  : constant Uns64 := To_Uns (abs Y);
      Yhi : constant Uns32 := Hi (Yu);
      Ylo : constant Uns32 := Lo (Yu);

      Zu  : constant Uns64 := To_Uns (abs Z);
      Zhi : constant Uns32 := Hi (Zu);
      Zlo : constant Uns32 := Lo (Zu);

      T1, T2     : Uns64;
      Du, Qu, Ru : Uns64;
      Den_Pos    : Boolean;

   begin
      if Yu = 0 or else Zu = 0 then
         raise Constraint_Error;
      end if;

      --  Compute Y * Z. Note that if the result overflows 64 bits unsigned,
      --  then the rounded result is clearly zero (since the dividend is at
      --  most 2**63 - 1, the extra bit of precision is nice here!)

      if Yhi /= 0 then
         if Zhi /= 0 then
            Q := 0;
            R := X;
            return;
         else
            T2 := Yhi * Zlo;
         end if;

      else
         if Zhi /= 0 then
            T2 := Ylo * Zhi;
         else
            T2 := 0;
         end if;
      end if;

      T1 := Ylo * Zlo;
      T2 := T2 + Hi (T1);

      if Hi (T2) /= 0 then
         Q := 0;
         R := X;
         return;
      end if;

      Du := Lo (T2) & Lo (T1);
      Qu := Xu / Du;
      Ru := Xu rem Du;

      --  Deal with rounding case

      if Round and then Ru > (Du - Uns64'(1)) / Uns64'(2) then
         Qu := Qu + Uns64'(1);
      end if;

      --  Set final signs (RM 4.5.5(27-30))

      Den_Pos := (Y < 0) = (Z < 0);

      --  Case of dividend (X) sign positive

      if X >= 0 then
         R := To_Int (Ru);

         if Den_Pos then
            Q := To_Int (Qu);
         else
            Q := -To_Int (Qu);
         end if;

      --  Case of dividend (X) sign negative

      else
         R := -To_Int (Ru);

         if Den_Pos then
            Q := -To_Int (Qu);
         else
            Q := To_Int (Qu);
         end if;
      end if;
   end Double_Divide;

   -------------------------------
   -- Multiply_With_Ovflo_Check --
   -------------------------------

   function Multiply_With_Ovflo_Check (X, Y : Int64) return Int64 is
      Xu  : constant Uns64 := To_Uns (abs X);
      Xhi : constant Uns32 := Hi (Xu);
      Xlo : constant Uns32 := Lo (Xu);

      Yu  : constant Uns64 := To_Uns (abs Y);
      Yhi : constant Uns32 := Hi (Yu);
      Ylo : constant Uns32 := Lo (Yu);

      T1, T2 : Uns64;

   begin
      if Xhi /= 0 then
         if Yhi /= 0 then
            raise Constraint_Error;
         else
            T2 := Xhi * Ylo;
         end if;

      else
         if Yhi /= 0 then
            T2 := Xlo * Yhi;
         else
            return X * Y;
         end if;
      end if;

      T1 := Xlo * Ylo;
      T2 := T2 + Hi (T1);

      if Hi (T2) /= 0 then
         raise Constraint_Error;
      end if;

      T2 := Lo (T2) & Lo (T1);

      if X >= 0 then
         if Y >= 0 then
            return To_Int_With_Ovflo_Check (T2);
         else
            return -To_Int_With_Ovflo_Check (T2);
         end if;
      else -- X < 0
         if Y < 0 then
            return To_Int_With_Ovflo_Check (T2);
         else
            return -To_Int_With_Ovflo_Check (T2);
         end if;
      end if;

   end Multiply_With_Ovflo_Check;

   -------------------
   -- Scaled_Divide --
   -------------------

   procedure Scaled_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean)
   is
      Xu  : constant Uns64 := To_Uns (abs X);
      Xhi : constant Uns32 := Hi (Xu);
      Xlo : constant Uns32 := Lo (Xu);

      Yu  : constant Uns64 := To_Uns (abs Y);
      Yhi : constant Uns32 := Hi (Yu);
      Ylo : constant Uns32 := Lo (Yu);

      Zu  : Uns64 := To_Uns (abs Z);
      Zhi : Uns32 := Hi (Zu);
      Zlo : Uns32 := Lo (Zu);

      D1, D2, D3, D4 : Uns32;
      --  The dividend, four digits (D1 is high order)

      Q1, Q2 : Uns32;
      --  The quotient, two digits (Q1 is high order)

      S1, S2, S3 : Uns32;
      --  Value to subtract, three digits (S1 is high order)

      Qu : Uns64;
      Ru : Uns64;
      --  Unsigned quotient and remainder

      Scale : Natural;
      --  Scaling factor used for multiple-precision divide. Dividend and
      --  Divisor are multiplied by 2 ** Scale, and the final remainder
      --  is divided by the scaling factor. The reason for this scaling
      --  is to allow more accurate estimation of quotient digits.

      T1, T2, T3 : Uns64;
      --  Temporary values

   begin
      --  First do the multiplication, giving the four digit dividend

      T1 := Xlo * Ylo;
      D4 := Lo (T1);
      D3 := Hi (T1);

      if Yhi /= 0 then
         T1 := Xlo * Yhi;
         T2 := D3 + Lo (T1);
         D3 := Lo (T2);
         D2 := Hi (T1) + Hi (T2);

         if Xhi /= 0 then
            T1 := Xhi * Ylo;
            T2 := D3 + Lo (T1);
            D3 := Lo (T2);
            T3 := D2 + Hi (T1);
            T3 := T3 + Hi (T2);
            D2 := Lo (T3);
            D1 := Hi (T3);

            T1 := (D1 & D2) + Uns64'(Xhi * Yhi);
            D1 := Hi (T1);
            D2 := Lo (T1);

         else
            D1 := 0;
         end if;

      else
         if Xhi /= 0 then
            T1 := Xhi * Ylo;
            T2 := D3 + Lo (T1);
            D3 := Lo (T2);
            D2 := Hi (T1) + Hi (T2);

         else
            D2 := 0;
         end if;

         D1 := 0;
      end if;

      --  Now it is time for the dreaded multiple precision division. First
      --  an easy case, check for the simple case of a one digit divisor.

      if Zhi = 0 then
         if D1 /= 0 or else D2 >= Zlo then
            raise Constraint_Error;

         --  Here we are dividing at most three digits by one digit

         else
            T1 := D2 & D3;
            T2 := Lo (T1 rem Zlo) & D4;

            Qu := Lo (T1 / Zlo) & Lo (T2 / Zlo);
            Ru := T2 rem Zlo;
         end if;

      --  If divisor is double digit and too large, raise error

      elsif (D1 & D2) >= Zu then
         raise Constraint_Error;

      --  This is the complex case where we definitely have a double digit
      --  divisor and a dividend of at least three digits. We use the classical
      --  multiple division algorithm (see  section (4.3.1) of Knuth's "The Art
      --  of Computer Programming", Vol. 2 for a description (algorithm D).

      else
         --  First normalize the divisor so that it has the leading bit on.
         --  We do this by finding the appropriate left shift amount.

         Scale := 0;

         if (Zhi and 16#FFFF0000#) = 0 then
            Scale := 16;
            Zu := Shift_Left (Zu, 16);
         end if;

         if (Hi (Zu) and 16#FF00_0000#) = 0 then
            Scale := Scale + 8;
            Zu := Shift_Left (Zu, 8);
         end if;

         if (Hi (Zu) and 16#F000_0000#) = 0 then
            Scale := Scale + 4;
            Zu := Shift_Left (Zu, 4);
         end if;

         if (Hi (Zu) and 16#C000_0000#) = 0 then
            Scale := Scale + 2;
            Zu := Shift_Left (Zu, 2);
         end if;

         if (Hi (Zu) and 16#8000_0000#) = 0 then
            Scale := Scale + 1;
            Zu := Shift_Left (Zu, 1);
         end if;

         Zhi := Hi (Zu);
         Zlo := Lo (Zu);

         --  Note that when we scale up the dividend, it still fits in four
         --  digits, since we already tested for overflow, and scaling does
         --  not change the invariant that (D1 & D2) >= Zu.

         T1 := Shift_Left (D1 & D2, Scale);
         D1 := Hi (T1);
         T2 := Shift_Left (0 & D3, Scale);
         D2 := Lo (T1) or Hi (T2);
         T3 := Shift_Left (0 & D4, Scale);
         D3 := Lo (T2) or Hi (T3);
         D4 := Lo (T3);

         --  Compute first quotient digit. We have to divide three digits by
         --  two digits, and we estimate the quotient by dividing the leading
         --  two digits by the leading digit. Given the scaling we did above
         --  which ensured the first bit of the divisor is set, this gives an
         --  estimate of the quotient that is at most two too high.

         if D1 = Zhi then
            Q1 := 2 ** 32 - 1;
         else
            Q1 := Lo ((D1 & D2) / Zhi);
         end if;

         --  Compute amount to subtract

         T1 := Q1 * Zlo;
         T2 := Q1 * Zhi;
         S3 := Lo (T1);
         T1 := Hi (T1) + Lo (T2);
         S2 := Lo (T1);
         S1 := Hi (T1) + Hi (T2);

         --  Adjust quotient digit if it was too high

         loop
            exit when S1 < D1;

            if S1 = D1 then
               exit when S2 < D2;

               if S2 = D2 then
                  exit when S3 <= D3;
               end if;
            end if;

            Q1 := Q1 - 1;

            T1 := (S2 & S3) - Zlo;
            S3 := Lo (T1);
            T1 := (S1 & S2) - Zhi;
            S2 := Lo (T1);
            S1 := Hi (T1);
         end loop;

         --  Subtract from dividend (note: do not bother to set D1 to
         --  zero, since it is no longer needed in the calculation).

         T1 := (D2 & D3) - S3;
         D3 := Lo (T1);
         T1 := (D1 & Hi (T1)) - S2;
         D2 := Lo (T1);

         --  Compute second quotient digit in same manner

         if D2 = Zhi then
            Q2 := 2 ** 32 - 1;
         else
            Q2 := Lo ((D2 & D3) / Zhi);
         end if;

         T1 := Q2 * Zlo;
         T2 := Q2 * Zhi;
         S3 := Lo (T1);
         T1 := Hi (T1) + Lo (T2);
         S2 := Lo (T1);
         S1 := Hi (T1) + Hi (T2);

         loop
            exit when S1 < D2;

            if S1 = D2 then
               exit when S2 < D3;

               if S2 = D3 then
                  exit when S3 <= D4;
               end if;
            end if;

            Q2 := Q2 - 1;

            T1 := (S2 & S3) - Zlo;
            S3 := Lo (T1);
            T1 := (S1 & S2) - Zhi;
            S2 := Lo (T1);
            S1 := Hi (T1);
         end loop;

         T1 := (D3 & D4) - S3;
         D4 := Lo (T1);
         T1 := (D2 & Hi (T1)) - S2;
         D3 := Lo (T1);

         --  The two quotient digits are now set, and the remainder of the
         --  scaled division is in (D3 & D4). To get the remainder for the
         --  original unscaled division, we rescale this dividend.

         Qu := Q1 & Q2;
         Ru := Shift_Right (D3 & D4, Scale);
      end if;

      --  Deal with rounding case

      if Round and then Ru > (Zu - Uns64'(1)) / Uns64'(2) then
         Qu := Qu + Uns64 (1);
      end if;

      --  Set final signs (RM 4.5.5(27-30))

      --  Case of dividend (X * Y) sign positive

      if (X >= 0 and then Y >= 0)
        or else (X < 0 and then Y < 0)
      then
         R := To_Int_With_Ovflo_Check (Ru);

         if Z > 0 then
            Q := To_Int_With_Ovflo_Check (Qu);
         else
            Q := -To_Int_With_Ovflo_Check (Qu);
         end if;

      --  Case of dividend (X * Y) sign negative

      else
         R := -To_Int_With_Ovflo_Check (Ru);

         if Z > 0 then
            Q := -To_Int_With_Ovflo_Check (Qu);
         else
            Q := To_Int_With_Ovflo_Check (Qu);
         end if;
      end if;

   end Scaled_Divide;

   -------------------------------
   -- Subtract_With_Ovflo_Check --
   -------------------------------

   function Subtract_With_Ovflo_Check (X, Y : Int64) return Int64 is
      R : constant Int64 := To_Int (To_Uns (X) - To_Uns (Y));

   begin
      if X >= 0 then
         if Y > 0 or else R >= 0 then
            return R;
         end if;

      else -- X < 0
         if Y <= 0 or else R < 0 then
            return R;
         end if;
      end if;

      raise Constraint_Error;
   end Subtract_With_Ovflo_Check;

   -----------------------------
   -- To_Int_With_Ovflo_Check --
   -----------------------------

   function To_Int_With_Ovflo_Check (A : Uns64) return Int64 is
      R : constant Int64 := To_Int (A);

   begin
      if R >= 0 then
         return R;
      else
         raise Constraint_Error;
      end if;
   end To_Int_With_Ovflo_Check;


end System.Arith_64;
