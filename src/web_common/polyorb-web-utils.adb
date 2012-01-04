------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . W E B . U T I L S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Numerics.Discrete_Random;

package body PolyORB.Web.Utils is

   use Ada;

   package Integer_Random is new Ada.Numerics.Discrete_Random (Random_Integer);

   Random_Generator : Integer_Random.Generator;

   -------------------
   -- CRLF_2_Spaces --
   -------------------

   function CRLF_2_Spaces (Str : String) return String is
   begin
      return Strings.Fixed.Trim
        (Strings.Fixed.Translate
           (Str, Strings.Maps.To_Mapping
              (From => ASCII.CR & ASCII.LF, To   => "  ")),
         Strings.Right);
   end CRLF_2_Spaces;

   ---------
   -- Hex --
   ---------

   function Hex (V : Natural; Width : Natural := 0) return String is
      use Strings;

      Hex_V : String (1 .. Integer'Size / 4 + 4);
   begin
      Ada.Integer_Text_IO.Put (Hex_V, V, 16);

      declare
         Result : constant String
           := Hex_V (Fixed.Index (Hex_V, "#") + 1
                       .. Fixed.Index (Hex_V, "#", Backward) - 1);
      begin
         if Width = 0 then
            return Result;

         elsif Result'Length < Width then
            declare
               use Ada.Strings.Fixed;
               Zero : constant String := (Width - Result'Length) * '0';
            begin
               return Zero & Result;
            end;

         else
            return Result (Result'Last - Width + 1 .. Result'Last);
         end if;
      end;
   end Hex;

   ---------------
   -- Hex_Value --
   ---------------

   function Hex_Value (Hex : String) return Natural is

      function Value (C : Character) return Natural;
      pragma Inline (Value);
      --  Return value for single character C.

      function Value (C : Character) return Natural is
      begin
         case C is
            when '0'       => return 0;
            when '1'       => return 1;
            when '2'       => return 2;
            when '3'       => return 3;
            when '4'       => return 4;
            when '5'       => return 5;
            when '6'       => return 6;
            when '7'       => return 7;
            when '8'       => return 8;
            when '9'       => return 9;
            when 'a' | 'A' => return 10;
            when 'b' | 'B' => return 11;
            when 'c' | 'C' => return 12;
            when 'd' | 'D' => return 13;
            when 'e' | 'E' => return 14;
            when 'f' | 'F' => return 15;
            when others    => raise Constraint_Error;
         end case;
      end Value;

      R   : Natural := 0;
      Exp : Natural := 1;

   begin
      for K in reverse Hex'Range loop
         R := R + Exp * Value (Hex (K));
         Exp := Exp * 16;
      end loop;

      return R;
   end Hex_Value;

   -----------
   -- Image --
   -----------

   function Image (N : Natural) return String is
      N_Img : constant String := Natural'Image (N);
   begin
      return N_Img (N_Img'First + 1 .. N_Img'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (D : Duration) return String is
      D_Img : constant String  := Duration'Image (D);
      K     : constant Natural := Strings.Fixed.Index (D_Img, ".");
   begin
      if K = 0 then
         return D_Img (D_Img'First + 1 .. D_Img'Last);
      else
         return D_Img (D_Img'First + 1 .. K + 2);
      end if;
   end Image;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (S : String) return Boolean is
      use Strings.Maps;
   begin
      return S'Length > 0
        and then Is_Subset (To_Set (S), Constants.Decimal_Digit_Set);
   end Is_Number;

   -----------
   -- Quote --
   -----------

   function Quote (Str : String) return String is
   begin
      return '"' & Str & '"';
   end Quote;

   ------------
   -- Random --
   ------------

   function Random return Random_Integer is
   begin
      return Integer_Random.Random (Random_Generator);
   end Random;

   ------------------
   -- RW_Semaphore --
   ------------------

   protected body RW_Semaphore is

      ----------
      -- Read --
      ----------

      entry Read when W = 0 and then Write'Count = 0 is
      begin
         R := R + 1;
      end Read;

      ------------------
      -- Release_Read --
      ------------------

      procedure Release_Read is
      begin
         R := R - 1;
      end Release_Read;

      -------------------
      -- Release_Write --
      -------------------

      procedure Release_Write is
      begin
         W := W - 1;
      end Release_Write;

      -----------
      -- Write --
      -----------

      entry Write when R = 0 and then W < Writers is
      begin
         W := W + 1;
      end Write;

   end RW_Semaphore;

   ---------------
   -- Semaphore --
   ---------------

   protected body Semaphore is

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Seized := False;
      end Release;

      -----------
      -- Seize --
      -----------

      entry Seize when not Seized is
      begin
         Seized := True;
      end Seize;

   end Semaphore;

begin
   Integer_Random.Reset (Random_Generator);
end PolyORB.Web.Utils;
