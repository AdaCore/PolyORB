------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ C N V                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

--  This package contains generic subprograms used for converting between
--  sequences of Character and Wide_Character. All access to wide character
--  sequences is isolated in this unit.

with System.WCh_Con; use System.WCh_Con;
with System.WCh_JIS; use System.WCh_JIS;

package body System.WCh_Cnv is

   --------------------------------
   -- Char_Sequence_To_Wide_Char --
   --------------------------------

   function Char_Sequence_To_Wide_Char
     (C    : Character;
      EM   : WC_Encoding_Method)
      return Wide_Character
   is
      B1 : Integer;
      C1 : Character;

      procedure Get_Hex (N : Character);
      --  If N is a hex character, then set B1 to 16 * B1 + character N.
      --  Raise Constraint_Error if character N is not a hex character.

      -------------
      -- Get_Hex --
      -------------

      procedure Get_Hex (N : Character) is
         B2 : constant Integer := Character'Pos (N);

      begin
         if B2 in Character'Pos ('0') .. Character'Pos ('9') then
            B1 := B1 * 16 + B2 - Character'Pos ('0');

         elsif B2 in Character'Pos ('A') .. Character'Pos ('F') then
            B1 := B1 * 16 + B2 - (Character'Pos ('A') - 10);

         elsif B2 in Character'Pos ('a') .. Character'Pos ('f') then
            B1 := B1 * 16 + B2 - (Character'Pos ('a') - 10);

         else
            raise Constraint_Error;
         end if;
      end Get_Hex;

   --  Start of processing for Char_Sequence_To_Wide_Char

   begin
      case EM is

         when WCEM_None | WCEM_Brackets =>

            if C /= '[' then
               return Wide_Character'Val (Character'Pos (C));
            end if;

            if In_Char /= '"' then
               raise Constraint_Error;
            end if;

            B1 := 0;
            Get_Hex (In_Char);
            Get_Hex (In_Char);
            C1 := In_Char;

            if C1 /= '"' then
               Get_Hex (C1);
               Get_Hex (In_Char);
               C1 := In_Char;

               if C1 /= '"' then
                  raise Constraint_Error;
               end if;
            end if;

            if In_Char /= ']' then
               raise Constraint_Error;
            end if;

            return Wide_Character'Val (B1);

         when WCEM_Hex =>
            if C /= Ascii.ESC then
               return Wide_Character'Val (Character'Pos (C));

            else
               B1 := 0;
               Get_Hex (In_Char);
               Get_Hex (In_Char);
               Get_Hex (In_Char);
               Get_Hex (In_Char);

               return Wide_Character'Val (B1);
            end if;

         when WCEM_Upper =>
            if C > Ascii.DEL then
               return
                 Wide_Character'Val
                   (Integer (256 * Character'Pos (C)) +
                    Character'Pos (In_Char));
            else
               return Wide_Character'Val (Character'Pos (C));
            end if;

         when WCEM_Shift_JIS =>
            if C > Ascii.DEL then
               return Shift_JIS_To_JIS (C, In_Char);
            else
               return Wide_Character'Val (Character'Pos (C));
            end if;

         when WCEM_EUC =>
            if C > Ascii.DEL then
               return EUC_To_JIS (C, In_Char);
            else
               return Wide_Character'Val (Character'Pos (C));
            end if;

      end case;
   end Char_Sequence_To_Wide_Char;

   --------------------------------
   -- Wide_Char_To_Char_Sequence --
   --------------------------------

   procedure Wide_Char_To_Char_Sequence
     (WC : Wide_Character;
      EM : WC_Encoding_Method)
   is
      Val    : constant Natural := Wide_Character'Pos (WC);
      Hexc   : constant array (0 .. 15) of Character := "0123456789ABCDEF";
      C1, C2 : Character;

   begin
      case EM is

         when WCEM_None =>

            if Val < 256 then
               Out_Char (Character'Val (Val));

            else
               Out_Char ('[');
               Out_Char ('"');
               Out_Char (Hexc (Val / (16**3)));
               Out_Char (Hexc ((Val / (16**2)) mod 16));
               Out_Char (Hexc ((Val / 16) mod 16));
               Out_Char (Hexc (Val mod 16));
               Out_Char ('"');
               Out_Char (']');
            end if;

         when WCEM_Hex =>
            if Val < 256 then
               Out_Char (Character'Val (Val));

            else
               Out_Char (Ascii.ESC);
               Out_Char (Hexc (Val / (16**3)));
               Out_Char (Hexc ((Val / (16**2)) mod 16));
               Out_Char (Hexc ((Val / 16) mod 16));
               Out_Char (Hexc (Val mod 16));
            end if;

         when WCEM_Upper =>
            if Val < 128 then
               Out_Char (Character'Val (Val));

            elsif Val < 16#8000# then
               raise Constraint_Error;

            else
               Out_Char (Character'Val (Val / 256));
               Out_Char (Character'Val (Val mod 256));
            end if;

         when WCEM_Shift_JIS =>
            if Val < 128 then
               Out_Char (Character'Val (Val));
            else
               JIS_To_Shift_JIS (WC, C1, C2);
               Out_Char (C1);
               Out_Char (C2);
            end if;

         when WCEM_EUC =>
            if Val < 128 then
               Out_Char (Character'Val (Val));
            else
               JIS_To_EUC (WC, C1, C2);
               Out_Char (C1);
               Out_Char (C2);
            end if;

         when WCEM_Brackets =>

            if Val < 128 then
               Out_Char (Character'Val (Val));

            else
               Out_Char ('[');
               Out_Char ('"');

               if Val >= 256 then
                  Out_Char (Hexc (Val / (16**3)));
                  Out_Char (Hexc ((Val / (16**2)) mod 16));
               end if;

               Out_Char (Hexc ((Val / 16) mod 16));
               Out_Char (Hexc (Val mod 16));
               Out_Char ('"');
               Out_Char (']');
            end if;

      end case;
   end Wide_Char_To_Char_Sequence;

end System.WCh_Cnv;
