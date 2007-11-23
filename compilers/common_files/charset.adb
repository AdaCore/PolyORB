------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              C H A R S E T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Charset is

   type Translate_Table is array (Character) of Character;
   --  Type used to describe translate tables

   Fold_Lower : constant Translate_Table :=
     (
      'A'                      => LC_A,
      'B'                      => LC_B,
      'C'                      => LC_C,
      'D'                      => LC_D,
      'E'                      => LC_E,
      'F'                      => LC_F,
      'G'                      => LC_G,
      'H'                      => LC_H,
      'I'                      => LC_I,
      'J'                      => LC_J,
      'K'                      => LC_K,
      'L'                      => LC_L,
      'M'                      => LC_M,
      'N'                      => LC_N,
      'O'                      => LC_O,
      'P'                      => LC_P,
      'Q'                      => LC_Q,
      'R'                      => LC_R,
      'S'                      => LC_S,
      'T'                      => LC_T,
      'U'                      => LC_U,
      'V'                      => LC_V,
      'W'                      => LC_W,
      'X'                      => LC_X,
      'Y'                      => LC_Y,
      'Z'                      => LC_Z,
      LC_A                     => LC_A,
      LC_B                     => LC_B,
      LC_C                     => LC_C,
      LC_D                     => LC_D,
      LC_E                     => LC_E,
      LC_F                     => LC_F,
      LC_G                     => LC_G,
      LC_H                     => LC_H,
      LC_I                     => LC_I,
      LC_J                     => LC_J,
      LC_K                     => LC_K,
      LC_L                     => LC_L,
      LC_M                     => LC_M,
      LC_N                     => LC_N,
      LC_O                     => LC_O,
      LC_P                     => LC_P,
      LC_Q                     => LC_Q,
      LC_R                     => LC_R,
      LC_S                     => LC_S,
      LC_T                     => LC_T,
      LC_U                     => LC_U,
      LC_V                     => LC_V,
      LC_W                     => LC_W,
      LC_X                     => LC_X,
      LC_Y                     => LC_Y,
      LC_Z                     => LC_Z,
      UC_A_Grave               => LC_A_Grave,
      UC_A_Acute               => LC_A_Acute,
      UC_A_Circumflex          => LC_A_Circumflex,
      UC_A_Tilde               => LC_A_Tilde,
      UC_A_Diaeresis           => LC_A_Diaeresis,
      UC_A_Ring                => LC_A_Ring,
      UC_AE_Diphthong          => LC_AE_Diphthong,
      UC_C_Cedilla             => LC_C_Cedilla,
      UC_E_Grave               => LC_E_Grave,
      UC_E_Acute               => LC_E_Acute,
      UC_E_Circumflex          => LC_E_Circumflex,
      UC_E_Diaeresis           => LC_E_Diaeresis,
      UC_I_Grave               => LC_I_Grave,
      UC_I_Acute               => LC_I_Acute,
      UC_I_Circumflex          => LC_I_Circumflex,
      UC_I_Diaeresis           => LC_I_Diaeresis,
      UC_Icelandic_Eth         => LC_Icelandic_Eth,
      UC_N_Tilde               => LC_N_Tilde,
      UC_O_Grave               => LC_O_Grave,
      UC_O_Acute               => LC_O_Acute,
      UC_O_Circumflex          => LC_O_Circumflex,
      UC_O_Tilde               => LC_O_Tilde,
      UC_O_Diaeresis           => LC_O_Diaeresis,
      UC_O_Oblique_Stroke      => LC_O_Oblique_Stroke,
      UC_U_Grave               => LC_U_Grave,
      UC_U_Acute               => LC_U_Acute,
      UC_U_Circumflex          => LC_U_Circumflex,
      UC_U_Diaeresis           => LC_U_Diaeresis,
      UC_Y_Acute               => LC_Y_Acute,
      UC_Icelandic_Thorn       => LC_Icelandic_Thorn,
      LC_German_Sharp_S        => LC_German_Sharp_S,
      LC_A_Grave               => LC_A_Grave,
      LC_A_Acute               => LC_A_Acute,
      LC_A_Circumflex          => LC_A_Circumflex,
      LC_A_Tilde               => LC_A_Tilde,
      LC_A_Diaeresis           => LC_A_Diaeresis,
      LC_A_Ring                => LC_A_Ring,
      LC_AE_Diphthong          => LC_AE_Diphthong,
      LC_C_Cedilla             => LC_C_Cedilla,
      LC_E_Grave               => LC_E_Grave,
      LC_E_Acute               => LC_E_Acute,
      LC_E_Circumflex          => LC_E_Circumflex,
      LC_E_Diaeresis           => LC_E_Diaeresis,
      LC_I_Grave               => LC_I_Grave,
      LC_I_Acute               => LC_I_Acute,
      LC_I_Circumflex          => LC_I_Circumflex,
      LC_I_Diaeresis           => LC_I_Diaeresis,
      LC_Icelandic_Eth         => LC_Icelandic_Eth,
      LC_N_Tilde               => LC_N_Tilde,
      LC_O_Grave               => LC_O_Grave,
      LC_O_Acute               => LC_O_Acute,
      LC_O_Circumflex          => LC_O_Circumflex,
      LC_O_Tilde               => LC_O_Tilde,
      LC_O_Diaeresis           => LC_O_Diaeresis,
      LC_O_Oblique_Stroke      => LC_O_Oblique_Stroke,
      LC_U_Grave               => LC_U_Grave,
      LC_U_Acute               => LC_U_Acute,
      LC_U_Circumflex          => LC_U_Circumflex,
      LC_U_Diaeresis           => LC_U_Diaeresis,
      LC_Y_Acute               => LC_Y_Acute,
      LC_Icelandic_Thorn       => LC_Icelandic_Thorn,
      LC_Y_Diaeresis           => LC_Y_Diaeresis,
      others                   => ' ');

   -----------------------------
   -- Is_Alphabetic_Character --
   -----------------------------

   function Is_Alphabetic_Character (C : Character) return Boolean is
   begin
      return Fold_Lower (C) /= ' ';
   end Is_Alphabetic_Character;

   -----------------------------
   -- Is_Identifier_Character --
   -----------------------------

   function Is_Identifier_Character (C : Character) return Boolean is
   begin
      return C = '_'
        or else C in '0' .. '9'
        or else Fold_Lower (C) /= ' ';
   end Is_Identifier_Character;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in out String) is
   begin
      for I in S'Range loop
         if Fold_Lower (S (I)) /= ' ' then
            S (I) := Fold_Lower (S (I));
         end if;
      end loop;
   end To_Lower;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (C : Character) return Character is
   begin
      if Fold_Lower (C) /= ' ' then
         return Fold_Lower (C);
      else
         return C;
      end if;
   end To_Lower;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (S : String) return String is
      LS : String := S;
   begin
      To_Lower (LS);
      return LS;
   end To_Lower;

end Charset;
