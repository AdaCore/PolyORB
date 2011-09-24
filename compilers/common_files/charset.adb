------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              C H A R S E T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2011, Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Charset is

   -----------------------------
   -- Is_Alphabetic_Character --
   -----------------------------

   function Is_Alphabetic_Character (C : Character) return Boolean is
   begin
      return Is_Letter (C);
   end Is_Alphabetic_Character;

   -----------------------------
   -- Is_Identifier_Character --
   -----------------------------

   function Is_Identifier_Character (C : Character) return Boolean is
   begin
      return C = '_' or else Is_Alphanumeric (C);
   end Is_Identifier_Character;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (S : in out String) is
   begin
      S := Ada.Characters.Handling.To_Lower (S);
   end To_Lower;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (C : Character) return Character is
   begin
      return Ada.Characters.Handling.To_Lower (C);
   end To_Lower;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (S : String) return String is
   begin
      return Ada.Characters.Handling.To_Lower (S);
   end To_Lower;

end Charset;
