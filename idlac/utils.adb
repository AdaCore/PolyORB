------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body Utils is

   ---------
   -- Img --
   ---------

   function Img (N : Character) return String is
      S : String (1 .. 1);
   begin
      S (1) := N;
      return S;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Long_Long_Integer) return String is
      S : constant String := Long_Long_Integer'Image (N);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Long_Integer) return String is
   begin
      return Img (Long_Long_Integer (N));
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Integer) return String is
   begin
      return Img (Long_Long_Integer (N));
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Long_Long_Float) return String is
   begin
      return Long_Long_Float'Image (N);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Node_Id) return String is
   begin
      if N = No_Node then
         return "No_Node";
      else
         return Img (Natural (N));
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Node_Kind) return String is
   begin
      return Node_Kind'Image (N);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (B : Boolean) return String is
   begin
      if B then
         return "True";
      else
         return "False";
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (A : System.Address) return String is

      type U_32 is mod 2 ** 32;
      for U_32'Size use 32;

      function To_U_32 is
         new Ada.Unchecked_Conversion (System.Address, U_32);

      Hex_Digits : constant array
        (U_32 range 0 .. 15) of Character
        := "0123456789abcdef";

      Integer_Address : U_32
        := To_U_32 (A);

      Result : String (1 .. 8);
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits
           (Integer_Address mod 16);
         Integer_Address := Integer_Address / 16;
      end loop;
      return Result;
   end Img;

end Utils;
