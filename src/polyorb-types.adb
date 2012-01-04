------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O L Y O R B . T Y P E S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Base data types for the whole middleware.

package body PolyORB.Types is

   -----------------------------
   -- Trimmed_Image functions --
   -----------------------------

   function Trimmed_Image (X : Long_Long) return Standard.String is
      R : constant Standard.String := Long_Long'Image (X);
   begin
      if X >= 0 then
         declare
            subtype Slide is Standard.String (1 .. R'Length - 1);
            --  It seems slightly beneficial to make sure the result has
            --  'First = 1.
         begin
            return Slide (R (R'First + 1 .. R'Last));
         end;
      else
         return R;
      end if;
   end Trimmed_Image;

   function Trimmed_Image (X : Unsigned_Long_Long) return Standard.String is
      R : constant Standard.String := Unsigned_Long_Long'Image (X);
      subtype Slide is Standard.String (1 .. R'Length - 1);
      --  It seems slightly beneficial to make sure the result has 'First = 1.
   begin
      return Slide (R (R'First + 1 .. R'Last));
   end Trimmed_Image;

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   function To_PolyORB_String
     (Source : Standard.String)
     return Types.String is
   begin
      return Types.String
        (Ada.Strings.Unbounded.To_Unbounded_String
         (Source));
   end To_PolyORB_String;

   function To_Standard_String
     (Source : Types.String)
     return Standard.String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (Source));
   end To_Standard_String;

   function To_PolyORB_Wide_String
     (Source : Standard.Wide_String)
     return Types.Wide_String is
   begin
      return Types.Wide_String
        (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String
         (Source));
   end To_PolyORB_Wide_String;

   function To_Standard_Wide_String
     (Source : Types.Wide_String)
     return Standard.Wide_String is
   begin
      return Ada.Strings.Wide_Unbounded.To_Wide_String
        (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String
         (Source));
   end To_Standard_Wide_String;

end PolyORB.Types;
