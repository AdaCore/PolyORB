------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O L Y O R B . T Y P E S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2001 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Base data types for the whole middleware.

package body PolyORB.Types is

   -----------------------------
   -- Trimmed_Image functions --
   -----------------------------

   function Trimmed_Image (X : Short) return Standard.String is
      R : constant Standard.String := Short'Image (X);
   begin
      if X >= 0 then
         return R (R'First + 1 .. R'Last);
      else
         return R;
      end if;
   end Trimmed_Image;

   function Trimmed_Image (X : Long) return Standard.String is
      R : constant Standard.String := Long'Image (X);
   begin
      if X >= 0 then
         return R (R'First + 1 .. R'Last);
      else
         return R;
      end if;
   end Trimmed_Image;

   function Trimmed_Image (X : Long_Long) return Standard.String is
      R : constant Standard.String := Long_Long'Image (X);
   begin
      if X >= 0 then
         return R (R'First + 1 .. R'Last);
      else
         return R;
      end if;
   end Trimmed_Image;

   function Trimmed_Image (X : Unsigned_Short) return Standard.String is
      R : constant Standard.String := Unsigned_Short'Image (X);
   begin
      return R (R'First + 1 .. R'Last);
   end Trimmed_Image;

   function Trimmed_Image (X : Unsigned_Long) return Standard.String is
      R : constant Standard.String := Unsigned_Long'Image (X);
   begin
      return R (R'First + 1 .. R'Last);
   end Trimmed_Image;

   function Trimmed_Image (X : Unsigned_Long_Long) return Standard.String is
      R : constant Standard.String := Unsigned_Long_Long'Image (X);
   begin
      return R (R'First + 1 .. R'Last);
   end Trimmed_Image;

   function Trimmed_Image (X : Octet) return Standard.String is
      R : constant Standard.String := Octet'Image (X);
   begin
      return R (R'First + 1 .. R'Last);
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
