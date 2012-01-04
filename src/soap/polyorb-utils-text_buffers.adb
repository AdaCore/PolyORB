------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . U T I L S . T E X T _ B U F F E R S            --
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

with Ada.Streams;
with PolyORB.Utils.Buffers;

package body PolyORB.Utils.Text_Buffers is

   use Ada.Streams;
   use PolyORB.Utils.Buffers;

   -------------------
   -- Marshall_Char --
   -------------------

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character) is
   begin
      Align_Marshall_Copy
        (B, Stream_Element_Array'(1 => Stream_Element (Character'Pos (C))));
   end Marshall_Char;

   ---------------------
   -- Unmarshall_Char --
   ---------------------

   function Unmarshall_Char (B : access Buffer_Type) return Character
   is
      A : Stream_Element_Array (1 .. 1);
   begin
      Align_Unmarshall_Copy (B, Align_1, A);
      return Character'Val (A (1));
   end Unmarshall_Char;

   ---------------------
   -- Marshall_String --
   ---------------------

   procedure Marshall_String
     (B : access Buffer_Type;
      S : String)
   is
      subtype SEA is Stream_Element_Array (1 .. S'Length);
      A : SEA;
      for A'Address use S'Address;
      pragma Import (Ada, A);
   begin
      Align_Marshall_Copy (B, A);
   end Marshall_String;

   -----------------------
   -- Unmarshall_String --
   -----------------------

   procedure Unmarshall_String
     (B : access Buffer_Type;
      S : out String)
   is
      subtype SEA is Stream_Element_Array (1 .. S'Length);
      A : SEA;
      for A'Address use S'Address;
      pragma Import (Ada, A);
   begin
      Align_Unmarshall_Copy (B, Align_1, A);
   end Unmarshall_String;

end PolyORB.Utils.Text_Buffers;
