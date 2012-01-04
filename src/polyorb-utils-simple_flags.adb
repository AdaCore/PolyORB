------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . U T I L S . S I M P L E _ F L A G S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

package body PolyORB.Utils.Simple_Flags is

   ----------
   -- Mask --
   ----------

   function Mask (N : Bit_Count) return Flags_Type is
   begin
      return Shift_Left (1, Natural (N));
   end Mask;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Flag_To_Test : Flags_Type;
      In_Flags     : Flags_Type) return Boolean
   is
   begin
      return ((Flag_To_Test and In_Flags) = Flag_To_Test);
   end Is_Set;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (N        : Bit_Count;
      In_Flags : Flags_Type) return Boolean
   is
      M : constant Flags_Type := Mask (N);
   begin
      return Is_Set (M, In_Flags);
   end Is_Set;

   ---------
   -- Set --
   ---------

   function Set
     (Flag_To_Set : Flags_Type;
      In_Flags    : Flags_Type) return Flags_Type
   is
   begin
      return (In_Flags and Flag_To_Set);
   end Set;

   ---------
   -- Set --
   ---------

   function Set
     (N        : Bit_Count;
      In_Flags : Flags_Type) return Flags_Type
   is
      M : constant Flags_Type := Mask (N);
   begin
      return Set (M, In_Flags);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Flag_Field : in out Flags_Type;
      N          : Bit_Count;
      Value      : Boolean)
   is
      M : constant Flags_Type := Mask (N);
   begin
      if Value then
         Flag_Field := (Flag_Field and (not M)) or M;
      else
         Flag_Field := Flag_Field and (not M);
      end if;
   end Set;

end PolyORB.Utils.Simple_Flags;
