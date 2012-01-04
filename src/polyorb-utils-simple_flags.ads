------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . U T I L S . S I M P L E _ F L A G S            --
--                                                                          --
--                                 S p e c                                  --
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

--  Utility to provide support for simple binary flags

generic
   type Flags_Type is mod <>;
   with function Shift_Left
     (Value : Flags_Type; N : Natural) return Flags_Type is <>;
package PolyORB.Utils.Simple_Flags is

   pragma Pure;

   type Bit_Count is new Natural range 0 .. (Flags_Type'Size - 1);

   function Mask (N : Bit_Count) return Flags_Type;
   pragma Inline (Mask);
   --  Create a binary mask equal to 2 ** N

   function Is_Set
     (Flag_To_Test : Flags_Type;
      In_Flags     : Flags_Type) return Boolean;
   pragma Inline (Is_Set);
   --  Test if Flag_To_Test has been set in In_Flags
   --  Flag_To_Test is a mask

   function Is_Set
     (N        : Bit_Count;
      In_Flags : Flags_Type) return Boolean;
   pragma Inline (Is_Set);
   --  Test if bit N has been set in In_Flags

   function Set
     (Flag_To_Set : Flags_Type;
      In_Flags    : Flags_Type) return Flags_Type;
   pragma Inline (Set);
   --  Set Flag_To_Set in In_Flags
   --  Flag_To_Set is a mask

   function Set
     (N        : Bit_Count;
      In_Flags : Flags_Type) return Flags_Type;
   pragma Inline (Set);
   --  Set bit N in In_Flags

   procedure Set
     (Flag_Field : in out Flags_Type;
      N          : Bit_Count;
      Value      : Boolean);
   --  Set bit N of Flag_Field to Value

end PolyORB.Utils.Simple_Flags;
