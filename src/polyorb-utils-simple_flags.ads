------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . U T I L S . S I M P L E _ F L A G S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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
