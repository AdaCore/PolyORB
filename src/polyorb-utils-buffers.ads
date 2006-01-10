------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . B U F F E R S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

--  Utility subprograms for data representation methods and buffer access.

with Ada.Streams;
with System.Address_To_Access_Conversions;
with PolyORB.Buffers;

package PolyORB.Utils.Buffers is

   pragma Elaborate_Body;

   use PolyORB.Buffers;
   use Ada.Streams;

   procedure Align_Marshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of
   --  Octets into it.
   --  The data in Octets shall be presented in big-endian
   --  byte order.

   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   pragma Unreferenced (Align_Unmarshall_Big_Endian_Copy);
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in big-endian byte order.

   generic
      Size     : Stream_Element_Count;
      Alignment : Alignment_Type := 1;
   package Fixed_Size_Unmarshall is
      type Z is new Stream_Element_Array (0 .. Size - 1);
      package Address_To_Access_Conversion is
         new System.Address_To_Access_Conversions (Z);
      subtype AZ is Address_To_Access_Conversion.Object_Pointer;

      function Align_Unmarshall (Buffer : access Buffer_Type) return AZ;
      pragma Inline (Align_Unmarshall);
      --  Align Buffer on Alignment, then unmarshall Size octets from it,
      --  and return an access to the unmarshalled data. Note that the
      --  returned access value must not be dereferenced once Buffer's contents
      --  have been released.
   end Fixed_Size_Unmarshall;
   --
   procedure Align_Marshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of
   --  Octets into it.
   --  The data in Octets shall be presented in the
   --  host's byte order.

   function Align_Unmarshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy of
   --  Size octets from it.
   --  The data is returned in the host's byte order.

   procedure Align_Marshall_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy
   --  of Octets into Buffer, as is.

   function Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array;
   --  Align Buffer on Alignment, then unmarshall a copy
   --  of Size octets from Buffer's data, as is.

end PolyORB.Utils.Buffers;
