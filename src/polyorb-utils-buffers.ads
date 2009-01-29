------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . B U F F E R S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2009, Free Software Foundation, Inc.          --
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
with PolyORB.Buffers;

package PolyORB.Utils.Buffers is

   pragma Elaborate_Body;

   use PolyORB.Buffers;
   use Ada.Streams;

   ----------------------------------------------------
   --  Marshalling/unmarshalling of elementary types --
   ----------------------------------------------------

   --  This generic package provides marshalling and unmarshalling operations
   --  that transfer the memory representation of T to/from the buffer,
   --  with optional alignment (equal to the data size), and performing byte
   --  swapping if the buffer endianness differs from the host order.

   generic
      type T is private;
      with function Swapped (Item : T) return T is <>;

      With_Alignment : Boolean := True;
      --  If With_Alignment is False, then don't align the buffer prior to
      --  transfers in the routines below.

   package Align_Transfer_Elementary is
      procedure Marshall (Buffer : access Buffer_Type; Item : T);
      --  Align buffer on T'Size, then marshall a copy of Item, swapping its
      --  bytes using the provided procedure if Buffer's endianness is not
      --  Host_Order.

      function Unmarshall (Buffer : access Buffer_Type) return T;
      --  Align buffer on T'Size, then unmarshall a T value, swapping its bytes
      --  using the provided swapper if Buffer's endianness is not Host_Order.
   end Align_Transfer_Elementary;

   procedure Align_Marshall_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1);
   --  Align Buffer on Alignment, then marshall a copy of Octets into it, as is

   procedure Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Alignment : Alignment_Type := 1;
      Data      : out Stream_Element_Array);
   --  Align Buffer on Alignment, then fill Data by extracting Data'Length
   --  bytes at the current position. The data need not be contiguous in the
   --  in (it may span multiple chunks).

end PolyORB.Utils.Buffers;
