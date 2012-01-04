------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . B U F F E R S                 --
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

with System;

with PolyORB.Opaque; use PolyORB.Opaque;

package body PolyORB.Utils.Buffers is

   -------------------------------
   -- Align_Transfer_Elementary --
   -------------------------------

   package body Align_Transfer_Elementary is

      subtype SEA is Stream_Element_Array (1 .. T'Size / 8);
      Alignment_Of_T : constant Alignment_Type := Alignment_Of (T'Size / 8);

      --------------
      -- Marshall --
      --------------

      procedure Marshall
        (Buffer : access Buffer_Type;
         Item   : T)
      is
         Item_Address : System.Address := Item'Address;
         Data_Address : Opaque_Pointer;
         Item_Swapped : aliased T;
      begin
         if Alignment_Of_T /= Align_1 and then With_Alignment then
            Pad_Align (Buffer, Alignment_Of_T);
         end if;

         Allocate_And_Insert_Cooked_Data (Buffer, T'Size / 8, Data_Address);

         --  Note: we can't just have a T object at Data_Address and assign
         --  it with Item / Swapped (Item) because Data_Address may not be
         --  suitably aligned. So instead overlay a constrained stream element
         --  array, and assign that.

         declare
            Z_Addr : constant System.Address := Data_Address;
            Z : SEA;
            for Z'Address use Z_Addr;
            pragma Import (Ada, Z);
         begin
            if Item'Size > 8 and then Endianness (Buffer) /= Host_Order then
               Item_Swapped := Swapped (Item);
               Item_Address := Item_Swapped'Address;
            end if;

            declare
               Item_Storage : SEA;
               pragma Import (Ada, Item_Storage);
               for Item_Storage'Address use Item_Address;
            begin
               Z := Item_Storage;
            end;
         end;
      end Marshall;

      ----------------
      -- Unmarshall --
      ----------------

      function Unmarshall (Buffer : access Buffer_Type) return T is
         Data_Address : Opaque_Pointer;
      begin
         if Alignment_Of_T /= Align_1 and then With_Alignment then
            Align_Position (Buffer, Alignment_Of_T);
         end if;
         Extract_Data (Buffer, Data_Address, T'Size / 8);

         --  Note: Need to go through a stream element array to account for
         --  possibly misaligned extracted data (see comments in Marshall).

         declare
            Z_Addr : constant System.Address := Data_Address;
            Z : SEA;
            for Z'Address use Z_Addr;
            pragma Import (Ada, Z);

            Item : aliased T;
            Item_Storage : SEA;
            pragma Import (Ada, Item_Storage);
            for Item_Storage'Address use Item'Address;
         begin
            Item_Storage := Z;

            if Item'Size > 8 and then Endianness (Buffer) = Host_Order then
               return Item;
            else
               return Swapped (Item);
            end if;
         end;
      end Unmarshall;
   end Align_Transfer_Elementary;

   -------------------------
   -- Align_Marshall_Copy --
   -------------------------

   procedure Align_Marshall_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := Align_1)
   is
      Data_Address : Opaque_Pointer;
   begin
      Pad_Align (Buffer, Alignment);
      Allocate_And_Insert_Cooked_Data
        (Buffer,
         Octets'Length,
         Data_Address);

      declare
         Z_Addr : constant System.Address := Data_Address;
         Z : Stream_Element_Array (Octets'Range);
         for Z'Address use Z_Addr;
         pragma Import (Ada, Z);
      begin
         Z := Octets;
      end;
   end Align_Marshall_Copy;

   ---------------------------
   -- Align_Unmarshall_Copy --
   ---------------------------

   procedure Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Alignment : Alignment_Type := Align_1;
      Data      : out Stream_Element_Array)
   is
      Index : Stream_Element_Offset := Data'First;
      Size  : Stream_Element_Count;

      Data_Address : Opaque_Pointer;
   begin
      Align_Position (Buffer, Alignment);
      while Index /= Data'Last + 1 loop
         Size := Data'Last - Index + 1;
         Partial_Extract_Data (Buffer, Data_Address, Size);
         pragma Assert (Size > 0);
         --  Size may be less than what we requested, in case we are at
         --  a chunk boundary, but at least *some* data must always be
         --  returned.

         declare
            Extracted_Data : Stream_Element_Array (1 .. Size);
            for Extracted_Data'Address use Data_Address;
            pragma Import (Ada, Extracted_Data);
         begin
            Data (Index .. Index + Size - 1) := Extracted_Data;
         end;
         Index := Index + Size;
      end loop;
   end Align_Unmarshall_Copy;

end PolyORB.Utils.Buffers;
