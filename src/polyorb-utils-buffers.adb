------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . B U F F E R S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body PolyORB.Utils.Buffers is

   ---------
   -- Rev --
   ---------

   function Rev (Octets : Stream_Element_Array) return Stream_Element_Array is
      Result : Stream_Element_Array (Octets'Range);
   begin
      for I in Octets'Range loop
         Result (Octets'Last - I + Octets'First) := Octets (I);
      end loop;
      return Result;
   end Rev;

   ------------------------------------
   -- Align_Marshall_Big_Endian_Copy --
   ------------------------------------

   procedure Align_Marshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1)
   is
   begin
      if Endianness (Buffer.all) = Big_Endian then
         Align_Marshall_Copy (Buffer, Octets, Alignment);
      else
         Align_Marshall_Copy (Buffer, Rev (Octets), Alignment);
      end if;
   end Align_Marshall_Big_Endian_Copy;

   --------------------------------------
   -- Align_Unmarshall_Big_Endian_Copy --
   --------------------------------------

   function Align_Unmarshall_Big_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array
   is
   begin
      if Endianness (Buffer.all) = Big_Endian then
         return Align_Unmarshall_Copy (Buffer, Size, Alignment);
      else
         return Rev (Align_Unmarshall_Copy
                     (Buffer, Size, Alignment));
      end if;
   end Align_Unmarshall_Big_Endian_Copy;

   -------------------------------------
   -- Align_Marshall_Host_Endian_Copy --
   -------------------------------------

   procedure Align_Marshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Octets    : Stream_Element_Array;
      Alignment : Alignment_Type := 1)
   is
   begin
      if Endianness (Buffer.all) = Host_Order then
         Align_Marshall_Copy (Buffer, Octets, Alignment);
      else
         Align_Marshall_Copy (Buffer, Rev (Octets), Alignment);
      end if;
   end Align_Marshall_Host_Endian_Copy;

   --------------------------------------
   -- Align_Unmarshall_Host_Endian_Copy --
   --------------------------------------

   function Align_Unmarshall_Host_Endian_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array
   is
   begin
      if Endianness (Buffer.all) = Host_Order then
         return Align_Unmarshall_Copy (Buffer, Size, Alignment);
      else
         return Rev (Align_Unmarshall_Copy
                     (Buffer, Size, Alignment));
      end if;
   end Align_Unmarshall_Host_Endian_Copy;

   -------------------------
   -- Align_Marshall_Copy --
   -------------------------

   procedure Align_Marshall_Copy
     (Buffer    : access Buffer_Type;
      Octets    : in Stream_Element_Array;
      Alignment : Alignment_Type := 1)
   is
      Data_Address : Opaque_Pointer;
   begin
      Pad_Align (Buffer, Alignment);
      Allocate_And_Insert_Cooked_Data
        (Buffer,
         Octets'Length,
         Data_Address);

      Data_Address.Zone (Data_Address.Offset
        .. Data_Address.Offset + Octets'Length - 1)
        := Octets;
   end Align_Marshall_Copy;

   ---------------------------
   -- Align_Unmarshall_Copy --
   ---------------------------

   function Align_Unmarshall_Copy
     (Buffer    : access Buffer_Type;
      Size      : Stream_Element_Count;
      Alignment : Alignment_Type := 1)
     return Stream_Element_Array
   is
      Data_Address : Opaque_Pointer;
   begin
      Align_Position (Buffer, Alignment);
      Extract_Data (Buffer, Data_Address, Size);
      return Data_Address.Zone
        (Data_Address.Offset .. Data_Address.Offset + Size - 1);
   end Align_Unmarshall_Copy;

end PolyORB.Utils.Buffers;
