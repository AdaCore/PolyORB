------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . F I L T E R S . M I O P                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  MIOP stack headers function

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;

package body PolyORB.Filters.MIOP is

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.filters.miop");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --  Unique Id counter
   Index_Unique_Id : Natural := 0;

   --------------------------
   -- Marshall_MIOP_Header --
   --------------------------

   procedure Marshall_MIOP_Header
     (Buffer : access Buffers.Buffer_Type;
      Header : MIOP_Header)
   is
      use Octet_Flags;
      use PolyORB.Types;

      Flags   : Types.Octet := 0;
   begin
      --  Magic
      for J in Magic'Range loop
         Marshall (Buffer, Types.Octet (Magic (J)));
      end loop;

      --  Version
      Flags := (MIOP_Minor_Version and 15) or
        ((MIOP_Major_Version and 15) * 16);
      Marshall (Buffer, Flags);

      --  Flags
      Set (Flags, Bit_Little_Endian, Header.Endianness = Little_Endian);
      Set (Flags, Bit_Collect_Mode, Header.Collect_Mode);
      Marshall (Buffer, Flags);
      pragma Assert (Endianness (Buffer) = Header.Endianness);

      --  Size
      Marshall (Buffer, Header.Packet_Size);

      --  Number
      Marshall (Buffer, Header.Packet_Number);

      --  Total
      Marshall (Buffer, Header.Packet_Total);

      --  Unique_Id
      Marshall (Buffer, Types.Identifier (Header.Unique_Id));

      --  Final padding
      Pad_Align (Buffer, Align_8);
   end Marshall_MIOP_Header;

   ----------------------------
   -- Unmarshall_MIOP_Header --
   ----------------------------

   procedure Unmarshall_MIOP_Header
     (Buffer : access Buffers.Buffer_Type;
      Header :    out MIOP_Header)
   is
      use Octet_Flags;
      use PolyORB.Types;

      Flags : Types.Octet;
      Message_Magic : Stream_Element_Array (Magic'Range);
   begin
      --  Get Endianness
      Flags := Types.Octet (Peek (Buffer, Flags_Index));

      if Is_Set (Bit_Little_Endian, Flags) then
         Set_Endianness (Buffer, Little_Endian);
      else
         Set_Endianness (Buffer, Big_Endian);
      end if;

      --  Begining of GIOP message is byte-ordering independent

      --  Magic
      for J in Message_Magic'Range loop
         Message_Magic (J) := Stream_Element
           (Types.Octet'(Unmarshall (Buffer)));
      end loop;

      if Message_Magic /= Magic then
         raise MIOP_Packet_Error;
      end if;

      --  Version
      if Types.Octet'(Unmarshall (Buffer))
        /= (MIOP_Major_Version * 16 or MIOP_Minor_Version)
      then
         raise MIOP_Packet_Error;
      end if;

      pragma Debug (C, O ("MIOP Version OK"));

      --  Flags
      Flags := Unmarshall (Buffer);

      if Is_Set (Bit_Little_Endian, Flags) then
         Header.Endianness := Little_Endian;
      else
         Header.Endianness := Big_Endian;
      end if;
      pragma Assert (Header.Endianness = Endianness (Buffer));

      pragma Debug (C, O ("Message Endianness : "
                       & Header.Endianness'Img));

      if Is_Set (Bit_Collect_Mode, Flags) then
         Header.Collect_Mode := True;
      else
         Header.Collect_Mode := False;
      end if;
      pragma Debug (C, O ("Collect Mode       : "
                       & Header.Collect_Mode'Img));

      --  Extract size
      Header.Packet_Size := Unmarshall (Buffer);
      pragma Debug (C, O ("Packer Size        :"
                       & Header.Packet_Size'Img));

      --  Extract Number
      Header.Packet_Number := Unmarshall (Buffer);
      pragma Debug (C, O ("Packet Number      :"
                       & Header.Packet_Number'Img));

      --  Extract Total
      Header.Packet_Total := Unmarshall (Buffer);
      pragma Debug (C, O ("Packet Total       :"
                       & Header.Packet_Total'Img));

      --  Unique_Id_Size, Unique Id will be extracted later
      Header.Unique_Id_Size := Unmarshall (Buffer);
      pragma Debug (C, O ("Unique Id Size     :"
                       & Header.Unique_Id_Size'Img));

   end Unmarshall_MIOP_Header;

   --------------------------
   -- Unmarshall_Unique_Id --
   --------------------------

   --  This function unmarshall unique id
   --  Unique_Id size must be known
   --  This is an adpatation of string unmarshalling function

   procedure Unmarshall_Unique_Id
     (Buffer : access Buffers.Buffer_Type;
      Length : Types.Unsigned_Long;
      Str    :    out Types.String)
   is
      use PolyORB.Types;

      Equiv : String (1 .. Integer (Length - 1));
   begin
      if Length = 0 then
         Str := To_PolyORB_String ("");
         return;
      end if;

      for J in Equiv'Range loop
         Equiv (J) := Character'Val
           (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)));
      end loop;

      --  A 0 must end the string
      if Character'Val
           (PolyORB.Types.Char'Pos (Unmarshall_Latin_1_Char (Buffer)))
        /= ASCII.NUL
      then
         raise Constraint_Error;
      end if;

      Str := To_PolyORB_String (Equiv);
   end Unmarshall_Unique_Id;

   ------------------------
   -- Generate_Unique_Id --
   ------------------------

   function Generate_Unique_Id
     return Types.String
   is
      use PolyORB.Types;
   begin
      Index_Unique_Id := Index_Unique_Id + 1;
      return To_PolyORB_String ("PolyORB" & Index_Unique_Id'Img);
   end Generate_Unique_Id;

end PolyORB.Filters.MIOP;
