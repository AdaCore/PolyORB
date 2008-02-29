------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . F I L T E R S . M I O P                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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
      Pad_Align (Buffer, 8);
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
      --  This code works only if the endianness bit dont move
      --  in different miop version
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
        /= ASCII.Nul
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
