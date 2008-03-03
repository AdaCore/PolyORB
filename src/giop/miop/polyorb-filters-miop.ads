------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . F I L T E R S . M I O P                  --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Streams; use Ada.Streams;

with PolyORB.Buffers;
with PolyORB.Types;
with PolyORB.Utils.Simple_Flags;

package PolyORB.Filters.MIOP is

   MIOP_Packet_Error : exception;

   --  MIOP Header type
   type MIOP_Header is private;
   type MIOP_Header_Access is access MIOP_Header;

private

   package Octet_Flags is
      new PolyORB.Utils.Simple_Flags (Types.Octet, Types.Shift_Left);

   --  Default max MIOP packet size used to fragment
   Default_Max_MIOP_Message_Size : constant Integer := 1000;

   --  Location of flags in MIOP packet
   Flags_Index       : constant Stream_Element_Offset := 6 - 1;
   --  Note: Flags is at 6th position in MIOP 1.0 PacketHeader
   --  structure, hence at index 6 - 1.

   Bit_Little_Endian : constant Octet_Flags.Bit_Count := 0;
   Bit_Collect_Mode  : constant Octet_Flags.Bit_Count := 1;

   --  MIOP header size (with only the size of Unique Id)
   MIOP_Header_Size : constant Stream_Element_Count := 20;

   --  Magic identifier
   --  Begin of all MIOP Messages
   Magic : constant Stream_Element_Array (1 .. 4)
     := (Character'Pos ('M'),
         Character'Pos ('I'),
         Character'Pos ('O'),
         Character'Pos ('P'));

   --  MIOP Version
   MIOP_Major_Version : constant Types.Octet := 1;
   MIOP_Minor_Version : constant Types.Octet := 0;

   --  MIOP Header
   type MIOP_Header is record
      --  Packet endianness
      Endianness     : Buffers.Endianness_Type
        := PolyORB.Buffers.Host_Order;
      --  Fragmenting mode
      Collect_Mode   : Boolean;
      --  Packet Size
      Packet_Size    : Types.Unsigned_Short;
      --  Packet Number in Collection
      Packet_Number  : Types.Unsigned_Long := 0;
      --  Number of Packet in Collection
      Packet_Total   : Types.Unsigned_Long := 0;
      --  Unique Id of Collection
      Unique_Id      : Types.String;
      --  Unique Id string size
      Unique_Id_Size : Types.Unsigned_Long;
   end record;

   --  Marshall MIOP Header
   procedure Marshall_MIOP_Header
     (Buffer : access Buffers.Buffer_Type;
      Header : MIOP_Header);

   --  Unmarshall MIOP Header
   procedure Unmarshall_MIOP_Header
     (Buffer : access Buffers.Buffer_Type;
      Header :    out MIOP_Header);

   --  Unmarshall Unique Id
   procedure Unmarshall_Unique_Id
     (Buffer : access Buffers.Buffer_Type;
      Length : Types.Unsigned_Long;
      Str    :    out Types.String);

   --  Generate a new Unique Id
   function Generate_Unique_Id
     return Types.String;

end PolyORB.Filters.MIOP;
