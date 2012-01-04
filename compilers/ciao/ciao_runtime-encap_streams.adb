------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C I A O _ R U N T I M E . E N C A P _ S T R E A M S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

--  A DSA stream based on a CORBA Encapsulation
--  (sequence<octet>).
package body CIAO_Runtime.Encap_Streams is

   use IDL_SEQUENCE_Octet;

   procedure Set_Seq (St : in out Stream; Ar : Octet_Array) is
   begin
      St.Seq := IDL_Sequence_Octet.To_Sequence (Ar);
      St.Pos := 1;
   end Set_Seq;

   function Get_Seq (St : Stream) return Octet_Array is
   begin
      return To_Element_Array (St.Seq);
   end Get_Seq;

   procedure Read
     (St : in out Stream;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Read_Length : Integer;
   begin
      if Item'Length > Length (St.Seq) - St.Pos + 1 then
         Read_Length := Length (St.Seq) - St.Pos;
      else
         Read_Length := Item'Length;
      end if;

      if Read_Length <= 0 then
         Last := Item'First - 1;
         return;
      end if;

      declare
         Data : Octet_Array
           := Slice (St.Seq, St.Pos, St.Pos + Read_Length - 1);
      begin
         for I in Data'Range loop
            Item (Stream_Element_Offset (I - Data'First) + Item'First)
              := Stream_Element (Data (I));
         end loop;
      end;

      Last := Item'First + Stream_Element_Offset (Read_Length - 1);
      St.Pos := St.Pos + Read_Length;
   end Read;

   procedure Write
     (St   : in out Stream;
      Item : in Stream_Element_Array) is
   begin
      declare
         Data : Octet_Array (Integer (Item'First) .. Integer (Item'Last));
      begin
         for I in Item'Range loop
            Data (Integer (I)) := CORBA.Octet (Item (I));
         end loop;

         Append (St.Seq, Data);
         St.Pos := St.Pos + Item'Size;
      end;
   end Write;

end CIAO_Runtime.Encap_Streams;
