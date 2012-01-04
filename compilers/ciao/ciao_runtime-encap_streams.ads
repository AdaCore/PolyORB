------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C I A O _ R U N T I M E . E N C A P _ S T R E A M S            --
--                                                                          --
--                                 S p e c                                  --
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
with Ada.Streams; use Ada.Streams;
with CORBA.Sequences.Unbounded;

package CIAO_Runtime.Encap_Streams is

   package IDL_SEQUENCE_Octet is new CORBA.Sequences.Unbounded (CORBA.Octet);
   subtype Sequence is IDL_SEQUENCE_Octet.Sequence;
   subtype Octet_Array is IDL_SEQUENCE_Octet.Element_Array;

   type Stream is new Ada.Streams.Root_Stream_Type with private;

   procedure Set_Seq (St : in out Stream; Ar : Octet_Array);
   function Get_Seq (St : Stream) return Octet_Array;

private

   type Stream is new Ada.Streams.Root_Stream_Type with record
      Seq : Sequence;
      Pos : Natural := 0;
   end record;

   procedure Read (St : in out Stream;
                   Item : out Stream_Element_Array;
                   Last : out Stream_Element_Offset);
   procedure Write (St : in out Stream;
                    Item : in Stream_Element_Array);

end CIAO_Runtime.Encap_Streams;
