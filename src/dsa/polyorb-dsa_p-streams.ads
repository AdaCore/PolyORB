------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . D S A _ P . S T R E A M S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
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

with Ada.Streams;
use  Ada.Streams;

package PolyORB.DSA_P.Streams is

   type Memory_Resident_Stream (Size : Stream_Element_Count) is
      new Root_Stream_Type with private;
   --  A stream type that holds the stream elements in memory

   overriding procedure Read (This : in out Memory_Resident_Stream;
                   Item :    out Stream_Element_Array;
                   Last :    out Stream_Element_Offset);
   --  Reads the entire value of Item from the stream This,
   --  setting Last to the last index of Item that is assigned.
   --  If the length of Item is greater than the number of
   --  elements in the stream, reading stops and Last will not
   --  be equal to Item'Last.

   overriding procedure Write (This : in out Memory_Resident_Stream;
                    Item : Stream_Element_Array);
   --  Writes the elements in Item to the stream.

   procedure Reset_Reading (This : access Memory_Resident_Stream);
   --  Start reading from the beginning of the stream.

   procedure Reset_Writing (This : access Memory_Resident_Stream);
   --  Start writing to the beginning of the stream.

   function Empty (This : Memory_Resident_Stream) return Boolean;
   --  Returns whether the stream contains any stream elements.

   procedure Reset (This : access Memory_Resident_Stream);
   --  Performs a complete reset, as if no reading or writing
   --  had ever occurred.

   function Extent (This : Memory_Resident_Stream) return Stream_Element_Count;
   --  Returns the number of elements in the stream.

private

   type Memory_Resident_Stream (Size : Stream_Element_Count) is
      new Root_Stream_Type with
         record
            Count    : Stream_Element_Count := 0;
            --  The number of stream elements currently held
            Next_In  : Stream_Element_Offset := 1;
            --  The index of the next stream element to be written
            Next_Out : Stream_Element_Offset := 1;
            --  The index of the next stream element to be read
            Values   : Stream_Element_Array (1 .. Size);
            --  The stream elements currently held
         end record;

end PolyORB.DSA_P.Streams;
