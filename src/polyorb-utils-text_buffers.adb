------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . U T I L S . T E X T _ B U F F E R S            --
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

--  A dummy data representation method, just for show.

--  $Id$

with Ada.Streams;
with PolyORB.Utils.Buffers;

package body PolyORB.Utils.Text_Buffers is

   use Ada.Streams;
   use PolyORB.Buffers;
   use PolyORB.Utils.Buffers;

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character) is
   begin
      Align_Marshall_Copy
        (B, Stream_Element_Array'
         (1 => Stream_Element (Character'Pos (C))));
   end Marshall_Char;

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character
   is
      A : constant Stream_Element_Array
        := Align_Unmarshall_Copy (B, 1);
   begin
      return Character'Val (A (A'First));
   end Unmarshall_Char;

   procedure Marshall_String
     (B : access Buffer_Type;
      S : String)
   is
   begin
      for I in S'Range loop
         Marshall_Char (B, S (I));
      end loop;
   end Marshall_String;

   procedure Unmarshall_String
     (B : access Buffer_Type;
      S : out String)
   is
   begin
      for I in S'Range loop
         S (I) := Unmarshall_Char (B);
      end loop;
   end Unmarshall_String;

end PolyORB.Utils.Text_Buffers;
