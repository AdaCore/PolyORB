------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . B U F F E R _ S O U R C E S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

--  An XML/Ada input source type based on PolyORB buffers.

--  $Id$

with PolyORB.Utils.Text_Buffers;

package body PolyORB.Buffer_Sources is

   use PolyORB.Buffers;
   use PolyORB.Utils.Text_Buffers;
   use type Ada.Streams.Stream_Element_Offset;

   procedure Set_Buffer
     (S : in out Input_Source;
      B : PolyORB.Buffers.Buffer_Access)
   is
   begin
      S.Buf := B;
      S.Final_Position := CDR_Position (B) + Length (B);
   end Set_Buffer;

   procedure Next_Char
     (From : in out Input_Source;
      C    : out Unicode.Unicode_Char)
   is
   begin
      C := Unicode.To_Unicode (Unmarshall_Char (From.Buf));
   end Next_Char;

   function Eof (From : Input_Source)
     return Boolean
   is
   begin
      return CDR_Position (From.Buf) = From.Final_Position;
   end Eof;

end PolyORB.Buffer_Sources;
