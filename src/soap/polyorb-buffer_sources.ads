------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . B U F F E R _ S O U R C E S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  An XML/Ada input source type based on PolyORB buffers.

with PolyORB.Buffers;

with Input_Sources;
with Unicode;

package PolyORB.Buffer_Sources is

   type Input_Source is new Input_Sources.Input_Source with private;

   procedure Set_Buffer
     (S : in out Input_Source;
      B : PolyORB.Buffers.Buffer_Access);

   procedure Next_Char
     (From : in out Input_Source;
      C    : out Unicode.Unicode_Char);

   function Eof (From : Input_Source)
     return Boolean;

private

   type Input_Source is new Input_Sources.Input_Source with record
      Buf : PolyORB.Buffers.Buffer_Access;
   end record;

end PolyORB.Buffer_Sources;
