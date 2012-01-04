------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . B U F F E R _ S O U R C E S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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
