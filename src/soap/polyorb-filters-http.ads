------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . F I L T E R S . H T T P                  --
--                                                                          --
--                                 S p e c                                  --
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

--  $Id$

with Ada.Streams;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with PolyORB.Buffers;
with PolyORB.Components;

with PolyORB.Types; use PolyORB.Types;



package PolyORB.Filters.HTTP is

   pragma Elaborate_Body;

   type HTTP_Filter_Factory is new Factory with private;

   procedure Create
     (Fact   : access HTTP_Filter_Factory;
      Filt   : out Filter_Access);

   Unexpected_Data : exception;
   --  Raised when unexpected data is received by this filter.

private

   Char_Length : constant Ada.Streams.Stream_Element_Offset := 1;

   Str_CRLF : constant Types.String := To_PolyORB_String (CR & LF & CR & LF);

   type HTTP_Filter_Factory is new Factory with null record;

   type HTTP_Filter is new Filter with record
      In_Buf        : Buffers.Buffer_Access;
      Data_Expected : Ada.Streams.Stream_Element_Count;
      Buffer_Length : Ada.Streams.Stream_Element_Count;
      Expected_Size_Fixed  : Boolean := False;
   end record;

   function Handle_Message
     (F : access HTTP_Filter;
      S : Components.Message'Class)
     return Components.Message'Class;

end PolyORB.Filters.HTTP;
