------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . F I L T E R S . F R A G M E N T E R            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

--  Fragmenter filter
--  Fragment data which comes from endpoint whithout read length control
--  For example UDP sockets

with Ada.Streams;

with PolyORB.Buffers;
with PolyORB.Components;

package PolyORB.Filters.Fragmenter is

   pragma Elaborate_Body;

   type Fragmenter_Factory is new Factory with private;

   procedure Create
     (Fact   : access Fragmenter_Factory;
      Fragmenter : out Filter_Access);

private

   type Fragmenter_Factory is new Factory with null record;

   --  Fragmenter status
   type Fragmenter_Filter is new Filter with record
      --  Buffer to upper filter
      In_Buf         : Buffers.Buffer_Access;
      --  Buffer from lower filter
      Socket_Buf     : Buffers.Buffer_Access;
      --  Size of data expected by upper filter
      Data_Expected  : Ada.Streams.Stream_Element_Count;
      --  Size of data expected by upper filter at demand
      Initial_Data_Expected : Ada.Streams.Stream_Element_Count;
   end record;

   function Handle_Message
     (F : access Fragmenter_Filter;
      S : Components.Message'Class)
     return Components.Message'Class;

end PolyORB.Filters.Fragmenter;
