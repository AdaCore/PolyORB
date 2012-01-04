------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . F I L T E R S . F R A G M E N T E R            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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
     (F : not null access Fragmenter_Filter;
      S : Components.Message'Class) return Components.Message'Class;

end PolyORB.Filters.Fragmenter;
