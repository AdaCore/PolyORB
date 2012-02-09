------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . F I L T E R S . S L I C E R S               --
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

pragma Ada_2005;

--  A filter that slices a stream into a set of known-length messages.

with Ada.Streams;

with PolyORB.Buffers;
with PolyORB.Components;

package PolyORB.Filters.Slicers is

   pragma Elaborate_Body;

   type Slicer_Factory is new Factory with private;

   overriding procedure Create
     (Fact   : access Slicer_Factory;
      Slicer :    out Filter_Access);

private

   type Slicer_Factory is new Factory with null record;

   type Slicer_Filter is new Filter with record
      In_Buf        : Buffers.Buffer_Access;
      Data_Expected : Ada.Streams.Stream_Element_Count;
      Initial_Data_Expected : Ada.Streams.Stream_Element_Count;
      Buffer_Length : Ada.Streams.Stream_Element_Count;
   end record;

   overriding function Handle_Message
     (F : not null access Slicer_Filter;
      S : Components.Message'Class) return Components.Message'Class;

end PolyORB.Filters.Slicers;
