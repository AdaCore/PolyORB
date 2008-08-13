------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . O P A Q U E . C H U N K _ P O O L S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Pools of memory chunks, with associated client metadata.

with PolyORB.Utils.Chained_Lists;

generic

   type Chunk_Metadata is private;
   --  The metadata associated with each storage chunk.

   Null_Metadata : Chunk_Metadata;

package PolyORB.Opaque.Chunk_Pools is

   pragma Preelaborate;

   type Chunk (Size : Ada.Streams.Stream_Element_Count) is limited private;
   type Chunk_Access is access all Chunk;

   Default_Chunk_Size : constant Ada.Streams.Stream_Element_Count := 512;

   type Pool_Type is limited private;
   --  A Pool of chunks with one preallocated chunk and a
   --  set of dynamically created ones.

   type Metadata_Access is access all Chunk_Metadata;

   procedure Allocate
     (Pool    : access Pool_Type;
      A_Chunk : out Chunk_Access;
      Size    : Ada.Streams.Stream_Element_Count := Default_Chunk_Size);
   --  Create a chunk in Pool and return an access to it.
   --  On the first call where Size is no more than Default_Chunk_Size,
   --  the Prealloc chunk is returned. On all other calls, a chunk of
   --  size Default_Chunk_Size or Size, whichever is greater, is
   --  dynamically allocated.

   function Chunk_Storage
     (A_Chunk : Chunk_Access) return Opaque.Opaque_Pointer;
   --  Return a pointer to a chunk's storage space.

   procedure Release
     (Pool : access Pool_Type);
   --  Signals that Pool will not be used anymore.
   --  The associated storage is returned to the system.

   function Metadata
     (A_Chunk : Chunk_Access)
     return Metadata_Access;
   --  Returns an access to the metadata associated
   --  with A_Chunk by the client of the Chunk_Pool
   --  package.

private

   pragma Inline (Metadata);

   --  A chunk pool is managed as a linked list
   --  of chunks.

   type Chunk (Size : Ada.Streams.Stream_Element_Count) is
     tagged limited record
        Metadata : aliased Chunk_Metadata;
         --  Metadata associated by a client to this chunk.

        Data     : aliased Ada.Streams.Stream_Element_Array (1 .. Size);
         --  The storage space of the chunk.
     end record;

   package Chunk_Lists is new Utils.Chained_Lists (Chunk_Access);

   type Pool_Type is limited record
      Prealloc : aliased Chunk (Default_Chunk_Size);
      --  A pre-allocated chunk.

      Prealloc_Used : Boolean := False;
      --  The pre-allocated chunk has been used.

      Chunks   : Chunk_Lists.List;
      --  The list of all dynamically allocated chunks in this pool.
   end record;

end PolyORB.Opaque.Chunk_Pools;
