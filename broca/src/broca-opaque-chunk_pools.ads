------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--             B R O C A . O P A Q U E . C H U N K _ P O O L S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines pools of memory chunks,
--  with metadata associated to each chunk by the client.

generic
   type Chunk_Metadata is private;
   --  The metadata associated with each
   --  storage chunk.

   Null_Metadata : Chunk_Metadata;

package Broca.Opaque.Chunk_Pools is

   pragma Preelaborate;

   use Broca.Opaque;

   type Chunk (Size : Positive_Index_Type) is private;
   type Chunk_Access is access all Chunk;

   Default_Chunk_Size : constant Index_Type := 4096
     - ((Chunk_Metadata'Size + Chunk_Access'Size + 3)
        and 4084);

   type Pool_Type is private;
   --  A Pool of chunks with one preallocated
   --  chunk and a set of dynamically created ones.

   type Metadata_Access is access all Chunk_Metadata;

   procedure Allocate
     (Pool    : access Pool_Type;
      A_Chunk : out Chunk_Access;
      Size    : Positive_Index_Type
        := Default_Chunk_Size);
   --  Create a chunk in Pool and return an access to it.
   --  On the first call where Size is no more than Default_Chunk_Size,
   --  the Prealloc chunk is returned. On all other calls, a chunk of
   --  size Default_Chunk_Size or Size, whichever is greater, is
   --  dynamically allocated.

   function Chunk_Storage
     (A_Chunk : Chunk_Access)
     return Opaque_Pointer;
   --  Return a pointer to a chunk's storage space.

   procedure Release
     (Pool : access Pool_Type);
   --  Signals that Pool will not be used anymore.
   --  The associated storage is returned to the system.

   function First
     (Pool : Pool_Type)
     return Chunk_Access;
   --  Returns an access to the first chunk in Pool.

   function Next
     (A_Chunk : Chunk_Access)
     return Chunk_Access;
   --  Returns the chunk that follows A_Chunk in its
   --  pool. Returns a null access if A_Chunk designates
   --  the last chunk in a pool.

   function Metadata
     (A_Chunk : Chunk_Access)
     return Metadata_Access;
   --  Returns an access to the metadata associated
   --  with A_Chunk by the client of the Chunk_Pool
   --  package.

private

   --  A chunk pool is managed as a linked list
   --  of chunks.

   type Chunk (Size : Positive_Index_Type) is record
      Next     : Chunk_Access := null;
      --  The next chunk in the pool.

      Metadata : aliased Chunk_Metadata;
      --  Metadata associated by a client to this chunk.

      Data     : aliased Octet_Array (1 .. Size);
      --  The storage space of the chunk.
   end record;

   type Pool_Type is record
      Prealloc : aliased Chunk (Default_Chunk_Size);
      --  A pre-allocated chunk.

      Prealloc_Used : Boolean := False;
      --  The pre-allocated chunk has been used.

      First    : Chunk_Access := null;
      Last     : Chunk_Access := null;
      --  The first and last elements of the list
      --  of dynamically-allocated chunks.
   end record;

end Broca.Opaque.Chunk_Pools;

