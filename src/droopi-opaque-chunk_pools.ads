--  Pools of memory chunks, with associated client metadata.

--  $Id: //droopi/main/src/droopi-opaque-chunk_pools.ads#1 $

generic

   type Chunk_Metadata is private;
   --  The metadata associated with each storage chunk.

   Null_Metadata : Chunk_Metadata;

package Droopi.Opaque.Chunk_Pools is

   pragma Preelaborate;

   use Ada.Streams;
   use Droopi.Opaque;

   type Chunk (Size : Stream_Element_Count) is private;
   type Chunk_Access is access all Chunk;

   Default_Chunk_Size : constant Stream_Element_Count := 4096;

   type Pool_Type is private;
   --  A Pool of chunks with one preallocated
   --  chunk and a set of dynamically created ones.

   type Metadata_Access is access all Chunk_Metadata;

   procedure Allocate
     (Pool    : access Pool_Type;
      A_Chunk : out Chunk_Access;
      Size    : Stream_Element_Count := Default_Chunk_Size);
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

   type Chunk (Size : Stream_Element_Count) is record
      Next     : Chunk_Access;
      --  The next chunk in the pool.

      Metadata : aliased Chunk_Metadata;
      --  Metadata associated by a client to this chunk.

      Data     : aliased Stream_Element_Array (1 .. Size);
      --  The storage space of the chunk.
   end record;

   type Pool_Type is record
      Prealloc : aliased Chunk (Default_Chunk_Size);
      --  A pre-allocated chunk.

      Prealloc_Used : Boolean := False;
      --  The pre-allocated chunk has been used.

      First    : Chunk_Access;
      Last     : Chunk_Access;
      --  The first and last elements of the list
      --  of dynamically-allocated chunks.
   end record;

end Droopi.Opaque.Chunk_Pools;

