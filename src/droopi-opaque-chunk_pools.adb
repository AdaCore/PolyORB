--  Pools of memory chunks, with associated client metadata.

--  $Id: //droopi/main/src/droopi-opaque-chunk_pools.adb#1 $

with Ada.Unchecked_Deallocation;

package body Droopi.Opaque.Chunk_Pools is

   procedure Allocate
     (Pool    : access Pool_Type;
      A_Chunk : out Chunk_Access;
      Size    : Stream_Element_Count := Default_Chunk_Size)
   is
      Allocation_Size : Stream_Element_Count;
      New_Chunk       : Chunk_Access;
   begin
      if Size <= Default_Chunk_Size then
         Allocation_Size := Default_Chunk_Size;
      else
         Allocation_Size := Size;
      end if;

      if Allocation_Size = Default_Chunk_Size
        and then not Pool.Prealloc_Used then
         New_Chunk := Pool.Prealloc'Access;
         Pool.Prealloc_Used := True;
      else
         New_Chunk := new Chunk'(Size     => Allocation_Size,
                                 Next     => null,
                                 Data     => (others => 176),
                                 Metadata => Null_Metadata);
      end if;

      if Pool.Last = null then
         pragma Assert (Pool.First = null);

         Pool.First := New_Chunk;
         Pool.Last  := New_Chunk;
      else
         Pool.Last.Next := New_Chunk;
         Pool.Last := New_Chunk;
      end if;

      A_Chunk := New_Chunk;
   end Allocate;

   function Chunk_Storage
     (A_Chunk : Chunk_Access)
     return Opaque_Pointer is
   begin
      return Opaque_Pointer'
        (Zone  => A_Chunk.Data'Access,
         First => A_Chunk.Data'First,
         Last  => A_Chunk.Data'Last);
   end Chunk_Storage;

   procedure Release
     (Pool : access Pool_Type)
   is

      procedure Free is new Ada.Unchecked_Deallocation
        (Chunk, Chunk_Access);

      Current : Chunk_Access
        := Pool.First;

   begin
      while Current /= null loop
         declare
            Next : Chunk_Access := Current.Next;
         begin
            if Current /= Pool.Prealloc'Access then
               Free (Current);
            end if;
            Current := Next;
         end;
      end loop;

      Pool.Prealloc.Next := null;
      Pool.First := null;
      Pool.Last  := null;
      Pool.Prealloc_Used := False;
   end Release;

   function First
     (Pool : Pool_Type)
     return Chunk_Access is
   begin
      return Pool.First;
   end First;

   function Next
     (A_Chunk : Chunk_Access)
     return Chunk_Access is
   begin
      return A_Chunk.Next;
   end Next;

   function Metadata
     (A_Chunk : Chunk_Access)
     return Metadata_Access is
   begin
      return A_Chunk.Metadata'Access;
   end Metadata;

end Droopi.Opaque.Chunk_Pools;

