------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . O P A Q U E . C H U N K _ P O O L S            --
--                                                                          --
--                                 B o d y                                  --
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

--  Pools of memory chunks, with associated client metadata.

--  $Id: //droopi/main/src/polyorb-opaque-chunk_pools.adb#4 $

with Ada.Unchecked_Deallocation;

package body PolyORB.Opaque.Chunk_Pools is

   procedure Initialize (X : in out Chunk) is
   begin
      pragma Assert (X.Data = null);
      X.Data := new Ada.Streams.Stream_Element_Array (1 .. X.Size);
      pragma Assert (X.Data /= null);
   end Initialize;

   procedure Finalize (X : in out Chunk) is
   begin
      Free (X.Data);
   end Finalize;

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
         New_Chunk := new Chunk (Size => Allocation_Size);
         New_Chunk.Next := null;
         New_Chunk.Data.all := (others => 176);
         New_Chunk.Metadata := Null_Metadata;
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
        (Zone   => A_Chunk.Data,
         Offset => A_Chunk.Data'First);
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

end PolyORB.Opaque.Chunk_Pools;

