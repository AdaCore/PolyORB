------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . O P A Q U E . C H U N K _ P O O L S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2009, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;
with System;

package body PolyORB.Opaque.Chunk_Pools is

   use Ada.Streams;
   use Chunk_Lists;

   --------------
   -- Allocate --
   --------------

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
        and then not Pool.Prealloc_Used
      then
         New_Chunk := Pool.Prealloc'Access;
         Pool.Prealloc_Used := True;
      else
         New_Chunk := new Chunk (Size => Allocation_Size);
         New_Chunk.Metadata := Null_Metadata;
         Append (Pool.Dynamic_Chunks, New_Chunk);
      end if;

      A_Chunk := New_Chunk;
   end Allocate;

   -------------------
   -- Chunk_Storage --
   -------------------

   function Chunk_Storage (A_Chunk : Chunk_Access) return Opaque_Pointer is
   begin
      pragma Assert (A_Chunk /= null);
      return A_Chunk.Data (A_Chunk.Data'First)'Address;
   end Chunk_Storage;

   ----------
   -- Link --
   ----------

   function Link
     (C     : access Chunk;
      Which : Utils.Ilists.Link_Type) return access Chunk_Access
   is
      use Utils.Ilists;
   begin
      pragma Assert (Which = Next);
      return C.Next'Unchecked_Access;
   end Link;

   --------------
   -- Metadata --
   --------------

   function Metadata
     (A_Chunk : Chunk_Access) return Metadata_Access is
   begin
      return A_Chunk.Metadata'Access;
   end Metadata;

   -------------
   -- Release --
   -------------

   procedure Release (Pool : access Pool_Type) is
      procedure Free is new Ada.Unchecked_Deallocation (Chunk, Chunk_Access);
      It : Chunk_Lists.Iterator := First (Pool.Dynamic_Chunks);
   begin
      while not Last (It) loop
         declare
            use type System.Address;
            This : Chunk_Access := Value (It);
         begin
            Remove (Pool.Dynamic_Chunks, It);
            Free (This);
         end;
      end loop;
      Pool.Prealloc_Used := False;
   end Release;

end PolyORB.Opaque.Chunk_Pools;
