------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--             B R O C A . O P A Q U E . C H U N K _ P O O L S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--         Copyright (C) 1999, 2000 ENST Paris University, France.          --
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

with Ada.Unchecked_Deallocation;

package body Broca.Opaque.Chunk_Pools is

   procedure Allocate
     (Pool    : access Pool_Type;
      A_Chunk : out Chunk_Access;
      Size    : Positive_Index_Type
        := Default_Chunk_Size)
   is
      Allocation_Size : Positive_Index_Type;
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
         --  XXX Debug
         New_Chunk.Data := (others => 16#aa#);
         Pool.Prealloc_Used := True;
      else
         New_Chunk := new Chunk'(Size => Allocation_Size,
                                 Next => null,
                                 Data => (others => 176),
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

      --  XXX DEBUG.
      A_Chunk := New_Chunk;
   end Allocate;

   function Chunk_Storage
     (A_Chunk : Chunk_Access)
     return Opaque_Pointer is
   begin
      return A_Chunk.Data (A_Chunk.Data'First)'Address;
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

end Broca.Opaque.Chunk_Pools;

