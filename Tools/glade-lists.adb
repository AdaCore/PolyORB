------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                          G L A D E . L I S T S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body GLADE.Lists is

   procedure Free is new Ada.Unchecked_Deallocation (Item_Node, Item_List);

   ------------
   -- Append --
   ------------

   procedure Append (L : List; I : Item) is
   begin
      if L.Head = null then
         L.Head  := new Item_Node'(I, null);
         L.Tail  := L.Head;
         L.Index := L.Head;
      else
         L.Tail.Next  := new Item_Node'(I, null);
         L.Tail       := L.Tail.Next;
      end if;
   end Append;

   ------------
   -- Create --
   ------------

   function Create return List is
   begin
      return new List_Type'(null, null, null);
   end Create;

   ----------
   -- Head --
   ----------

   procedure Head (L : List) is
   begin
      L.Index := null;
   end Head;

   ------------
   -- Modify --
   ------------

   procedure Modify (L : List; I : Item) is
   begin
      L.Index.Data := I;
   end Modify;

   ----------
   -- Next --
   ----------

   function Next (L : List) return Boolean is
   begin
      if L.Index = null then
         L.Index := L.Head;
      else
         L.Index := L.Index.Next;
      end if;
      return L.Index /= null;
   end Next;

   ----------
   -- Read --
   ----------

   procedure Read (L : List; I : out Item) is
   begin
      I := L.Index.Data;
   end Read;

   ------------
   -- Remove --
   ------------

   procedure Remove (L : List; I : Item) is
      N, C : Item_List;
   begin
      if L.Head.Data = I then
         C := L.Head;
         L.Head := L.Head.Next;
         if L.Head = null then
            L.Tail := null;
         end if;
         Free (C);
      else
         C := L.Head;
         while C.Next /= null loop
            if C.Next.Data = I then
               N := C.Next;
               C.Next := C.Next.Next;
               Free (N);
               exit;
            else
               C := C.Next;
            end if;
         end loop;
         if C.Next = null then
            L.Tail := C;
         end if;
      end if;
      L.Index := L.Head;
   end Remove;

end GLADE.Lists;
