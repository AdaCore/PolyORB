------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . C H A I N E D _ L I S T S           --
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

--  Generic chained list.

--  $Id$

with Ada.Unchecked_Deallocation;

package body PolyORB.Utils.Chained_Lists is

   function First (L : List) return Iterator is
   begin
      return Iterator (L);
   end First;

   function Element (I : Iterator) return Element_Access is
   begin
      return I.Value;
   end Element;

   function Last (I : Iterator) return Boolean is
   begin
      return I = null;
   end Last;

   procedure Next (I : in out Iterator) is
   begin
      I := Iterator (I.Next);
   end Next;

   procedure Prepend
     (L : in out List;
      I : T) is
   begin
      L := new Node'(Next => L, Value => new T'(I));
   end Prepend;

   procedure Append
     (L : in out List;
      I : T)
   is
      N : constant List
        := new Node'(Next => null, Value => new T'(I));
      Prev : List := L;
   begin
      if L = null then
         L := N;
      else
         while Prev.Next /= null loop
            Prev := Prev.Next;
         end loop;
         Prev.Next := N;
      end if;
   end Append;

   procedure Free is new Ada.Unchecked_Deallocation
     (T, Element_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (Node, List);

   procedure Deallocate (L : in out List) is
   begin
      if L /= null then
         Deallocate (L.Next);
         Free (L.Value);
         Free (L);
      end if;
   end Deallocate;

end PolyORB.Utils.Chained_Lists;
