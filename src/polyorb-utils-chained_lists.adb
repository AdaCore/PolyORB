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

   --  Local declarations

   procedure Free is new Ada.Unchecked_Deallocation
     (Node, Node_Access);

   ------------
   -- Append --
   ------------

   procedure Append (L : in out List; I : T) is
      L_Last : Iterator := Last (L);
   begin
      Insert (L, I, Before => L_Last);
   end Append;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (L : in out Node_Access);

   procedure Deallocate (L : in out Node_Access) is
   begin
      if L /= null then
         Deallocate (L.Next);
         Free (L);
      end if;
   end Deallocate;

   procedure Deallocate (L : in out List) is
   begin
      Deallocate (L.First);
      L.Last := null;
   end Deallocate;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (L : List) return List
   is
      D : List        := Empty;
      N : Node_Access := L.First;
      P : Node_Access;
   begin
      if L = Empty then
         return D;
      end if;
      P := new Node'(Value => N.Value, Next => null);
      D.First := P;
      N := N.Next;
      while N /= null loop
         P.Next := new Node'(Value => N.Value, Next => null);
         N := N.Next;
      end loop;
      D.Last := P.Next;
      return D;
   end Duplicate;

   -------------
   -- Element --
   -------------

   function Element (L : List; Index : Natural) return Element_Access
   is
      N : Node_Access := L.First;
      C : Natural := 0;
   begin
      while N /= null loop
         if C = Index then
            return N.Value'Access;
         end if;
         C := C + 1;
         N := N.Next;
      end loop;
      raise Constraint_Error;
   end Element;

   -------------------
   -- Extract_First --
   -------------------

   procedure Extract_First
     (L      : in out List;
      Result : out T)
   is
      First : Node_Access := L.First;
   begin
      if First = null then
         raise Constraint_Error;
      end if;

      L.First := First.Next;
      if L.First = null then
         L.Last := null;
      end if;

      Result := First.Value;
      Free (First);
   end Extract_First;

   -----------
   -- First --
   -----------

   function First (L : List) return Iterator is
   begin
      return Iterator'(Current => L.First, Previous => null);
   end First;

   -----------
   -- First --
   -----------

   function First (I : Iterator) return Boolean is
   begin
      return I.Previous = null;
   end First;

   ------------
   -- Insert --
   ------------

   procedure Insert (L : in out List; I : T; Before : in out Iterator)
   is
      N : constant Node_Access
        := new Node'(Next => Before.Current, Value => I);
   begin
      if Before.Previous = null then
         L.First := N;
      else
         Before.Previous.Next := N;
      end if;
      Before.Previous := N;

      if Before.Current = null then
         L.Last := N;
      end if;
   end Insert;

   ----------
   -- Last --
   ----------

   function Last (L : List) return Iterator is
   begin
      return Iterator'(Current => null, Previous => L.Last);
   end Last;

   ----------
   -- Last --
   ----------

   function Last (I : Iterator) return Boolean is
   begin
      return I.Current = null;
   end Last;

   ------------
   -- Length --
   ------------

   function Length (L : List) return Natural is
      N : Node_Access := L.First;
      C : Natural := 0;
   begin
      while N /= null loop
         C := C + 1;
         N := N.Next;
      end loop;
      return C;
   end Length;

   ----------
   -- Next --
   ----------

   procedure Next (I : in out Iterator) is
   begin
      I.Previous := I.Current;
      I.Current  := I.Current.Next;
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (L : in out List; I : T) is
      L_First : Iterator := First (L);
   begin
      Insert (L, I, Before => L_First);
   end Prepend;

   ------------
   -- Remove --
   ------------

   procedure Remove (L : in out List; I : T)
   is
      It : Iterator := First (L);
   begin
      while It.Current /= null loop
         if It.Current.Value = I then
            Remove (L, It);
         else
            Next (It);
         end if;
      end loop;
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove (L : in out List; I : in out Iterator) is
      Current : Node_Access := I.Current;
   begin
      if I.Previous = null then
         L.First := Current.Next;
      else
         I.Previous.Next := Current.Next;
      end if;

      if L.Last = Current then
         L.Last := I.Previous;
      end if;

      I.Current := Current.Next;
      Free (Current);
   end Remove;

   -----------
   -- Value --
   -----------

   function Value (I : Iterator) return Element_Access is
   begin
      return I.Current.Value'Access;
   end Value;

   ---------
   -- "+" --
   ---------

   function "+" (I : T) return List is
      N : constant Node_Access := new Node'(Next => null, Value => I);
   begin
      return List'(First => N, Last => N);
   end "+";

   ---------
   -- "&" --
   ---------

   function "&" (I : T; L : List) return List is
      LL : List := L;
   begin
      Prepend (LL, I);
      return LL;
   end "&";

   function "&" (L : List; I : T) return List is
      LL : List := L;
   begin
      Append (LL, I);
      return LL;
   end "&";

   function "&" (L1, L2 : List) return List is
      LL : List := L1;
   begin
      if L1.First = null then
         return L2;
      elsif L2.First = null then
         return L1;
      end if;

      LL.Last.Next := L2.First;
      LL.Last := L2.Last;
      return LL;
   end "&";

end PolyORB.Utils.Chained_Lists;
