------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . C H A I N E D _ L I S T S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Generic chained list

with Ada.Unchecked_Deallocation;

package body PolyORB.Utils.Chained_Lists is

   --  Local declarations

   procedure Remove
     (L        : in out List;
      Item     : Node_Access;
      Previous : Node_Access);
   --  Remove Item from L, where Previous is the previous item

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

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

   procedure Deallocate (L : in out List) is
      Current, Next : Node_Access;
   begin
      Current := L.First;
      while Current /= null loop
         Next := Current.Chain (Next_Node);
         Free (Current);
         Current := Next;
      end loop;
      L.First := null;
      L.Last  := null;
   end Deallocate;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (L : List) return List is
      D : List        := Empty;
      --  New list

      N : Node_Access := L.First;
      --  Iterator on original list

      P : Node_Access;
      --  Iterator on new list

   begin
      if N = null then
         return D;
      end if;
      P := new Node;
      P.Value := N.Value;
      P.Chain (Next_Node)  := null;
      if Doubly_Chained then
         P.Chain (Prev_Node)  := null;
      end if;
      D.First := P;
      loop
         N := N.Chain (Next_Node);
         exit when N = null;
         P.Chain (Next_Node) := new Node;
         P.Chain (Next_Node).Value := N.Value;
         P.Chain (Next_Node).Chain (Next_Node) := null;
         if Doubly_Chained then
            P.Chain (Next_Node).Chain (Prev_Node) := P;
         end if;
         P := P.Chain (Next_Node);
      end loop;
      D.Last := P;
      return D;
   end Duplicate;

   -------------
   -- Element --
   -------------

   function Element (L : List; Index : Natural) return Element_Access is
      N : Node_Access := L.First;
      C : Natural := 0;
   begin
      while N /= null loop
         if C = Index then
            return N.Value'Access;
         end if;
         C := C + 1;
         N := N.Chain (Next_Node);
      end loop;
      raise Constraint_Error;
   end Element;

   -------------------
   -- Extract_First --
   -------------------

   procedure Extract_First (L : in out List; Result : out T) is
   begin
      if Is_Empty (L) then
         raise Constraint_Error;
      end if;
      Result := L.First.Value;
      Remove (L, Item => L.First, Previous => null);
   end Extract_First;

   -----------
   -- First --
   -----------

   function First (L : List) return Iterator is
   begin
      return Iterator'(Current => L.First);
   end First;

   ------------
   -- Insert --
   ------------

   procedure Insert (L : in out List; I : T; Before : in out Iterator) is
      N : Node_Access;
   begin
      pragma Assert ((L.First = null) = (L.Last = null));
      N := new Node;
      N.Value := I;
      N.Chain (Next_Node) := Before.Current;
      if Doubly_Chained then
         N.Chain (Prev_Node) := null;
      end if;

      if Before.Current = L.First then

         --  Insert at first position

         L.First := N;

      elsif Before.Current = null then

         --  Insert at end of a non-empty list

         L.Last.Chain (Next_Node) := N;

      elsif Doubly_Chained then

         --  Insert in the middle of a doubly-chained list

         Before.Current.Chain (Prev_Node).Chain (Next_Node) := N;

      else

         --  Inserts in the middle of a list are only possible for
         --  doubly-chained lists.

         raise Program_Error;
      end if;

      if Before.Current = null then
         if Doubly_Chained then
            N.Chain (Prev_Node) := L.Last;
         end if;
         L.Last := N;
      elsif Doubly_Chained then
         N.Chain (Prev_Node) := Before.Current.Chain (Prev_Node);
         Before.Current.Chain (Prev_Node) := N;
      end if;
      pragma Assert ((L.First = null) = (L.Last = null));
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List) return Boolean is
   begin
      return L.First = null;
   end Is_Empty;

   ----------
   -- Last --
   ----------

   function Last (L : List) return Iterator is
      pragma Unreferenced (L);
   begin
      return Iterator'(Current => null);
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
         N := N.Chain (Next_Node);
      end loop;
      return C;
   end Length;

   ----------
   -- Next --
   ----------

   procedure Next (I : in out Iterator) is
   begin
      I.Current  := I.Current.Chain (Next_Node);
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (L : in out List; I : T) is
      L_First : Iterator := First (L);
   begin
      Insert (L, I, Before => L_First);
   end Prepend;

   --------------
   -- Remove_G --
   --------------

   procedure Remove_G (L : in out List; All_Occurrences : Boolean := True) is
      Item : Node_Access := L.First;
      Prev : Node_Access := null;
      Next : Node_Access;
   begin
      All_Items :
      while Item /= null loop
         Next := Item.Chain (Next_Node);
         if Predicate (Item.Value) then
            Remove (L, Item, Prev);
            exit All_Items when not All_Occurrences;
         else
            Prev := Item;
         end if;
         Item := Next;
      end loop All_Items;
   end Remove_G;

   ------------
   -- Remove --
   ------------

   procedure Remove (L : in out List; I : T; All_Occurrences : Boolean := True)
   is
      function Equality (X : T) return Boolean;
      --  True iff X = I

      function Equality (X : T) return Boolean is
      begin
         return X = I;
      end Equality;

      procedure Remove is new Remove_G (Equality);

   begin
      Remove (L, All_Occurrences);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (L        : in out List;
      Item     : Node_Access;
      Previous : Node_Access)
   is
      Current : Node_Access := Item;
   begin
      if Previous = null then
         L.First := Current.Chain (Next_Node);
      else
         Previous.Chain (Next_Node) := Current.Chain (Next_Node);
      end if;

      if L.Last = Current then
         L.Last := Previous;
      end if;

      if Doubly_Chained then
         if Current.Chain (Next_Node) /= null then
            Current.Chain (Next_Node).Chain (Prev_Node) := Previous;
         end if;
      end if;
      Free (Current);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove (L : in out List; I : in out Iterator) is
      Next : constant Node_Access := I.Current.Chain (Next_Node);
   begin
      if Doubly_Chained then
         Remove (L, I.Current, I.Current.Chain (Prev_Node));
         I.Current := Next;
      else
         raise Program_Error;
      end if;
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
      N : constant Node_Access := new Node;
   begin
      N.Value := I;
      N.Chain (Next_Node) := null;
      if Doubly_Chained then
         N.Chain (Prev_Node) := null;
      end if;
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

   ---------
   -- "&" --
   ---------

   function "&" (L : List; I : T) return List is
      LL : List := L;
   begin
      Append (LL, I);
      return LL;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L1, L2 : List) return List is
      LL : List := L1;
   begin
      if L1.First = null then
         return L2;
      elsif L2.First = null then
         return L1;
      end if;

      LL.Last.Chain (Next_Node) := L2.First;
      if Doubly_Chained then
         L2.First.Chain (Prev_Node) := L1.Last;
      end if;
      LL.Last := L2.Last;
      return LL;
   end "&";

end PolyORB.Utils.Chained_Lists;
