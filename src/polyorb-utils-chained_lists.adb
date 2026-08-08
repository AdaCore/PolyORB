------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . C H A I N E D _ L I S T S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  Generic chained list

with PolyORB.Utils.Unchecked_Deallocation;

package body PolyORB.Utils.Chained_Lists is

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Node,
      Name => Node_Access);

   ---------
   -- "+" --
   ---------

   function "+" (I : T) return List is
   begin
      return Result : List do
         Append (Result, Node_Access'(new Node'(Value => I, others => <>)));
      end return;
   end "+";

   ---------
   -- "&" --
   ---------

   function "&" (L : List; I : T) return List is
   begin
      return LL : List := L do
         Append (LL, I);
      end return;
   end "&";

   ------------
   -- Append --
   ------------

   procedure Append (L : in out List; I : T) is
   begin
      Append (L, Node_Access'(new Node'(Value => I, others => <>)));
   end Append;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (L : in out List) is
      It : Iterator := First (L);
   begin
      while not Last (It) loop
         Remove (L, It);
      end loop;
   end Deallocate;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (L : List) return List is
      D : List;
      --  New list

      It : Iterator := First (L);
      --  Iterator on original list
   begin
      while not Last (It) loop
         Append (D, Node_Access'(new Node'(Value  => Value (It).Value,
                                           others => <>)));
         Next (It);
      end loop;
      return D;
   end Duplicate;

   -------------
   -- Element --
   -------------

   function Element (L : List; Index : Natural) return Element_Access is
      It : Iterator := First (L);
      C  : Natural  := 0;
   begin
      while not Last (It) loop
         if C = Index then
            return Value (It).Value'Access;
         end if;
         C := C + 1;
         Next (It);
      end loop;
      raise Constraint_Error;
   end Element;

   -----------
   -- Empty --
   -----------

   function Empty return List is
      Empty_List : List;
   begin
      return Empty_List;
   end Empty;

   -------------------
   -- Extract_First --
   -------------------

   procedure Extract_First (L : in out List; Result : out T) is
      It : Iterator := First (L);
   begin
      if Last (It) then
         raise Constraint_Error;
      end if;
      Result := Value (It).Value;
      Remove (L, It);
   end Extract_First;

   -----------
   -- First --
   -----------

   function First (L : List) return Iterator is
   begin
      return First (Node_Lists.List (L));
   end First;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (L : List) return Boolean is
   begin
      return Node_Lists.Is_Empty (Node_Lists.List (L));
   end Is_Empty;

   ----------
   -- Last --
   ----------

   function Last (L : List) return Iterator is
   begin
      return Iterator (Node_Lists.Last (Node_Lists.List (L)));
   end Last;

   ----------
   -- Last --
   ----------

   overriding function Last (I : Iterator) return Boolean is
   begin
      return Node_Lists.Last (Node_Lists.Iterator (I));
   end Last;

   ------------
   -- Length --
   ------------

   overriding function Length (L : List) return Natural is
   begin
      return Node_Lists.Length (Node_Lists.List (L));
   end Length;

   ----------
   -- Link --
   ----------

   function Link
     (N     : access Node;
      Which : Ilists.Link_Type) return access Node_Access
   is
   begin
      return N.Links (Which)'Unchecked_Access;
   end Link;

   ----------
   -- Next --
   ----------

   overriding procedure Next (I : in out Iterator) is
   begin
      Node_Lists.Next (Node_Lists.Iterator (I));
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (L : in out List; I : T) is
   begin
      Prepend (L, Node_Access'(new Node'(Value => I, others => <>)));
   end Prepend;

   --------------
   -- Remove_G --
   --------------

   procedure Remove_G (L : in out List; All_Occurrences : Boolean := True) is
      Item, Prev_Item : Node_Access;
      Iter : Iterator;
   begin
      Iter := First (L);
      Prev_Item := null;

      All_Items :
      while not Last (Iter) loop
         Item := Value (Iter);
         if Predicate (Item.Value) then
            Next (Iter);
            Remove_Element (L, Item, Prev_Item);
            Free (Item);
            exit All_Items when not All_Occurrences;

         else
            Prev_Item := Value (Iter);
            Next (Iter);
         end if;
      end loop All_Items;
   end Remove_G;

   ------------
   -- Remove --
   ------------

   procedure Remove (L : in out List; I : in out Iterator) is
      N : Node_Access := Value (I);
   begin
      Node_Lists.Remove (Node_Lists.List (L), Node_Lists.Iterator (I));
      Free (N);
   end Remove;

   ------------------------
   -- Remove_Occurrences --
   ------------------------

   procedure Remove_Occurrences
     (L : in out List; I : T; All_Occurrences : Boolean := True)
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
   end Remove_Occurrences;

   -----------
   -- Value --
   -----------

   function Value (I : Iterator) return Element_Access is
   begin
      return Value (I).Value'Unchecked_Access;
   end Value;

end PolyORB.Utils.Chained_Lists;
