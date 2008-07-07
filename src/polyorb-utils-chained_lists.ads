------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . C H A I N E D _ L I S T S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
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

--  Generic chained list

generic
   type T is private;
   with function "=" (X, Y : T) return Boolean is <>;
   Doubly_Chained : Boolean := False;
package PolyORB.Utils.Chained_Lists is

   pragma Preelaborate;

   type List is private;
   type Iterator is private;
   type Element_Access is access all T;

   function Length (L : List) return Natural;
   --  Return the number of elements in L

   function Element (L : List; Index : Natural) return Element_Access;
   --  Return the element at position Index (0-based) in L

   procedure Extract_First
     (L      : in out List;
      Result : out T);
   --  Return the first element of L into Result, and remove if from L

   function First (L : List) return Iterator;
   --  Return an iterator on L positioned at L's first element. If L is empty,
   --  returns the same value as Last (L).

   function Value (I : Iterator) return Element_Access;
   --  Return an access to the value of the element designated by I

   procedure Next (I : in out Iterator);
   --  Move I to the next element in the list

   function Last (L : List) return Iterator;
   --  Return an iterator position at the end of L (i.e. immediately after the
   --  last element in L; this iterator has no associated value).

   function Last (I : Iterator) return Boolean;
   --  True when I is positioned at the end of L (i.e. after the last element)

   procedure Prepend (L : in out List; I : T);
   --  Prepend value I at the beginning of L

   procedure Append (L : in out List; I : T);
   --  Append value I at the end of L

   procedure Insert (L : in out List; I : T; Before : in out Iterator);
   --  Insert I into L before the designated position.
   --  If Before is not either First (L) or Last (L), then the list must be
   --  be doubly linked (else Program_Error is raised).

   procedure Remove (L : in out List; I : in out Iterator);
   --  Remove the item designated by I from L, and advance I to the next item
   --  in L. This procedure can be used only if Doubly_Chained is True (else
   --  Program_Error is raised).

   generic
      with function Predicate (X : T) return Boolean;
   procedure Remove_G (L : in out List; All_Occurrences : Boolean := True);
   --  Remove from L items for which Predicate is True. If All_Occurrences is
   --  True, remove all such items, else only the first such item (if any).

   procedure Remove
     (L : in out List; I : T; All_Occurrences : Boolean := True);
   --  Remove first/all occurences of value I from list L

   function Is_Empty (L : List) return Boolean;
   --  True iff L contains no elements

   Empty : constant List;
   --  A list that contains no elements

   --  NOTE: No guarantee is made that a list for which Is_Empty is true is
   --  equal to Empty.

   function "+" (I : T) return List;
   --  Make a list with I as its only element

   function "&" (L : List; I : T) return List;
   --  Append I to L and return L

   function Duplicate (L : List) return List;
   --  Return a copy of list L

   procedure Deallocate (L : in out List);
   --  Release the storage associated with L

private
   pragma Inline (First);
   pragma Inline (Value);
   pragma Inline (Last);
   pragma Inline (Next);
   pragma Inline (Prepend);
   pragma Inline (Append);
   pragma Inline (Insert);
   pragma Inline ("+");
   pragma Inline ("&");

   type Node;
   type Node_Access is access all Node;

   --  For simply chained lists, we only have one Next pointer in each node;
   --  for doubly chained lists, we have Next and Prev.

   type Node_Chain is array (False .. Doubly_Chained) of Node_Access;
   pragma Suppress (Index_Check, Node_Chain);
   --  No index checks are required for accesses to Node_Chain, because all
   --  all cases of access to Node_Chain (Prev) are correctly protected by an
   --  'if Doubly_Chained' condition.

   Next_Node : constant Boolean := False;
   Prev_Node : constant Boolean := True;

   type Node is limited record
      Value : aliased T;
      --  Value associated with this list node

      Chain : Node_Chain;
      --  Next and optional Prev nodes.
      --  Note that all accesses to Chain (Prev_Node) must be protected by an
      --  'if Doubly_Chained' conditition.
   end record;

   type Iterator is record
     Current : Node_Access;
   end record;

   type List is record
      First, Last : Node_Access;
   end record;

   Empty : constant List := (null, null);

end PolyORB.Utils.Chained_Lists;
