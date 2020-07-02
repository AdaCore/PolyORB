------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . C H A I N E D _ L I S T S           --
--                                                                          --
--                                 S p e c                                  --
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

--  Generic chained list of nonlimited values with dynamic allocation

pragma Ada_2012;

with PolyORB.Utils.Ilists;

generic
   type T is private;
   with function "=" (X, Y : T) return Boolean is <>;
   Doubly_Chained : Boolean := False;
package PolyORB.Utils.Chained_Lists is

   pragma Preelaborate;

   type List is private;
   --  pragma Preelaborable_Initialization (List);
   --  WAG:61
   --  Compiler fails to note that a type derived from a private type with
   --  preelaborable initialization also has.

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

   function Last (L : List) return Iterator;
   --  Return an iterator on L positioned past L's last element

   function Value (I : Iterator) return Element_Access;
   --  Return an access to the value of the element designated by I

   procedure Next (I : in out Iterator);
   --  Move I to the next element in the list

   function Last (I : Iterator) return Boolean;
   --  True when I is positioned at the end of L (i.e. after the last element)

   procedure Prepend (L : in out List; I : T);
   --  Prepend value I at the beginning of L

   procedure Append (L : in out List; I : T);
   --  Append value I at the end of L

   procedure Remove (L : in out List; I : in out Iterator);
   --  Remove the item designated by I from L, and advance I to the next item
   --  in L. This procedure can be used only if Doubly_Chained is True (else
   --  Program_Error is raised).

   generic
      with function Predicate (X : T) return Boolean;
   procedure Remove_G (L : in out List; All_Occurrences : Boolean := True);
   --  Remove from L items for which Predicate is True. If All_Occurrences is
   --  True, remove all such items, else only the first such item (if any).

   procedure Remove_Occurrences
     (L : in out List; I : T; All_Occurrences : Boolean := True);
   --  Remove first/all occurences of value I from list L

   function Is_Empty (L : List) return Boolean;
   --  True iff L contains no elements

   function "+" (I : T) return List;
   --  Make a list with I as its only element

   function "&" (L : List; I : T) return List;
   --  Append I to L and return L

   function Duplicate (L : List) return List;
   --  Return a copy of list L

   procedure Deallocate (L : in out List);
   --  Release the storage associated with L

   function Empty return List;

private
   pragma Inline (First);
   pragma Inline (Value);
   pragma Inline (Last);
   pragma Inline (Next);
   pragma Inline (Prepend);
   pragma Inline (Append);
   pragma Inline (Empty);
   pragma Inline (Remove);
   pragma Inline ("+");
   pragma Inline ("&");

   type Node;
   type Node_Access is access all Node;

   type Links_Type is
     array (Ilists.Link_Type range
       Ilists.Link_Type'(if Doubly_Chained then Ilists.Prev else Ilists.Next)
         .. Ilists.Next)
       of aliased Node_Access;
   --  If Doubly_Chained, Links_Type has indices Prev and Next, else just Next

   type Node is limited record
      Value : aliased T;
      --  Value associated with this list node

      Links : Links_Type;
      --  Next and (optional) Prev nodes.
   end record;

   function Link
     (N     : access Node;
      Which : Ilists.Link_Type) return access Node_Access;
   pragma Inline (Link);
   --  Accessor for Links

   package Node_Lists is new Ilists.Lists
     (T             => Node,
      T_Acc         => Node_Access,
      Doubly_Linked => Doubly_Chained);

   type List is new Node_Lists.List;
   type Iterator is new Node_Lists.Iterator;

end PolyORB.Utils.Chained_Lists;
