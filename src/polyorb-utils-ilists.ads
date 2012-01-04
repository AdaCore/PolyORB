------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . I L I S T S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2009-2012, Free Software Foundation, Inc.          --
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

--  In-place chained lists

--  This unit provides a chained list data type operating on any limited type
--  that exposes Next and (for the doubly linked case) Previous pointers.
--  None of the provided list operations use any dynamic memory allocation.

pragma Ada_2005;

package PolyORB.Utils.Ilists is

   pragma Preelaborate;

   type Link_Type is (Prev, Next);

   generic
      type T (<>) is limited private;
      --  List item type

      type T_Acc is access all T;
      --  Access to list item type

      with function Link
        (X : access T; Which : Link_Type) return access T_Acc is <>;
      --  Accessor for the list pointers. For a doubly linked list, Prev and
      --  Next must be supported. For a simply linked list, only Next needs to
      --  be supported.

      Doubly_Linked : Boolean;
      --  If True, the list is doubly linked

   package Lists is
      type List is private;
      pragma Preelaborable_Initialization (List);
      --  A list of objects of type T

      type Iterator is private;
      --  Iterator over List

      procedure Append (L : in out List; X : access T);
      --  Append X to L. Note that any given object cannot be appended to more
      --  than one list (from the same instance of this unit) at a given time.

      procedure Prepend (L : in out List; X : access T);
      --  Prepend X to L. Note that any given object cannot be appended to more
      --  than one list (from the same instance of this unit) at a given time.

      procedure Remove (L : in out List; It : in out Iterator);
      --  Remove the element denoted by L and advance It to the next element
      --  in L.

      procedure Remove_Element (L : in out List; X : access T);
      --  Remove X from L (L needs to be doubly linked)

      procedure Remove_Element (L : in out List; X : access T; PX : access T);
      --  Remove X from L (L may be simply linked). PX is the previous element
      --  in L.

      function First (L : List) return Iterator;
      --  Return an iterator denoting the first element of L

      function Last (L : List) return Iterator;
      --  Return an iterator denoting a position past the last element of L

      procedure Next (It : in out Iterator);
      --  Advance It to the next element in its list

      function Last (It : Iterator) return Boolean;
      --  True when It is past the last element in its list

      function Value (It : Iterator) return T_Acc;
      --  Return the element at It

      function Length (L : List) return Natural;
      --  Return the length of L

      function Is_Empty (L : List) return Boolean;
      --  True when L has no elements

   private
      pragma Inline (First);
      pragma Inline (Next);
      pragma Inline (Last);
      pragma Inline (Value);
      pragma Inline (Length);
      pragma Inline (Is_Empty);

      type Iterator is new T_Acc;

      type List is record
         First, Last : T_Acc;
         Length      : Natural := 0;
      end record;

   end Lists;

end PolyORB.Utils.Ilists;
