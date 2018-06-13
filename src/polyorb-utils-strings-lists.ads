------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . S T R I N G S . L I S T S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Generic chained list.

with PolyORB.Utils.Chained_Lists;

package PolyORB.Utils.Strings.Lists is

   pragma Preelaborate;

   package String_Ptr_Lists is new PolyORB.Utils.Chained_Lists (String_Ptr);

   type List is new String_Ptr_Lists.List;
   type Iterator is new String_Ptr_Lists.Iterator;
   overriding function Empty return List;

   function First (L : List) return Iterator;
   function Value (I : Iterator) return String_Ptr;
   procedure Prepend (L : in out List; I : String);
   procedure Append (L : in out List; I : String);

   function "+" (I : String) return List;
   --  Make a list with I as its only element

   function "&" (L : List; I : String) return List;
   --  Append I to L

   overriding procedure Deallocate (L : in out List);

private

   pragma Inline (Empty);
   pragma Inline (First);
   pragma Inline (Value);
   pragma Inline (Prepend);
   pragma Inline (Append);
   pragma Inline ("+");
   pragma Inline ("&");

end PolyORB.Utils.Strings.Lists;
