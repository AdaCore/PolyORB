------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . C H A I N E D _ L I S T S           --
--                                                                          --
--                                 S p e c                                  --
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

generic
   type T (<>) is private;
package PolyORB.Utils.Chained_Lists is

   pragma Elaborate_Body;

   type List is private;
   type Iterator is private;
   type Element_Access is access T;

   function Length (L : List) return Natural;
   function First (L : List) return Iterator;
   function Value (I : Iterator) return Element_Access;
   function Last (I : Iterator) return Boolean;
   procedure Next (I : in out Iterator);

   procedure Prepend
     (L : in out List;
      I : T);
   procedure Append
     (L : in out List;
      I : T);

   procedure Deallocate (L : in out List);

   pragma Inline (First);
   pragma Inline (Value);
   pragma Inline (Last);
   pragma Inline (Next);
   pragma Inline (Prepend);
   pragma Inline (Append);

private

   type Node;
   type List is access all Node;
   type Node is record
      Value : Element_Access;
      Next  : List;
   end record;

   type Iterator is new List;

end PolyORB.Utils.Chained_Lists;
