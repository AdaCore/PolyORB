------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . S T R I N G S . L I S T S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2002 Free Software Fundation              --
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

package body PolyORB.Utils.Strings.Lists is

   function First (L : List) return Iterator is
   begin
      return Iterator (String_Ptr_Lists.Iterator'(First (L)));
   end First;

   function Value (I : Iterator) return String_Ptr is
   begin
      return Value (I).all;
   end Value;

   procedure Prepend (L : in out List; I : String) is
   begin
      Prepend (L, new String'(I));
   end Prepend;

   procedure Append (L : in out List; I : String) is
   begin
      Append (L, new String'(I));
   end Append;

   function "+" (I : String) return List is
   begin
      return +new String'(I);
   end "+";

   function "&" (I : String; L : List) return List is
   begin
      return new String'(I) & L;
   end "&";

   function "&" (L : List; I : String) return List is
   begin
      return L & new String'(I);
   end "&";

   procedure Deallocate (L : in out List) is
      I : Iterator := First (L);
   begin
      while not Last (I) loop
         Free (Value (I).all);
         Next (I);
      end loop;
      String_Ptr_Lists.Deallocate
        (String_Ptr_Lists.List (L));
   end Deallocate;

end PolyORB.Utils.Strings.Lists;
