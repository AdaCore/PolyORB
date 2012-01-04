------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . S T R I N G S . L I S T S           --
--                                                                          --
--                                 B o d y                                  --
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

--  Generic chained list

package body PolyORB.Utils.Strings.Lists is

   -----------
   -- Empty --
   -----------

   function Empty return List is
      Empty_List : List;
   begin
      return Empty_List;
   end Empty;

   -----------
   -- First --
   -----------

   function First (L : List) return Iterator is
   begin
      return Iterator (String_Ptr_Lists.Iterator'(First (L)));
   end First;

   -----------
   -- Value --
   -----------

   function Value (I : Iterator) return String_Ptr is
   begin
      return Value (I).all;
   end Value;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (L : in out List; I : String) is
   begin
      Prepend (L, new String'(I));
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append (L : in out List; I : String) is
   begin
      Append (L, new String'(I));
   end Append;

   ---------
   -- "+" --
   ---------

   function "+" (I : String) return List is
   begin
      return +new String'(I);
   end "+";

   ---------
   -- "&" --
   ---------

   function "&" (L : List; I : String) return List is
   begin
      return L & new String'(I);
   end "&";

   ----------------
   -- Deallocate --
   ----------------

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
