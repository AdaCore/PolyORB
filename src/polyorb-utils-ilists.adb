------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . I L I S T S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2009, Free Software Foundation, Inc.             --
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

package body PolyORB.Utils.Ilists is

   package body Lists is

      function "+" (X : access T) return T_Acc;
      pragma Inline ("+");
      --  Convert X to a general access type, bypassing accessibility checks

      function Link (X : access T; Which : Link_Type) return T_Acc;
      procedure Set_Link (X : access T; Which : Link_Type; To : T_Acc);
      pragma Inline (Link);
      pragma Inline (Set_Link);
      --  Short-hand notation for read and write usage of the Link accessor

      ----------
      -- Link --
      ----------

      function Link (X : access T; Which : Link_Type) return T_Acc is
      begin
         return Link (X, Which).all;
      end Link;

      --------------
      -- Set_Link --
      --------------

      procedure Set_Link (X : access T; Which : Link_Type; To : T_Acc) is
      begin
         Link (X, Which).all := To;
      end Set_Link;

      ---------
      -- "+" --
      ---------

      function "+" (X : access T) return T_Acc is
         pragma Suppress (Access_Check);
      begin
         return X.all'Unchecked_Access;
      end "+";

      ------------
      -- Append --
      ------------

      procedure Append (L : in out List; X : access T) is
      begin
         if L.Last /= null then
            if Doubly_Linked then
               Set_Link (X, Prev, L.Last);
            end if;
            Set_Link (L.Last, Next, +X);
         end if;
         L.Last := +X;
         if L.First = null then
            L.First := +X;
         end if;
         L.Length := L.Length + 1;
      end Append;

      -----------
      -- First --
      -----------

      function First (L : List) return Iterator is
      begin
         return Iterator (L.First);
      end First;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (L : List) return Boolean is
      begin
         return L.Length = 0;
      end Is_Empty;

      ----------
      -- Last --
      ----------

      function Last (L : List) return Iterator is
         pragma Unreferenced (L);
      begin
         return null;
      end Last;

      ----------
      -- Last --
      ----------

      function Last (It : Iterator) return Boolean is
      begin
         return It = null;
      end Last;

      ------------
      -- Length --
      ------------

      function Length (L : List) return Natural is
      begin
         return L.Length;
      end Length;

      ----------
      -- Next --
      ----------

      procedure Next (It : in out Iterator) is
      begin
         It := Iterator (T_Acc'(Link (It, Next)));
      end Next;

      -------------
      -- Prepend --
      -------------

      procedure Prepend (L : in out List; X : access T) is
      begin
         Set_Link (X, Next, L.First);
         if Doubly_Linked and then L.First /= null then
            Set_Link (L.First, Prev, +X);
         end if;

         L.First := +X;
         if L.Last = null then
            L.Last := +X;
         end if;
         L.Length := L.Length + 1;
      end Prepend;

      ------------
      -- Remove --
      ------------

      procedure Remove (L : in out List; It : in out Iterator) is
         Element : constant access T := It;
      begin
         Next (It);
         Remove_Element (L, Element);
      end Remove;

      --------------------
      -- Remove_Element --
      --------------------

      procedure Remove_Element (L : in out List; X : access T) is
         Previous : T_Acc;
      begin
         if Doubly_Linked then
            Previous := Link (X, Prev);
         else
            pragma Assert (X = L.First);
            Previous := null;
         end if;
         Remove_Element (L, X => X, PX => Previous);
      end Remove_Element;

      --------------------
      -- Remove_Element --
      --------------------

      procedure Remove_Element
        (L  : in out List;
         X  : access T;
         PX : access T)
      is
      begin
         if PX = null then
            L.First := Link (X, Next);
         else
            Set_Link (PX, Next, Link (X, Next));
         end if;

         if L.Last = X then
            L.Last := +PX;
         end if;

         if Doubly_Linked then
            if Link (X, Next) /= T_Acc'(null) then
               Set_Link (Link (X, Next), Prev, +PX);
            end if;
            Set_Link (X, Prev, null);
         end if;

         Set_Link (X, Next, null);
         L.Length := L.Length - 1;
      end Remove_Element;

      -----------
      -- Value --
      -----------

      function Value (It : Iterator) return T_Acc is
      begin
         return T_Acc (It);
      end Value;

   end Lists;

end PolyORB.Utils.Ilists;
