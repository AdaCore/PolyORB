------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                          G N A T . H T A B L E                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

package body GNAT.HTable is

   --------------------
   --  Static_HTable --
   --------------------

   package body Static_HTable is

      Table : array (Header_Num) of Elmt_Ptr;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         for J in Table'Range loop
            Table (J) := Null_Ptr;
         end loop;
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (E : Elmt_Ptr) is
         Index : Header_Num;

      begin
         Index := Hash (Get_Key (E));
         Set_Next (E, Table (Index));
         Table (Index) := E;
      end Set;

      ---------
      -- Get --
      ---------

      function  Get (K : Key) return Elmt_Ptr is
         Elmt  : Elmt_Ptr;

      begin
         Elmt := Table (Hash (K));

         loop
            if Elmt = Null_Ptr then
               return Null_Ptr;

            elsif Equal (Get_Key (Elmt), K) then
               return Elmt;

            else
               Elmt := Next (Elmt);
            end if;
         end loop;
      end Get;

      ------------
      -- Remove --
      ------------

      procedure Remove  (K : Key) is
         Index     : constant Header_Num := Hash (K);
         Elmt      : Elmt_Ptr;
         Next_Elmt : Elmt_Ptr;

      begin
         Elmt := Table (Index);

         if Elmt = Null_Ptr then
            return;

         elsif Equal (Get_Key (Elmt), K) then
            Table (Index) := Next (Elmt);

         else
            loop
               Next_Elmt :=  Next (Elmt);

               if Next_Elmt = Null_Ptr then
                  return;

               elsif Equal (Get_Key (Next_Elmt), K) then
                  Set_Next (Elmt, Next (Next_Elmt));
                  return;

               else
                  Elmt := Next_Elmt;
               end if;
            end loop;
         end if;
      end Remove;
   end Static_HTable;

   --------------------
   --  Simple_HTable --
   --------------------

   package body Simple_HTable is

      type Element_Wrapper;
      type Elmt_Ptr is access all Element_Wrapper;
      type Element_Wrapper is record
         K    : Key;
         E    : Element;
         Next : Elmt_Ptr;
      end record;

      procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      function  Next     (E : Elmt_Ptr) return Elmt_Ptr;
      function Get_Key   (E : Elmt_Ptr) return Key;

      package Tab is new Static_HTable (
        Header_Num => Header_Num,
        Element    => Element_Wrapper,
        Elmt_Ptr   => Elmt_Ptr,
        Null_Ptr   => null,
        Set_Next   => Set_Next,
        Next       => Next,
        Key        => Key,
        Get_Key    => Get_Key,
        Hash       => Hash,
        Equal      => Equal);

      procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr) is
      begin
         E.Next := Next;
      end Set_Next;

      function Next (E : Elmt_Ptr) return Elmt_Ptr is
      begin
         return E.Next;
      end Next;

      function Get_Key (E : Elmt_Ptr) return Key is
      begin
         return E.K;
      end Get_Key;

      procedure Set (K : Key; E : Element) is
         Tmp : Elmt_Ptr := Tab.Get (K);

      begin
         if Tmp = null then
            Tab.Set (new Element_Wrapper'(K, E, null));
         else
            Tmp.E := E;
         end if;
      end Set;

      function  Get (K : Key) return Element is
         Tmp : Elmt_Ptr := Tab.Get (K);

      begin
         if Tmp = null then
            return No_Element;
         else
            return Tmp.E;
         end if;
      end Get;
   end Simple_HTable;

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Header_Num is

      type Uns is mod 2 ** 32;

      function Rotate_Left (Value : Uns; Amount : Natural) return Uns;
      pragma Import (Intrinsic, Rotate_Left);

      Tmp : Uns := 0;

   begin
      for J in Key'Range loop
         Tmp := Rotate_Left (Tmp, 1) + Character'Pos (Key (J));
      end loop;

      return Header_Num'First +
               Header_Num'Base (Tmp mod Header_Num'Range_Length);
   end Hash;

end GNAT.HTable;
