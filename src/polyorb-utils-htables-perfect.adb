------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . U T I L S . H T A B L E S . P E R F E C T         --
--                                                                          --
--                         I m p l e m e n t a t i o n                      --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  This package provides fonctions to use the package
--  PolyORB.Utils.HTables with a generic type. Each Item is associated with
--  an element. When hashing a key, Lookup returns this Item.

with Ada.Unchecked_Deallocation;

package body PolyORB.Utils.HTables.Perfect is

   procedure Free_Item is
     new Ada.Unchecked_Deallocation (Item, Item_Access);
   procedure Free_Item_Array is
     new Ada.Unchecked_Deallocation (Item_Array, Item_Array_Ptr);

   procedure Initialize
     (T      : out Table;
      Prime  : Natural;
      Max    : Natural)
   is
   begin
      Initialize (T.HTable, Prime, Max);
      T.Items := new Item_Array (0 .. (15 * T.HTable.Info.High));
   end Initialize;

   procedure Delete
     (T   : in out Table;
      Key : String)
   is
   begin
      Delete (T.Htable, Key);
   end Delete;

   procedure Insert
     (T     : in out Table;
      Key   : String;
      Value : Item)
   is
      ST_Index  : Natural;
      ST_Offset : Natural;
      To_Do     : What_To_Do;
      Temp_Index : Natural;
   begin
      Insert (T.HTable, Key, ST_Index, ST_Offset, To_Do);
      if To_Do = Insert_Item then
         Temp_Index := T.HTable.Elements.all
           (T.HTable.Subtables.all (ST_Index).First +
            ST_Offset).Item_Index;
         if T.Items.all (Temp_Index) = null then
            T.Items.all (Temp_Index) := new Item'(Value);
         else
            Free_Item (T.Items.all (Temp_Index));
            T.Items.all (Temp_Index) := new Item'(Value);
         end if;
      end if;
   end Insert;

   function Lookup
     (T           : Table;
      Key         : String;
      Error_Value : Item)
      return Item
   is
      ST_Index   : Natural;
      ST_Offset  : Natural;
      Found       : Boolean;
      Temp_Index : Natural;
   begin
      Lookup (T.HTable, Key, ST_Index, ST_Offset, Found);
      if Found = True then
         Temp_Index := T.HTable.Elements.all
           (T.HTable.Subtables.all (ST_Index).First +
            ST_Offset).Item_Index;
         return T.Items.all (Temp_Index).all;
      else
         return Error_Value;
      end if;
   end Lookup;

   procedure Finalize
     (T : in out Table)
   is
   begin
      Finalize (T.HTable);
      Free_Item_Array (T.Items);
   end Finalize;

end PolyORB.Utils.HTables.Perfect;
