------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . U T I L S . H T A B L E S . P E R F E C T         --
--                                                                          --
--                                 B o d y                                  --
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

--  $Id$

with Ada.Unchecked_Deallocation;
with PolyORB.Utils.Dynamic_Tables;

package body PolyORB.Utils.HTables.Perfect is

   procedure Free_Item is
     new Ada.Unchecked_Deallocation (Item, Item_Access);

   procedure Free_Table is
      new Ada.Unchecked_Deallocation (Table, Table_Access);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (T      : out Table_Instance;
      Prime  : Natural := Default_Prime;
      Max    : Natural := Default_Max)
   is
   begin
      T.T := new Table;
      Initialize (T.T.HTable, Prime, Max);
      Init (T.T.Items);
      Set_Last (T.T.Items, 15 * T.T.HTable.Info.High);
   end Initialize;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (T   : Table_Instance;
      Key : String)
   is
   begin
      Delete (T.T.HTable, Key);
   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (T     : Table_Instance;
      Key   : String;
      Value : Item)
   is
      ST_Index  : Natural;
      ST_Offset : Natural;
      To_Do     : What_To_Do;
      Temp_Index : Natural;
      Old_Last : Natural := 0;
   begin
      Insert (T.T.HTable, Key, ST_Index, ST_Offset, To_Do);
      if (To_Do = Reorder_Table) and
        (Last (T.T.HTable.Elements) > Last (T.T.Items))
      then
         Old_Last := Last (T.T.Items);
         Set_Last (T.T.Items, 15 * T.T.HTable.Info.High);
         for I in Old_Last + 1 .. Last (T.T.Items) loop
            T.T.Items.Table.all (I) := null;
         end loop;
      end if;
      if (To_Do /= Do_Nothing) then
         Temp_Index := T.T.HTable.Elements.Table.all
           (T.T.HTable.Subtables.Table.all (ST_Index).First +
            ST_Offset).Item_Index;
         if T.T.Items.Table.all (Temp_Index) = null then
            T.T.Items.Table.all (Temp_Index) := new Item'(Value);
         else
            Free_Item (T.T.Items.Table.all (Temp_Index));
            T.T.Items.Table.all (Temp_Index) := new Item'(Value);
         end if;
      end if;
   end Insert;

   --------------
   --  Lookup  --
   --------------

   function Lookup
     (T           : Table_Instance;
      Key         : String;
      Error_Value : Item)
      return Item
   is
      ST_Index   : Natural;
      ST_Offset  : Natural;
      Found       : Boolean;
      Temp_Index : Natural;
   begin
      Lookup (T.T.HTable, Key, ST_Index, ST_Offset, Found);
      if Found = True then
         Temp_Index := T.T.HTable.Elements.Table.all
           (T.T.HTable.Subtables.Table.all (ST_Index).First +
            ST_Offset).Item_Index;
         return T.T.Items.Table.all (Temp_Index).all;
      else
         return Error_Value;
      end if;
   end Lookup;

   --------------
   --  Lookup  --
   --------------

   function Lookup
     (T           : Table_Instance;
      Key         : String)
      return Item
   is
      ST_Index   : Natural;
      ST_Offset  : Natural;
      Found       : Boolean;
      Temp_Index : Natural;
   begin
      Lookup (T.T.HTable, Key, ST_Index, ST_Offset, Found);
      if Found = True then
         Temp_Index := T.T.HTable.Elements.Table.all
           (T.T.HTable.Subtables.Table.all (ST_Index).First +
            ST_Offset).Item_Index;
         return T.T.Items.Table.all (Temp_Index).all;
      else
         raise No_Key;
      end if;
   end Lookup;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize
     (T : in out Table_Instance)
   is
   begin
      Finalize (T.T.HTable);
      Deallocate (T.T.Items);
      Free_Table (T.T);
   end Finalize;

end PolyORB.Utils.HTables.Perfect;
