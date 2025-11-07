------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . U T I L S . H T A B L E S . P E R F E C T         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2013, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

package body PolyORB.Utils.HTables.Perfect is

   use PolyORB.Utils.Strings;

   ------------------------
   -- Utility procedures --
   ------------------------

   --  These procedures are utiliy procedures used by the Hash_Table
   --  type, some of them are defined in the Dietzfelbinger algorithm.

   procedure Add_Key_To_Subtable
     (Key      :        String;
      ST_Index :        Natural;
      T        : in out Hash_Table);
   --  Insert the Key in an unused Element in subtable ST_Index

   procedure Find_Hash_For_Subtable
     (ST_Index : Natural;
      T        : Hash_Table);
   --  Find an injective hash function associated to the subtable
   --  at position ST_Index.

   function Is_Injective
     (ST_Index : Natural;
      T        : Hash_Table)
     return Boolean;
   --  Return true iff the hash function associated to the subtable at
   --  position subtable ST_Index is injective.

   procedure Process_Subtable
     (ST_Index :        Natural;
      T        : in out Hash_Table);
   --  Find the K parameter of subtable ST_Index in order to have
   --  an injective hash function and to reorder the subtable.

   procedure Rehash_Subtable
     (ST_Index : Natural;
      T        : Hash_Table);
   --  Apply the Hashcode function to each element of subtable
   --  ST_Index, store the results in the ST_Offset component
   --  of the component.

   procedure Rehash_All
     (Key :        String;
      T   : in out Hash_Table);
   --  Reorganize all the tables.

   procedure Swap_Elements
     (T      : in out Hash_Table;
      Index1 :        Natural;
      Index2 :        Natural);
   pragma Inline (Swap_Elements);
   --  Swap elements at index1 and index2.

   procedure Free_Table is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Table,


      Name   => Table_Access);

   procedure Free_Item is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Item,


      Name   => Item_Access);

   ----------------------------------
   -- Hash_Table related functions --
   ----------------------------------

   --  These functions allows for the management of the Hash_Table
   --  index table used by the hash table.

   procedure Initialize
     (T      : out Hash_Table;
      HParam :     Hash_Parameters := Default_Hash_Parameters;
      Max    :     Natural);
   --  Initialize the hash table.
   --  'HParam' are the hash function parameters,
   --  'Max' is the max number of elements to store.

   procedure Finalize
     (T : in out Hash_Table);
   --  Finalize the Hast Table.

   type Next_Action is (Reorder_Subtable,
                        Reorder_Table,
                        Do_Nothing,
                        Insert_Item);
   --  Indicate the next action to do after a value has been inserted
   --  in the hash table index.  See the specification of the Insert
   --  procedure for more details.

   procedure Insert
     (T         : in out Hash_Table;
      Key       :        String;
      ST_Index  : out    Natural;
      ST_Offset : out    Natural;
      To_Do     : out    Next_Action);
   --  Insert key in hash table. In case of an already existing Key,
   --  Insert ignores insertion. Key is the string to hash.
   --  ST_Index corresponds to the subtable index and ST_Offset to
   --  the offset in this subtable of the inserted Key
   --  To_Do indicates if:
   --     -  a reorder of a sub-table or the table is
   --        necessary or not after the insertion (Reorder_Subtable or
   --        Reorder_Table)
   --     -  an item associated with the key can be inserted (Insert_Item)
   --     -  the key already exists (Nothing_To_Do)

   procedure Insert
     (T     : in out Hash_Table;
      Key   :        String;
      Index : out    Natural;
      To_Do : out    Next_Action);
   --  Insert 'Key' in hash table. This function is a wrapper on the
   --  previous function, it directly returns the index of the Item
   --  corresponding to 'Key'

   procedure Lookup
     (T         :     Hash_Table;
      Key       :     String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      Found     : out Boolean);
   --  Find 'Key' in hash table.
   --  If the key is 'Found', then 'ST_Index', 'ST_Offset' return the
   --  object position in the subtable.

   procedure Lookup
     (T     :     Hash_Table;
      Key   :     String;
      Index : out Natural;
      Found : out Boolean);
   --  Find 'Key' in hash table. This function is a wrapper on the
   --  previous function, it directly returns the index of the Item
   --  corresponding to 'Key'

   procedure Delete
     (T   : in out Hash_Table;
      Key :        String;
      Index : out Natural);
   --  Delete key in hash table. In case of a non-existing Key, Delete
   --  ignores deletion. Key is the string to hash.
   --  When a Key is deleted, it's not physically. Indeed it puts just
   --  the tag Used to False

   ---------------------------------------
   -- Utility procedures implementation --
   ---------------------------------------

   -------------------
   -- Swap_Elements --
   -------------------

   procedure Swap_Elements
     (T      : in out Hash_Table;
      Index1 :        Natural;
      Index2 :        Natural)
   is
      Elements : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Swap : Element;

   begin
      Swap              := Elements (Index1);
      Elements (Index1) := Elements (Index2);
      Elements (Index2) := Swap;
   end Swap_Elements;

   ---------------------
   -- Rehash_Subtable --
   ---------------------

   procedure Rehash_Subtable
     (ST_Index : Natural;
      T        : Hash_Table)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

   begin
      for J in Subtables (ST_Index).First .. Subtables (ST_Index).Last
      loop
         if Elements (J).Key /= null
           and then Elements (J).Used
         then
            Elements (J).ST_Index := ST_Index;

            Elements (J).ST_Offset :=
              Hash (Elements (J).Key.all,
                    Subtables (ST_Index).HParam,
                    Subtables (ST_Index).Max);

         end if;
      end loop;
   end Rehash_Subtable;

   ------------------
   -- Is_Injective --
   ------------------

   function Is_Injective
     (ST_Index : Natural;
      T        : Hash_Table)
     return Boolean
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

   begin
      for J in Subtables (ST_Index).First .. Subtables (ST_Index).Last - 1
      loop
         for K in J + 1 .. Subtables (ST_Index).Last loop

            if Elements (J).Used
              and then Elements (K).Used
              and then Elements (J).ST_Offset = Elements (K).ST_Offset
            then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Is_Injective;

   ----------------------------
   -- Find_Hash_For_Subtable --
   ----------------------------

   procedure Find_Hash_For_Subtable
     (ST_Index : Natural;
      T        : Hash_Table)
   is
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

   begin
      Subtables (ST_Index).HParam := Default_Hash_Parameters;

      loop
         Rehash_Subtable (ST_Index, T);

         exit when Is_Injective (ST_Index, T);

         Subtables (ST_Index).HParam :=
           Next_Hash_Parameters (Subtables (ST_Index).HParam);
      end loop;

      pragma Assert (Is_Injective (ST_Index, T));
   end Find_Hash_For_Subtable;

   -------------------------
   -- Add_Key_To_Subtable --
   -------------------------

   procedure Add_Key_To_Subtable
     (Key      :        String;
      ST_Index :        Natural;
      T        : in out Hash_Table)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

      Key_Inserted : Boolean := False;

   begin
      for J in Subtables (ST_Index).First .. Subtables (ST_Index).Last
      loop
         if Elements (J).Key = null then
            Elements (J).Key  := new String'(Key);
            Elements (J).Used := True;
            Key_Inserted := True;
            exit;

         elsif not Elements (J).Used then
            Free (Elements (J).Key);
            Elements (J).Key  := new String'(Key);
            Elements (J).Used := True;
            Key_Inserted := True;
            exit;
         end if;
      end loop;

      pragma Assert (Key_Inserted);
   end Add_Key_To_Subtable;

   ----------------
   -- Rehash_All --
   ----------------

   procedure Rehash_All
     (Key :        String;
      T   : in out Hash_Table)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

      Max_Sum     : Natural := 0;
      ST_Index    : Natural := 0;
      ST_Offset   : Natural := 0;
      E_Index     : Natural := 0;
      Index       : Natural;
      Offset      : Natural := 0;
      Swap_Index1 : Natural := 0;
      Swap_Index2 : Natural := Last (T.Elements);

   begin
      --  Add the new element at the end of the table of elements
      --  and deallocate unused element if necessary

      if Elements (Last (T.Elements)).Key /= null then
         Free (T.Elements.Table.all (Last (T.Elements)).Key);
      end if;

      Elements (Last (T.Elements)).Key := new String'(Key);
      Elements (Last (T.Elements)).Used := True;

      --  Put all the elements at the beginning of the table
      --  XXX why are we doing a swap and not a simple affectation ????

      while Swap_Index1 < Swap_Index2 loop
         if not Elements (Swap_Index1).Used then
            while not Elements (Swap_Index2).Used
              and then Swap_Index1 + 1 < Swap_Index2 loop
               Swap_Index2 := Swap_Index2 - 1;
            end loop;

            if Elements (Swap_Index2).Used
              and then Swap_Index1 < Swap_Index2
            then
               Swap_Elements (T, Swap_Index1, Swap_Index2);
            end if;

         end if;
         Swap_Index1 := Swap_Index1 + 1;
      end loop;

      --  Find a hash function for the table

      T.Info.HParam := Default_Hash_Parameters;
      loop

         --  Reinitialize the subtables paramters

         for J in First (T.Subtables) .. Last (T.Subtables) loop
            Subtables (J).Count  := 0;
            Subtables (J).HParam := Default_Hash_Parameters;
         end loop;

         --  Find the repartition of the elements among the subtables

         for J in 0 .. T.Info.Count - 1 loop
            Elements (J).ST_Index :=
              Hash (Elements (J).Key.all, T.Info.HParam, T.Info.N_Subtables);
            Subtables (Elements (J).ST_Index).Count
              := Subtables (Elements (J).ST_Index).Count + 1;

         end loop;

         --  Compute High and Max parameters for each subtables,
         --  compute Max_Sum, which serves as a break condition.

         Max_Sum := 0;

         for J in First (T.Subtables) .. Last (T.Subtables) loop

            if Subtables (J).Count = 0 then
               Subtables (J).High := 1;
               Subtables (J).Max  := 2;
            else
               Subtables (J).High := 2 * Subtables (J).Count;

               Subtables (J).Max  := 2 *
                 Subtables (J).High * (Subtables (J).High - 1);
            end if;

            Max_Sum := Max_Sum + Subtables (J).Max;
         end loop;

         --  Dietzfelbinger algorithm searches a hash function so that
         --  Max_Sum <= 32 * T.Info.High ^2 / s (T.Info.High) + 4 * T.Info.High
         --  (inequality #7 p. 5), with s (T.Info.High) the number of subsets
         --  to be created to accomodate for T.Info.High elements.

         --  Choosing s : x -> 3 * x is sufficient to ensure linear
         --  memory usage, thus the following inequality :

         exit when Max_Sum <= 44 * T.Info.High / 3;

         T.Info.HParam := Next_Hash_Parameters (T.Info.HParam);

      end loop;

      --  Compute boundaries of the different subtables.

      Index := 0;
      for J in First (T.Subtables) .. Last (T.Subtables) loop
         Subtables (J).First := Index;
         Index := Index + Subtables (J).Max;
         Subtables (J).Last  := Index - 1;
      end loop;

      --  Reorder the elements

      for J in 0 .. T.Info.Count - 1 loop

         ST_Index := Elements (J).ST_Index;
         ST_Offset := Hash
           (Elements (J).Key.all,
            Subtables (ST_Index).HParam,
            Subtables (ST_Index).Max);

         Elements (J).ST_Offset := ST_Offset;
         E_Index := ST_Offset + Subtables (ST_Index).First;

         if (J /= E_Index and then Subtables (ST_Index).Count = 1)
           or else J < Subtables (ST_Index).First
           or else J > Subtables (ST_Index).Last
         then
            while ((J /= E_Index
                    and then Subtables (ST_Index).Count = 1)
                   or else J < Subtables (ST_Index).First
                   or else J > Subtables (ST_Index).Last)
              and then Elements (J).Used
            loop
               if Subtables (ST_Index).Count = 1 then
                  Swap_Elements (T, J, E_Index);

                  ST_Index := Elements (J).ST_Index;

                  if Elements (J).Used then
                     ST_Offset := Hash
                       (Elements (J).Key.all,
                        Subtables (ST_Index).HParam,
                        Subtables (ST_Index).Max);
                  end if;

                  Elements (J).ST_Offset := ST_Offset;
                  E_Index := ST_Offset + Subtables (ST_Index).First;

               else
                  Offset := Subtables (ST_Index).First;
                  while Elements (Offset).Used
                    and then ST_Index = Elements (Offset).ST_Index
                  loop
                     Offset := Offset + 1;
                  end loop;

                  Swap_Elements (T, J, Offset);

                  ST_Index := Elements (J).ST_Index;
                  if Elements (J).Used then
                     ST_Offset := Hash
                       (Elements (J).Key.all,
                        Subtables (ST_Index).HParam,
                        Subtables (ST_Index).Max);
                  end if;
                  Elements (J).ST_Offset := ST_Offset;
                  E_Index := ST_Offset + Subtables (ST_Index).First;
               end if;

            end loop;
         end if;
      end loop;

      --  Apply the Process_Subtable procedure to all the subtables
      --  that have more than two elements. Otherwise we can directly
      --  use the default parameters.

      for J in First (T.Subtables) .. Last (T.Subtables) loop
         if Subtables (J).Count > 1 then
            Process_Subtable (J, T);
         elsif Subtables (J).Count = 0 then
            Subtables (J).HParam := Default_Hash_Parameters;
         end if;
      end loop;

   end Rehash_All;

   ----------------------
   -- Process_Subtable --
   ----------------------

   procedure Process_Subtable
     (ST_Index :        Natural;
      T        : in out Hash_Table)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

      Offset : Natural;

   begin
      --  Find Hash parameter for this subtable.
      Find_Hash_For_Subtable (ST_Index, T);

      --  Reorder subtable.
      for J in Subtables (ST_Index).First .. Subtables (ST_Index).Last
      loop
         Offset := Subtables (ST_Index).First + Elements (J).ST_Offset;

         while Elements (J).Used
           and then J /= Offset loop

            if J /= Offset then
               Swap_Elements (T, J, Offset);
            end if;

            Offset := Subtables (ST_Index).First + Elements (J).ST_Offset;
         end loop;
      end loop;

   end Process_Subtable;

   ----------------------------------
   -- Hash_Table related functions --
   ----------------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (T      : out Hash_Table;
      HParam :     Hash_Parameters := Default_Hash_Parameters;
      Max    :     Natural)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

   begin
      --  Initialization of T.Info

      T.Info.Count        := 0;
      T.Info.High         := Max + (Max + 9) / 10;
      T.Info.N_Subtables  := T.Info.High * 3;
      T.Info.HParam       := HParam;

      --  Allocation of T.Elements

      Initialize (T.Elements);
      Dynamic_Element_Array.Set_Last (T.Elements, 15 * T.Info.High);
      for J in First (T.Elements) .. Last (T.Elements) loop
         Elements (J) := Empty;
         Elements (J).Item_Index := J;
      end loop;

      --  Allocation of T.Subtables

      Initialize (T.Subtables);
      Set_Last (T.Subtables, T.Info.N_Subtables - 1);

      for J in First (T.Subtables) .. Last (T.Subtables) loop
         Subtables (J).First  := 2 * J;
         Subtables (J).Last   := 2 * J + 1;
         Subtables (J).Count  := 0;
         Subtables (J).High   := 1;
         Subtables (J).Max    := 2;
         Subtables (J).HParam := HParam;
      end loop;

   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out Hash_Table)
   is
      Elements : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;

   begin
      for J in First (T.Elements) .. Last (T.Elements) loop
         if Elements (J).Key /= null then
            Free (Elements (J).Key);
         end if;
      end loop;

      Deallocate (T.Subtables);
      Deallocate (T.Elements);
   end Finalize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (T         : in out Hash_Table;
      Key       :        String;
      ST_Index  : out    Natural;
      ST_Offset : out    Natural;
      To_Do     : out    Next_Action)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

      Found       : Boolean;
      Temp_Index  : Natural;
      Old_Last    : Natural;

   begin
      if T.Info.Count = T.Info.High then

         --  Extend the table and Rehash_All,

         Old_Last           := Last (T.Elements);
         T.Info.Count       := T.Info.Count + 1;
         T.Info.High        := T.Info.Count + (T.Info.Count + 1) / 2;
         T.Info.N_Subtables := T.Info.High * 3;
         Set_Last (T.Elements, 15 * T.Info.High);

         for J in Old_Last + 1 .. Last (T.Elements) loop
            Elements (J) := Empty;
            Elements (J).Item_Index := J;
         end loop;

         Set_Last (T.Subtables, T.Info.N_Subtables - 1);
         Rehash_All (Key, T);

         Lookup (T, Key, ST_Index, ST_Offset, Found);
         pragma Assert (Found);

         To_Do := Reorder_Table;

      else

         --  ... else search if the key is already in the table.

         Lookup (T, Key, ST_Index, ST_Offset, Found);

         if Found then

            --  If key in table and is used, don't insert
            To_Do := Do_Nothing;

         else

            --  Temp_Index will be the position of the new key,
            --  unless we need to reorganize the tables

            Temp_Index := Subtables (ST_Index).First + ST_Offset;

            T.Info.Count := T.Info.Count + 1;
            Subtables (ST_Index).Count := Subtables (ST_Index).Count + 1;

            if Subtables (ST_Index).Count > Subtables (ST_Index).High then

               --  If Count > High, Rehash_all

               Rehash_All (Key, T);

               Lookup (T, Key, ST_Index, ST_Offset, Found);
               pragma Assert (Found);

               To_Do := Reorder_Table;

            elsif Elements (Temp_Index).Key = null then

               --  If the positon is empty insert directly

               Elements (Temp_Index).Key       := new String'(Key);
               Elements (Temp_Index).Used      := True;
               Elements (Temp_Index).ST_Index  := ST_Index;
               Elements (Temp_Index).ST_Offset := ST_Offset;
               To_Do := Insert_Item;

            elsif Elements (Temp_Index).Key.all = Key
              and then not Elements (Temp_Index).Used
            then

               --  If the position contains the same key but unused
               --  just change the flag Used.

               Elements (Temp_Index).Used := True;
               To_Do := Insert_Item;

            elsif not Elements (Temp_Index).Used then

               --  If the position contains a key that is unused,
               --  deallocate the string and then insert the new key

               Free (Elements (Temp_Index).Key);
               Elements (Temp_Index).Key       := new String'(Key);
               Elements (Temp_Index).Used      := True;
               Elements (Temp_Index).ST_Index  := ST_Index;
               Elements (Temp_Index).ST_Offset := ST_Offset;
               To_Do := Insert_Item;

            else

               --  Worst case -> reorganize the subtable

               Add_Key_To_Subtable (Key, ST_Index, T);
               Process_Subtable (ST_Index, T);

               Lookup (T, Key, ST_Index, ST_Offset, Found);
               pragma Assert (Found);

               To_Do := Reorder_Subtable;

            end if;
         end if;
      end if;
   end Insert;

   procedure Insert
     (T     : in out Hash_Table;
      Key   :        String;
      Index : out    Natural;
      To_Do : out    Next_Action)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

      ST_Index   : Natural;
      ST_Offset  : Natural;

   begin
      Insert (T, Key, ST_Index, ST_Offset, To_Do);

      Index := Elements (Subtables (ST_Index).First + ST_Offset).Item_Index;
   end Insert;

   ------------
   -- Lookup --
   ------------

   procedure Lookup
     (T         :     Hash_Table;
      Key       :     String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      Found     : out Boolean)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

      Index : Natural;

   begin
      ST_Index  := Hash (Key,
                         T.Info.HParam,
                         T.Info.N_Subtables);

      ST_Offset := Hash (Key,
                         Subtables (ST_Index).HParam,
                         Subtables (ST_Index).Max);

      Index := Subtables (ST_Index).First + ST_Offset;

      Found := Elements (Index).Key /= null
        and then Elements (Index).Key.all = Key
        and then Elements (Index).Used;

   end Lookup;

   procedure Lookup
     (T         :     Hash_Table;
      Key       :     String;
      Index     : out Natural;
      Found     : out Boolean)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

      ST_Index   : Natural;
      ST_Offset  : Natural;

   begin
      Lookup (T, Key, ST_Index, ST_Offset, Found);

      if Found then
         Index := Elements
           (Subtables (ST_Index).First + ST_Offset).Item_Index;

      else
         Index := 0;
      end if;
   end Lookup;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (T     : in out Hash_Table;
      Key   : String;
      Index : out Natural)
   is
      Elements  : access Dynamic_Element_Array.Table_Type
        renames T.Elements.Table;
      Subtables : access Dynamic_Subtable_Array.Table_Type
        renames T.Subtables.Table;

      ST_Index  : Natural;
      ST_Offset : Natural;
      Found     : Boolean;

   begin
      Lookup (T, Key, ST_Index, ST_Offset, Found);

      if Found then
         T.Info.Count := T.Info.Count - 1;
         Subtables (ST_Index).Count := Subtables (ST_Index).Count - 1;
         Elements (Subtables (ST_Index).First + ST_Offset).Used := False;

         Index :=
           Elements (Subtables (ST_Index).First + ST_Offset).Item_Index;

      else
         Index := 0;
      end if;
   end Delete;

   --  Implementation of the public specification begins here

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (T      : out Table_Instance;
      HParam :     Hash_Parameters := Default_Hash_Parameters;
      Max    :     Natural := Default_Max) is
   begin
      T.T := new Table;
      Initialize (T.T.HTable, HParam, Max);
      Initialize (T.T.Items);
      Set_Last (T.T.Items, 15 * T.T.HTable.Info.High);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (T : in out Table_Instance) is
   begin
      Finalize (T.T.HTable);
      Deallocate (T.T.Items);
      Free_Table (T.T);
   end Finalize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (T     : Table_Instance;
      Key   : String;
      Value : Item)
   is
      Items : access Dynamic_Item_Array.Table_Type renames T.T.Items.Table;

      To_Do    : Next_Action;
      Index    : Natural;
      Old_Last : Natural;

   begin
      Insert (T.T.HTable, Key, Index, To_Do);

      --  First check if 'Elements' table must be extended.

      if To_Do = Reorder_Table
        and then Last (T.T.HTable.Elements) > Last (T.T.Items)
      then
         Old_Last := Last (T.T.Items);
         Set_Last (T.T.Items, 15 * T.T.HTable.Info.High);

         for J in Old_Last + 1 .. Last (T.T.Items) loop
            Items (J) := null;
         end loop;
      end if;

      --  Then insert the element.

      if To_Do /= Do_Nothing then
         pragma Assert (Items (Index) = null);

         Items (Index) := new Item'(Value);
      end if;
   end Insert;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (T           : Table_Instance;
      Key         : String;
      Error_Value : Item) return Item
   is
      Items : access Dynamic_Item_Array.Table_Type renames T.T.Items.Table;

      Index : Natural;
      Found : Boolean;

   begin
      Lookup (T.T.HTable, Key, Index, Found);

      if Found then
         return Items (Index).all;
      else
         return Error_Value;
      end if;
   end Lookup;

   ------------
   -- Delete --
   ------------

   procedure Delete (T   : Table_Instance; Key : String) is
      Index : Natural;
   begin
      Delete (T.T.HTable, Key, Index);

      if Index /= 0 then
         Free_Item (T.T.Items.Table (Index));
      end if;
   end Delete;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (T : Table_Instance) return Boolean is
   begin
      return T.T.HTable.Info.Count = 0;
   end Is_Empty;

   -----------
   -- First --
   -----------

   function First (T : Table_Instance) return Iterator is
      Elements : Element_Array renames T.T.HTable.Elements;

   begin
      for J in First (Elements) .. Last (Elements) loop
         if Elements.Table (J).Used then
            return Iterator'(On_Table => T, Position => J);
         end if;
      end loop;

      return Iterator'(On_Table => T, Position => Last (Elements) + 1);
   end First;

   -----------
   -- Value --
   -----------

   function Value (I : Iterator) return Item is
      Elements : access Dynamic_Element_Array.Table_Type
        renames I.On_Table.T.HTable.Elements.Table;
      Items    : access Dynamic_Item_Array.Table_Type
        renames I.On_Table.T.Items.Table;

   begin
      return Items (Elements (I.Position).Item_Index).all;
   end Value;

   ---------
   -- Key --
   ---------

   function Key (I : Iterator) return String is
      Elements : access Dynamic_Element_Array.Table_Type
        renames I.On_Table.T.HTable.Elements.Table;
   begin
      return Elements (I.Position).Key.all;
   end Key;

   ----------
   -- Last --
   ----------

   function Last (I : Iterator) return Boolean is
      Elements : Element_Array renames I.On_Table.T.HTable.Elements;
   begin
      for J in I.Position .. Last (Elements) loop
         if Elements.Table (J).Used then
            return False;
         end if;
      end loop;
      return True;
   end Last;

   ----------
   -- Next --
   ----------

   procedure Next (I : in out Iterator) is
      Elements : Element_Array renames I.On_Table.T.HTable.Elements;

   begin
      for J in I.Position + 1 .. Last (Elements) loop
         if Elements.Table (J).Used then
            I.Position := J;
            return;
         end if;
      end loop;

      I.Position := Last (Elements) + 1;
   end Next;

end PolyORB.Utils.HTables.Perfect;
