------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . U T I L S . H T A B L E S . P E R F E C T         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Utils.Dynamic_Tables;

package body PolyORB.Utils.HTables.Perfect is

   ------------------------
   -- Utility procedures --
   ------------------------

   --  These procedures are utiliy procedures used by the Hash_Table
   --  type, some of them are defined in the Dietzfelbinger algorithm.

   procedure Add_Key_To_Subtable
     (Key      : String;
      ST_Index : Natural;
      T        : in out Hash_Table);
   --  Insert the Key in an unused Element in subtable ST_Index

   procedure Find_K
     (ST_Index : Natural;
      T        : Hash_Table);
   --  Find the K parameter of the subtable ST_Index to construct
   --  an injective hash function.

   function Hashcode
     (S     : String;
      K     : Natural;
      Size  : Natural;
      Prime : Natural)
      return Natural;
   --  Hashcode function returns the hashcode associated with S;
   --  h(S) = (( K * S ) mod Prime) mod Size.

   function Is_Injective
     (ST_Index : Natural;
      T        : Hash_Table)
      return Boolean;
   --  Return true iff the hash function associated with the
   --  subtable ST_Index is injective.

   procedure Process_Subtable
     (ST_Index : Natural;
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
     (Key : String;
      T   : in out Hash_Table);
   --  Reorganize all the tables.

   procedure Swap_Elements
     (T : in out Hash_Table;
      Index1 : Natural;
      Index2 : Natural);
   pragma Inline (Swap_Elements);
   --  Swap elements at index1 and index2.

   procedure Free_String is
     new Ada.Unchecked_Deallocation (String, String_Access);

   ----------------------------------
   -- Hash_Table related functions --
   ----------------------------------

   --  These functions allows for the management of the Hash_Table
   --  index table used by the hash table.

   procedure Initialize
     (T      : out Hash_Table;
      Prime  : Natural;
      Max    : Natural);
   --  Initialize the hash table.
   --  'Prime' is a prime number used by hash functions.
   --  'Max' is the max number of elements to store.

   procedure Finalize
     (T : in out Hash_Table);
   --  Finalize the Hast Table.

   type Next_Action is (Reorder_SubTable,
                        Reorder_Table,
                        Do_Nothing,
                        Insert_Item);
   --  Indicate the next action to do after a value has been inserted
   --  in the hash table index.  See the specification of the Insert
   --  procedure for more details.

   procedure Insert
     (T         : in out Hash_Table;
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      To_Do     : out Next_Action);
   --  Insert key in hash table. In case of an already existing Key,
   --  Insert ignores insertion. Key is the string to hash.
   --  ST_Index corresponds to the subtable index and ST_Offset to
   --  the offset in this subtable of the inserted Key
   --  To_Do indicates if :
   --     -  a reorder of a sub-table or the table is
   --        necessary or not after the insertion (Reorder_SubTable or
   --        Reorder_Table)
   --     -  an item associated with the key can be inserted (Insert_Item)
   --     -  the key already exists (Nothing_To_Do)

   procedure Insert
     (T     : in out Hash_Table;
      Key   : String;
      Index : out Natural;
      To_Do : out Next_Action);
   --  Insert 'Key' in hash table. This function is a wrapper on the
   --  previous function, it directly returns the index of the Item
   --  corresponding to 'Key'

   procedure Lookup
     (T         : Hash_Table;
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      Found     : out Boolean);
   --  Find 'Key' in hash table.
   --  If the key is 'Found', then 'ST_Index', 'ST_Offset' return the
   --  object position in the subtable.

   procedure Lookup
     (T     : Hash_Table;
      Key   : String;
      Index : out Natural;
      Found : out Boolean);
   --  Find 'Key' in hash table. This function is a wrapper on the
   --  previous function, it directly returns the index of the Item
   --  corresponding to 'Key'

   procedure Delete
     (T   : in out Hash_Table;
      Key : String);
   --  Delete key in hash table. In case of a non-existing Key, Delete
   --  ignores deletion. Key is the string to hash.
   --  When a Key is deleted, it's not physically. Indeed it puts just
   --  the tag Used to False

   ---------------------------------------
   -- Utility procedures implementation --
   ---------------------------------------

   --------------
   -- Hashcode --
   --------------

   --  XXX should better use a string access to avoid copy ?? ...
   --  XXX THIS IS NOT A FULL DETERMINISTIC FUNCTION ...

   function Hashcode
     (S     : String;
      K     : Natural;
      Size  : Natural;
      Prime : Natural)
      return Natural
   is
      Result : Long_Long_Integer := 0;
   begin
      for I in S'Range loop
         Result := (Result * 65599
                    + Long_Long_Integer (Character'Pos (S (I)))
                    * Long_Long_Integer (K))
           mod Long_Long_Integer (Prime);
      end loop;

      return Natural (Result mod Long_Long_Integer (Size));
   end Hashcode;

   -------------------
   -- Swap_Elements --
   -------------------

   procedure Swap_Elements
     (T : in out Hash_Table;
      Index1 : Natural;
      Index2 : Natural)
   is
      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;

      Swap : Element;
   begin
      Swap := Elements (Index1);
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
      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

   begin
      for J in Subtables (ST_Index).First .. Subtables (ST_Index).Last
      loop
         if Elements (J).Key /= null
           and then Elements (J).Used
         then
            Elements (J).ST_Index := ST_Index;

            Elements (J).ST_Offset :=
              Hashcode (Elements (J).Key.all,
                        Subtables (ST_Index).K,
                        Subtables (ST_Index).Max,
                        T.Info.Prime);

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
      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

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

   ------------
   -- Find_K --
   ------------

   procedure Find_K
     (ST_Index : Natural;
      T        : Hash_Table)
   is
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

   begin
      for J in 1 .. T.Info.Prime - 1 loop
         Subtables (ST_Index).K := J;
         Rehash_Subtable (ST_Index, T);
         exit when Is_Injective (ST_Index, T);
      end loop;
   end Find_K;

   -------------------------
   -- Add_Key_To_Subtable --
   -------------------------

   procedure Add_Key_To_Subtable
     (Key      : String;
      ST_Index : Natural;
      T        : in out Hash_Table)
   is
      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

      Key_Inserted : Boolean := False;

   begin
      for J in Subtables (ST_Index).First .. Subtables (ST_Index).Last
      loop
         if Elements (J).Key = null then
            Elements (J).Key := new String'(Key);
            Elements (J).Used := True;
            Key_Inserted := True;
            exit;

         elsif not Elements (J).Used then
            Free_String (Elements (J).Key);
            Elements (J).Key := new String'(Key);
            Elements (J).Used := True;
            Key_Inserted := True;
            exit;
         end if;
      end loop;

      if not Key_Inserted then
         raise Program_Error;
         --  XXX should not come to this point.
      end if;
   end Add_Key_To_Subtable;

   ----------------
   -- Rehash_All --
   ----------------

   procedure Rehash_All
     (Key : String;
      T   : in out Hash_Table)
   is
      Max_Sum     : Natural := 0;
      ST_Index    : Natural := 0;
      ST_Offset   : Natural := 0;
      E_Index     : Natural := 0;
      Index       : Natural;
      Offset      : Natural := 0;
      Swap_Index1 : Natural := 0;
      Swap_Index2 : Natural := Last (T.Elements);

      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

   begin
      --  Add the new element at the end of the table of elements
      --  and deallocate unused element if necessary

      if Elements (Last (T.Elements)).Key /= null then
         Free_String (T.Elements.Table.all (Last (T.Elements)).Key);
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

      --  Find the K that satisfies the condition for the subtable

      for K in 1 .. T.Info.Prime loop

         --  Reinitialize the Count param of the subtables

         for J in First (T.Subtables) .. Last (T.Subtables) loop
            Subtables (J).Count := 0;
            Subtables (J).K := 1;
         end loop;

         --  Find the repartition of the elements among the subtables

         for J in 0 .. T.Info.Count - 1 loop
               Elements (J).ST_Index :=
                 Hashcode (Elements (J).Key.all,
                           K,
                           T.Info.N_Subtables,
                           T.Info.Prime);
               Subtables (Elements (J).ST_Index).Count
                 := Subtables (Elements (J).ST_Index).Count + 1;

         end loop;

         Max_Sum := 0;

         --  Calculate param High and Max for each subtables
         --  Also calculate the sum of the Max param.

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

         --  Test a certain condition is satisfied.

         --  Dietzfelbinger algorithm searches a 'K' value so that
         --  Max_Sum <= 32 * T.Info.High ^2 / s (T.Info.High) + 4 * T.Info.High
         --  (inequality #7 p. 5), with s (T.Info.High) the number of subsets
         --  to be created to accomodate for T.Info.High elements.

         --  Choosing s : x -> 3 * x is sufficient to ensure linear
         --  memory usage, thus the following inequality :

         exit when Max_Sum <= 44 * T.Info.High / 3;


      end loop;

      --  Fix the begin and the end of the subtables

      Index := 0;
      for J in First (T.Subtables) .. Last (T.Subtables) loop
         Subtables (J).First := Index;
         Index := Index + Subtables (J).Max;
         Subtables (J).Last  := Index - 1;
      end loop;

      --  Reorder the elements

      for J in 0 .. T.Info.Count - 1 loop

         ST_Index := Elements (J).ST_Index;
         ST_Offset := Hashcode
           (Elements (J).Key.all,
            Subtables (ST_Index).K,
            Subtables (ST_Index).Max,
            T.Info.Prime);
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
                     ST_Offset := Hashcode
                       (Elements (J).Key.all,
                        Subtables (ST_Index).K,
                        Subtables (ST_Index).Max,
                        T.Info.Prime);
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
                     ST_Offset := Hashcode
                       (Elements (J).Key.all,
                        Subtables (ST_Index).K,
                        Subtables (ST_Index).Max,
                        T.Info.Prime);
                  end if;
                  Elements (J).ST_Offset := ST_Offset;
                  E_Index := ST_Offset + Subtables (ST_Index).First;
               end if;

            end loop;
         end if;
      end loop;

      --  Apply the Process_Subtable procedure to all the subtables
      --  that have more than two elements

      for J in First (T.Subtables) .. Last (T.Subtables) loop
         if Subtables (J).Count > 1 then
            Process_Subtable (J, T);
         elsif Subtables (J).Count = 0 then
            Subtables (J).K := 1;
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
      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

      Offset : Natural;

   begin
      Find_K (ST_Index, T);

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
      Prime  : Natural;
      Max    : Natural)
   is
      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

   begin
      --  Initialization of the Hash_Table.Info

      T.Info.Prime        := Prime;
      T.Info.Count        := 0;
      T.Info.High         := Integer ((1.0 + 0.1) * Float (Max));
      T.Info.N_Subtables  := T.Info.High * 3;
      T.Info.K            := 1;

      --  Allocation of the Hash_Table.Elements

      Init (T.Elements);
      Dynamic_Element_Array.Set_Last (T.Elements, 15 * T.Info.High);
      for J in First (T.Elements) .. Last (T.Elements) loop
         Elements (J) := Empty;
         Elements (J).Item_Index := J;
      end loop;

      --  Allocation of the Hash_Table.Subtables

      Init (T.Subtables);
      Set_Last (T.Subtables, T.Info.N_Subtables - 1);

      for J in First (T.Subtables) .. Last (T.Subtables) loop
         Subtables (J).First := 2 * J;
         Subtables (J).Last  := 2 * J + 1;
         Subtables (J).Count := 0;
         Subtables (J).High  := 1;
         Subtables (J).Max   := 2;
         Subtables (J).K     := 1;
      end loop;

   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out Hash_Table)
   is
      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;

   begin
      for J in First (T.Elements) .. Last (T.Elements) loop
         if Elements (J).Key /= null then
            Free_String (Elements (J).Key);
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
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      To_Do     : out Next_Action)
   is
      Found       : Boolean;
      Temp_Index  : Natural;
      Old_Last    : Natural;

      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

   begin
      if T.Info.Count = T.Info.High then

         --  Extend the table and Rehash_All,

         Old_Last           := Last (T.Elements);
         T.Info.Count       := T.Info.Count + 1;
         T.Info.High        := Integer (1.5 * Float (T.Info.Count));
         T.Info.N_Subtables := T.Info.High * 3;
         Set_Last (T.Elements, 15 * T.Info.High);

         for J in Old_Last + 1 .. Last (T.Elements) loop
            Elements (J) := Empty;
            Elements (J).Item_Index := J;
         end loop;

         Set_Last (T.Subtables, T.Info.N_Subtables - 1);
         Rehash_All (Key, T);
         Lookup (T, Key, ST_Index, ST_Offset, Found);
         To_Do := Reorder_Table;

      else

         --  .. else search if the key is already in the table.

         Lookup (T, Key, ST_Index, ST_Offset, Found);

         if Found then

            --  If key in table and is used, don't insert
            To_Do := Do_Nothing;

         else

            --  Temp_Index is the a priori position of the new key
            --  XXX What does that mean????

            Temp_Index := Subtables (ST_Index).First + ST_Offset;

            Subtables (ST_Index).Count := Subtables (ST_Index).Count + 1;
            T.Info.Count := T.Info.Count + 1;

            if Subtables (ST_Index).Count > Subtables (ST_Index).High then

               --  When Count > High, must Rehash_all

               Rehash_All (Key, T);
               Lookup (T, Key, ST_Index, ST_Offset, Found);
               To_Do := Reorder_Table;

            elsif Elements (Temp_Index).Key = null then

               --  When the positon is empty insert directly

               Elements (Temp_Index).Key := new String'(Key);
               Elements (Temp_Index).Used := True;
               Elements (Temp_Index).ST_Index := ST_Index;
               Elements (Temp_Index).ST_Offset := ST_Offset;
               To_Do := Insert_Item;

            elsif Elements (Temp_Index).Key.all = Key
              and then not Elements (Temp_Index).Used then

               --  When the position contains the same key but unused
               --  just change the flag Used.

               Elements (Temp_Index).Used := True;
               To_Do := Insert_Item;

            elsif not Elements (Temp_Index).Used then

               --  If the position contains a key that is unused,
               --  deallocate the string and insert the new key

               Free_String (Elements (Temp_Index).Key);
               Elements (Temp_Index).Key := new String'(Key);
               Elements (Temp_Index).Used := True;
               Elements (Temp_Index).ST_Index := ST_Index;
               Elements (Temp_Index).ST_Offset := ST_Offset;
               To_Do := Insert_Item;

            else

               --  Worst case -> reorganize the subtable

               Add_Key_To_Subtable (Key, ST_Index, T);
               Process_Subtable (ST_Index, T);
               Lookup (T, Key, ST_Index, ST_Offset, Found);
               To_Do := Reorder_SubTable;
            end if;
         end if;
      end if;
   end Insert;

   procedure Insert
     (T     : in out Hash_Table;
      Key   : String;
      Index : out Natural;
      To_Do : out Next_Action)
   is
      ST_Index   : Natural;
      ST_Offset  : Natural;

      Elements : Dynamic_Element_Array.Table_Ptr
        renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr
        renames T.Subtables.Table;

   begin
      Insert (T, Key, ST_Index, ST_Offset, To_Do);

      Index := Elements (Subtables (ST_Index).First + ST_Offset).Item_Index;
   end Insert;

   ------------
   -- Lookup --
   ------------

   procedure Lookup
     (T         : Hash_Table;
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      Found     : out Boolean)
   is
      Index : Natural;

      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

   begin
      ST_Index  := Hashcode (Key,
                             T.Info.K,
                             T.Info.N_Subtables,
                             T.Info.Prime);

      ST_Offset := Hashcode (Key,
                             Subtables (ST_Index).K,
                             Subtables (ST_Index).Max,
                             T.Info.Prime);

      Index := Subtables (ST_Index).First + ST_Offset;

      Found := Elements (Index).Key /= null
        and then Elements (Index).Key.all = Key
        and then Elements (Index).Used;

   end Lookup;

   procedure Lookup
     (T         : Hash_Table;
      Key       : String;
      Index     : out Natural;
      Found     : out Boolean)
   is
      ST_Index   : Natural;
      ST_Offset  : Natural;

      Elements : Dynamic_Element_Array.Table_Ptr
        renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr
        renames T.Subtables.Table;

   begin
      Lookup (T, Key, ST_Index, ST_Offset, Found);

      if Found then
         Index := Elements
           (Subtables (ST_Index).First + ST_Offset).Item_Index;
      end if;
   end Lookup;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (T   : in out Hash_Table;
      Key : String)
   is
      ST_Index  : Natural := 0;
      ST_Offset : Natural := 0;
      Found     : Boolean;

      Elements : Dynamic_Element_Array.Table_Ptr renames T.Elements.Table;
      Subtables : Dynamic_Subtable_Array.Table_Ptr renames T.Subtables.Table;

   begin
      Lookup (T, Key, ST_Index, ST_Offset, Found);

      if Found then
         Subtables (ST_Index).Count := Subtables (ST_Index).Count - 1;
         T.Info.Count := T.Info.Count - 1;
         Elements (Subtables (ST_Index).First + ST_Offset).Used := False;
      end if;
   end Delete;

   --  Implementation of the public specification begins here.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (T      : out Table_Instance;
      Prime  : Natural := Default_Prime;
      Max    : Natural := Default_Max) is
   begin
      T.T := new Table;
      Initialize (T.T.HTable, Prime, Max);
      Init (T.T.Items);
      Set_Last (T.T.Items, 15 * T.T.HTable.Info.High);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (T : in out Table_Instance)
   is
      procedure Free_Table is
         new Ada.Unchecked_Deallocation (Table, Table_Access);

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
      procedure Free_Item is
         new Ada.Unchecked_Deallocation (Item, Item_Access);

      To_Do      : Next_Action;
      Index : Natural;
      Old_Last   : Natural;

      Items : Dynamic_Item_Array.Table_Ptr renames T.T.Items.Table;

   begin
      Insert (T.T.HTable, Key, Index, To_Do);

      --  First check the size of 'Elements' table.

      if To_Do = Reorder_Table
        and then Last (T.T.HTable.Elements) > Last (T.T.Items)
      then
         --  Extend the 'Items' table.

         Old_Last := Last (T.T.Items);
         Set_Last (T.T.Items, 15 * T.T.HTable.Info.High);

         for J in Old_Last + 1 .. Last (T.T.Items) loop
            Items (J) := null;
         end loop;
      end if;

      --  Then insert the element.

      if To_Do /= Do_Nothing then

         if Items (Index) /= null then
            Free_Item (Items (Index));
         end if;

         Items (Index) := new Item'(Value);
      end if;
   end Insert;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (T           : Table_Instance;
      Key         : String;
      Error_Value : Item)
      return Item
   is
      Items : Dynamic_Item_Array.Table_Ptr renames T.T.Items.Table;
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

   function Lookup
     (T   : Table_Instance;
      Key : String)
      return Item
   is
      Items : Dynamic_Item_Array.Table_Ptr renames T.T.Items.Table;
      Index : Natural;
      Found : Boolean;

   begin
      Lookup (T.T.HTable, Key, Index, Found);

      if Found then
         return Items (Index).all;
      else
         raise No_Key;
      end if;
   end Lookup;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (T   : Table_Instance;
      Key : String) is
   begin
      Delete (T.T.HTable, Key);

      --  Items are lazy deleted, i.e. the actual item will be freed
      --  iff we need to reclaim the slot used by this element on the
      --  next insertion.

   end Delete;

end PolyORB.Utils.HTables.Perfect;
