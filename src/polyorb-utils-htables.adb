------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . H T A B L E S                 --
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

--  This package provides dynamic perfect hash tables.

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Utils.Dynamic_Tables;

package body PolyORB.Utils.HTables is

   -------------------------------------
   -- Local procedures specifications --
   -------------------------------------

   procedure Add_Key_To_Subtable
     (Key      : String;
      ST_Index : Natural;
      T        : in out Hash_Table);
   --  Insert the Key on an unused Element in subtable ST_Index

   procedure Find_K
     (ST_Index : Natural;
      T        : Hash_Table);
   --  The Find_k procedure is used to find the K parameter of the subtable
   --  ST_Index in order to have an injectiv hash function

   function Hashcode
     (S     : String;
      K     : Natural;
      Size  : Natural;
      Prime : Natural)
      return Natural;
   --  Hashcode function returns the hashcode associated with S
   --  h(S) = (( K * S ) mod Prime) mod Size

   function Is_Injective
     (ST_Index : Natural;
      T        : Hash_Table)
      return Boolean;
   --  This function indicates if the hash function associated with the
   --  subtable ST_Index is injectiv

   procedure Process_Subtable
     (ST_Index : Natural;
      T : Hash_Table);
   --  Find the K parameter of subtable ST_Index in order to have
   --  an injective hash function and to reorder the subtable.

   procedure Process_Subtable_Hashcode
     (ST_Index : Natural;
      T        : Hash_Table);
   --  Apply the Hashcode function to each element of subtable
   --  ST_Index, and the stores the results in the ST_Offset component
   --  of the component.

   procedure Rehash_All
     (Key : String;
      T   : in out Hash_Table);
   --  Reorganize all the table when it is necessary.

   ---------------------------
   -- Deallocation Function --
   ---------------------------

   procedure Free_String is
     new Ada.Unchecked_Deallocation (String, String_Access);

   -----------------------------------------
   -- Internal procedures implementations --
   -----------------------------------------

   --------------
   -- Hashcode --
   --------------

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

   -------------------------------
   -- Process_Subtable_Hashcode --
   -------------------------------

   procedure Process_Subtable_Hashcode
     (ST_Index : Natural;
      T        : Hash_Table)
   is
      Temp_Str_Ptr : String_Access := null;
   begin
      for I in T.Subtables.Table.all (ST_Index).First ..
        T.Subtables.Table.all (ST_Index).Last
      loop
         Temp_Str_Ptr := T.Elements.Table.all (I).Key;
         if Temp_Str_Ptr /= null
           and then T.Elements.Table.all (I).Used
         then
            T.Elements.Table.all (I).ST_Offset :=
              Hashcode (Temp_Str_Ptr.all,
                        T.Subtables.Table.all (ST_Index).K,
                        T.Subtables.Table.all (ST_Index).Max,
                        T.Info.Prime);
            T.Elements.Table.all (I).ST_Index  := ST_Index;
         end if;
      end loop;
   end Process_Subtable_Hashcode;

   ------------------
   -- Is_Injective --
   ------------------

   function Is_Injective
     (ST_Index : Natural;
      T        : Hash_Table)
      return Boolean
   is
   begin
      for I in T.Subtables.Table.all (ST_Index).First ..
        T.Subtables.Table.all (ST_Index).Last - 1 loop
         for J in I + 1 .. T.Subtables.Table.all (ST_Index).Last loop
            if T.Elements.Table.all (I).Used then
               if T.Elements.Table.all (I).ST_Offset =
                 T.Elements.Table.all (J).ST_Offset
               then
                  return False;
               end if;
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
   begin
      for K in 1 .. T.Info.Prime - 1 loop
         T.Subtables.Table.all (ST_Index).K := K;
         Process_Subtable_Hashcode (ST_Index, T);
         exit when Is_Injective (ST_Index, T);
      end loop;
   end Find_K;

   -------------------------
   -- Add_Key_To_Subtable --
   -------------------------

   procedure Add_Key_To_Subtable
     (Key      : String;
      ST_Index : Natural;
      T        : in out Hash_Table) is
   begin
      for I in T.Subtables.Table.all (ST_Index).First ..
        T.Subtables.Table.all (ST_Index).Last - 1
      loop
         if T.Elements.Table.all (I).Key = null then
            T.Elements.Table.all (I).Key := new String'(Key);
            T.Elements.Table.all (I).Used := True;
            exit;
         elsif not T.Elements.Table.all (I).Used then
            Free_String (T.Elements.Table.all (I).Key);
            T.Elements.Table.all (I).Key := new String'(Key);
            T.Elements.Table.all (I).Used := True;
            exit;
         end if;
      end loop;
   end Add_Key_To_Subtable;

   ----------------
   -- Rehash_All --
   ----------------

   procedure Rehash_All
     (Key : String;
      T   : in out Hash_Table)
   is
      Max_Sum : Natural  := 0;
      Step    : Natural  := 0;
      ST_Index   : Natural  := 0;
      ST_Offset : Natural := 0;
      E_Index : Natural := 0;
      Index : Natural := 0;
      Swap    : Element  := Empty;
      J       : Natural  := 0;
      Swap_Index1 : Natural := 0;
      Swap_Index2 : Natural := Last (T.Elements);

   begin
      --  Add the new element at the end of the table of elements
      --  and deallocate unused element if necessary

      if T.Elements.Table.all (Last (T.Elements)).Key /= null then
         Free_String (T.Elements.Table.all (Last (T.Elements)).Key);
      end if;

      T.Elements.Table.all (Last (T.Elements)).Key := new String'(Key);
      T.Elements.Table.all (Last (T.Elements)).Used := True;

      --  Put all the elements at the beginning of the table

      while Swap_Index1 < Swap_Index2 loop

         if not T.Elements.Table.all (Swap_Index1).Used then
            if T.Elements.Table.all (Swap_Index2).Used then
               Swap := T.Elements.Table.all (Swap_Index1);
               T.Elements.Table.all (Swap_Index1) :=
                 T.Elements.Table.all (Swap_Index2);
               T.Elements.Table.all (Swap_Index2) := Swap;
            else
               while not T.Elements.Table.all (Swap_Index2).Used loop
                  Swap_Index2 := Swap_Index2 - 1;
               end loop;
               if Swap_Index1 < Swap_Index2 then
                  Swap := T.Elements.Table.all (Swap_Index1);
                  T.Elements.Table.all (Swap_Index1) :=
                    T.Elements.Table.all (Swap_Index2);
                  T.Elements.Table.all (Swap_Index2) := Swap;
               end if;
            end if;
         end if;
         Swap_Index1 := Swap_Index1 + 1;
      end loop;

      --  Find the K that satisfies the condition for the subtable

      for K in 1 .. T.Info.Prime loop

         --  Reinitialize the Count param of the subtables

         for I in 0 .. Last (T.Subtables) loop
            T.Subtables.Table.all (I).Count := 0;
            T.Subtables.Table.all (I).K := 1;
         end loop;

         Max_Sum := 0;

         --  Find the repartition of the elements among the subtables

         for I in 0 .. (T.Info.Count - 1) loop
               T.Elements.Table.all (I).ST_Index :=
                 Hashcode (T.Elements.Table.all (I).Key.all,
                           K,
                           T.Info.N_Subtables,
                           T.Info.Prime);
               T.Subtables.Table.all (T.Elements.Table.all (I).ST_Index).Count
                 := T.Subtables.Table.all (T.Elements.Table.all
                                           (I).ST_Index).Count + 1;
         end loop;

         --  Calculate param High and Max for each subtables
         --  Also calculate the sum of the Max param
         for I in 0 .. Last (T.Subtables) loop

            T.Subtables.Table.all (I).High :=
              2 * T.Subtables.Table.all (I).Count;

            T.Subtables.Table.all (I).Max  := 2 *
              T.Subtables.Table.all (I).High *
              (T.Subtables.Table.all (I).High - 1);

            if T.Subtables.Table.all (I).High = 0 then
               T.Subtables.Table.all (I).High := 1;
               T.Subtables.Table.all (I).Max  := 1;
            end if;
            Max_Sum := Max_Sum +  T.Subtables.Table.all (I).Max;
         end loop;

         --  Test if condition is satisfied
         exit when Max_Sum <= 44 * T.Info.High / 3;

      end loop;


      --  Maximisation of each subtables if possible
      --  XXXXX Can be improved

      for I in 0 .. Last (T.Subtables) loop
         if T.Subtables.Table.all (I).Max = 1 then
            if Max_Sum + 3 <= ((44 * T.Info.High) / 3) then
               T.Subtables.Table.all (I).Max :=
                 T.Subtables.Table.all (I).Max + 3;
               T.Subtables.Table.all (I).High := 2;
               Max_Sum := Max_Sum + 3;
            else
               exit;
            end if;
         end if;
      end loop;

      Step := 1;

      for I in 0 .. Last (T.Subtables) loop
         if Max_Sum + (16 * Step) + 4 <= ((44 * T.Info.High) / 3) then
            if T.Subtables.Table.all (I).Count = Step then
               T.Subtables.Table.all (I).Max := T.Subtables.Table.all (I).Max
                 + 16 * Step + 4;
               T.Subtables.Table.all (I).High :=
                 T.Subtables.Table.all (I).High + 2;
               Max_Sum := Max_Sum + 16 * Step + 4;
            end if;
         else
            exit;
         end if;
      end loop;

      --  Fix the begin and the end of the subtables

      for I in 0 .. T.Info.N_Subtables - 1 loop
         T.Subtables.Table.all (I).First := Index;
         Index := Index + T.Subtables.Table.all (I).Max;
         T.Subtables.Table.all (I).Last  := Index - 1;
      end loop;

      --  Reorder the elements

      for I in 0 .. T.Info.Count - 1 loop

         ST_Index := T.Elements.Table.all (I).ST_Index;
         ST_Offset := Hashcode
           (T.Elements.Table.all (I).Key.all,
            T.Subtables.Table.all (ST_Index).K,
            T.Subtables.Table.all (ST_Index).Max,
            T.Info.Prime);
         T.Elements.Table.all (I).ST_Offset := ST_Offset;
         E_Index := ST_Offset + T.Subtables.Table.all (ST_Index).First;

         if ((I /= E_Index)
             and then (T.Subtables.Table.all (ST_Index).Count = 1))
           or else I < T.Subtables.Table.all (ST_Index).First
           or else I > T.Subtables.Table.all (ST_Index).Last
         then
            while (((I /= E_Index)
                    and then (T.Subtables.Table.all (ST_Index).Count = 1))
                   or else I < T.Subtables.Table.all (ST_Index).First
                   or else I > T.Subtables.Table.all (ST_Index).Last)
              and then T.Elements.Table.all (I).Used
            loop
               if T.Subtables.Table.all (ST_Index).Count = 1 then
                  Swap := T.Elements.Table.all (I);
                  T.Elements.Table.all (I) := T.Elements.Table.all (E_Index);
                  T.Elements.Table.all (E_Index) := Swap;
                  ST_Index := T.Elements.Table.all (I).ST_Index;
                  if T.Elements.Table.all (I).Used then
                     ST_Offset := Hashcode
                       (T.Elements.Table.all (I).Key.all,
                        T.Subtables.Table.all (ST_Index).K,
                        T.Subtables.Table.all (ST_Index).Max,
                        T.Info.Prime);
                  end if;
                  T.Elements.Table.all (I).ST_Offset := ST_Offset;
                  E_Index := ST_Offset +
                    T.Subtables.Table.all (ST_Index).First;
               else
                  J := T.Subtables.Table.all (ST_Index).First;
                  while (T.Elements.Table.all (J).Used)
                    and then  (ST_Index = T.Elements.Table.all (J).ST_Index)
                  loop
                     J := J + 1;
                  end loop;

                  Swap := T.Elements.Table.all (I);
                  T.Elements.Table.all (I) := T.Elements.Table.all (J);
                  T.Elements.Table.all (J) := Swap;
                  ST_Index := T.Elements.Table.all (I).ST_Index;
                  if T.Elements.Table.all (I).Used then
                     ST_Offset := Hashcode
                       (T.Elements.Table.all (I).Key.all,
                        T.Subtables.Table.all (ST_Index).K,
                        T.Subtables.Table.all (ST_Index).Max,
                        T.Info.Prime);
                  end if;
                  T.Elements.Table.all (I).ST_Offset := ST_Offset;
                  E_Index := ST_Offset +
                    T.Subtables.Table.all (ST_Index).First;
               end if;

            end loop;
         end if;
      end loop;

      --  Apply the Process_Subtable procedure to all the subtables
      --  that have more than two elements

      for I in 0 .. Last (T.Subtables) loop
         if T.Subtables.Table.all (I).Count > 1 then
            Process_Subtable (I, T);
         elsif T.Subtables.Table.all (I).Count = 0 then
            T.Subtables.Table.all (I).K := 1;
         end if;
      end loop;

   end Rehash_All;

   ----------------------
   -- Process_Subtable --
   ----------------------

   procedure Process_Subtable (ST_Index : Natural; T : Hash_Table)
   is
      Swap  : Element := Empty;
      First : Natural := 0;
      J     : Natural := 0;
   begin
      if T.Subtables.Table.all (ST_Index).Count = 1 then
         --  deals with the case of a subtable with only one element
         T.Subtables.Table.all (ST_Index).K  := 1;
         First := T.Subtables.Table.all (ST_Index).First;
         for I in First .. T.Subtables.Table.all (ST_Index).Last
         loop
            if T.Elements.Table.all (I).Used then
               J := Hashcode (T.Elements.Table.all (I).Key.all,
                              1,
                              T.Subtables.Table.all (ST_Index).Max,
                              T.Info.Prime);
               Swap := T.Elements.Table.all (First + J);
               T.Elements.Table.all (First + J) := T.Elements.Table.all (I);
               T.Elements.Table.all (I) := Swap;
            end if;
         end loop;
      else
         --  in the other cases
         Find_K (ST_Index, T);
         First := T.Subtables.Table.all (ST_Index).First;
         for I in  First .. T.Subtables.Table.all (ST_Index).Last
         loop
            if  T.Elements.Table.all (I).Used then
               if I /= First + T.Elements.Table.all (I).ST_Offset then
                  J := First + T.Elements.Table.all (I).ST_Offset;
                  while T.Elements.Table.all (I).Used
                    and then J /= I
                  loop
                     Swap := T.Elements.Table.all (J);
                     T.Elements.Table.all (J) := T.Elements.Table.all (I);
                     T.Elements.Table.all (I) := Swap;
                     J := Swap.ST_Offset + First;
                  end loop;
               end if;
            end if;
         end loop;
      end if;
   end Process_Subtable;

   ----------------------------------------
   -- Private procedures implementations --
   ----------------------------------------

   ------------
   -- Delete --
   ------------

   procedure Delete
     (T   : in out Hash_Table;
      Key : String)
   is
      ST_Index  : Natural := 0;
      ST_Offset : Natural := 0;
      Found : Boolean;
   begin
      Lookup (T, Key, ST_Index, ST_Offset, Found);
      if Found then
         T.Subtables.Table.all (ST_Index).Count :=
           T.Subtables.Table.all (ST_Index).Count - 1;
         T.Info.Count :=  T.Info.Count - 1;
         T.Elements.Table.all (T.Subtables.Table.all (ST_Index).First
                               + ST_Offset).Used := False;
      end if;
   end Delete;

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
      I : Natural := 0;
   begin
      Found := False;
      ST_Index  := Hashcode (Key, T.Info.K, T.Info.N_Subtables, T.Info.Prime);
      ST_Offset := Hashcode (Key,
                             T.Subtables.Table.all (ST_Index).K,
                             T.Subtables.Table.all (ST_Index).Max,
                             T.Info.Prime);
      I := T.Subtables.Table.all (ST_Index).First + ST_Offset;
      if T.Elements.Table.all (I).Key /= null then
         if T.Elements.Table.all (I).Key.all = Key
           and then T.Elements.Table.all (I).Used
         then
            Found := True;
         end if;
      end if;
   end Lookup;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out Hash_Table) is
   begin
      for I in 0 .. Last (T.Elements) loop
         if T.Elements.Table.all (I).Key /= null then
            Free_String (T.Elements.Table.all (I).Key);
         end if;
      end loop;

      Deallocate (T.Subtables);
      Deallocate (T.Elements);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (T      : out Hash_Table;
      Prime  : Natural;
      Max    : Natural)
   is
      Temp : Natural;
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
      for I in 0 .. Last (T.Elements) loop
         T.Elements.Table.all (I) := Empty;
         T.Elements.Table.all (I).Item_Index := I;
      end loop;

      --  Allocation of the Hash_Table.Subtables

      Init (T.Subtables);
      Set_Last (T.Subtables, T.Info.N_Subtables - 1);
      if  ((T.Info.High * 21) / 8) +1 + ((T.Info.High * 3) / 8) >
        T.Info.N_Subtables
      then
         Temp := (T.Info.High * 21) / 8 - 1;
      else
         Temp := (T.Info.High * 21) / 8;
      end if;

      for I in 0 .. Temp loop
         T.Subtables.Table.all (I).First := I * 4;
         T.Subtables.Table.all (I).Last  := I * 4 + 3;
         T.Subtables.Table.all (I).Count := 0;
         T.Subtables.Table.all (I).High  := 2;
         T.Subtables.Table.all (I).Max   := 4;
         T.Subtables.Table.all (I).K     := 1;
      end loop;
      for I in  0 .. (T.Info.High * 3) / 8 - 1 loop
         T.Subtables.Table.all (I + Temp + 1).First := I * 12 + 4 * (Temp + 1);
         T.Subtables.Table.all (I + Temp + 1).Last  := I * 12 + 11
           + 4 * (Temp + 1);
         T.Subtables.Table.all (I + Temp + 1).Count := 0;
         T.Subtables.Table.all (I + Temp + 1).High  := 3;
         T.Subtables.Table.all (I + Temp + 1).Max   := 12;
         T.Subtables.Table.all (I + Temp + 1).K     := 1;
      end loop;
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (T         : in out Hash_Table;
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      To_Do     : out What_To_Do)
   is
      Found       : Boolean       := False;
      Temp_Index  : Natural       := 0;
      New_Key_Ptr : String_Access := null;
      Old_Last    : Natural       := 0;
   begin

      if T.Info.Count = T.Info.High then

         --  When  T.Info.Count > T.Info.high, extend the table and
         --  Rehash_All.

         Old_Last := Last (T.Elements);
         T.Info.Count := T.Info.Count + 1;
         T.Info.High         := Integer ((1.0 + 0.5) * Float (T.Info.Count));
         T.Info.N_Subtables  := T.Info.High * 3;
         Set_Last (T.Elements, 15 * T.Info.High);
         for I in Old_Last + 1 .. Last (T.Elements) loop
            T.Elements.Table.all (I) := Empty;
            T.Elements.Table.all (I).Item_Index := I;
         end loop;

         Set_Last (T.Subtables, T.Info.N_Subtables - 1);
         Rehash_All (Key, T);
         Lookup (T, Key, ST_Index, ST_Offset, Found);
         To_Do := Reorder_Table;

      else
         --  ...else search if the key is already in the table

         Lookup (T, Key, ST_Index, ST_Offset, Found);
         if Found then

            --  If key in table and is used, don't insert

            To_Do := Do_Nothing;
         else

            --  Temp_Index is the a priori position of the new key
            --  XXX What does that mean????

            Temp_Index := T.Subtables.Table.all (ST_Index).First + ST_Offset;

            T.Subtables.Table.all (ST_Index).Count :=
              T.Subtables.Table.all (ST_Index).Count + 1;

            T.Info.Count := T.Info.Count + 1;

            if T.Subtables.Table.all (ST_Index).Count >
              T.Subtables.Table.all  (ST_Index).High
            then

               --  When Count > High, must Rehash_all

               Rehash_All (Key, T);
               Lookup (T, Key, ST_Index, ST_Offset, Found);
               To_Do := Reorder_Table;

            elsif T.Elements.Table.all (Temp_Index).Key = null then

               --  When the positon is empty insert directly

               New_Key_Ptr := new String'(Key);
               T.Elements.Table.all (Temp_Index).Key := New_Key_Ptr;
               T.Elements.Table.all (Temp_Index).Used := True;
               T.Elements.Table.all (Temp_Index).ST_Index := ST_Index;
               T.Elements.Table.all (Temp_Index).ST_Offset := ST_Offset;
               To_Do := Insert_Item;

            elsif T.Elements.Table.all (Temp_Index).Key.all = Key then

               --  When the position contains the same key but unused
               --  just change the flag Used.

               T.Elements.Table.all (Temp_Index).Used := True;
               To_Do := Insert_Item;

            elsif not T.Elements.Table.all (Temp_Index).Used then

               --  If the position contains a key that is unused,
               --  deallocate the string and insert the new key

               Free_String (T.Elements.Table.all (Temp_Index).Key);
               New_Key_Ptr := new String'(Key);
               T.Elements.Table.all (Temp_Index).Key := New_Key_Ptr;
               T.Elements.Table.all (Temp_Index).Used := True;
               T.Elements.Table.all (Temp_Index).ST_Index := ST_Index;
               T.Elements.Table.all (Temp_Index).ST_Offset := ST_Offset;
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

end PolyORB.Utils.HTables;
