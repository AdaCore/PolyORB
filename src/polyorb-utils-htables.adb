------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . H T A B L E S                 --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body PolyORB.Utils.HTables is
   ------------------------------------------
   --  Internal procedures specifications  --
   ------------------------------------------

   --  The Add_Key2Subtable procedure inserts the Key on an unused Element
   --  part of the subtable ST_Index
   procedure Add_Key2Subtable
     (Key      : String;
      ST_Index : Natural;
      T        : in out Hash_Table);

   --  The Find_k procedure is used to find the K parameter of the subtable
   --  ST_Index in order to have an injectiv hash function
   procedure Find_K
     (ST_Index : Natural;
      T        : Hash_Table);

   --  Hashcode function returns the hashcode associated with S
   --  h(S) = (( K * S ) mod Prime) mod Size
   function Hashcode
     (S     : String;
      K     : Natural;
      Size  : Natural;
      Prime : Natural)
      return Natural;

   --  This function indicates if the hash function associated with the
   --  subtable ST_Index is injectiv
   function Is_Injective
     (ST_Index : Natural;
      T        : Hash_Table)
      return Boolean;

   --  the Max function returns the max of two integers
   function Max (X : Integer; Y : Integer) return Integer;

   --  the Process_Subtable procedure is used to find the K parameter of the
   --  subtable ST_Index in order to have an injectiv hash function and to
   --  reorder the subtable
   procedure Process_Subtable
     (ST_Index : Natural;
      T : Hash_Table);

   --  the Process_Subtable_Hashcode applies hashcode function to all the
   --  the elements of the subtable ST_Index, and the stores the results in
   --  the field ST_Offset of the elements
   procedure Process_Subtable_Hashcode
     (ST_Index : Natural;
      T        : Hash_Table);

   --  the ReHash_All procedure is used to reorganize all the table when it
   --  is necessary
   procedure ReHash_All
     (Key : String;
      T   : in out Hash_Table);

   -----------------------------
   --  Deallocation Functions --
   -----------------------------
   procedure Free_String is
     new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Free_Element_Array is
     new Ada.Unchecked_Deallocation (Element_Array,
                                     Element_Array_Ptr);
   procedure Free_Subtable_Array is
      new Ada.Unchecked_Deallocation (Subtable_Array,
                                      Subtable_Array_Ptr);

   -------------------------------------------
   --  Internal procedures implementations  --
   -------------------------------------------
   --  Hashcode function
   function Hashcode (S     : String;
                      K     : Natural;
                      Size  : Natural;
                      Prime : Natural)
                      return Natural
   is
      Result : Natural := 0;
   begin
      for I in S'Range loop
         Result := (Result * 65599 + Character'Pos (S (I)) * K) mod Prime;
      end loop;
      Result := Result mod Size;
      return Result;
   end Hashcode;


   --  Max function
   --  This function takes two integer and return the max
   function Max (X : Integer; Y : Integer) return Integer is
   begin
      if Y > X then
         return Y;
      else
         return X;
      end if;
   end Max;

   --  Process_Subtable_hashcode
   --  Calculate the hashcode for all the elements in the subtable
   procedure Process_Subtable_Hashcode (ST_Index : Natural;
                                        T        : Hash_Table)
   is
      Temp_Str_Ptr : String_Access := null;
   begin
      for I in T.Subtables.all (ST_Index).First ..
        T.Subtables.all (ST_Index).Last loop
         Temp_Str_Ptr := T.Elements.all (I).Key;
         if Temp_Str_Ptr /= null and T.Elements.all (I).Used = True then
            T.Elements.all (I).ST_Offset :=
              Hashcode (Temp_Str_Ptr.all,
                        T.Subtables.all (ST_Index).K,
                        T.Subtables.all (ST_Index).Max,
                        T.Info.Prime);
            T.Elements.all (I).ST_Index  := ST_Index;
         end if;
      end loop;
   end Process_Subtable_Hashcode;


   --  Function Is_Injective
   function Is_Injective (ST_Index : Natural;
                          T        : Hash_Table)
                          return Boolean
   is
   begin
      for I in T.Subtables.all (ST_Index).First ..
        T.Subtables.all (ST_Index).Last - 1 loop
         for J in I + 1 .. T.Subtables.all (ST_Index).Last loop
            if T.Elements.all (I).Used = True then
               if T.Elements.all (I).ST_Offset =
                 T.Elements.all (J).ST_Offset
               then
                  return False;
               end if;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Injective;

   --  Procedure Find_K
   procedure Find_K (ST_Index : Natural;
                     T        : Hash_Table)
   is
   begin
      for K in 1 .. T.Info.Prime - 1 loop
         T.Subtables.all (ST_Index).K := K;
         Process_Subtable_Hashcode (ST_Index, T);
         if Is_Injective (ST_Index, T) = True then
            exit;
         end if;
      end loop;
   end Find_K;

   --  Procedure Add_Key2Subtable
   procedure Add_Key2Subtable (Key      : String;
                               ST_Index : Natural;
                               T        : in out Hash_Table) is
   begin
      for I in T.Subtables.all (ST_Index).First ..
        T.Subtables.all (ST_Index).Last - 1 loop

         if T.Elements.all (I).Key = null then
            T.Elements.all (I).Key := new String'(Key);
            T.Elements.all (I).Used := True;
            exit;
         elsif T.Elements.all (I).Used = False then
            Free_String (T.Elements.all (I).Key);
            T.Elements.all (I).Key := new String'(Key);
            T.Elements.all (I).Used := True;
            exit;
         end if;
      end loop;
   end Add_Key2Subtable;

   --  Procedure ReHash_All
   procedure ReHash_All (Key : String;
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
      Swap_Index2 : Natural := T.Elements.all'Last;
   begin
      --  Add the new element at the end of the table of elements
      --  and deallocate unused element if necessary
      if T.Elements.all (T.Elements.all'Last).Key /= null then
         Free_String (T.Elements.all (T.Elements.all'Last).Key);
      end if;

      T.Elements.all (T.Elements.all'Last).Key := new String'(Key);
      T.Elements.all (T.Elements.all'Last).Used := True;

      --  Put all the elements at the beginning of the table
      while Swap_Index1 < Swap_Index2 loop

         if T.Elements.all (Swap_Index1).Used = False then
            if T.Elements.all (Swap_Index2).Used = True then
               Swap := T.Elements.all (Swap_Index1);
               T.Elements.all (Swap_Index1) := T.Elements.all (Swap_Index2);
               T.Elements.all (Swap_Index2) := Swap;
            else
               while T.Elements.all (Swap_Index2).Used = False loop
                  Swap_Index2 := Swap_Index2 - 1;
               end loop;
               if Swap_Index1 < Swap_Index2 then
                  Swap := T.Elements.all (Swap_Index1);
                  T.Elements.all (Swap_Index1) := T.Elements.all (Swap_Index2);
                  T.Elements.all (Swap_Index2) := Swap;
               end if;
            end if;
         end if;
         Swap_Index1 := Swap_Index1 + 1;
      end loop;


      --  Find the K that satisfies the condition for the subtable
      for K in 1 .. T.Info.Prime loop

         --  Reinitialize the Count param of the subtables
         for I in T.Subtables'Range loop
            T.Subtables.all (I).Count := 0;
            T.Subtables.all (I).K := 1;
         end loop;

         Max_Sum := 0;

         --  Find the repartition of the elements among the subtables
         for I in 0 .. (T.Info.Count - 1) loop
               T.Elements.all (I).ST_Index :=
                 Hashcode (T.Elements.all (I).Key.all,
                           K,
                           T.Info.N_Subtables,
                           T.Info.Prime);
               T.Subtables.all (T.Elements.all (I).ST_Index).Count :=
                 T.Subtables.all (T.Elements.all (I).ST_Index).Count + 1;
         end loop;

         --  Calculate param High and Max for each subtables
         --  Also calculate the sum of the Max param
         for I in T.Subtables'Range loop

            T.Subtables.all (I).High := 2 * T.Subtables.all (I).Count;
            T.Subtables.all (I).Max  := 2 * T.Subtables.all (I).High *
              (T.Subtables.all (I).High - 1);
            if T.Subtables.all (I).High = 0 then
               T.Subtables.all (I).High := 1;
               T.Subtables.all (I).Max  := 1;
            end if;
            Max_Sum := Max_Sum +  T.Subtables.all (I).Max;
         end loop;

         --  Test if condition is satisfied
         if Max_Sum <= 44 * T.Info.High / 3 then
            exit;
         end if;
      end loop;


      --  Maximize of each subtables if possible
      --  XXXXX Can be ameliorated
      for I in T.Subtables'Range loop
         if T.Subtables.all (I).Max = 1 then
            if Max_Sum + 3 <= ((44 * T.Info.High) / 3) then
               T.Subtables.all (I).Max := T.Subtables.all (I).Max + 3;
               T.Subtables.all (I).High := 2;
               Max_Sum := Max_Sum + 3;
            else
               exit;
            end if;
         end if;
      end loop;

      Step := 1;

      for I in T.Subtables'Range loop
         if Max_Sum + (16 * Step) + 4 <= ((44 * T.Info.High) / 3) then
            if T.Subtables.all (I).Count = Step then
               T.Subtables.all (I).Max := T.Subtables.all (I).Max
                 + 16 * Step + 4;
               T.Subtables.all (I).High := T.Subtables.all (I).High + 2;
               Max_Sum := Max_Sum + 16 * Step + 4;
            end if;
         else
            exit;
         end if;
      end loop;
      --------------------------------------------

      --  Fix the begin and the end of the subtables
      for I in 0 .. T.Info.N_Subtables - 1 loop
         T.Subtables.all (I).First := Index;
         Index := Index + T.Subtables.all (I).Max;
         T.Subtables.all (I).Last  := Index - 1;
      end loop;

      --  Reorder the elements
      for I in 0 .. T.Info.Count - 1 loop
         ST_Index := T.Elements.all (I).ST_Index;
         ST_Offset := Hashcode
           (T.Elements.all (I).Key.all,
            T.Subtables.all (ST_Index).K,
            T.Subtables.all (ST_Index).Max,
            T.Info.Prime);
         T.Elements.all (I).ST_Offset := ST_Offset;
         E_Index := ST_Offset + T.Subtables.all (ST_Index).First;

         if ((I /= E_Index) and (T.Subtables.all (ST_Index).Count = 1))
           or I <T.Subtables.all (ST_Index).First
           or I >T.Subtables.all (ST_Index).Last
         then
           while (((I /= ST_Offset) and (T.Subtables.all (ST_Index).Count = 1))
                  or I <T.Subtables.all (ST_Index).First
                  or I >T.Subtables.all (ST_Index).Last)
             and T.Elements.all (I).Used = True
           loop
              if T.Subtables.all (ST_Index).Count = 1 then
                 Swap := T.Elements.all (I);
                 T.Elements.all (I) := T.Elements.all (E_Index);
                 T.Elements.all (E_Index) := Swap;
                 ST_Index := T.Elements.all (I).ST_Index;
                 if T.Elements.all (I).Used = True then
                    ST_Offset := Hashcode
                      (T.Elements.all (I).Key.all,
                       T.Subtables.all (ST_Index).K,
                       T.Subtables.all (ST_Index).Max,
                       T.Info.Prime);
                 end if;
                 T.Elements.all (I).ST_Offset := ST_Offset;
                 E_Index := ST_Offset + T.Subtables.all (ST_Index).First;
              else
                 J := T.Subtables.all (ST_Index).First;
                 while (T.Elements.all (J).Used = True)
                   and (ST_Index = T.Elements.all (J).ST_Index)
                 loop
                    J := J + 1;
                 end loop;
                 Swap := T.Elements.all (I);
                 T.Elements.all (I) := T.Elements.all (J);
                 T.Elements.all (J) := Swap;
                 ST_Index := T.Elements.all (I).ST_Index;
                 if T.Elements.all (I).Used = True then
                    ST_Offset := Hashcode
                      (T.Elements.all (I).Key.all,
                       T.Subtables.all (ST_Index).K,
                       T.Subtables.all (ST_Index).Max,
                       T.Info.Prime);
                 end if;
                 T.Elements.all (I).ST_Offset := ST_Offset;
                 E_Index := ST_Offset + T.Subtables.all (ST_Index).First;
              end if;

           end loop;
         end if;
      end loop;



      --  Apply the Process_Subtable procedure to all the subtables
      --  that have more than two elements
      for I in T.Subtables.all'Range loop
         if T.Subtables.all (I).Count > 1 then
            Process_Subtable (I, T);
         elsif T.Subtables.all (I).Count = 0 then
            T.Subtables.all (I).K := 1;
         end if;
      end loop;

   end ReHash_All;

   -- Procedure Process_Subtable
   procedure Process_Subtable (ST_Index : Natural; T : Hash_Table)
   is
      Swap  : Element := Empty;
      First : Natural := 0;
      J     : Natural := 0;
   begin
      if T.Subtables.all (ST_Index).Count = 1 then
          -- deals with the case of a subtable with only one element
         T.Subtables.all (ST_Index).K  :=1;
         First := T.Subtables.all (ST_Index).First;
         for I in First .. T.Subtables.all (ST_Index).Last
         loop
            if T.Elements.all (I).Used = True then
               J := Hashcode (T.Elements.all (I).Key.all,
                              1,
                              T.Subtables.all (ST_Index).Max,
                              T.Info.Prime);
               Swap := T.Elements.all (First + J);
               T.Elements.all (First + J) := T.Elements.all (I);
               T.Elements.all (I) := Swap;
            end if;
         end loop;
      else
         -- in the other cases
         Find_K (ST_Index, T);
         First := T.Subtables.all (ST_Index).First;
         for I in  First .. T.Subtables.all (ST_Index).Last
         loop
            if  T.Elements.all (I).Used = True then
               if I /= First + T.Elements.all (I).ST_Offset then
                  J := First + T.Elements.all (I).ST_Offset;
                  while T.Elements.all (I).Used = True and
                    J /= I
                  loop
                     Swap := T.Elements.all (J);
                     T.Elements.all (J) := T.Elements.all (I);
                     T.Elements.all (I) := Swap;
                     J := Swap.ST_Offset + First;
                  end loop;
               end if;
            end if;
         end loop;
      end if;
   end Process_Subtable;


   ------------------------------------------
   --  Private procedures implementations  --
   ------------------------------------------
   procedure Delete (T   : in out Hash_Table;
                     Key : String)
   is
      ST_Index  : Natural := 0;
      ST_Offset : Natural := 0;
      Found : Boolean := False;
   begin
      Lookup (T, Key, ST_Index, ST_Offset, Found);
      if Found = True then
         T.Subtables.all (ST_Index).Count :=
           T.Subtables.all (ST_Index).Count - 1;
         T.Info.Count :=  T.Info.Count - 1;
         T.Elements.all (T.Subtables.all (ST_Index).First + ST_Offset).Used :=
           False;
      else
         Put_Line ("Error in Delete : Can't delete an inexistant element");
      end if;
   end Delete;


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
                             T.Subtables.all (ST_Index).K,
                             T.Subtables.all (ST_Index).Max,
                             T.Info.Prime);
      I := T.Subtables.all (ST_Index).First + ST_Offset;
      if T.Elements.all (I).Key /= null then
         if T.Elements.all (I).Key.all = Key and
           T.Elements.all (I).Used = True
         then
            Found := True;
         end if;
      end if;
   end Lookup;


   procedure Finalize
     (T : in out Hash_Table)
   is

   begin
      for I in T.Elements'Range loop
         if T.Elements.all (I).Key /= null then
            Free_String (T.Elements.all (I).Key);
         end if;
      end loop;
      Free_Subtable_Array (T.Subtables);
      Free_Element_Array  (T.Elements);
   end Finalize;


   --  Procedure Initialize
   procedure Initialize
     (T      : out Hash_Table;
      Prime  : Natural;
      Max    : Natural)
   is
      Temp : Natural;
   begin
      --  Initialization of the Hash_Table.Info
      T.Info.Prime        := Prime;
      T.Info.Count := 0;
      T.Info.High         := Integer ((1.0 + 0.1 ) * Float (Max));
      T.Info.N_Subtables  := T.Info.High * 3;
      T.Info.K            := 1;
      --  Allocation of the Hash_Table.Elements
      T.Elements := new Element_Array (0 .. (15 * T.Info.High));
      for I in T.Elements'Range loop
         T.Elements.all (I) := Empty;
         T.Elements.all (I).Item_Index := I;
      end loop;

      --  Allocation of the Hash_Table.Subtables
      T.Subtables := new Subtable_Array (0 .. T.Info.N_Subtables - 1);
      if  ((T.Info.High * 21) / 8) +1 + ((T.Info.High * 3) / 8) >
        T.Info.N_Subtables
      then
         Temp := (T.Info.High * 21) / 8 - 1;
      else
         Temp := (T.Info.High * 21) / 8;
      end if;

      for I in 0 .. Temp loop
         T.Subtables.all (I).First := I * 4;
         T.Subtables.all (I).Last  := I * 4 + 3;
         T.Subtables.all (I).Count := 0;
         T.Subtables.all (I).High  := 2;
         T.Subtables.all (I).Max   := 4;
         T.Subtables.all (I).K     := 1;
      end loop;
      for I in  0 .. (T.Info.High * 3) / 8 - 1 loop
         T.Subtables.all (I + Temp + 1).First := I * 12 + 4 * (Temp + 1);
         T.Subtables.all (I + Temp + 1).Last  := I * 12 + 11 + 4 * (Temp + 1);
         T.Subtables.all (I + Temp + 1).Count := 0;
         T.Subtables.all (I + Temp + 1).High  := 3;
         T.Subtables.all (I + Temp + 1).Max   := 12;
         T.Subtables.all (I + Temp + 1).K     := 1;
      end loop;
   end Initialize;

   ------------------------
   --  Procedure Insert  --
   ------------------------
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
     (T         : in out Hash_Table;
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      To_Do     : out What_To_Do)
   is
      Found       : Boolean       := False;
      Temp_Index  : Natural       := 0;
      New_Key_Ptr : String_Access := null;
   begin

      if T.Info.Count = T.Info.High then
         --  Impossible to have T.Info.Count > T.Info.high
         Put_Line ("Insertion not allowed");
      else
         --  else search if the key is already in the table
         Lookup (T, Key, ST_Index, ST_Offset, Found);
         if Found = True then
            --  if key in table and is used don't insert
            --  Put_Line ("Key already inserted");
            To_Do := Do_Nothing;
         else
            --  Temp_Index is the a priori position of the new key
            Temp_Index := T.Subtables.all (ST_Index).First + ST_Offset;

            T.Subtables.all (ST_Index).Count :=
              T.Subtables.all (ST_Index).Count + 1;

            T.Info.Count := T.Info.Count + 1;

            if T.Subtables.all (ST_Index).Count >
              T.Subtables.all (ST_Index).High then
               --  when Count > High must Rehash_all
               ReHash_All (Key, T);
               To_Do := Reorder_Table;
            elsif T.Elements.all (Temp_Index).Key = null then
               --  when the positon is empty insert directly
               New_Key_Ptr := new String'(Key);
               T.Elements.all (Temp_Index).Key := New_Key_Ptr;
               T.Elements.all (Temp_Index).Used := True;
               T.Elements.all (Temp_Index).ST_Index := ST_Index;
               T.Elements.all (Temp_Index).ST_Offset := ST_Offset;
               To_Do := Insert_Item;
            elsif T.Elements.all (Temp_Index).Key.all = Key then
               --  when the position contains the same key but unused
               --  just change the flag Used
               T.Elements.all (Temp_Index).Used := True;
               To_Do := Insert_Item;
            elsif T.Elements.all (Temp_Index).Used = False then
               --  if the position contains a key that is unused
               --  deallocate the string and insert the new key
               Free_String (T.Elements.all (Temp_Index).Key);
               New_Key_Ptr := new String'(Key);
               T.Elements.all (Temp_Index).Key := New_Key_Ptr;
               T.Elements.all (Temp_Index).Used := True;
               T.Elements.all (Temp_Index).ST_Index := ST_Index;
               T.Elements.all (Temp_Index).ST_Offset := ST_Offset;
               To_Do := Insert_Item;
            else
               --  worst case -> reorganize the subtable
               Add_Key2Subtable (Key, ST_Index, T);
               Process_Subtable (ST_Index, T);
               To_Do := Reorder_SubTable;
            end if;
         end if;
      end if;
   end Insert;

end PolyORB.Utils.HTables;
