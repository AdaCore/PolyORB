------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            P o l y O R B . U t i l s . D y n a m i c _ T a b l e s       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
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

package body PolyORB.Utils.Dynamic_Tables is

   Min : constant Integer := Integer (Table_Low_Bound);
   --  Subscript of the minimum entry in the currently allocated table

   type size_t is new Integer;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Reallocate (T : in out Instance);
   --  Reallocate the existing table according to the current value stored
   --  in Max. Works correctly to do an initial allocation if the table
   --  is currently null.

   procedure Free_Table is
     new Ada.Unchecked_Deallocation (Table_Type, Table_Ptr);

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (T   : in out Instance;
      Num : Integer := 1)
   is
   begin
      T.P.Last_Val := T.P.Last_Val + Num;

      if T.P.Last_Val > T.P.Max then
         Reallocate (T);
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------
   procedure Deallocate (T : in out Instance) is
   begin
      Free_Table (T.Table);
      T.Table := null;
   end Deallocate;
   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last (T : in out Instance) is
   begin
      T.P.Last_Val := T.P.Last_Val - 1;
   end Decrement_Last;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last (T : in out Instance) is
   begin
      T.P.Last_Val := T.P.Last_Val + 1;

      if T.P.Last_Val > T.P.Max then
         Reallocate (T);
      end if;
   end Increment_Last;

   ----------
   -- Init --
   ----------

   procedure Init (T : in out Instance) is
      Old_Length : constant Integer := T.P.Length;

   begin
      T.P.Last_Val := Min - 1;
      T.P.Max      := Min + Table_Initial - 1;
      T.P.Length   := T.P.Max - Min + 1;

      --  If table is same size as before (happens when table is never
      --  expanded which is a common case), then simply reuse it. Note
      --  that this also means that an explicit Init call right after
      --  the implicit one in the package body is harmless.

      if Old_Length = T.P.Length then
         return;

      --  Otherwise we can use Reallocate to get a table of the right size.
      --  Note that Reallocate works fine to allocate a table of the right
      --  initial size when it is first allocated.

      else
         Reallocate (T);
      end if;
   end Init;

   ----------
   -- Last --
   ----------

   function Last (T : in Instance) return Table_Index_Type is
   begin
      return Table_Index_Type (T.P.Last_Val);
   end Last;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate (T : in out Instance) is
      Old_Table : Table_Ptr := T.Table;
   begin
      if T.P.Max < T.P.Last_Val then
         while T.P.Max < T.P.Last_Val loop
            T.P.Length := T.P.Length * (100 + Table_Increment) / 100;
            T.P.Max := Min + T.P.Length - 1;
         end loop;
      end if;


      if T.Table = null then
         T.Table := new Table_Type (Table_Low_Bound ..
                                    Table_Index_Type (T.P.Max));
      elsif T.P.Max - Min + 1 > 0 then

         T.Table := new Table_Type (Table_Low_Bound ..
                                    Table_Index_Type (T.P.Max));
         for J in Old_Table'Range loop
            T.Table.all (J) := Old_Table.all (J);
         end loop;
         Free_Table (Old_Table);

      end if;

      if T.P.Length /= 0 and then T.Table = null then
         raise Storage_Error;
      end if;

   end Reallocate;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Instance) is
   begin
      T.P.Length := T.P.Last_Val - Integer (Table_Low_Bound) + 1;
      T.P.Max    := T.P.Last_Val;
      Reallocate (T);
   end Release;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (T : in out Instance; New_Val : Table_Index_Type) is
      Old_Last : Integer;

   begin
      if Integer (New_Val) < T.P.Last_Val then
         T.P.Last_Val := Integer (New_Val);

      else
         Old_Last := T.P.Last_Val;
         T.P.Last_Val := Integer (New_Val);

         if T.P.Last_Val > T.P.Max then
            Reallocate (T);
         end if;
      end if;
   end Set_Last;

end PolyORB.Utils.Dynamic_Tables;
