------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . U T I L S . D Y N A M I C _ T A B L E S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

--  This package provides one-dimensional, variable-size arrays support.
--  See the package specification for more details.

with Ada.Unchecked_Deallocation;

package body PolyORB.Utils.Dynamic_Tables is

   Table_First : constant Integer := Integer (Table_Low_Bound);
   --  Subscript of the first entry in the currently allocated table

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

   procedure Allocate (T : in out Instance; Num : Integer := 1) is
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
      T.P.Length := 0;
   end Deallocate;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last (T : in out Instance) is
   begin
      T.P.Last_Val := T.P.Last_Val - 1;
   end Decrement_Last;

   -----------
   -- First --
   -----------

   function First (T : Instance) return Table_Index_Type is
      pragma Unreferenced (T);

   begin
      return First_Index;
   end First;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out Instance) is
      Old_Length : constant Integer := T.P.Length;

   begin
      T.P.Last_Val := Table_First - 1;
      T.P.Max      := Table_First + Table_Initial - 1;
      T.P.Length   := T.P.Max - Table_First + 1;

      if Old_Length = T.P.Length then

         --  If table is same size as before (happens when table is never
         --  expanded which is a common case), then simply reuse it. Note
         --  that this also means that an explicit Init call right after
         --  the implicit one in the package body is harmless.

         return;

      else
         --  Otherwise we can use Reallocate to get a table of the right size.
         --  Note that Reallocate works fine to allocate a table of the right
         --  initial size when it is first allocated.

         Reallocate (T);
      end if;

      T.P.Initialized := True;
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   function Initialized (T : Instance) return Boolean is
   begin
      return T.P.Initialized;
   end Initialized;

   ----------
   -- Last --
   ----------

   function Last (T : Instance) return Table_Index_Type is
   begin
      return Table_Index_Type (T.P.Last_Val);
   end Last;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate (T : Instance) return Instance is
      Result : Instance;

   begin
      Initialize (Result);
      Set_Last (Result, Last (T));
      Result.Table.all := T.Table.all;

      return Result;
   end Duplicate;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate (T : in out Instance) is
      Old_Table : Table_Ptr := T.Table;

   begin
      if T.P.Max < T.P.Last_Val then
         while T.P.Max < T.P.Last_Val loop
            T.P.Length
              := Integer'Max (T.P.Length * (100 + Table_Increment) / 100,
                              T.P.Length + 10);

            --  We use the maximum of these 2 values to ensure
            --  T.P.Length (and then T.P.Max) increases; avoiding
            --  infinite loop in case Table_Increment is too small,
            --  implying Increment = 1.

            T.P.Max := Table_First + T.P.Length - 1;
         end loop;
      end if;

      if T.Table = null then
         T.Table := new Table_Type (Table_Low_Bound ..
                                    Table_Index_Type (T.P.Max));

      elsif T.P.Max >= Table_First then
         T.Table := new Table_Type (Table_Low_Bound ..
                                    Table_Index_Type (T.P.Max));

         T.Table (Old_Table'Range) := Old_Table (Old_Table'Range);
         Free_Table (Old_Table);
      end if;
   end Reallocate;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Instance) is
   begin
      T.P.Length := T.P.Last_Val - Table_First + 1;
      T.P.Max    := T.P.Last_Val;
      Reallocate (T);
   end Release;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (T : in out Instance; New_Val : Table_Index_Type) is
   begin
      T.P.Last_Val := Integer (New_Val);

      if T.P.Last_Val > T.P.Max then
         Reallocate (T);
      end if;
   end Set_Last;

end PolyORB.Utils.Dynamic_Tables;
