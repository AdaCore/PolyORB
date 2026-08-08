------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . U T I L S . D Y N A M I C _ T A B L E S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  This package provides one-dimensional, variable-size arrays support.
--  See the package specification for more details.

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;
with System;

package body PolyORB.Utils.Dynamic_Tables is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Table_First return Integer;
   pragma Inline (Table_First);
   --  Subscript of the first entry in the currently allocated table
   --  Note: the value here is a conversion to Integer of a generic formal,
   --  which is not preelaborable in Ada 2005 as the actual might involve a
   --  function call. So, we cannot use a constant here.

   procedure Reallocate (T : in out Instance);
   --  Reallocate the existing table according to the current value stored
   --  in Max. Works correctly to do an initial allocation if the table
   --  is currently null.

   type Table_Ptr is access all Table_Type;
   --  The table is actually represented as a pointer to allow
   --  reallocation.

   procedure Free_Table is
     new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Table_Type,


      Name   => Table_Ptr);

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
      Free_Table (Table_Ptr (T.Table));
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

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Instance)
   is
      Last_Index : Table_Index_Type;
   begin
      Initialize (X);
      Table_Index_Type'Read (S, Last_Index);
      Set_Last (X, Last_Index);

      for J in First (X) .. Last (X) loop
         Table_Component_Type'Read (S, X.Table (J));
      end loop;
   end Read;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate (T : in out Instance) is
      use type System.Address;
      Table_Address : System.Address;
      for Table_Address'Address use T.Table'Address;
      pragma Import (Ada, Table_Address);

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

      if Table_Address = System.Null_Address then

         --  WAG:62
         --  Here we need to test if Table is null. In equality below, "null"
         --  is a valid literal for the anonymous access type of the record
         --  component in Ada 2005, but when the instance of this generic
         --  package is compiled in Ada 95 mode, this generates an
         --  instantiation error.

         T.Table := new Table_Type (Table_Low_Bound ..
                                      Table_Index_Type (T.P.Max));

      elsif T.P.Max >= Table_First then
         declare
            Old_Table : Table_Ptr := T.Table.all'Unchecked_Access;
         begin
            T.Table := new Table_Type (Table_Low_Bound ..
                                         Table_Index_Type (T.P.Max));
            T.Table (Old_Table'Range) := Old_Table (Old_Table'Range);
            Free_Table (Old_Table);
         end;
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

   -----------------
   -- Table_First --
   -----------------

   function Table_First return Integer is
   begin
      return Integer (Table_Low_Bound);
   end Table_First;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : Instance) is
   begin
      Table_Index_Type'Write (S, Last (X));
      for J in First (X) .. Last (X) loop
         Table_Component_Type'Write (S, X.Table (J));
      end loop;
   end Write;

end PolyORB.Utils.Dynamic_Tables;
