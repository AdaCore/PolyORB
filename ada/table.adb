------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T A B L E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

with Debug;   use Debug;
with Output;  use Output;
with System;  use System;
with Tree_IO; use Tree_IO;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body Table is

   Min : constant Int := Int (Table_Low_Bound);
   --  Subscript of the minimum entry in the currently allocated table

   Length : Int := 0;
   --  Number of entries in currently allocated table. The value of zero
   --  ensures that we initially allocate the table.

   procedure Free is
     new Unchecked_Deallocation (Big_Table_Type, Table_Ptr);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Reallocate;
   --  Reallocate and extend the existing table

   --------------
   -- Allocate --
   --------------

   function Allocate (Num : Int := 1) return Table_Index_Type is
      Old_Last : constant Int := Last_Val;

   begin
      Last_Val := Last_Val + Num;

      if Last_Val > Max then
         Reallocate;
      end if;

      return Table_Index_Type (Old_Last + 1);
   end Allocate;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last is
   begin
      Last_Val := Last_Val - 1;
   end Decrement_Last;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last is
   begin
      Last_Val := Last_Val + 1;

      if Last_Val > Max then
         Reallocate;
      end if;
   end Increment_Last;

   ----------
   -- Init --
   ----------

   procedure Init is
      Old_Length : Int := Length;

   begin
      Last_Val := Min - 1;
      Max      := Min + Table_Initial - 1;
      Length   := Max - Min + 1;

      --  If table is same size as before (happens when table is never
      --  expanded which is a common case), then simply reuse it, else free
      --  the old table and allocate a new one of the proper size.

      if Old_Length /= Length then
         Free (Table);

         declare
            subtype Local_Table is
              Table_Type (Table_Index_Type (Min) .. Table_Index_Type (Max));
            type Local_Table_Ptr is access all Local_Table;
            Tmp : Local_Table_Ptr;

            function To_Table_Ptr is
              new Unchecked_Conversion (Local_Table_Ptr, Table_Ptr);

         begin
            Tmp := new Local_Table;
            Table := To_Table_Ptr (Tmp);
         end;
      end if;
   end Init;

   ----------
   -- Last --
   ----------

   function Last return Table_Index_Type is
   begin
      return Table_Index_Type (Last_Val);
   end Last;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate is
      Old_Table : Table_Ptr := Table;
      Old_Max   : Int := Max;

   begin
      if Table_Increment = 0 then
         Write_Str ("Fatal error, table ");
         Write_Str (Table_Name);
         Write_Str (" capacity exceeded");
         Write_Eol;
         raise Unrecoverable_Error;
      end if;

      while Max < Last_Val loop
         Length := Length * (100 + Table_Increment) / 100;
         Max := Min + Length - 1;
      end loop;

      declare
         subtype Local_Table is
           Table_Type (Table_Index_Type (Min) .. Table_Index_Type (Max));
         type Local_Table_Ptr is access all Local_Table;
         Tmp : Local_Table_Ptr;

         --  We allocate an array of the bounds we want (Local_Table) and
         --  then use unchecked conversion to convert this to the fake
         --  pointer to giant array type that we use for access. This is
         --  done to allow efficient thin pointer access to the table with
         --  a fixed and known lower bound.

         function To_Table_Ptr is
           new Unchecked_Conversion (Local_Table_Ptr, Table_Ptr);

      begin
         Tmp   := new Local_Table;
         Table := To_Table_Ptr (Tmp);
      end;

      if Debug_Flag_D then
         Write_Str ("--> Allocating new ");
         Write_Str (Table_Name);
         Write_Str (" table, size = ");
         Write_Int (Max - Min + 1);
         Write_Eol;
      end if;

      for J in Min .. Old_Max loop
         Table (Table_Index_Type (J)) := Old_Table (Table_Index_Type (J));
      end loop;

      Free (Old_Table);
   end Reallocate;

   -------------
   -- Restore --
   -------------

   procedure Restore (T : Saved_Table) is
   begin
      Free (Table);
      Last_Val := T.Last_Val;
      Max      := T.Max;
      Table    := T.Table;
      Length   := Max - Min + 1;
   end Restore;

   ----------
   -- Save --
   ----------

   function Save return Saved_Table is
      Res : Saved_Table;

   begin
      Res.Last_Val := Last_Val;
      Res.Max      := Max;
      Res.Table    := Table;

      Table  := null;
      Length := 0;
      Init;
      return Res;
   end Save;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (New_Val : Table_Index_Type) is
      Old_Last : Int;

   begin
      if Int (New_Val) < Last_Val then
         Last_Val := Int (New_Val);
      else
         Old_Last := Last_Val;
         Last_Val := Int (New_Val);

         if Last_Val > Max then
            Reallocate;
         end if;
      end if;
   end Set_Last;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
      N : Int;

   begin
      Tree_Read_Int (N);
      Set_Last (Table_Index_Type (N));

      Tree_Read_Data
        (Table (First)'Address,
         (Last_Val - Int (First) + 1) *
           Table_Type'Component_Size / Storage_Unit);
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is

   begin
      Tree_Write_Int (Int (Last));
      Tree_Write_Data
        (Table (First)'Address,
         (Last_Val - Int (First) + 1) *
           Table_Type'Component_Size / Storage_Unit);
   end Tree_Write;

begin
   Init;
end Table;
