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
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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
with Opt;
with Output;  use Output;
with System;  use System;
with Tree_IO; use Tree_IO;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body Table is
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
      --  Reallocate the existing table according to the current value stored
      --  in Max. Works correctly to do an initial allocation if the table
      --  is currently null.

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
         Max      := Min + (Table_Initial * Opt.Table_Factor) - 1;
         Length   := Max - Min + 1;

         --  If table is same size as before (happens when table is never
         --  expanded which is a common case), then simply reuse it. Note
         --  that this also means that an explicit Init call right after
         --  the implicit one in the package body is harmless.

         if Old_Length = Length then
            return;

         --  Otherwise we can use Reallocate to get a table of the right size.
         --  Note that Reallocate works fine to allocate a table of the right
         --  initial size when it is first allocated.

         else
            Reallocate;
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

         function realloc
           (memblock : Table_Ptr;
            size     : size_t)
            return     Table_Ptr;
         pragma Import (C, realloc);

         function malloc
           (size     : size_t)
            return     Table_Ptr;
         pragma Import (C, malloc);

         New_Size : size_t;

      begin
         if Max < Last_Val then
            pragma Assert (not Locked);

            while Max < Last_Val loop
               Length := Length * (100 + Table_Increment) / 100;
               Max := Min + Length - 1;
            end loop;

            if Debug_Flag_D then
               Write_Str ("--> Allocating new ");
               Write_Str (Table_Name);
               Write_Str (" table, size = ");
               Write_Int (Max - Min + 1);
               Write_Eol;
            end if;
         end if;

         New_Size :=
           size_t ((Max - Min + 1) *
                   (Table_Type'Component_Size / Storage_Unit));

         if Table = null then
            Table := malloc (New_Size);
         else
            Table :=
              realloc
                (memblock => Table,
                 size     => New_Size);
         end if;

         if Length /= 0 and then Table = null then
            raise Storage_Error;
         end if;

      end Reallocate;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Length := Last_Val - Int (Table_Low_Bound) + 1;
         Max    := Last_Val;
         Reallocate;
      end Release;

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

      --  Note: we allocate only the space required to accomodate the data
      --  actually written, which means that a Tree_Write/Tree_Read sequence
      --  does an implicit Release.

      procedure Tree_Read is
      begin
         Tree_Read_Int (Max);
         Last_Val := Max;
         Reallocate;

         Tree_Read_Data
           (Table (First)'Address,
            (Last_Val - Int (First) + 1) *
              Table_Type'Component_Size / Storage_Unit);
      end Tree_Read;

      ----------------
      -- Tree_Write --
      ----------------

      --  Note: we write out only the currently valid data, not the entire
      --  contents of the allocated array. See note above on Tree_Read.

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
end Table;
