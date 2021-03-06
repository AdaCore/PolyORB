------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . U T I L S . D Y N A M I C _ T A B L E S          --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides an implementation of dynamically resizable one
--  dimensional arrays. The idea is to mimic the normal Ada semantics for
--  arrays as closely as possible with the one additional capability of
--  dynamically modifying the value of the Last attribute.

--  This package is notionnaly equivalent to GNAT.Dynamic_Table's, yet
--   * it is Preelaborate (GNAT.Dynamic_Table can't because it depends
--     on System.Memory).
--   * declares a type that can be used to define dynamic instances of
--  the table, while an instantiation of GNAT.Table creates a single
--  instance of the table type.

--  Note: controlled types are not supported by this package. In particular
--  the type provided for Table_Component_Type may not be a controlled type.

--  This is a derived version of GNAT.Dynamic_Table, simplified in order
--  to be preelaborable.

pragma Ada_2012;

generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound : Table_Index_Type;
   Table_Initial   : Positive;
   Table_Increment : Natural;

   --  Table_Component_Type and Table_Index_Type specify the type of the
   --  array, Table_Low_Bound is the lower bound. Index_type must be an
   --  integer type. The effect is roughly to declare:

   --    Table : array (Table_Low_Bound .. <>) of Table_Component_Type;

   --  The Table_Component_Type can be any Ada type but note that default
   --  initialization will NOT occur for the array elements.

   --  The Table_Initial values controls the allocation of the table when
   --  it is first allocated, either by default, or by an explicit Init
   --  call.

   --  The Table_Increment value controls the amount of increase, if the
   --  table has to be increased in size. The value given is a percentage
   --  value (e.g. 100 = increase table size by 100%, i.e. double it).

   --  The Last and Set_Last subprograms provide control over the current
   --  logical allocation. They are quite efficient, so they can be used
   --  freely (expensive reallocation occurs only at major granularity
   --  chunks controlled by the allocation parameters).

   --  Note: we do not make the table components aliased, since this would
   --  restrict the use of table for discriminated types. If it is necessary
   --  to take the access of a table element, use Unrestricted_Access.

package PolyORB.Utils.Dynamic_Tables is

   pragma Preelaborate;
   pragma Remote_Types;

   type Table_Type is
     array (Table_Index_Type range <>) of Table_Component_Type;

   type Table_Private is private;
   --  Table private data that is not exported in Instance

   type Instance is record
      Table : access Table_Type;
      --  The table itself. The lower bound is the value of Low_Bound.
      --  Logically the upper bound is the current value of Last (although
      --  the actual size of the allocated table may be larger than this).
      --  The program may only access and modify Table entries in the
      --  range First .. Last.

      P     : Table_Private;
   end record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Instance);

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : Instance);

   for Instance'Read  use Read;
   for Instance'Write use Write;

   procedure Initialize (T : in out Instance);
   --  This procedure allocates a new table of size Initial (freeing any
   --  previously allocated larger table). Init must be called before using
   --  the table. Init is convenient in reestablishing a table for new use.

   function Initialized (T : Instance) return Boolean;
   pragma Inline (Initialized);
   --  Return True iff T has been initialized

   First_Index : constant Table_Index_Type := Table_Low_Bound;
   --  Export First as synonym for Low_Bound (parallel with use of Last)

   function First (T : Instance) return Table_Index_Type;
   pragma Inline (First);
   --  Returns the 'First value of the table, basically this function
   --  returns First_Index. This function is a facility to access this
   --  value.

   function Last (T : Instance) return Table_Index_Type;
   pragma Inline (Last);
   --  Returns the current value of the last used entry in the table,
   --  which can then be used as a subscript for Table. Note that the
   --  only way to modify Last is to call the Set_Last procedure. Last
   --  must always be used to determine the logical last entry.

   procedure Set_Last (T : in out Instance; New_Val : Table_Index_Type);
   pragma Inline (Set_Last);
   --  This procedure sets Last to the indicated value. If necessary the
   --  table is reallocated to accomodate the new value (i.e. on return
   --  the allocated table has an upper bound of at least Last). If
   --  Set_Last reduces the size of the table, then logically entries are
   --  removed from the table. If Set_Last increases the size of the
   --  table, then new entries are logically added to the table.

   procedure Increment_Last (T : in out Instance);
   pragma Inline (Increment_Last);
   --  Adds 1 to Last (same as Set_Last (T, Last (T) + 1)

   procedure Decrement_Last (T : in out Instance);
   pragma Inline (Decrement_Last);
   --  Subtracts 1 from Last (same as Set_Last (T, Last (T) - 1))

   procedure Release (T : in out Instance);
   --  Storage is allocated in chunks according to the values given in the
   --  Initial and Increment parameters. A call to Release releases all
   --  storage that is allocated, but is not logically part of the current
   --  array value. Current array values are not affected by this call.

   procedure Allocate (T : in out Instance; Num : Integer := 1);
   pragma Inline (Allocate);
   --  Allocate room for Num Table_Component_Type in table T,
   --  eventually reallocate T.

   function Duplicate (T : Instance) return Instance;
   --  Return a copy of T

   procedure Deallocate (T : in out Instance);
   --  Deallocate T instance

private

   type Table_Private is record
      Initialized : Boolean := False;

      Max : Integer := 0;
      --  Subscript of the maximum entry in the currently allocated table

      Length : Integer := 0;
      --  Number of entries in currently allocated table. The value of zero
      --  ensures that we initially allocate the table.

      Last_Val : Integer := Integer (Table_Low_Bound) - 1;
      --  Current value of Last (table is initially empty)

   end record;

end PolyORB.Utils.Dynamic_Tables;
