------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . U T I L S . D Y N A M I C _ T A B L E S          --
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

--  This package provides one-dimensional, variable-size arrays support

--  It has the same specification as GNAT.Dynamic_Table, but in addition
--  it is Preelaborate (GNAT.Dynamic_Table can't, because it uses
--  System.Memory).

--  This package provides an implementation of dynamically resizable one
--  dimensional arrays. The idea is to mimic the normal Ada semantics for
--  arrays as closely as possible with the one additional capability of
--  dynamically modifying the value of the Last attribute.

--  This package provides a facility similar to that of GNAT.Table, except
--  that this package declares a type that can be used to define dynamic
--  instances of the table, while an instantiation of GNAT.Table creates a
--  single instance of the table type.

--  Note controlled types are not supported by this package. In particular
--  the type provided for Table_Component_Type may not be a controlled type.

--  $Id$

generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound : Table_Index_Type;
   Table_Initial   : Positive;
   Table_Increment : Natural;

package PolyORB.Utils.Dynamic_Tables is

   pragma Preelaborate;

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

   type Table_Type is
     array (Table_Index_Type range <>) of Table_Component_Type;

--   subtype Big_Table_Type is
--     Table_Type (Table_Low_Bound .. Table_Index_Type'Last);
   --  We work with pointers to a bogus array type that is constrained
   --  with the maximum possible range bound. This means that the pointer
   --  is a thin pointer, which is more efficient. Since subscript checks
   --  in any case must be on the logical, rather than physical bounds,
   --  safety is not compromised by this approach.

   type Table_Ptr is access all Table_Type;
   --  The table is actually represented as a pointer to allow
   --  reallocation.

   type Table_Private is private;
   --  table private data that is not exported in Instance.

   type Instance is record
      Table : aliased Table_Ptr := null;
   --  The table itself. The lower bound is the value of Low_Bound.
   --  Logically the upper bound is the current value of Last (although
   --  the actual size of the allocated table may be larger than this).
   --  The program may only access and modify Table entries in the
   --  range First .. Last.

      P : Table_Private;
   end record;

   procedure Init (T : in out Instance);
   --  This procedure allocates a new table of size Initial (freeing any
   --  previously allocated larger table). Init must be called before using
   --  the table. Init is convenient in reestablishing a table for new use.

   function Last (T : in Instance) return Table_Index_Type;
   pragma Inline (Last);
   --  Returns the current value of the last used entry in the table,
   --  which can then be used as a subscript for Table. Note that the
   --  only way to modify Last is to call the Set_Last procedure. Last
   --  must always be used to determine the logically last entry.

   procedure Release (T : in out Instance);
   --  Storage is allocated in chunks according to the values given in the
   --  Initial and Increment parameters. A call to Release releases all
   --  storage that is allocated, but is not logically part of the current
   --  array value. Current array values are not affected by this call.

   First : constant Table_Index_Type := Table_Low_Bound;
   --  Export First as synonym for Low_Bound (parallel with use of Last)

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
   --  Adds 1 to Last (same as Set_Last (Last + 1).

   procedure Decrement_Last (T : in out Instance);
   pragma Inline (Decrement_Last);
   --  Subtracts 1 from Last (same as Set_Last (Last - 1).

   procedure Allocate (T   : in out Instance;
                       Num : Integer := 1);
   pragma Inline (Allocate);
   --  Adds Num to Last.

   procedure Deallocate (T : in out Instance);

private

   type Table_Private is record
      Max : Integer;
      --  Subscript of the maximum entry in the currently allocated table

      Length : Integer := 0;
      --  Number of entries in currently allocated table. The value of zero
      --  ensures that we initially allocate the table.

      Last_Val : Integer;
      --  Current value of Last.
   end record;

end PolyORB.Utils.Dynamic_Tables;
