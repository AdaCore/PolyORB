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

--  This package provides dynamic perfect hash tables.

--  It implements the Dietzfelbinger algorithm as described in
--  "Dynamic Perfect Hashing: Upper and Lower Bounds", Dietzfelbinger et al.
--  in SIAM Journal on Computing, 1994, pp 738-761.

--  XXX if yes, why is it separate from polyorb-utils-htables-perfect ?

--  $Id$

with PolyORB.Utils.Dynamic_Tables;
with PolyORB.Utils.Strings;

package PolyORB.Utils.HTables is

   pragma Preelaborate;

private

   subtype String_Access is PolyORB.Utils.Strings.String_Ptr;
   function "=" (L, R : String_Access) return Boolean
     renames PolyORB.Utils.Strings."=";

   --  'Element' type.

   type Element is record
      Key        : String_Access;  --  Key of the element to hash.
      Used       : Boolean;        --  Is the slot really used ?
      ST_Index   : Natural;        --  Index in the Sub Table.
      ST_Offset  : Natural;        --  Offset in the Sub Table.
      Item_Index : Natural;        --  Index of the element.
   end record;

   Empty : constant Element := Element'(null, False, 0, 0, 0);

   package Dynamic_Element_Array is new
     PolyORB.Utils.Dynamic_Tables (Element, Natural, 0, 10, 50);
   use Dynamic_Element_Array;

   subtype Element_Array is Dynamic_Element_Array.Instance;

   --  'Subtable' type.

   type Subtable is record
      First  : Natural;  --  'First subtable index.
      Last   : Natural;  --  'Last subtable index.
      Count  : Natural;  --  Number of elements used.
      High   : Natural;  --  Highest count value before reorganization.
      Max    : Natural;  --  Subtable maximum size.
      K      : Natural;  --  K-parameter of the subtable.
   end record;

   package Dynamic_Subtable_Array is new
     PolyORB.Utils.Dynamic_Tables (Subtable, Natural, 0, 10, 50);
   use Dynamic_Subtable_Array;

   subtype Subtable_Array is Dynamic_Subtable_Array.Instance;

   --  'Table_Info' type.

   type Table_Info is record
      Prime        : Natural;  --  Used by the algorithm, user defined.
      Count        : Natural;  --  Number of Key stored in the table.
      High         : Natural;  --  When Count = High, the table is resized.
      N_Subtables  : Natural;  --  Number of subtables.
      K            : Natural;  --  K-parameter of the subtable.
   end record;

   --  The Hash table.

   type Hash_Table is record
      Info      : Table_Info;      --  Table information.
      Elements  : Element_Array;   --  Placeholder for elements.
      Subtables : Subtable_Array;  --  Sub tables information.
   end record;
   --  Each structure of the array contains the parameters for the sub-table
   --  hash function except the prime number stored in Info.Prime. In addition
   --  it contains the limit of each sub-table in the table Elements.
   --  We can note that :
   --          ..< Subtables.all (i).First   < Subtables.all (i).Last   <
   --              Subtables.all (i+1).First < Subtables.all (i+1).Last <..

   procedure Initialize
     (T      : out Hash_Table;
      Prime  : Natural;
      Max    : Natural);
   --  Initialize the hash table.
   --  'Prime' is a prime number used by hash functions.
   --  'Max' is the max number of elements to store.

   procedure Finalize
     (T : in out Hash_Table);
   --  Deallocate the Hast Table.

   procedure Lookup
     (T         : Hash_Table;
      Key       : String;
      ST_Index  : out Natural;
      ST_Offset : out Natural;
      Found     : out Boolean);
   --  Find key in hash table.
   --  'Key' is the string to hash.
   --  If the key is 'Found', then
   --  'ST_Index', 'ST_Offset' return the object position in the subtable.

   type Next_Action is (Reorder_SubTable,
                        Reorder_Table,
                        Do_Nothing,
                        Insert_Item);
   --  Indicate the next action to do after a value has been inserted.
   --  See the specification of the Insert procedure for more details.

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

   procedure Delete
     (T   : in out Hash_Table;
      Key : String);
   --  Delete key in hash table. In case of a non-existing Key, Delete
   --  ignores deletion. Key is the string to hash.
   --  When a Key is deleted, it's not physically. Indeed it puts just
   --  the tag Used to False

end PolyORB.Utils.HTables;
