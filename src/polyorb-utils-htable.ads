------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . U T I L S . H A S H T A B L E                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

package PolyORB.Utils.HTables is

   --  Je ne comprends pas pourquoi tous ces types se trouvent la.
   --  Pour l'essentiel, ils ne sont utilisables que par Perfect.
   --  Donc, Perfect devrait etre rapatrie dans ce paquetage.

private

   type String_Access is access all String;

   type Element is record
      Key         : String_Access;
      Deleted     : Boolean;
      Table_Id    : Natural;
      Subtable_Id : Natural;
   end record;
   --  Key         Param for the hash function
   --  XXXXX Commentaire peu clair (param ?)

   --  Deleted     True when Element is unused
   --  XXXXX Donc Deleted => Unused

   --  Table_Id    Used by Insert
   --  Subtable_Id Idem
   --  XXXXX Expliquer leur role

   type Subtable is record
      Size  : Natural;
      K : Natural;
      Max_Elements : Natural;
      Number_Of_Elements : Natural;
      First : Natural;
      Last  : Natural;
   end record;
   --  Size                Size of Subtable
   --  Max_Elements        Maximal number of Element without resizing
   --  Number_Of_Elements  Actual number of Element
   --  XXXXX Expliquer les differences entre ces tailles
   --  Donc donner des noms courts mais explicites

   --  First               First subtable index
   --  Last                Last subtable index

   --  K                   Param for hash function
   --  XXXXX Expliquer K = key ?

   type Table_Info is record
      Size : Natural;
      Prime : Natural;
      K : Natural;
      Max_Elements : Natural;
      Count : Natural;
   end record;
   --  P     Prime number for hash function
   --  XXXXX Prime

   --  K     Param for hash function
   --  XXXXX ?????

   --  Count Number of modification since Re_hash_All
   --  Size  Number of Subtable
   --  M     Max number of element in the table
   --  XXXXX Max ? Expliquer les differences entre ces differentes
   --  notions. Size = N_Subtables ?

   type Element_Array is array (Natural range <>) of Element;
   type Element_Array_Ptr is access all Element_Array;

   type Subtable_Array is array (Natural range <>) of Subtable;
   type Subtable_Array_Ptr is access all Subtable_Array;

   type Hash_Table is record
      Info      : Table_Info;
      Elements  : Element_Array_Ptr;
      Subtables : Subtable_Array_Ptr;
   end record;

end PolyORB.Utils.HTables;
