------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . U T I L S . H A S H T A B L E                   --
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

--  This package provide functionnalities to manipulate dynamic perfect     --
--  hash Tables                                                             --





package PolyORB.Utils.H_Table is

private

   ----------------------------------
   -- Private Types Specifications --
   ----------------------------------

   type String_Access is access all String;

   ------------------
   -- Type Element --
   ------------------
   -- *_Hashcode have been added in order to avoid the use,
   -- and the allocation of the lists used in FKS
   ------------------
   -- Key                Param for the hash function
   -- Field              Item return by Look_Up
   -- Deleted            Equal True when Element is unused
   -- Table_Hashcode     Used by Insert
   -- Sub_Table_Hashcode Idem
   ------------------
   type Element is record
      Key : String_Access;
      Deleted : Boolean;
      Table_Hashcode : Natural;
      Sub_Table_Hashcode : Natural;
   end record;

   --------------------
   -- Type Sub_Table --
   --------------------
   -- Elements are stored in a unique table
   -- ST_Begin and ST_End marked the limit of
   -- the sub_tables in this table
   --------------------
   -- Size                Size of Sub_Table
   -- K                   Param for hash function
   -- Max_Elements        Maximal number of Element without resizing
   -- Number_Of_Elements  Actual number of Element
   -- First               Index of the beginning of Sub_Table
   -- Last                Index of the end
   --------------------
   type Sub_Table is record
      Size  : Natural;
      K : Natural;
      Max_Elements : Natural;
      Number_Of_Elements : Natural;
      First : Natural;
      Last  : Natural;
   end record;


   ---------------------
   -- Type Table_Info --
   ---------------------
   -- Size  Number of Sub_Table
   -- P     Prime number for hash function
   -- K     Param for hash function
   -- M     Max number of element in the table
   -- Count Number of modification since Re_hash_All
   ---------------------
   type Table_Info is record
      Size : Natural;
      Prime : Natural;
      K : Natural;
      Max_Elements : Natural;
      Count : Natural;
   end record;

   -- Table of all Elements
   type Elements is array (Natural range <>) of Element;
   type Elements_Ptr is access all Elements;

   -- Table of Sub_Tables information
   type Sub_Tables is array (Natural range <>) of Sub_Table;
   type Sub_Tables_Ptr is access all Sub_Tables;

   -- Aggregation of Table_Info, Elements and Sub_Tables
   type H_Table is record
      T_I : Table_Info;
      E   : Elements_Ptr;
      S_T : Sub_Tables_Ptr;
   end record;

end PolyORB.Utils.H_Table;


