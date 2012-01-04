------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . H T A B L E S                 --
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

--  Parent package for Hash Tables.

package PolyORB.Utils.HTables is

   pragma Pure;

   --  Every hash table HTable on a given Item type must define
   --  the following procedures and functions.

   --     function Lookup
   --       (T           : HTable;
   --        Key         : String;
   --        Error_Value : Item)
   --        return Item;
   --     --  Find Key in hash table and return its associated Item.
   --     --  When Key does not exist, the function returns Error_Value.

   --     function Lookup
   --       (T     : HTable;
   --        Key   : String)
   --        return Item;
   --     --  Find Key in hash table and return its associated Item.
   --     --  When Key does not exist, the function raise No_Key exception.

   --     procedure Insert
   --       (T     : HTable;
   --        Key   : String;
   --        Value : Item);
   --     --  Insert (Key, Value) in hash table.
   --     --  Key is the string to hash and Value its associated Item.
   --     --  If Key already exists, nothing is done.

   --     procedure Delete
   --       (T   : HTable;
   --        Key : String);
   --     --  Delete key in hash table. In case of a non-existing Key, Delete
   --     --  ignores deletion. Key is the string to hash.

end PolyORB.Utils.HTables;
