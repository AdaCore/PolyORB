------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A W S . C O N T A I N E R S . T A B L E S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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

with Ada.Strings.Unbounded;

with GNAT.Dynamic_Tables;

with Table_Of_Strings_And_Static_Values_G;
pragma Elaborate_All (Table_Of_Strings_And_Static_Values_G);

package AWS.Containers.Tables is

   type Table_Type is tagged private;

   type Element (Name_Length, Value_Length : Natural) is record
      Name  : String (1 .. Name_Length);
      Value : String (1 .. Value_Length);
   end record;
   --  Data type to store name/value pair retrieved from a Table_Type.

   type VString_Array is array (Positive range <>)
     of Ada.Strings.Unbounded.Unbounded_String;

   function Count (Table : Table_Type) return Natural;
   --  Returns the number of item in Table.

   function Name_Count (Table : Table_Type) return Natural;
   --  Returns the number of unique key name in Table.

   function Count (Table : Table_Type; Name : String) return Natural;
   --  Returns the number of value for Key Name in Table. It returns
   --  0 if Key does not exist.

   function Exist (Table : Table_Type; Name : String) return Boolean;
   --  Returns True if Key exist in Table.

   function Get
     (Table : Table_Type;
      Name  : String;
      N     : Positive := 1)
      return String;
   --  Returns the Nth value associated with Key into Table. Returns
   --  the emptry string if key does not exist.

   function Get_Name
     (Table : Table_Type;
      N     : Positive := 1)
      return String;
   --  Returns the Nth Name in Table or the empty string if there is
   --  no parameter with this number.

   function Get_Value
     (Table : Table_Type;
      N     : Positive := 1)
      return String;
   --  Returns the Nth Value in Table or the empty string if there is
   --  no parameter with this number.

   function Get
     (Table : Table_Type;
      N     : Positive)
      return Element;
   --  Return N'th name/value pair.

   function Get_Names
     (Table : Table_Type;
      Sort  : Boolean := False)
      return VString_Array;
   --  Returns array of unique key names. If Sort is True, the returned names
   --  array is sorted in alphabetical order. This is of course slightly
   --  slower than returning unsorted results.

   function Get_Values
     (Table : Table_Type;
      Name  : String)
      return VString_Array;
   --  Returns all values for the specified parameter key name.

private
   --  A Table_Type must be initialized by calling
   --  AWS.Containers.Tables.Set.Reset, Server is responsible for doing that.

   type Element_Access is access all Element;
   --  Data type to keep the name/value pair in the
   --  GNAT.Dynamic_Tables.Table_Type.
   --  We cannot use Unbounded_String becouse GNAT.Dynamic_Tables
   --  does not support controlled objects.

   type Key_Positive is new Positive;

   package Name_Indexes is new GNAT.Dynamic_Tables
     (Table_Component_Type => Positive,
      Table_Index_Type     => Key_Positive,
      Table_Low_Bound      => 1,
      Table_Initial        => 4,
      Table_Increment      => 30);

   subtype Name_Index_Table is Name_Indexes.Instance;

   package Data_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Element_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 8,
      Table_Increment      => 30);

   package Index_Table is new Table_Of_Strings_And_Static_Values_G
     (Character, String, "<", "=", Name_Index_Table);
   --  Index of the Element_Array.

   type Index_Table_Type is new Index_Table.Table_Type;

   type Index_Access is access Index_Table_Type;

   type Table_Type is tagged record
      Case_Sensitive : Boolean := True;
      Index          : Index_Access;
      --  Index to find appropriate Name/Value pairs in Data by the name
      Data           : Data_Table.Instance;
      --  Ordered array of name and value pairs
   end record;

   function Internal_Get
     (Table : Table_Type;
      Name  : String;
      N     : Natural)
      return String;
   pragma Inline (Internal_Get);
   --  Returns the Nth value associated with Key into Table. Returns
   --  the emptry string if key does not exist. If N = 0 it returns as-is all
   --  the values as inserted in the tree for Key.

   function Normalize_Name
     (Name : String; To_Upper : Boolean)
      return String;
   --  Returns Name in upper case if To_Upper is set to True and it returns
   --  Name unchanged otherwise.

end AWS.Containers.Tables;
