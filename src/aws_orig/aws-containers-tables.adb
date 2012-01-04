------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A W S . C O N T A I N E R S . T A B L E S                 --
--                                                                          --
--                                 B o d y                                  --
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

--  Parameters name/value are put into the GNAT.Dynamic_Tables.Table_Type
--  (Data field). The name as a key and the numeric index as a value is
--  placing to the AVL Tree for the fast find all Name/Value pairs with the
--  same name. Each value of the AVL Tree is a table of numeric indexes
--  in the Data field. The parameters must be accessible
--  through their string index by name and also using an numeric index in
--  the place order. So given a set of parameters (K1=V1, K2=V2...),
--  one must be able to ask for the value for K1 but also the name of the
--  second key or the value of the third key.
--
--  Each K/V pair is then inserted into the Data table for access by numeric
--  index. And its numeric index is placing to the AVL tree indexed by name.
--  The AVL Tree values is a tables of numeric indexes in the Data table.

with Ada.Characters.Handling;

package body AWS.Containers.Tables is

   use Ada.Strings.Unbounded;

   Missing_Item_Error : exception
      renames Index_Table.Missing_Item_Error;

   -----------
   -- Count --
   -----------

   function Count (Table : Table_Type) return Natural is
   begin
      pragma Assert (Table.Index /= null);
      return Data_Table.Last (Table.Data);
   end Count;

   -----------
   -- Count --
   -----------

   function Count
     (Table : Table_Type;
      Name           : String)
      return Natural
   is
      Value : Name_Index_Table;
   begin
      pragma Assert (Table.Index /= null);
      Get_Value
        (Table.Index.all,
         Normalize_Name (Name, not Table.Case_Sensitive),
         Value);

      return Natural (Name_Indexes.Last (Value));

   exception
      when Missing_Item_Error =>
         return 0;
   end Count;

   -----------
   -- Exist --
   -----------

   function Exist
     (Table : Table_Type;
      Name  : String)
      return Boolean is
   begin
      pragma Assert (Table.Index /= null);
      return Is_Present
        (Table.Index.all,
         Normalize_Name (Name, not Table.Case_Sensitive));
   end Exist;

   ---------
   -- Get --
   ---------

   function Get
     (Table : Table_Type;
      Name  : String;
      N     : Positive := 1)
      return String is
   begin
      return Internal_Get (Table, Name, N);
   end Get;

   function Get
     (Table : Table_Type;
      N     : Positive)
      return Element
   is
   begin
      pragma Assert (Table.Index /= null);

      if N <= Data_Table.Last (Table.Data) then
         return Table.Data.Table (N).all;
      else
         return
           (Name_Length  => 0,
            Value_Length => 0,
            Name         => "",
            Value        => "");
      end if;
   end Get;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Table : Table_Type;
      N     : Positive := 1)
      return String is
   begin
      pragma Assert (Table.Index /= null);
      if N <= Data_Table.Last (Table.Data) then
         return Table.Data.Table (N).Name;
      else
         return "";
      end if;
   end Get_Name;

   ---------------
   -- Get_Names --
   ---------------

   function Get_Names
     (Table : Table_Type;
      Sort  : Boolean := False)
      return VString_Array
   is

      procedure Process
        (Key      : String;
         Value    : Name_Index_Table;
         Order    : Positive;
         Continue : in out Boolean);

      Result : VString_Array (1 .. Name_Count (Table));

      -------------
      -- Process --
      -------------

      procedure Process
        (Key      : String;
         Value    : Name_Index_Table;
         Order    : Positive;
         Continue : in out Boolean)
      is
         pragma Warnings (Off, Value);
         pragma Warnings (Off, Continue);
      begin
         Result (Order) := To_Unbounded_String (Key);
      end Process;

      -----------------------
      -- Disorder_Traverse --
      -----------------------

      procedure Disorder_Traverse is
         new Index_Table.Disorder_Traverse_G (Process);

      ------------------
      -- Traverse_Asc --
      ------------------

      procedure Traverse_Asc is
         new Index_Table.Traverse_Asc_G (Process);

   begin
      if Table.Index /= null then
         if Sort then
            Traverse_Asc (Index_Table.Table_Type (Table.Index.all));
         else
            Disorder_Traverse (Index_Table.Table_Type (Table.Index.all));
         end if;
      end if;

      return Result;
   end Get_Names;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Table : Table_Type;
      N              : Positive := 1)
      return String is
   begin
      pragma Assert (Table.Index /= null);

      if N <= Data_Table.Last (Table.Data) then
         return Table.Data.Table (N).Value;
      else
         return "";
      end if;
   end Get_Value;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values
     (Table : Table_Type;
      Name  : String)
      return VString_Array
   is
      Value : Name_Index_Table;
   begin
      pragma Assert (Table.Index /= null);

      Get_Value
        (Table.Index.all,
         Normalize_Name (Name, not Table.Case_Sensitive),
         Value);

      declare
         Last : constant Key_Positive := Name_Indexes.Last (Value);
         Result : VString_Array (1 .. Natural (Last));
      begin
         for I in 1 .. Last loop
            Result (Natural (I)) := To_Unbounded_String (Table.Data.Table
               (Value.Table (I)).Value);
         end loop;
         return Result;
      end;

   exception
      when Missing_Item_Error =>
         return (1 .. 0 => Null_Unbounded_String);
   end Get_Values;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Table : Table_Type;
      Name  : String;
      N     : Natural)
      return String
   is
      Value : Name_Index_Table;
   begin
      pragma Assert (Table.Index /= null);

      Get_Value
        (Table.Index.all,
         Normalize_Name (Name, not Table.Case_Sensitive),
         Value);

      if Key_Positive (N) <= Name_Indexes.Last (Value) then
         return Table.Data.Table (Value.Table (Key_Positive (N))).Value;
      else
         return "";
      end if;

   exception
      when Missing_Item_Error =>
         return "";
   end Internal_Get;

   ----------------
   -- Name_Count --
   ----------------

   function Name_Count (Table : Table_Type) return Natural is
   begin
      if Table.Index = null then
         return 0;
      else
         return Size (Table.Index.all);
      end if;
   end Name_Count;

   --------------------
   -- Normalize_Name --
   --------------------

   function Normalize_Name
     (Name : String; To_Upper : Boolean)
      return String is
   begin
      if To_Upper then
         return Ada.Characters.Handling.To_Upper (Name);
      else
         return Name;
      end if;
   end Normalize_Name;

end AWS.Containers.Tables;
