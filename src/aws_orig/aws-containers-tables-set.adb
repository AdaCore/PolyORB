------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            A W S . C O N T A I N E R S . T A B L E S . S E T             --
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

with PolyORB.Utils.Unchecked_Deallocation;

package body AWS.Containers.Tables.Set is

   procedure Reset (Table : in out Index_Table_Type);
   --  Free all elements and destroy his entries.

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Element,
      Name => Element_Access);

   procedure Free_Elements (Data : in out Data_Table.Instance);
   --  Free all dynamically allocated strings in the data table.

   ---------
   -- Add --
   ---------

   procedure Add
     (Table       : in out Table_Type;
      Name, Value : String)
   is
      L_Key : constant String
        :=  Normalize_Name (Name, not Table.Case_Sensitive);

      Found : Boolean;

      procedure Add_Value
        (Key   : String;
         Value : in out Name_Index_Table);
      --  Append value to the current key's values

      ---------------
      -- Add_Value --
      ---------------

      procedure Add_Value
        (Key   : String;
         Value : in out Name_Index_Table)
      is
         pragma Warnings (Off, Key);
      begin
         Name_Indexes.Append (Value, Data_Table.Last (Table.Data));
      end Add_Value;

      procedure Update is new Index_Table.Update_Value_Or_Status_G (Add_Value);

   begin
      --  Add name/value pair into the Data table

      Data_Table.Append
        (Table.Data,
         new Element'
           (Name_Length  => Name'Length,
            Value_Length => Value'Length,
            Name         => Name,
            Value        => Value));

      --  ???

      Update
        (Table => Index_Table.Table_Type (Table.Index.all),
         Key   => L_Key,
         Found => Found);

      --  ???

      if not Found then
         declare
            Value : Name_Index_Table;
         begin
            Name_Indexes.Init (Value);
            Name_Indexes.Append (Value, Data_Table.Last (Table.Data));
            Insert (Table.Index.all, L_Key, Value);
         end;
      end if;
   end Add;

   --------------------
   -- Case_Sensitive --
   --------------------

   procedure Case_Sensitive
     (Table : in out Table_Type;
      Mode  : Boolean) is
   begin
      Table.Case_Sensitive := Mode;
   end Case_Sensitive;

   ----------
   -- Free --
   ----------

   procedure Free (Table : in out Table_Type) is

      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Index_Table_Type,
         Name => Index_Access);

   begin
      if Table.Index /= null then
         Reset (Table.Index.all);
         Free (Table.Index);

         Free_Elements (Table.Data);
         Data_Table.Free (Table.Data);
      end if;
   end Free;

   -------------------
   -- Free_Elements --
   -------------------

   procedure Free_Elements (Data : in out Data_Table.Instance) is
   begin
      for I in Data_Table.First .. Data_Table.Last (Data) loop
         Free (Data.Table (I));
      end loop;
   end Free_Elements;

   -----------
   -- Reset --
   -----------

   procedure Reset (Table : in out Index_Table_Type) is

      procedure Release_Value
        (Key          : String;
         Value        : in out Name_Index_Table;
         Order_Number : Positive;
         Continue     : in out Boolean);
      --  Release memory associted with the value

      -------------------
      -- Release_Value --
      -------------------

      procedure Release_Value
        (Key          : String;
         Value        : in out Name_Index_Table;
         Order_Number : Positive;
         Continue     : in out Boolean)
      is
         pragma Warnings (Off, Key);
         pragma Warnings (Off, Order_Number);
         pragma Warnings (Off, Continue);
      begin
         Name_Indexes.Free (Value);
      end Release_Value;

      procedure Release_Values is new
        Index_Table.Disorder_Traverse_And_Update_Value_G (Release_Value);

   begin
      Release_Values (Index_Table.Table_Type (Table));
      Destroy (Table);
   end Reset;

   procedure Reset (Table : in out Table_Type) is
   begin
      if Table.Index = null then
         Table.Index := new Index_Table_Type;

      else
         Reset (Table.Index.all);
         Free_Elements (Table.Data);
      end if;

      Data_Table.Init (Table.Data);
   end Reset;

   ------------
   -- Update --
   ------------

   procedure Update
     (Table : in out Table_Type;
      Name  : String;
      Value : String;
      N     : Positive := 1)
   is

      L_Key : constant String
        :=  Normalize_Name (Name, not Table.Case_Sensitive);

      Found : Boolean;

      procedure Update_Value
        (Key    : String;
         Values : in out Name_Index_Table);
      --  Append value to the current key's values

      ------------------
      -- Update_Value --
      ------------------

      procedure Update_Value
        (Key    : String;
         Values : in out Name_Index_Table)
      is
         pragma Warnings (Off, Key);
      begin
         if Key_Positive (N) <= Name_Indexes.Last (Values) then

            declare
               Index : constant Positive := Values.Table (Key_Positive (N));
            begin
               Free (Table.Data.Table (Index));
               Table.Data.Table (Index) :=
                 new Element'
                       (Name_Length  => Name'Length,
                        Value_Length => Value'Length,
                        Name         => Name,
                        Value        => Value);
            end;

         elsif Key_Positive (N) = Name_Indexes.Last (Values) + 1 then
            Data_Table.Append
              (Table.Data,
               new Element'
                 (Name_Length  => Name'Length,
                  Value_Length => Value'Length,
                  Name         => Name,
                  Value        => Value));

            Name_Indexes.Append (Values, Data_Table.Last (Table.Data));
         else
            raise Constraint_Error;
         end if;
      end Update_Value;

      procedure Update is
         new Index_Table.Update_Value_Or_Status_G (Update_Value);

   begin
      Update
        (Table => Index_Table.Table_Type (Table.Index.all),
         Key   => L_Key,
         Found => Found);

      if not Found then

         if N /= 1 then
            raise Constraint_Error;
         end if;

         declare
            Values : Name_Index_Table;
         begin
            Name_Indexes.Init (Values);

            Data_Table.Append
              (Table.Data,
               new Element'
                 (Name_Length  => Name'Length,
                  Value_Length => Value'Length,
                  Name         => Name,
                  Value        => Value));

            Name_Indexes.Append (Values, Data_Table.Last (Table.Data));
            Insert (Table.Index.all, L_Key, Values);
         end;
      end if;
   end Update;

end AWS.Containers.Tables.Set;
