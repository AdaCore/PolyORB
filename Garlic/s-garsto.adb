------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . S T O R A G E S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;    use Ada.Streams;

with GNAT.HTable;
with GNAT.OS_Lib;
with GNAT.Strings; use GNAT.Strings;

with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.Exceptions; use System.Garlic.Exceptions;
with System.Garlic.Partitions; use System.Garlic.Partitions;
with System.Garlic.Soft_Links; use System.Garlic.Soft_Links;
with System.Garlic.Types;      use System.Garlic.Types;
with System.Garlic.Units;      use System.Garlic.Units;

with System.Garlic.Physical_Location;
with System.Garlic.Platform_Specific;

use  System.Garlic.Platform_Specific;

package body System.Garlic.Storages is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARSTO", "(s-garsto): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   package OS  renames GNAT.OS_Lib;

   subtype Hash_Header is Natural range 0 .. 30;

   function Hash  (F : OS.String_Access)      return Hash_Header;
   function Equal (F1, F2 : OS.String_Access) return Boolean;
   --  Hash and equality functions for hash table

   function Extract_Pkg_Name (Var_Name : String) return String;
   --  Var_Name is a fully qualified variable string name. Remove suffix
   --  to get package string name.

   procedure Lookup_Partition
     (Partition : in Partition_ID;
      Par_Data  : out Shared_Data_Access;
      Error     : in out Error_Type);
   --  Return partition shared data. Needed to create a package storage.

   function Major (Location : String) return String;
   --  Return left string (separated by ://).

   function Minor (Location : String) return String;
   --  Return right string (separated by ://).

   First_Storage : constant := 1;
   Last_Storage  : Natural := 0;
   Max_Storages  : constant := 10;
   Storage_Table : array (First_Storage .. Max_Storages) of Shared_Data_Access;

   package SST is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Header,
      Element    => Shared_Data_Access,
      No_Element => null,
      Key        => OS.String_Access,
      Hash       => Hash,
      Equal      => Equal);

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : OS.String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   ----------------------
   -- Extract_Pkg_Name --
   ----------------------

   function Extract_Pkg_Name (Var_Name : String) return String is
   begin
      for Index in reverse Var_Name'Range loop
         if Var_Name (Index) = '.' then
            return Var_Name (Var_Name'First .. Index - 1);
         end if;
      end loop;
      return "";
   end Extract_Pkg_Name;

   ----------
   -- Hash --
   ----------

   function Hash (F : OS.String_Access) return Hash_Header is
      N : Natural := 0;

   begin
      --  Add up characters of name, mod our table size

      for J in F'Range loop
         N := (N + Character'Pos (F (J))) mod (Hash_Header'Last + 1);
      end loop;

      return N;
   end Hash;

   --------------------
   -- Lookup_Package --
   --------------------

   procedure Lookup_Package
     (Pkg_Name : in     String;
      Pkg_Data : out    Shared_Data_Access;
      Error    : in out Error_Type)
   is
      Storage   : Shared_Data_Access;
      Partition : Partition_ID;

   begin
      pragma Debug (D ("lookup package " & Pkg_Name));

      Enter_Critical_Section;
      Pkg_Data := SST.Get (Pkg_Name'Unrestricted_Access);
      if Pkg_Data = null then
         Get_Partition (Get_Unit_Id (Pkg_Name), Partition, Error);
         if not Found (Error) then
            Lookup_Partition (Partition, Storage, Error);
            if not Found (Error) then
               Create_Package (Storage.all, Pkg_Name, Pkg_Data, Error);
               SST.Set (new String'(Pkg_Name), Pkg_Data);
            end if;
         end if;
      end if;
      Leave_Critical_Section;
   end Lookup_Package;

   ----------------------
   -- Lookup_Partition --
   ----------------------

   procedure Lookup_Partition
     (Partition : Partition_ID;
      Par_Data  : out Shared_Data_Access;
      Error     : in out Error_Type)
   is
      Par_Name : String := Partition'Img;
      Location : String_Access;

   begin
      Enter_Critical_Section;
      Par_Data := SST.Get (Par_Name'Unrestricted_Access);
      if Par_Data = null then
         Get_Mem_Location (Partition, Location, Error);
         if not Found (Error) then
            Register_Partition (Partition, Location.all, Error);
            Par_Data := SST.Get (Par_Name'Unrestricted_Access);
         end if;
      end if;
      Leave_Critical_Section;
   end Lookup_Partition;

   --------------------
   -- Lookup_Storage --
   --------------------

   procedure Lookup_Storage
     (Storage_Name : in String;
      Storage_Data : out Shared_Data_Access;
      Error        : in out Error_Type)
   is
      Name : String :=  "_" & Major (Storage_Name);
      Data : Shared_Data_Access;

   begin
      Enter_Critical_Section;
      Data := SST.Get (Name'Unrestricted_Access);
      Leave_Critical_Section;
      if Data = null then
         Throw (Error, "no " & Storage_Name & " storage available");
      end if;
      Storage_Data := Data;
   end Lookup_Storage;

   ---------------------
   -- Lookup_Variable --
   ---------------------

   procedure Lookup_Variable
     (Var_Name : in  String;
      Var_Data : out Shared_Data_Access;
      Error    : in out Error_Type)
   is
      Pkg_Data : Shared_Data_Access;

   begin
      pragma Debug (D ("lookup variable " & Var_Name));

      Enter_Critical_Section;
      Var_Data := SST.Get (Var_Name'Unrestricted_Access);
      if Var_Data = null then
         Lookup_Package (Extract_Pkg_Name (Var_Name), Pkg_Data, Error);
         if not Found (Error) then
            Create_Variable (Pkg_Data.all, Var_Name, Var_Data, Error);
            SST.Set (new String'(Var_Name), Var_Data);
         end if;
      end if;
      Leave_Critical_Section;
   end Lookup_Variable;

   -----------
   -- Major --
   -----------

   function Major (Location : String) return String is
      Name : constant String
        := System.Garlic.Physical_Location.Get_Support_Name (Location);

   begin
      if Name'Length = 0 then
         return Default_Storage_Name;
      end if;
      return Name;
   end Major;

   -----------
   -- Minor --
   -----------

   function Minor (Location : String) return String is
   begin
      return System.Garlic.Physical_Location.Get_Support_Data (Location);
   end Minor;

   ----------------------
   -- Register_Package --
   ----------------------

   procedure Register_Package
     (Pkg_Name  : String;
      Partition : Types.Partition_ID;
      Error     : in out Error_Type)
   is
      Storage  : Shared_Data_Access;
      Pkg_Data : Shared_Data_Access;

   begin
      Enter_Critical_Section;
      Pkg_Data := SST.Get (Pkg_Name'Unrestricted_Access);
      if Pkg_Data = null then
         pragma Debug (D ("register package " & Pkg_Name));
         Lookup_Partition (Partition, Storage, Error);
         if not Found (Error) then
            Create_Package (Storage.all, Pkg_Name, Pkg_Data, Error);
            SST.Set (new String'(Pkg_Name), Pkg_Data);
         end if;
      end if;
      Leave_Critical_Section;
   end Register_Package;

   ------------------------
   -- Register_Partition --
   ------------------------

   procedure Register_Partition
     (Partition : in Types.Partition_ID;
      Location  : in String;
      Error     : in out Error_Type)
   is
      Storage  : Shared_Data_Access;
      Master   : Shared_Data_Access;
      Par_Name : String := Partition'Img;

   begin
      Enter_Critical_Section;
      Storage := SST.Get (Par_Name'Unrestricted_Access);
      if Storage = null then
         pragma Debug (D ("register partition" & Par_Name));
         Lookup_Storage (Location, Master, Error);
         if not Found (Error) then
            Create_Storage (Master.all, Minor (Location), Storage, Error);
            SST.Set (new String'(Par_Name), Storage);
         end if;
      end if;
      Leave_Critical_Section;
   end Register_Partition;

   ----------------------
   -- Register_Storage --
   ----------------------

   procedure Register_Storage
     (Storage_Name : String;
      Storage_Data : Shared_Data_Access)
   is
      Major_Name  : String := "_" & Major (Storage_Name);
      Old_Storage : Shared_Data_Access;

   begin
      Enter_Critical_Section;
      Old_Storage := SST.Get (Major_Name'Unrestricted_Access);
      if Old_Storage = null then
         pragma Debug (D ("register storage major " & Major_Name));
         SST.Set (new String'(Major_Name), Storage_Data);
         Last_Storage := Last_Storage + 1;
         Storage_Table (Last_Storage) := Storage_Data;
      end if;
      Leave_Critical_Section;
   end Register_Storage;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      for S in First_Storage .. Last_Storage loop
         Shutdown (Storage_Table (S).all);
      end loop;
   end Shutdown;

end System.Garlic.Storages;
