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
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams;    use Ada.Streams;

with GNAT.HTable;
with GNAT.OS_Lib;

with System.Garlic.Debug;      use System.Garlic.Debug;
with System.Garlic.Exceptions; use System.Garlic.Exceptions;
with System.Garlic.Partitions; use System.Garlic.Partitions;
with System.Garlic.Soft_Links; use System.Garlic.Soft_Links;
with System.Garlic.Types;      use System.Garlic.Types;
with System.Garlic.Units;      use System.Garlic.Units;
with System.Garlic.Utils;      use System.Garlic.Utils;

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

   function Lookup_Package
     (Pkg_Name : in String)
     return Shared_Data_Access;
   --  Return package shared data. Needed to create a variable storage.

   function Lookup_Partition
     (Partition : in Partition_ID)
     return Shared_Data_Access;
   --  Return partition shared data. Needed to create a package storage.

   function Major (Location : String) return String;
   --  Return left string (separated by ://).

   function Minor (Location : String) return String;
   --  Return right string (separated by ://).

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

   function Lookup_Package
     (Pkg_Name : in String)
     return Shared_Data_Access
   is
      Pkg_Data  : Shared_Data_Access;
      Storage   : Shared_Data_Access;
      Partition : Partition_ID;
      Error     : aliased Error_Type;

   begin
      pragma Debug (D ("lookup package " & Pkg_Name));

      Enter_Critical_Section;
      Pkg_Data := SST.Get (Pkg_Name'Unrestricted_Access);
      if Pkg_Data = null then
         Get_Partition (Get_Unit_Id (Pkg_Name), Partition, Error);
         if not Found (Error) then
            Storage := Lookup_Partition (Partition);
            if Storage /= null then
               Create_Package (Storage.all, Pkg_Name, Pkg_Data);
               SST.Set (new String'(Pkg_Name), Pkg_Data);
            end if;
         end if;
      end if;
      Leave_Critical_Section;
      return Pkg_Data;
   end Lookup_Package;

   ----------------------
   -- Lookup_Partition --
   ----------------------

   function Lookup_Partition
     (Partition : Partition_ID)
     return Shared_Data_Access
   is
      Par_Name : String := Partition'Img;
      Par_Data : Shared_Data_Access;
      Location : Utils.String_Access;
      Error    : aliased Error_Type;

   begin
      Enter_Critical_Section;
      Par_Data := SST.Get (Par_Name'Unrestricted_Access);
      if Par_Data = null then
         Get_Mem_Location (Partition, Location, Error);
         if not Found (Error) then
            Register_Partition (Partition, Location.all);
            Par_Data := SST.Get (Par_Name'Unrestricted_Access);
         end if;
      end if;
      Leave_Critical_Section;
      return Par_Data;
   end Lookup_Partition;

   --------------------
   -- Lookup_Storage --
   --------------------

   function Lookup_Storage
     (Storage_Name : in String)
     return Shared_Data_Access
   is
      Name : String :=  "_" & Major (Storage_Name);

   begin
      return SST.Get (Name'Unrestricted_Access);
   end Lookup_Storage;

   ---------------------
   -- Lookup_Variable --
   ---------------------

   function Lookup_Variable
     (Var_Name : in String)
     return Shared_Data_Access
   is
      Var_Data : Shared_Data_Access;
      Pkg_Data : Shared_Data_Access;

   begin
      pragma Debug (D ("lookup variable " & Var_Name));

      Enter_Critical_Section;
      Var_Data := SST.Get (Var_Name'Unrestricted_Access);
      if Var_Data = null then
         Pkg_Data := Lookup_Package (Extract_Pkg_Name (Var_Name));
         if Pkg_Data /= null then
            Create_Variable (Pkg_Data.all, Var_Name, Var_Data);
            SST.Set (new String'(Var_Name), Var_Data);
         end if;
      end if;
      Leave_Critical_Section;

      return Var_Data;
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
     (Pkg_Name  : in String;
      Partition : in Types.Partition_ID)
   is
      Storage  : Shared_Data_Access;
      Pkg_Data : Shared_Data_Access;

   begin
      if SST.Get (Pkg_Name'Unrestricted_Access) = null then
         pragma Debug (D ("register package " & Pkg_Name));
         Storage := Lookup_Partition (Partition);
         if Storage /= null then
            Create_Package (Storage.all, Pkg_Name, Pkg_Data);
            SST.Set (new String'(Pkg_Name), Pkg_Data);
         end if;
      end if;
   end Register_Package;

   ------------------------
   -- Register_Partition --
   ------------------------

   procedure Register_Partition
     (Partition : in Types.Partition_ID;
      Location  : in String)
   is
      Storage  : Shared_Data_Access;
      Master   : Shared_Data_Access;
      Par_Name : String := Partition'Img;

   begin
      if SST.Get (Par_Name'Unrestricted_Access) = null then
         pragma Debug (D ("register partition " & Par_Name));
         Master := Lookup_Storage (Location);
         if Master = null then
            Raise_Exception
              (Program_Error'Identity,
               "cannot find data storage for partition" & Partition'Img);
         end if;
         Create_Storage (Master.all, Minor (Location), Storage);
         SST.Set (new String'(Par_Name), Storage);
      end if;
   end Register_Partition;

   ----------------------
   -- Register_Storage --
   ----------------------

   procedure Register_Storage
     (Storage_Name : in String;
      Storage_Data : in Shared_Data_Access)
   is
      Major_Name : String := "_" & Major (Storage_Name);

   begin
      if SST.Get (Major_Name'Unrestricted_Access) = null then
         pragma Debug (D ("register storage major " & Major_Name));
         SST.Set (new String'(Major_Name), Storage_Data);
      end if;
   end Register_Storage;

end System.Garlic.Storages;
