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

   ---------------------
   -- Lookup_Variable --
   ---------------------

   function Lookup_Variable
     (Var_Name : in String;
      Var_Mode : in Access_Mode)
     return Shared_Data_Access
   is
      Var_Data : Shared_Data_Access;
      Pkg_Data : Shared_Data_Access;
      Failure  : Boolean;

   begin
      pragma Debug (D ("lookup variable " & Var_Name));

      Enter_Critical_Section;
      Var_Data := SST.Get (Var_Name'Unrestricted_Access);

      if Var_Data = null then
         declare
            Pkg_Name  : String := Extract_Pkg_Name (Var_Name);
            Partition : Partition_ID;
            Error     : aliased Error_Type;

         begin
            Pkg_Data := SST.Get (Pkg_Name'Unrestricted_Access);

            pragma Debug (D ("register package " & Pkg_Name));

            if Pkg_Data = null then
               Get_Partition (Get_Unit_Id (Pkg_Name), Partition, Error);
               if Found (Error) then
                  Leave_Critical_Section;
                  Raise_Communication_Error (Content (Error'Access));
               end if;

               declare
                  Par_Name : String := Partition'Img;
                  Location : String_Access;
                  Master   : Shared_Data_Access;
                  Storage  : Shared_Data_Access;

               begin
                  Storage := SST.Get (Par_Name'Unrestricted_Access);
                  if Storage = null then
                     pragma Debug (D ("register partition " & Par_Name));

                     Get_Mem_Location (Partition, Location, Error);
                     if Found (Error) then
                        Leave_Critical_Section;
                        Raise_Communication_Error (Content (Error'Access));
                     end if;

                     Master := Lookup_Storage (Location.all);
                     if Master = null then
                        Leave_Critical_Section;
                        Raise_Exception
                          (Program_Error'Identity,
                           "cannot find data storage for partition " &
                           Par_Name);
                     end if;

                     Create_Storage
                       (Master.all, Minor (Location.all), Storage);
                     SST.Set (new String'(Par_Name), Storage);
                  end if;

                  Create_Package (Storage.all, Pkg_Name, Pkg_Data);
                  SST.Set (new String'(Pkg_Name), Pkg_Data);
               end;
            end if;
         end;

         Create_Variable (Pkg_Data.all, Var_Name, Var_Data);

         pragma Assert (Var_Data /= null);

         SST.Set (new String'(Var_Name), Var_Data);
      end if;

      Set_Access_Mode (Var_Data.all, Var_Mode, Failure);
      Leave_Critical_Section;

      if Failure then
         return null;

      else
         return Var_Data;
      end if;
   end Lookup_Variable;

   --------------------
   -- Lookup_Storage --
   --------------------

   function Lookup_Storage
     (Storage_Name : in String)
     return Shared_Data_Access
   is
      Name : String :=  Major (Storage_Name);

   begin
      return SST.Get (Name'Unrestricted_Access);
   end Lookup_Storage;

   -----------
   -- Minor --
   -----------

   function Minor (Location : String) return String is
   begin
      return System.Garlic.Physical_Location.Get_Support_Data (Location);
   end Minor;

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

   ----------------------
   -- Register_Storage --
   ----------------------

   procedure Register_Storage
     (Storage_Name : in String;
      Storage_Data : in Shared_Data_Access)
   is
      Major_Name : String := Major (Storage_Name);

   begin
      pragma Debug (D ("register storage major " & Major_Name));

      if SST.Get (Major_Name'Unrestricted_Access) = null then
         SST.Set (new String'(Major_Name), Storage_Data);
      end if;
   end Register_Storage;

   ----------------------
   -- Register_Package --
   ----------------------

   procedure Register_Package
     (Pkg_Name  : in String;
      Partition : in Types.Partition_ID)
   is
      Pkg_Data : Shared_Data_Access;
      Storage  : Shared_Data_Access;
      Par_Name : String := Partition'Img;

   begin
      Pkg_Data := SST.Get (Pkg_Name'Unrestricted_Access);
      if Pkg_Data = null then
         pragma Debug (D ("register package " & Pkg_Name));

         Storage := SST.Get (Par_Name'Unrestricted_Access);
         if Storage = null then
            Raise_Exception
              (Program_Error'Identity,
               "cannot find data storage for partition" & Par_Name);
         end if;

         Create_Package (Storage.all, Pkg_Name, Pkg_Data);
         SST.Set (new String'(Pkg_Name), Pkg_Data);
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
      pragma Debug (D ("register passive partition " & Par_Name));

      if SST.Get (Par_Name'Unrestricted_Access) = null then
         Master := Lookup_Storage (Location);
         Create_Storage (Master.all, Minor (Location), Storage);
         SST.Set (new String'(Par_Name), Storage);
      end if;
   end Register_Partition;

end System.Garlic.Storages;
