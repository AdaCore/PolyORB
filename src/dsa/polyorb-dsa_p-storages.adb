------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . D S A _ P . S T O R A G E S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

with GNAT.HTable;
with GNAT.OS_Lib;

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

package body PolyORB.DSA_P.Storages is

   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package OS renames GNAT.OS_Lib;

   subtype Hash_Header is Natural range 0 .. 30;

   function Hash  (F : OS.String_Access)      return Hash_Header;
   function Equal (F1, F2 : OS.String_Access) return Boolean;
   --  Hash and equality functions for hash table

   package SST is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Header,
      Element    => Shared_Data_Manager_RACW,
      No_Element => null,
      Key        => OS.String_Access,
      Hash       => Hash,
      Equal      => Equal);
   --  Hash table containing shared variable managers of the local partiton.

   function Extract_Pkg_Name (Var_Name : String) return String;
   --  Var_Name is a fully qualified variable string name. Remove suffix
   --  to get package string name.

   ----------------------
   -- Critical_Section --
   ----------------------

   Critical_Section : Mutex_Access;

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

   -------------
   -- Logging --
   -------------

   package L is new Log.Facility_Log ("polyorb.dsa_p.storages");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
                renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;

   --------------------
   -- Lookup_Package --
   --------------------

   procedure Lookup_Package
     (Pkg_Name : String;
      Pkg_Data : out Shared_Data_Manager_RACW) is
   begin
      pragma Debug (C, O ("lookup package " & Pkg_Name));

      --  Try to find a manager factory for package Pkg_Name

      Pkg_Data := SST.Get (Pkg_Name'Unrestricted_Access);
      if Pkg_Data = null then
         raise Program_Error with "unregistred shared passive package "
           & Pkg_Name;
      end if;
   end Lookup_Package;

   ---------------------
   -- Lookup_Variable --
   ---------------------

   procedure Lookup_Variable
     (Var_Name    : String;
      Var_Data    : out Shared_Data_Manager_RACW)
   is
      Pkg_Data : Shared_Data_Manager_RACW;

   begin
      pragma Debug (C, O ("lookup variable " & Var_Name));
      Enter (Critical_Section);

      --  Try to find a manager for shared variable Var_Name

      Var_Data := SST.Get (Var_Name'Unrestricted_Access);
      if Var_Data = null then

         --  Manager for this variable isn't created yet,
         --  so search a manager factory.

         Lookup_Package (Extract_Pkg_Name (Var_Name), Pkg_Data);
         Var_Data := Create (Pkg_Data, Var_Name);
         SST.Set (new String'(Var_Name), Var_Data);
      end if;

      Leave (Critical_Section);
   end Lookup_Variable;

   ----------------------
   -- Register_Factory --
   ----------------------

   procedure Register_Factory
     (Factory_Name : String;
      Factory_Data : Shared_Data_Manager_RACW)
   is
      Old_Factory  : Shared_Data_Manager_RACW;

   begin
      --  Don't register factory if already exists

      Enter (Critical_Section);
      Old_Factory := SST.Get (Factory_Name'Unrestricted_Access);
      if Old_Factory = null then
         SST.Set (new String'(Factory_Name), Factory_Data);
      end if;
      Leave (Critical_Section);
   end Register_Factory;

begin
   Create (Critical_Section);
end PolyORB.DSA_P.Storages;
