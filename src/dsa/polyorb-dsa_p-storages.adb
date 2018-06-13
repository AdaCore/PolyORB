------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . D S A _ P . S T O R A G E S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

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
   --  Hash table containing shared variable managers of the local partition

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

      begin
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
      exception
         when E : others =>
            pragma Debug
              (C, O ("Lookup_Variable: got exception "
                     & Ada.Exceptions.Exception_Information (E)));
            Leave (Critical_Section);
            raise;
      end;
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
      pragma Debug (C, O ("Register_Factory: enter"));
      Enter (Critical_Section);
      Old_Factory := SST.Get (Factory_Name'Unrestricted_Access);
      if Old_Factory = null then
         SST.Set (new String'(Factory_Name), Factory_Data);
         Leave (Critical_Section);
      else
         Leave (Critical_Section);
         raise Program_Error with "duplicate factory " & Factory_Name;
      end if;
      pragma Debug (C, O ("Register_Factory: leave"));
   end Register_Factory;

begin
   pragma Debug (C, O ("Create critical section"));
   Create (Critical_Section);
end PolyORB.DSA_P.Storages;
