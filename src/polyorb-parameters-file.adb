------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . P A R A M E T E R S . F I L E               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2013, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with Ada.Text_IO;

with PolyORB.Initialization;
with PolyORB.Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Configuration_File;

package body PolyORB.Parameters.File is

   use Ada.Text_IO;

   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Configuration_File.Configuration_Table;

   --------------------------------------------------------
   -- Table of configuration parameters loaded from file --
   --------------------------------------------------------

   Configuration_Table : Table_Instance;

   ----------------------
   -- File data source --
   ----------------------

   type File_Source is new Parameters_Source with null record;

   overriding function Get_Conf
     (Source       : access File_Source;
      Section, Key : String) return String;

   The_File_Source : aliased File_Source;

   ---------------------
   -- Fetch_From_File --
   ---------------------

   function Fetch_From_File (Value : String) return String;
   --  Given a value of the form "file:<filename>", return the first line
   --  of the named file.

   function Fetch_From_File (Value : String) return String is
      Filename : constant String := Value (Value'First + 5 .. Value'Last);
      File     : File_Type;
      Result   : String (1 .. 1024);
      Last     : Natural;
   begin
      Open (File, In_File, Filename);
      Get_Line (File, Result, Last);
      Close (File);
      return Result (1 .. Last);

   exception
      when Name_Error =>
         return "";
   end Fetch_From_File;

   --------------
   -- Get_Conf --
   --------------

   overriding function Get_Conf
     (Source       : access File_Source;
      Section, Key : String) return String
   is
      pragma Unreferenced (Source);

      V : constant String_Ptr :=
        Lookup (Configuration_Table, Make_Global_Key (Section, Key), null);

   begin
      if V /= null then
         return V.all;
      else
         return "";
      end if;
   end Get_Conf;

   -----------------------------
   -- Load_Configuration_File --
   -----------------------------

   procedure Load_Configuration_File (Conf_File_Name : String) is
   begin
      PolyORB.Utils.Configuration_File.Load_Configuration_Table
        (Conf_File_Name,
         Conf_File_Name = PolyORB_Conf_Default_Filename,
         Configuration_Table);
   end Load_Configuration_File;

   -----------------------------
   -- Configuration_File_Name --
   -----------------------------

   function Configuration_File_Name return String is
   begin
      --  The key and section here are chosen so that the associated
      --  environment variable (in the context of the Parameters.Environment
      --  data source) is POLYORB_CONF.

      return Get_Conf (Section => "conf", Key => "",
                       Default => PolyORB_Conf_Default_Filename);
   end Configuration_File_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      --  Set up configuration table and initialize intrinsic defaults

      Initialize (Configuration_Table);
      Insert (Configuration_Table,
        Make_Global_Key ("macros", "hostname"),
        new String'(Sockets.Host_Name));

      --  Load configuration file

      Load_Configuration_File (Configuration_File_Name);

      --  Register parameter source in configuration framework

      Register_Source (The_File_Source'Access);

      Fetch_From_File_Hook := Fetch_From_File'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"parameters.file",
       Conflicts => Empty,
       Depends   => Empty
         & "parameters.command_line?"
         & "parameters.environment?"
         & "parameters.overrides?"
         & "utils.configuration_file",
       Provides  => +"parameters_sources" & "parameters_sources.runtime",
       Implicit  => True,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Parameters.File;
