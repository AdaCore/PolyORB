------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . P A R A M E T E R S . F I L E               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2011, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with PolyORB.Initialization;

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

   function Get_Conf
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

   function Get_Conf
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
      Initialize (Configuration_Table);
      Load_Configuration_File (Configuration_File_Name);
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
