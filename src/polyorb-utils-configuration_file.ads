------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . U T I L S . C O N F I G U R A T I O N _ F I L E      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Strings;
with PolyORB.Utils.HTables.Perfect;
with PolyORB.Utils.HFunctions.Hyper;

package PolyORB.Utils.Configuration_File is

   package Configuration_Table is new PolyORB.Utils.HTables.Perfect (
      PolyORB.Utils.Strings.String_Ptr,
      PolyORB.Utils.HFunctions.Hyper.Hash_Hyper_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Hyper.Hash,
      PolyORB.Utils.HFunctions.Hyper.Next_Hash_Parameters);

   procedure Load_Configuration_Table
     (Configuration_Filename : String;
      Is_Default             : Boolean;
      Table                  : in out Configuration_Table.Table_Instance);
   --  Load Configuration_Filename configuration file into Table.
   --  Is_Default is True if the Configuration_Filename is the default one.

   --  The following helper functions allow the manipulation of a
   --  configuration table and then writes it into a configuration
   --  file that can be further read by a PolyORB-based application.

   procedure Set_Conf (Configuration_Filename, Section, Key, Value : String);
   --  Add or rewrite a configuration (Section, Key) tuple with Value

   procedure Reset;
   --  Reset local configuration table

   procedure Display;
   --  Display the configuration table (only for debugging purposes)

   procedure Generate_Configuration_File (Configuration_Filename : String);
   --  Generate the configuration file

end PolyORB.Utils.Configuration_File;
