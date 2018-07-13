------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . P A R A M E T E R S . S T A T I C             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2015, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with System;
use type System.Address;

package body PolyORB.Parameters.Static is

   --  The length of the actual (user-provided) array is unknown: the last
   --  entry is denoted by Last_Entry. Note: the index subtype used to be
   --  1..1, but this may lead to a loop iterating on the array's elements
   --  to be incorrectly optimized out at -O2.

   pragma Suppress (Range_Check);
   Parameters : Static_Parameter_Array  (1 .. Positive'Last);
   pragma Import (Ada, Parameters, Static_Parameters_Link_Name);

   Last_Index : Natural := 0;
   --  Actual index of last entry

   pragma Warnings (Off); -- WAG:GPL2007
   pragma Weak_External (Parameters);
   pragma Warnings (On); -- WAG:GPL2007

   --  This symbol is optional, PolyORB can be configured using other methods
   --  like the command line or environment variables.
   --  In some platforms like VxWorks 5.5 the loader gives a warning even if
   --  the unresolved symbol is weak. This external name was chosen to avoid
   --  alarming the user when this happen instead of a more descriptive one.

   type Static_Source is new Parameters_Source with null record;

   overriding function Get_Conf
     (Source       : access Static_Source;
      Section, Key : String) return String;

   procedure Initialize;

   --------------
   -- Get_Conf --
   --------------

   overriding function Get_Conf
      (Source       : access Static_Source;
       Section, Key : String) return String
   is
      pragma Unreferenced (Source);

      S : constant String := Make_Global_Key (Section, Key);
   begin
      for J in 1 .. Last_Index loop
         if Parameters (J).Parameter.all = S then
            return Parameters (J).Value.all;
         end if;
      end loop;
      return "";
   end Get_Conf;

   The_Static_Source : aliased Static_Source;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  If a weak symbol isn't resolved by the linker, it is assigned the
      --  null address.

      if Parameters'Address /= System.Null_Address then
         loop
            --  Last entry has null Parameter and Value components

            if Parameters (Last_Index + 1).Parameter = null then
               exit;

            else
               Last_Index := Last_Index + 1;
            end if;
         end loop;
      end if;
      Register_Source (The_Static_Source'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   --  The static parameters source depends on parameters_sources.runtime
   --  so that its values can be overridden at run time.

   Register_Module
      (Module_Info'
         (Name      => +"parameters.static",
          Conflicts => Empty,
          Depends   => +"parameters_sources.runtime",
          Provides  => +"parameters_sources",
          Implicit  => True,
          Init      => Initialize'Access,
          Shutdown  => null));
end PolyORB.Parameters.Static;
