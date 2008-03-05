------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . P A R A M E T E R S . S T A T I C             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2008, Free Software Foundation, Inc.            --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with System;
use type System.Address;

package body PolyORB.Parameters.Static is

   --  The length of the array is unknown, the last entry must be marked with
   --  a null access.
   pragma Suppress (Range_Check);
   Parameters : Static_Parameter_Array  (1 .. 1);
   pragma Import (Ada, Parameters, "__polyorbconf_optional");
   pragma Weak_External (Parameters);
   --  This symbol is optional, PolyORB can be configured using other methods
   --  like the command line or environment variables.
   --  In some platforms like VxWorks 5.5 the loader gives a warning even if
   --  the unresolved symbol is weak. This external name was chosen to avoid
   --  alarming the user when this happen instead of a more descriptive one.

   Last : Natural := 0;

   type Partition_Source is new Parameters_Source with null record;

   --------------
   -- Get_Conf --
   --------------

   function Get_Conf
     (Source       : access Partition_Source;
      Section, Key : String) return String;

   function Get_Conf
      (Source       : access Partition_Source;
       Section, Key : String) return String
   is
      pragma Unreferenced (Source);

      S : constant String := Make_Global_Key (Section, Key);
   begin
      for I in 1 .. Last loop
         if Parameters (I).Parameter.all = S then
            return Parameters (I).Value.all;
         end if;
      end loop;
      return "";
   end Get_Conf;

   The_Static_Source : aliased Partition_Source;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Last := 0;
      --  If a weak symbol isn't resolved by the linker, it is assigned the
      --  null address.
      if Parameters'Address /= System.Null_Address then
         loop
            if Parameters (Last + 1).Parameter = null then
               exit;
            else
               Last := Last + 1;
            end if;
         end loop;
      end if;
      Register_Source (The_Static_Source'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;
begin
      Register_Module
      (Module_Info'
         (Name      => +"parameters.static",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => +"parameters_sources",
          Implicit  => True,
          Init      => Initialize'Access,
          Shutdown  => null));
end PolyORB.Parameters.Static;
