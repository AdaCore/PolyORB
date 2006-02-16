------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . P A R A M E T E R S . P A R T I T I O N          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Parameters.Partition_Conf;

with PolyORB.Utils.Strings;

package body PolyORB.Parameters.Partition is

   use PolyORB.Utils.Strings;
   use PolyORB.Utils;

   ---------------------------
   -- Partition data source --
   ---------------------------

   type Partition_Source is new Parameters_Source with null record;

   function Get_Conf
     (Source       : access Partition_Source;
      Section, Key : String) return String;

   The_Partition_Source : aliased Partition_Source;

   --------------
   -- Get_Conf --
   --------------

   function Get_Conf
     (Source       : access Partition_Source;
      Section, Key : String) return String
   is
      pragma Unreferenced (Source);
      Global_Key : constant String  := Make_Global_Key (Section, Key);
      Index      : constant Integer := Partition_Conf.Lookup (Global_Key);

   begin
      if Index = -1 then
         return "";
      end if;
      return Partition_Conf.Table (Index).Val.all;
   end Get_Conf;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Parameters.Partition_Conf.Initialize;
      Register_Source (The_Partition_Source'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"parameters.partition",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"parameters_sources",
       Implicit  => True,
       Init      => Initialize'Access));
end PolyORB.Parameters.Partition;
