------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O _ S T A T I C _ C O N F                        --
--                                                                          --
--                                 S p e c                                  --
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
with PolyORB.Parameters.Static;
use  PolyORB.Parameters.Static;

--  Static configuration of PolyORB
package PO_Static_Conf is

   --  Strings can be completely static (no need to import memory management
   --   symbols), or dynamic to ease editing.

   --  Parameters
   uipmc   : aliased constant String := "[access_points]uipmc";
   srp     : aliased constant String := "[access_points]srp";

   --  Values
   enable  : aliased constant String := "enable";
   disable : aliased constant String := "disable";

   --  Each line can easily commented out during development to test
   --  different configuration options.

   Static_Parameters : constant Static_Parameter_Array := (
   --  Parameters for tasking
--       (new String'("[tasking]max_threads"), new String'("25")),

   --  Enable/Disable access points
--       (new String'("[access_points]iiop"),  disable'Access),
--       (srp'Access,                          enable'Access),
      (uipmc'Access,                        disable'Access),

      --  This MUST be the last element of the array:
      (null, null)
   );

   --  The package and variables can have any name, but the array must be
   --  exported with the following Convetion and External name:
   pragma Export (Convention    => Ada,
                  Entity        => Static_Parameters,
                  External_Name => "__PolyORB_static_parameters");

end PO_Static_Conf;
