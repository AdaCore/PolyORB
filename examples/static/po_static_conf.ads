------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       P O _ S T A T I C _ C O N F                        --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Parameters.Static;
use  PolyORB.Parameters.Static;

--  Static configuration of PolyORB

package PO_Static_Conf is

   --  Strings can be completely static (no need to for dynamic memory
   --  allocation.

   --  Parameter names

   uipmc   : aliased constant String := "[access_points]uipmc";
   srp     : aliased constant String := "[access_points]srp";

   --  Values

   enable  : aliased constant String := "enable";
   disable : aliased constant String := "disable";

   --  Each line can easily commented out during development to test
   --  different configuration options.

   Static_Parameters : constant Static_Parameter_Array := (

   --  Parameters for tasking
--    (new String'("[tasking]max_threads"), new String'("25")),

   --  Enable/Disable access points
--    (new String'("[access_points]iiop"),  disable'Access),
--    (srp'Access,                          enable'Access),
      (uipmc'Access,                        disable'Access),

      --  This MUST be the last element of the array:

      (null, null)
   );

   --  The package and variables can have any name, but the array must be
   --  exported with the following Convetion and External name:

   pragma Export (Convention    => Ada,
                  Entity        => Static_Parameters,
                  External_Name => Static_Parameters_Link_Name);

end PO_Static_Conf;
