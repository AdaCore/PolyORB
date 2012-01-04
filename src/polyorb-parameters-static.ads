------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . P A R A M E T E R S . S T A T I C             --
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

package PolyORB.Parameters.Static is

   pragma Elaborate_Body;

   type Parameter_Ptr is access constant Standard.String;
   type Value_Ptr     is access constant Standard.String;

   type Parameter_Entry is record
      Parameter : Parameter_Ptr;
      Value     : Value_Ptr;
   end record;

   --  Static array of parameters for link-time configuration of PolyORB

   --  Requirements:
   --  - The last entry must be equal to Last_Entry: (null, null)
   --  - The application must export an array of the following type with
   --    Static_Parameters_Link_Name as the external name.

   --  See PolyORB's User Manual section 4.2 [Run-time configuration] for
   --  further information.

   type Static_Parameter_Array is
     array (Positive range <>) of Parameter_Entry;

   Static_Parameters_Link_Name : constant String :=
                                   "__PolyORB_static_parameters";
   Last_Entry : constant Parameter_Entry := (null, null);

end PolyORB.Parameters.Static;
