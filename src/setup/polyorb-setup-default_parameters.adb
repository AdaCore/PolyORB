------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . D E F A U L T _ P A R A M E T E R S      --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Parameters.Command_Line;
pragma Warnings (Off, PolyORB.Parameters.Command_Line);
pragma Elaborate_All (PolyORB.Parameters.Command_Line);

with PolyORB.Parameters.Environment;
pragma Warnings (Off, PolyORB.Parameters.Environment);
pragma Elaborate_All (PolyORB.Parameters.Environment);

with PolyORB.Parameters.File;
pragma Warnings (Off, PolyORB.Parameters.File);
pragma Elaborate_All (PolyORB.Parameters.File);

--  For embedded platforms, an additional parameter source, "Static",
--  is provided in the platform-specific base setup to support hard-coded
--  parameters provided by the application (see PolyORB.Parameters.Static
--  for usage details). For native platforms, this unit is not included by
--  default because in such context, a filesystem can be assumed to be
--  available, and so a configuration file is the preferred way of tuning
--  PolyORB. Additionally, the Static parameters source uses a weak external
--  symbol, which is not supported on all platforms.

package body PolyORB.Setup.Default_Parameters is

end PolyORB.Setup.Default_Parameters;
