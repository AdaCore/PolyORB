------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . D E F A U L T _ P A R A M E T E R S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
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
