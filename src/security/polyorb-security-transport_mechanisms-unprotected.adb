------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.SECURITY.TRANSPORT_MECHANISMS.UNPROTECTED             --
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

package body PolyORB.Security.Transport_Mechanisms.Unprotected is

   -----------------
   -- Is_Supports --
   -----------------

   function Is_Supports
     (Mechanism   : access Unprotected_Transport_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
      return Boolean
   is
      pragma Unreferenced (Mechanism);
      pragma Unreferenced (Credentials);

   begin
      return False;
   end Is_Supports;

   ---------------------
   -- Target_Requires --
   ---------------------

   function Target_Requires
     (Mechanism : access Unprotected_Transport_Mechanism)
      return PolyORB.Security.Types.Association_Options
   is
      pragma Unreferenced (Mechanism);

   begin
      return 0;
   end Target_Requires;

   ---------------------
   -- Target_Supports --
   ---------------------

   function Target_Supports
     (Mechanism : access Unprotected_Transport_Mechanism)
      return PolyORB.Security.Types.Association_Options
   is
      pragma Unreferenced (Mechanism);

   begin
      return 0;
   end Target_Supports;

end PolyORB.Security.Transport_Mechanisms.Unprotected;
