------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . B I N D I N G _ D A T A . C R E A T E           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data;
with PO_CreateRef_Parse_Cmd; use PO_CreateRef_Parse_Cmd;

package PolyORB.Binding_Data.Create is

   type Create_Procedure is access procedure
     (Param   : Parameter_Profile;
      Profile : out PolyORB.Binding_Data.Profile_Access;
      Error   : out Boolean);

   procedure Register
     (Profile : String;
      Create  : Create_Procedure);

   procedure Create_Profile
     (Param   : Parameter_Profile;
      Profile : out PolyORB.Binding_Data.Profile_Access;
      Error   : out Boolean);

end PolyORB.Binding_Data.Create;
