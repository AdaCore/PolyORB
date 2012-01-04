------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . B I N D I N G _ D A T A . C R E A T E           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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
