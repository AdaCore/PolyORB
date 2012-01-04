------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES.CREATE              --
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

with PO_CreateRef_Parse_Cmd; use PO_CreateRef_Parse_Cmd;
with PolyORB.GIOP_P.Tagged_Components.Policies;

package PolyORB.GIOP_P.Tagged_Components.Policies.Create is
   use PolyORB.GIOP_P.Tagged_Components.Policies;

   procedure Create_TC
     (Param      : Parameter_Component;
      TC         : in out TC_Policies;
      Error      : out Boolean);

end PolyORB.GIOP_P.Tagged_Components.Policies.Create;
