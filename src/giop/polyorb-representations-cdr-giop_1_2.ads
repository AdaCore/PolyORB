------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.REPRESENTATIONS.CDR.GIOP_1_2                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

--  Support package for CDR representation of char and strings for GIOP 1.2

with PolyORB.GIOP_P.Code_Sets.Converters;
with PolyORB.Representations.CDR.GIOP_1_1;

package PolyORB.Representations.CDR.GIOP_1_2 is

   pragma Elaborate_Body;

   type GIOP_1_2_CDR_Representation is
      new GIOP_1_1.GIOP_1_1_CDR_Representation with private;

   type GIOP_1_2_CDR_Representation_Access is
      access all GIOP_1_2_CDR_Representation;

   --  XXX Encapsulation is also GIOP version dependent.

   procedure Set_Converters
     (R : in out GIOP_1_2_CDR_Representation;
      C : PolyORB.GIOP_P.Code_Sets.Converters.Converter_Access;
      W : PolyORB.GIOP_P.Code_Sets.Converters.Wide_Converter_Access);

private

   type GIOP_1_2_CDR_Representation is
      new GIOP_1_1.GIOP_1_1_CDR_Representation with null record;

end PolyORB.Representations.CDR.GIOP_1_2;
