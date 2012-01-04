------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           R T C O R B A . P R O T O C O L P R O P E R T I E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2012, Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

pragma Style_Checks ("NM32766");

with PolyORB.Std;
with CORBA.Object;

package RTCORBA.ProtocolProperties is

   type Local_Ref is new CORBA.Object.Ref with null record;

   Repository_Id : constant PolyORB.Std.String
     := "IDL:omg.org/RTCORBA/ProtocolProperties:1.0";

   package Convert_Forward is
     new RTCORBA.ProtocolProperties_Forward.Convert (Local_Ref);
end RTCORBA.ProtocolProperties;
