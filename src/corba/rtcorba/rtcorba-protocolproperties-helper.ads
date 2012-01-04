------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    R T C O R B A . P R O T O C O L P R O P E R T I E S . H E L P E R     --
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

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package RTCORBA.ProtocolProperties.Helper is

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class) return RTCORBA.ProtocolProperties.Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class) return RTCORBA.ProtocolProperties.Local_Ref;

   TC_ProtocolProperties : CORBA.TypeCode.Object;

end RTCORBA.ProtocolProperties.Helper;
