------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.GIOP_P.TAGGED_COMPONENTS.ALTERNATE_IIOP_ADDRESS          --
--                                                                          --
--                                 S p e c                                  --
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

--  TAG_ALTERNATE_IIOP_ADDRESS tagged component

with PolyORB.Utils.Sockets;

package PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address is

   type TC_Alternate_IIOP_Address is
     new Tagged_Component
     (Tag => Tag_Alternate_IIOP_Address, At_Most_Once => False)
     with record
        Address : Utils.Sockets.Socket_Name_Ptr;
     end record;

   overriding procedure Marshall_Component_Data
     (C      : access TC_Alternate_IIOP_Address;
      Buffer : access Buffer_Type);

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_Alternate_IIOP_Address;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   overriding procedure Release_Contents
     (C : access TC_Alternate_IIOP_Address);

   overriding function Duplicate
     (C : TC_Alternate_IIOP_Address) return Tagged_Component_Access;

end PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
