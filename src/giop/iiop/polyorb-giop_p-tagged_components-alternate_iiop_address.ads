------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         POLYORB.GIOP_P.TAGGED_COMPONENTS.ALTERNATE_IIOP_ADDRESS          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

--  TAG_ALTERNATE_IIOP_ADDRESS tagged component

with PolyORB.Sockets;

package PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address is

   type TC_Alternate_IIOP_Address is
     new Tagged_Component
     (Tag => Tag_Alternate_IIOP_Address, At_Most_Once => False)
     with record
        Address : PolyORB.Sockets.Sock_Addr_Type;
     end record;

   procedure Marshall
     (C      : access TC_Alternate_IIOP_Address;
      Buffer : access Buffer_Type);

   procedure Unmarshall
     (C      : access TC_Alternate_IIOP_Address;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   procedure Release_Contents (C : access TC_Alternate_IIOP_Address);

   function Duplicate
     (C : TC_Alternate_IIOP_Address)
     return Tagged_Component_Access;

end PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
