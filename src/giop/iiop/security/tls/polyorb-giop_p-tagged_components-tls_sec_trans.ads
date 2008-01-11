------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.TLS_SEC_TRANS               --
--                                                                          --
--                                 S p e c                                  --
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

--  Used only as transport_mech in TAG_CSI_SEC_MECH_LIST tagged component

with PolyORB.Security.Types;
with PolyORB.Utils.Sockets;

package PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans is

   use PolyORB.Utils.Sockets;
   package Socket_Name_Lists is
     new PolyORB.Utils.Chained_Lists (Utils.Sockets.Socket_Name_Ptr);

   type TC_TLS_Sec_Trans is new Tagged_Component
     (Tag => Tag_TLS_Sec_Trans, At_Most_Once => False)
   with record
      Target_Supports : PolyORB.Security.Types.Association_Options;
      Target_Requires : PolyORB.Security.Types.Association_Options;
      Addresses       : Socket_Name_Lists.List;
   end record;

   procedure Marshall_Component_Data
     (C      : access TC_TLS_Sec_Trans;
      Buffer : access Buffer_Type);

   procedure Unmarshall_Component_Data
     (C      : access TC_TLS_Sec_Trans;
      Buffer : access Buffer_Type;
      Error  :    out PolyORB.Errors.Error_Container);

   procedure Release_Contents (C : access TC_TLS_Sec_Trans);

   function Duplicate (C : TC_TLS_Sec_Trans) return Tagged_Component_Access;

end PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans;
