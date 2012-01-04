------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.GIOP_P.TAGGED_COMPONENTS.TLS_SEC_TRANS               --
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
