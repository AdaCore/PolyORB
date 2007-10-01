------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . G I O P _ P . S E R V I C E _ C O N T E X T S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

--  Support package for GIOP Service Contexts

with PolyORB.Buffers;
with PolyORB.QoS.Service_Contexts;

package PolyORB.GIOP_P.Service_Contexts is

   package PRSC renames PolyORB.QoS.Service_Contexts;

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      SCP    : PRSC.QoS_GIOP_Service_Contexts_Parameter_Access);

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      SCP    :    out PRSC.QoS_GIOP_Service_Contexts_Parameter_Access);

end PolyORB.GIOP_P.Service_Contexts;
