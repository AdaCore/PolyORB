------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . Q O S . T R A N S P O R T _ C O N T E X T S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Objects;
with PolyORB.QoS.Clients_Security;
with PolyORB.Security.Credentials;
with PolyORB.Security.Transport_Mechanisms;

package PolyORB.QoS.Transport_Contexts is

   type QoS_Transport_Context_Parameter is
     new QoS_Parameter (Transport_Security) with
   record
      Selected               :
       PolyORB.QoS.Clients_Security.Client_Mechanism_Access;
      --  Client compound security mechanism selected by client security
      --  service for invocation.

      Invocation_Credentials : PolyORB.Security.Credentials.Credentials_Ref;
      --  Compound credentials used for request invocation.

      Transport              :
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access;
      --  Transport mechanism, used for receive request. If request is a local
      --  request then it null.

      Accepting_Credentials  : PolyORB.Security.Credentials.Credentials_Ref;
      --  Transport creadentials which has been used for setup connection.
      --  Null if request is local.

      Binding_Object         : PolyORB.Binding_Objects.Binding_Object_Access;
      --  Binding object, used for object reference binding.
   end record;

   type QoS_Transport_Context_Parameter_Access is
     access all QoS_Transport_Context_Parameter'Class;

   procedure Release_Contents (QoS : access QoS_Transport_Context_Parameter);

end PolyORB.QoS.Transport_Contexts;
