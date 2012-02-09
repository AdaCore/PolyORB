------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . Q O S . T R A N S P O R T _ C O N T E X T S        --
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

pragma Ada_2005;

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

   overriding procedure Release_Contents
     (QoS : access QoS_Transport_Context_Parameter);

end PolyORB.QoS.Transport_Contexts;
