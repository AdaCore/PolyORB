------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . C L I E N T S _ S E C U R I T Y          --
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

--  Client Side CORBA CSI Version 2 Compound Mechanism Configuration

with PolyORB.Annotations;
with PolyORB.Security.Authentication_Mechanisms;
with PolyORB.Security.Authority_Mechanisms;
with PolyORB.Security.Transport_Mechanisms;
with PolyORB.Security.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.QoS.Clients_Security is

   type Client_Mechanism is limited record
      Stateful                 : Boolean;
      --  Client may establish stateful security context

      Transport                :
        PolyORB.Security.Transport_Mechanisms.
          Client_Transport_Mechanism_Access;

      --  Authentication layer

      Authentication_Mechanism :
        PolyORB.Security.Authentication_Mechanisms.
          Client_Authentication_Mechanism_Access;
      Authentication_Required  : Boolean;

      --  Attribute layer

      Identity_Assertion       : Boolean;
      Authorities              :
        PolyORB.Security.Authority_Mechanisms.
          Client_Authority_Mechanism_Lists.List;
      Delegation_Supported     : Boolean;
      Delegation_Required      : Boolean;
      Naming_Mechanisms        : PolyORB.Security.Types.OID_Lists.List;
      Identity_Types           : PolyORB.Security.Types.Identity_Token_Type;

      Notepad                  : PolyORB.Annotations.Notepad;
   end record;

   type Client_Mechanism_Access is access all Client_Mechanism;

   function Is_Protected (Mechanism : Client_Mechanism) return Boolean;

   function Target_Supports
     (Mechanism : Client_Mechanism)
      return PolyORB.Security.Types.Association_Options;

   function Target_Requires
     (Mechanism : Client_Mechanism)
      return PolyORB.Security.Types.Association_Options;

   package Client_Mechanism_Lists is
     new PolyORB.Utils.Chained_Lists (Client_Mechanism_Access);

   type QoS_Client_Security_Parameter is
     new QoS_Parameter (Compound_Security) with
   record
      Mechanisms : Client_Mechanism_Lists.List;
      --  List of available compound security mechanisms.
   end record;

   type QoS_Client_Security_Parameter_Access is
     access all QoS_Client_Security_Parameter;

   procedure Release_Contents (QoS : access QoS_Client_Security_Parameter);

   procedure Destroy (Mechanism : in out Client_Mechanism_Access);

end PolyORB.QoS.Clients_Security;
