------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.CORBA_P.CSS_STATE_MACHINE_ACTIONS                 --
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

--  CORBA CSI Version 2 Client Security Service State Machine Actions

with PolyORB.CORBA_P.Security_Policy;
with PolyORB.QoS.Clients_Security;
with PolyORB.Security.Authorization_Elements;
with PolyORB.Security.Identities;
with PolyORB.Security.Connections;
with PolyORB.Security.Credentials;
with PolyORB.Security.Types;

package PolyORB.CORBA_P.CSS_State_Machine_Actions is

   package PS renames PolyORB.Security;

   type Context_Element_Kind is
     (No_Element, Establish_Context, Message_In_Context);

   type Context_Element (Kind : Context_Element_Kind) is record
      case Kind is
         when No_Element =>
            null;

         when Establish_Context | Message_In_Context =>
            Client_Context_Id : PolyORB.Security.Types.Context_Id;

            case Kind is
               when No_Element =>
                  null;

               when Establish_Context =>
                  Authorization_Token         :
                    PS.Authorization_Elements.Authorization_Element_Lists.List;
                  Identity_Token              :
                    PolyORB.Security.Identities.Identity_Access;
                  Client_Authentication_Token :
                    PolyORB.Security.Types.Stream_Element_Array_Access;

               when Message_In_Context =>
                  Discard_Context : Boolean;
            end case;
      end case;
   end record;

   procedure Get_Mechanism
     (Policy        :     PolyORB.CORBA_P.Security_Policy.Client_Policy;
      Configuration :
        PolyORB.QoS.Clients_Security.Client_Mechanism_Lists.List;
      Mechanism     : out PolyORB.QoS.Clients_Security.Client_Mechanism_Access;
      Success       : out Boolean);
   --  Select a compound mechanism that satisfy client policy. Mechanism
   --  may be null iff target don't have security support. Success
   --  should be set to True iff compound mechanism that satisfy client
   --  policy has been found.

   function Get_Client_Credentials
     (Policy    : PolyORB.CORBA_P.Security_Policy.Client_Policy;
      Mechanism : PolyORB.QoS.Clients_Security.Client_Mechanism_Access)
      return PolyORB.Security.Credentials.Credentials_Ref;
   --  Return the client credentials as necessary to satisfy client policy
   --  and then target policy in the mechanism

   function Get_Context_Element
     (Policy      : PolyORB.CORBA_P.Security_Policy.Client_Policy;
      Mechanism   : PolyORB.QoS.Clients_Security.Client_Mechanism_Access;
      Credentials : PolyORB.Security.Credentials.Credentials_Ref;
      Connection  : PolyORB.Security.Connections.Connection_Access)
      return Context_Element;
   --  In the scope of connection, use the client credentials to create
   --  context element that satisfies the client policy and the target
   --  policy in mechanism

   procedure Invalidate_Context
     (Connection        : PolyORB.Security.Connections.Connection_Access;
      Client_Context_Id : PolyORB.Security.Types.Context_Id);

   procedure Invalidate_Context
     (Connection        : PolyORB.Security.Connections.Connection_Access;
      Client_Context_Id : PolyORB.Security.Types.Context_Id;
      Error_Token       : PolyORB.Security.Types.Stream_Element_Array_Access);
   --  Mark context in connection scope as invalid

   procedure Complete_Context
     (Connection          : PolyORB.Security.Connections.Connection_Access;
      Client_Context_Id   : PolyORB.Security.Types.Context_Id;
      Context_Stateful    : Boolean;
      Final_Context_Token :
        PolyORB.Security.Types.Stream_Element_Array_Access);

end PolyORB.CORBA_P.CSS_State_Machine_Actions;
