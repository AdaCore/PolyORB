------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.CORBA_P.TSS_STATE_MACHINE_ACTIONS                 --
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

with PolyORB.Binding_Data;
with PolyORB.QoS.Transport_Contexts;
with PolyORB.References;
with PolyORB.Security.Authorization_Elements;
with PolyORB.Security.Identities;
with PolyORB.Security.Types;

package PolyORB.CORBA_P.TSS_State_Machine_Actions is

   package PS renames PolyORB.Security;

   type Accept_Context_Status is
     (Success,
      Invalid_Evidence,
      Invalid_Mechanism,
      Policy_Change,
      Conflicting_Evidence);

   function Accept_Transport_Context
     (Profile   : PolyORB.Binding_Data.Profile_Access;
      Transport :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access)
      return Boolean;
   --  Validate the request, arrives without a SAS protocol message

   procedure Accept_Context
     (Profile                     : PolyORB.Binding_Data.Profile_Access;
      Transport                   :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access;
      Client_Context_Id           :
        PolyORB.Security.Types.Context_Id;
      Client_Authentication_Token :
        PolyORB.Security.Types.Stream_Element_Array_Access;
      Identity_Token              :
        PolyORB.Security.Identities.Identity_Access;
      Authorization_Token         :
        PS.Authorization_Elements.Authorization_Element_Lists.List;
      Status                      : out Accept_Context_Status;
      Stateful                    : out Boolean;
      Reference                   : out PolyORB.References.Ref;
      Final_Token                 : out
        PolyORB.Security.Types.Stream_Element_Array_Access);
   --  Validates the security context

   procedure Reference_Context
     (Transport         :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access;
      Client_Context_Id :
        PolyORB.Security.Types.Context_Id;
      Status            : out Boolean;
      Final_Token       : out
        PolyORB.Security.Types.Stream_Element_Array_Access);
   --

   procedure Discard_Context
     (Transport         :
        PolyORB.QoS.Transport_Contexts.QoS_Transport_Context_Parameter_Access;
      Client_Context_Id :
        PolyORB.Security.Types.Context_Id);
   --  Delete context

end PolyORB.CORBA_P.TSS_State_Machine_Actions;
