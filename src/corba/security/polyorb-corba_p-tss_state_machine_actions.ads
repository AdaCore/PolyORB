------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.CORBA_P.TSS_STATE_MACHINE_ACTIONS                 --
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
