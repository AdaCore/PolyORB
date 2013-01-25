------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           POLYORB.GIOP_P.TAGGED_COMPONENTS.CSI_SEC_MECH_LIST             --
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

--  TAG_CSI_SEC_MECH_LIST Tagged Component

with PolyORB.ASN1;
with PolyORB.GIOP_P.Transport_Mechanisms;
with PolyORB.Security.Exported_Names;
with PolyORB.Security.Authority_Mechanisms;
with PolyORB.Security.Transport_Mechanisms;
with PolyORB.Security.Types;
with PolyORB.Transport;

package PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List is

   type Service_Configuration is record
      Syntax :
        PolyORB.Security.Authority_Mechanisms.Service_Configuration_Syntax;
      Name   : PolyORB.Security.Types.Stream_Element_Array_Access;
   end record;

   package Service_Configuration_Lists is
     new PolyORB.Utils.Chained_Lists (Service_Configuration);

   type Mechanism is record
      Target_Requires             : PolyORB.Security.Types.Association_Options;
      Transport_Mechanism_Tag     : Tagged_Component_Access;
      Authentication_Target_Supports :
        PolyORB.Security.Types.Association_Options;
      Authentication_Target_Requires :
        PolyORB.Security.Types.Association_Options;
      Authentication_Mechanism    : PolyORB.ASN1.Object_Identifier;
      Authentication_Target_Name  :
        PolyORB.Security.Exported_Names.Exported_Name_Access;
      Attribute_Target_Supports   : PolyORB.Security.Types.Association_Options;
      Attribute_Target_Requires   : PolyORB.Security.Types.Association_Options;
      Attribute_Privilege_Authorities : Service_Configuration_Lists.List;
      Attribute_Naming_Mechanisms : PolyORB.Security.Types.OID_Lists.List;
      Attribute_Identity_Types    : PolyORB.Security.Types.Identity_Token_Type;

      Transport_Mechanism         :
        PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Access;
      --  Corresponding GIOP Transport Mechanism. This item shared with
      --  Profile's list of Transport Mechanisms.
   end record;

   type Mechanism_Access is access all Mechanism;

   package Mechanism_Lists is
     new PolyORB.Utils.Chained_Lists (Mechanism_Access);

   type TC_CSI_Sec_Mech_List is new Tagged_Component
     (Tag => Tag_CSI_Sec_Mech_List, At_Most_Once => True)
   with record
      Stateful   : Boolean;
      Mechanisms : Mechanism_Lists.List;
   end record;

   type TC_CSI_Sec_Mech_List_Access is
     access all TC_CSI_Sec_Mech_List'Class;

   overriding procedure Marshall_Component_Data
     (C      : access TC_CSI_Sec_Mech_List;
      Buffer : access Buffer_Type);

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_CSI_Sec_Mech_List;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   overriding procedure Release_Contents (C : access TC_CSI_Sec_Mech_List);

   overriding function Duplicate (C : TC_CSI_Sec_Mech_List)
     return Tagged_Component_Access;

   --  Registry for Transport Mechanisms Tagged Components

   type To_Security_Transport_Mechanism is
     access function
     (TC : access Tagged_Component'Class)
      return
       PolyORB.Security.Transport_Mechanisms.Client_Transport_Mechanism_Access;

   type To_Tagged_Component is
     access function
     (TM : PolyORB.Security.Transport_Mechanisms.
       Target_Transport_Mechanism_Access)
     return Tagged_Component_Access;

   procedure Register
     (Tag            : Tag_Value;
      TC_Constructor : To_Tagged_Component;
      TM_Constructor : To_Security_Transport_Mechanism);

   --  Registry for Transport Mechanism's QoS constructors

   type QoS_Constructor is
     access function
     (End_Point : PolyORB.Transport.Transport_Endpoint_Access)
     return PolyORB.QoS.QoS_Parameter_Access;

   procedure Register (Constructor : QoS_Constructor);

end PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List;
