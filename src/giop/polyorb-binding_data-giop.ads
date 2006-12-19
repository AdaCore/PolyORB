------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . G I O P             --
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

with PolyORB.GIOP_P.Tagged_Components;
with PolyORB.GIOP_P.Transport_Mechanisms;

package PolyORB.Binding_Data.GIOP is

   type GIOP_Profile_Type is abstract new Profile_Type with private;
   type GIOP_Profile_Factory is abstract new Profile_Factory with private;

   procedure Bind_Profile
     (Profile : access GIOP_Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container);

   procedure Release (P : in out GIOP_Profile_Type);

   function Get_Component
     (P : GIOP_Profile_Type;
      C : PolyORB.GIOP_P.Tagged_Components.Tag_Value)
      return PolyORB.GIOP_P.Tagged_Components.Tagged_Component_Access;

   function Is_Colocated
     (Left  : GIOP_Profile_Type;
      Right : Profile_Type'Class) return Boolean;

   function Is_Local_Profile
     (PF : access GIOP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean;

   function Get_Primary_Transport_Mechanism
     (P : GIOP_Profile_Type)
      return PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Access;
   --  Return primary transport mechanism for profile.

   function Get_Primary_Transport_Mechanism_Factory
     (P : GIOP_Profile_Factory)
      return
        PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Factory_Access;
   --  Return primary transport mechanism factory for profile factory.

   type Is_Security_Selected_Hook is
     access function
     (QoS       : PolyORB.QoS.QoS_Parameters;
      Mechanism :
        PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Access)
     return Boolean;

   Is_Security_Selected : Is_Security_Selected_Hook := null;
   --  This hook is used in profile binding procedure for avoid binding
   --  transport mechanism others than selected by security service.
   --  Binding of such mechanism may have unexpected behavior because some
   --  security related information (credentials, for example) not available.

private

   type GIOP_Profile_Type is abstract new Profile_Type with record
      Version_Major : Types.Octet;
      Version_Minor : Types.Octet;

      --  Tagged components list

      Components    : PolyORB.GIOP_P.Tagged_Components.Tagged_Component_List;

      --  Transport mechanisms list

      Mechanisms    :
        PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_List;
   end record;

   type GIOP_Profile_Factory is abstract new Profile_Factory with record
      Mechanisms :
        PolyORB.GIOP_P.Transport_Mechanisms.Transport_Mechanism_Factory_List;
   end record;

end PolyORB.Binding_Data.GIOP;
