------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . D N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010, Free Software Foundation, Inc.          --
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
with PolyORB.DNS.Transport_Mechanisms;

package PolyORB.Binding_Data.DNS is

   package PDTM renames PolyORB.DNS.Transport_Mechanisms;

   type DNS_Profile_Type is abstract new Profile_Type with private;
   type DNS_Profile_Factory is abstract new Profile_Factory with private;

   procedure Bind_Profile
     (Profile : access DNS_Profile_Type;
      The_ORB : Components.Component_Access;
      QoS     : PolyORB.QoS.QoS_Parameters;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container);

   procedure Release (P : in out DNS_Profile_Type);

   function Is_Colocated
     (Left  : DNS_Profile_Type;
      Right : Profile_Type'Class) return Boolean;

   function Is_Local_Profile
     (PF : access DNS_Profile_Factory;
      P  : access Profile_Type'Class) return Boolean;

   function Get_Primary_Transport_Mechanism
     (P : DNS_Profile_Type) return PDTM.Transport_Mechanism_Access;
   --  Return primary transport mechanism for profile

   function Get_Primary_Transport_Mechanism_Factory
     (P : DNS_Profile_Factory) return PDTM.Transport_Mechanism_Factory_Access;
   --  Return primary transport mechanism factory for profile factory.

private

   type DNS_Profile_Type is abstract new Profile_Type with record
      --  Transport mechanisms list
      Mechanisms : PDTM.Transport_Mechanism_List;
   end record;

   type DNS_Profile_Factory is abstract new Profile_Factory with record
      Mechanisms : PDTM.Transport_Mechanism_Factory_List;
   end record;

end PolyORB.Binding_Data.DNS;
