------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.DNS.TRANSPORT_MECHANISMS.MDNS                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Sockets;

package PolyORB.DNS.Transport_Mechanisms.MDNS is

   type MDNS_Transport_Mechanism is new Transport_Mechanism with private;

   procedure Bind_Mechanism
     (Mechanism : MDNS_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container);

   procedure Release_Contents (M : access MDNS_Transport_Mechanism);

   --  MDNS Transport Mechanism specific subprograms

   function Address_Of
     (M : MDNS_Transport_Mechanism)
      return Utils.Sockets.Socket_Name;
   --  Return address of transport mechanism's transport access point.

   type MDNS_Transport_Mechanism_Factory is
     new Transport_Mechanism_Factory with private;

   procedure Create_Factory
     (MF  : out MDNS_Transport_Mechanism_Factory;
      TAP :     Transport.Transport_Access_Point_Access);

   function Is_Local_Mechanism
     (MF : access MDNS_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class) return Boolean;

   --  MDNS Transport Mechanism Factory specific subprograms

   function Create_Transport_Mechanism
     (MF : MDNS_Transport_Mechanism_Factory)
      return Transport_Mechanism_Access;
   --  Create transport mechanism

   function Create_Transport_Mechanism
     (Address : Utils.Sockets.Socket_Name)
      return Transport_Mechanism_Access;
   --  Create transport mechanism for specified transport access point address

   function Duplicate
     (TMA : MDNS_Transport_Mechanism)
     return MDNS_Transport_Mechanism;

   function Is_Colocated
     (Left  : MDNS_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean;

private
   Default_TTL : constant Natural := 15;
   type MDNS_Transport_Mechanism is new Transport_Mechanism with record
      Address : Utils.Sockets.Socket_Name_Ptr;
   end record;

   type MDNS_Transport_Mechanism_Factory is
     new Transport_Mechanism_Factory with
   record
      Address : Utils.Sockets.Socket_Name_Ptr;
   end record;

end PolyORB.DNS.Transport_Mechanisms.MDNS;
