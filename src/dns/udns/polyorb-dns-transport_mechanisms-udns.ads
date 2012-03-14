------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.DNS.TRANSPORT_MECHANISMS.UDNS                   --
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

with PolyORB.Utils.Sockets;

package PolyORB.DNS.Transport_Mechanisms.UDNS is

   type UDNS_Transport_Mechanism is new Transport_Mechanism with private;

   overriding procedure Bind_Mechanism
     (Mechanism : UDNS_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container);

   type UDNS_Transport_Mechanism_Factory is
     new Transport_Mechanism_Factory with private;

   overriding procedure Create_Factory
     (MF  : out UDNS_Transport_Mechanism_Factory;
      TAP :     Transport.Transport_Access_Point_Access);

   overriding function Is_Local_Mechanism
     (MF : access UDNS_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class) return Boolean;

   --  UDNS Transport Mechanism Factory specific subprograms

   overriding function Create_Transport_Mechanism
     (MF : UDNS_Transport_Mechanism_Factory)
      return Transport_Mechanism_Access;
   --  Create transport mechanism

   function Create_Transport_Mechanism
     (Address : Utils.Sockets.Socket_Name)
      return Transport_Mechanism_Access;
   --  Create transport mechanism for specified transport access point address

   overriding function Is_Colocated
     (Left  : UDNS_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean;

private

   type UDNS_Transport_Mechanism is new Transport_Mechanism with null record;
   type UDNS_Transport_Mechanism_Factory is
     new Transport_Mechanism_Factory with null record;

end PolyORB.DNS.Transport_Mechanisms.UDNS;
