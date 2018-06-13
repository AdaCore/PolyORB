------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.TRANSPORT_MECHANISMS.SSLIOP                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
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

with PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans;
with PolyORB.Utils.Sockets;

package PolyORB.GIOP_P.Transport_Mechanisms.SSLIOP is

   type SSLIOP_Transport_Mechanism is new Transport_Mechanism with private;

   overriding procedure Bind_Mechanism
     (Mechanism : SSLIOP_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container);

   overriding procedure Release_Contents
     (M : access SSLIOP_Transport_Mechanism);

   type SSLIOP_Transport_Mechanism_Factory is
     new Transport_Mechanism_Factory with private;

   overriding procedure Create_Factory
     (MF  : out SSLIOP_Transport_Mechanism_Factory;
      TAP : access Transport.Transport_Access_Point'Class);

   overriding function Is_Local_Mechanism
     (MF : access SSLIOP_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class)
      return Boolean;

   overriding function Create_Tagged_Components
     (MF : SSLIOP_Transport_Mechanism_Factory)
      return Tagged_Components.Tagged_Component_List;

   overriding function Duplicate
     (TMA : SSLIOP_Transport_Mechanism)
     return SSLIOP_Transport_Mechanism;

   overriding function Is_Colocated
     (Left  : SSLIOP_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean;

private

   type SSLIOP_Transport_Mechanism is new Transport_Mechanism with record
      Target_Supports : Tagged_Components.SSL_Sec_Trans.Association_Options;
      Target_Requires : Tagged_Components.SSL_Sec_Trans.Association_Options;
      Address         : Utils.Sockets.Socket_Name_Ptr;
   end record;

   type SSLIOP_Transport_Mechanism_Factory is
     new Transport_Mechanism_Factory with
   record
      Target_Supports : Tagged_Components.SSL_Sec_Trans.Association_Options;
      Target_Requires : Tagged_Components.SSL_Sec_Trans.Association_Options;
      Address         : Utils.Sockets.Socket_Name_Ptr;
   end record;

end PolyORB.GIOP_P.Transport_Mechanisms.SSLIOP;
