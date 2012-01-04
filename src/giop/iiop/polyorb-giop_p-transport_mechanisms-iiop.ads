------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TRANSPORT_MECHANISMS.IIOP                  --
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

with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Sockets;

package PolyORB.GIOP_P.Transport_Mechanisms.IIOP is

   type IIOP_Transport_Mechanism is new Transport_Mechanism with private;

   procedure Bind_Mechanism
     (Mechanism : IIOP_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container);

   procedure Release_Contents (M : access IIOP_Transport_Mechanism);

   --  IIOP Transport Mechanism specific subprograms

   function Primary_Address_Of
     (M : IIOP_Transport_Mechanism) return Utils.Sockets.Socket_Name;
   --  Return the primary access point name of M

   type IIOP_Transport_Mechanism_Factory is
     new Transport_Mechanism_Factory with private;

   procedure Create_Factory
     (MF  : out IIOP_Transport_Mechanism_Factory;
      TAP :     Transport.Transport_Access_Point_Access);

   function Is_Local_Mechanism
     (MF : access IIOP_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class)
      return Boolean;

   function Is_Colocated
     (Left  : IIOP_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean;

   function Create_Tagged_Components
     (MF : IIOP_Transport_Mechanism_Factory)
      return Tagged_Components.Tagged_Component_List;

   --  IIOP Transport Mechanism Factory specific subprograms

   function Create_Transport_Mechanism
     (MF : IIOP_Transport_Mechanism_Factory)
      return Transport_Mechanism_Access;
   --  Create transport mechanism

   function Create_Transport_Mechanism
     (Address : Utils.Sockets.Socket_Name)
      return Transport_Mechanism_Access;
   --  Create transport mechanism for specified transport access point name

   procedure Disable_Transport_Mechanism
     (MF : in out IIOP_Transport_Mechanism_Factory);
   --  Disable transport mechanism if it is a primary mechanism

   function Duplicate
     (TMA : IIOP_Transport_Mechanism) return IIOP_Transport_Mechanism;

private

   use Utils.Sockets;

   package Socket_Name_Lists is
     new PolyORB.Utils.Chained_Lists (Socket_Name_Ptr);
   type IIOP_Transport_Mechanism is new Transport_Mechanism with record
      Addresses : Socket_Name_Lists.List;
   end record;

   type IIOP_Transport_Mechanism_Factory is
     new Transport_Mechanism_Factory with
   record
      Disabled  : Boolean := False;
      Addresses : Socket_Name_Lists.List;
   end record;

end PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
