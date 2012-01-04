------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . D N S . T R A N S P O R T _ M E C H A N I S M S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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

--  Abstraction for DNS Transport Mechanisms

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.QoS;
with PolyORB.Smart_Pointers;
with PolyORB.Transport;
with PolyORB.Utils.Sockets;

package PolyORB.DNS.Transport_Mechanisms is

   --  Transport mechanism

   type Transport_Mechanism is abstract tagged private;
   type Transport_Mechanism_Access is access all Transport_Mechanism'Class;

   function Address_Of
     (M : Transport_Mechanism) return Utils.Sockets.Socket_Name;
   --  Return address of transport mechanism's transport access point

   procedure Bind_Mechanism
     (Mechanism : Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container) is abstract;

   procedure Release_Contents (M : access Transport_Mechanism);

   --  Transport mechanism factory

   type Transport_Mechanism_Factory is abstract tagged limited private;

   type Transport_Mechanism_Factory_Access is
     access all Transport_Mechanism_Factory'Class;

   procedure Create_Factory
     (MF  : out Transport_Mechanism_Factory;
      TAP :     Transport.Transport_Access_Point_Access) is abstract;
   --  Initialize MF to act as transport mechanism factory for
   --  transport access point TAP

   function Create_Transport_Mechanism
     (MF : Transport_Mechanism_Factory)
      return Transport_Mechanism_Access is abstract;

   function Is_Local_Mechanism
     (MF : access Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class)
      return Boolean is abstract;
   --  True iff M designates an mechanism that can be contacted at the access
   --  point associated with MF

   function Duplicate
     (TMA : Transport_Mechanism) return Transport_Mechanism'Class;

   function Is_Colocated
     (Left  : Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean is abstract;
   --  True iff Left and Right mechanisms point to the same node.

private

   type Transport_Mechanism_Factory is abstract tagged limited record
      Address : Utils.Sockets.Socket_Name_Ptr;
   end record;

   type Transport_Mechanism is abstract tagged record
      Address : Utils.Sockets.Socket_Name_Ptr;
   end record;

end PolyORB.DNS.Transport_Mechanisms;
