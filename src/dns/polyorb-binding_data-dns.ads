------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . B I N D I N G _ D A T A . D N S              --
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

with PolyORB.DNS.Transport_Mechanisms;

package PolyORB.Binding_Data.DNS is

   package PDTM renames PolyORB.DNS.Transport_Mechanisms;

   type DNS_Profile_Type is abstract new Profile_Type with private;
   type DNS_Profile_Factory is abstract new Profile_Factory with private;

   overriding function Image (Prof : DNS_Profile_Type) return String;

   overriding procedure Bind_Profile
     (Profile : access DNS_Profile_Type;
      The_ORB : Components.Component_Access;
      QoS     : PolyORB.QoS.QoS_Parameters;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container);

   overriding procedure Release (P : in out DNS_Profile_Type);

   overriding function Is_Colocated
     (Left  : DNS_Profile_Type;
      Right : Profile_Type'Class) return Boolean;

   overriding function Is_Local_Profile
     (PF : access DNS_Profile_Factory;
      P  : not null access Profile_Type'Class) return Boolean;

   function Get_Primary_Transport_Mechanism
     (P : DNS_Profile_Type) return PDTM.Transport_Mechanism_Access;
   --  Return primary transport mechanism for profile

   function Get_Primary_Transport_Mechanism_Factory
     (P : DNS_Profile_Factory) return PDTM.Transport_Mechanism_Factory_Access;
   --  Return primary transport mechanism factory for profile factory.

private

   type DNS_Profile_Type is abstract new Profile_Type with record
      Mechanism : PDTM.Transport_Mechanism_Access;
   end record;

   type DNS_Profile_Factory is abstract new Profile_Factory with record
      Mechanism : PDTM.Transport_Mechanism_Factory_Access;
   end record;

end PolyORB.Binding_Data.DNS;
