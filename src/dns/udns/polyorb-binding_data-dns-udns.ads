------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . B I N D I N G _ D A T A . D N S . U D N S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  Package providing the data binding facilities for the uDNS transport
--  mechanism, specific to the DNS protocol personality

package PolyORB.Binding_Data.DNS.UDNS is

   DNS_Error : exception;

   type UDNS_Profile_Type is new DNS_Profile_Type with private;
   type UDNS_Profile_Factory is new DNS_Profile_Factory with private;

   function Create_Profile
     (PF  : access UDNS_Profile_Factory;
      Oid : Objects.Object_Id) return Profile_Access;

   function Duplicate_Profile
     (P : UDNS_Profile_Type) return Profile_Access;

   function Get_Profile_Tag (Profile : UDNS_Profile_Type) return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : UDNS_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   procedure Create_Factory
     (PF  : out UDNS_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access);

private

   type UDNS_Profile_Type is new DNS_Profile_Type with null record;
   type UDNS_Profile_Factory is new DNS_Profile_Factory with null record;

end PolyORB.Binding_Data.DNS.UDNS;
