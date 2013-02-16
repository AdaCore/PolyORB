------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . D S A _ P . N A M E _ S E R V I C E . M D N S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010-2013, Free Software Foundation, Inc.          --
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

--  This package implements the multicast DNS unit discovery for DSA

with PolyORB.DSA_P.Name_Service;
with PolyORB.POA_Policies;
with PolyORB.References;

package PolyORB.DSA_P.Name_Service.mDNS is

   type MDNS_Name_Server is new
     PolyORB.DSA_P.Name_Service.Name_Server with null record;
   --  Concrete mDNS implementation of the abstract Name_Server type

   overriding
   procedure Nameserver_Register
     (Name_Ctx : access MDNS_Name_Server;
      Name : String;
      Kind : String;
      Obj  : PolyORB.References.Ref);
   --  Concrete mDNS implementation of the abstract Nameserver_Register
   --  procedure. In the context of mDNS this procedure is used on server side
   --  to populate the mDNS servant's list of local RCI/SP package infos.

   overriding
   function Nameserver_Lookup
     (Context : access MDNS_Name_Server;
      Name    : String;
      Kind    : String;
      Initial : Boolean := True) return PolyORB.References.Ref;
   --  Concrete mDNS implementation of the abstract Nameserver_Lookup function
   --  In the context of mDNS, it is used on the client side to invoke a
   --  request on the remote mDNS servant.

   procedure Initialize_MDNS_Policies
     (My_Default_Policies : out PolyORB.POA_Policies.PolicyList);
   --  Initialize POA Policies for the MDNS Servant.

   procedure Initiate_MDNS_Context
     (MDNS_Reference : String;
      Context        : out Name_Server_Access);
   --  Initiates the mDNS Name Context by initizalizing the servant object,
   --  and setting is as a default servant for a newly created child_POA.
   --  A stringified reference is assigned to Context.Stringified_Reference
   --  which is used to create the Context.Base_Ref in Nameserver_Lookup

   function Get_MDNS_Servant return PolyORB.References.Ref;
   --  Offered to the user, used by the partition main file in order to
   --  retrieve the default mDNS servant and assign it to the DNS protocol
end PolyORB.DSA_P.Name_Service.mDNS;
