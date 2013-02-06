------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.DNS.TRANSPORT_MECHANISMS.MDNS                   --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Binding_Data.DNS.MDNS;
with PolyORB.Binding_Objects;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.DNS;
with PolyORB.Sockets;
with PolyORB.Transport.Datagram.Sockets;
with PolyORB.Filters;

package body PolyORB.DNS.Transport_Mechanisms.MDNS is

   use PolyORB.Components;
   use PolyORB.Errors;
   use PolyORB.Parameters;
   use PolyORB.Sockets;
   use PolyORB.Transport.Datagram.Sockets;
   use PolyORB.Utils.Sockets;

   --  Factories

   Pro : aliased PolyORB.Protocols.DNS.DNS_Protocol;
   MDNS_Factories : constant PolyORB.Filters.Factory_Array :=
     (0 => Pro'Access);

   --------------------
   -- Bind_Mechanism --
   --------------------

   overriding procedure Bind_Mechanism
     (Mechanism : MDNS_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container)
   is
      pragma Unreferenced (QoS);

      use PolyORB.Binding_Data;
      use PolyORB.Binding_Objects;

      Sock        : Socket_Type;
      TE          : Transport.Transport_Endpoint_Access;
      TTL         : constant Natural :=
        Natural (Get_Conf ("dns", "polyorb.dns.ttl",
                                         Default_TTL));

   begin
      if Profile.all
        not in PolyORB.Binding_Data.DNS.MDNS.MDNS_Profile_Type then
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
         return;
      end if;

      Create_Socket (Socket => Sock,
                     Family => Family_Inet,
                     Mode   => Socket_Datagram);

      Set_Socket_Option
        (Sock,
         Socket_Level, (Reuse_Address, True));

      Set_Socket_Option
        (Sock,
         IP_Protocol_For_IP_Level, (Multicast_TTL, TTL));

      TE := new Socket_Endpoint;
      Create
        (Socket_Endpoint (TE.all),
         Sock,
         To_Address (Mechanism.Address.all));

      Binding_Objects.Setup_Binding_Object
        (The_ORB,
         TE,
         MDNS_Factories,
         BO_Ref,
         Profile_Access (Profile));

      ORB.Register_Binding_Object
        (ORB.ORB_Access (The_ORB),
         BO_Ref,
         ORB.Client);

   exception
      when Sockets.Socket_Error =>
         Throw (Error, Comm_Failure_E, System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
   end Bind_Mechanism;

   --------------------
   -- Create_Factory --
   --------------------

   overriding procedure Create_Factory
     (MF  : out MDNS_Transport_Mechanism_Factory;
      TAP : access Transport.Transport_Access_Point'Class)
   is
   begin
      MF.Address :=
        new Socket_Name'(Address_Of (Socket_Access_Point (TAP.all)));
   end Create_Factory;

   --------------------------------
   -- Create_Transport_Mechanism --
   --------------------------------

   overriding function Create_Transport_Mechanism
     (MF : MDNS_Transport_Mechanism_Factory)
      return Transport_Mechanism_Access
   is
      Result  : constant Transport_Mechanism_Access
        := new MDNS_Transport_Mechanism;
      TResult : MDNS_Transport_Mechanism
        renames MDNS_Transport_Mechanism (Result.all);

   begin
      TResult.Address := new Socket_Name'(MF.Address.all);
      return Result;
   end Create_Transport_Mechanism;

   function Create_Transport_Mechanism
     (Address : Utils.Sockets.Socket_Name)
      return Transport_Mechanism_Access
   is
      Result  : constant Transport_Mechanism_Access
        := new MDNS_Transport_Mechanism;
      TResult : MDNS_Transport_Mechanism
        renames MDNS_Transport_Mechanism (Result.all);

   begin
      TResult.Address := new Socket_Name'(Address);
      return Result;
   end Create_Transport_Mechanism;

   ------------------------
   -- Is_Local_Mechanism --
   ------------------------

   overriding function Is_Local_Mechanism
     (MF : access MDNS_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class) return Boolean
   is
   begin
      return M.all in MDNS_Transport_Mechanism
               and then
             MDNS_Transport_Mechanism (M.all).Address.all = MF.Address.all;
   end Is_Local_Mechanism;

   ------------------
   -- Is_Colocated --
   ------------------

   overriding function Is_Colocated
     (Left  : MDNS_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean
   is
   begin
      return Right in MDNS_Transport_Mechanism
        and then Left.Address = MDNS_Transport_Mechanism (Right).Address;
   end Is_Colocated;

end PolyORB.DNS.Transport_Mechanisms.MDNS;
