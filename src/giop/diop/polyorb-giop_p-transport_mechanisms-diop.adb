------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TRANSPORT_MECHANISMS.DIOP                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2017, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data.GIOP.DIOP;
with PolyORB.Binding_Objects;
with PolyORB.Filters;
with PolyORB.ORB;
with PolyORB.Protocols.GIOP.DIOP;
with PolyORB.Sockets;
with PolyORB.Transport.Datagram.Sockets;

package body PolyORB.GIOP_P.Transport_Mechanisms.DIOP is

   use PolyORB.Errors;
   use PolyORB.Sockets;
   use PolyORB.Transport.Datagram.Sockets;
   use PolyORB.Utils.Sockets;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of
     (M : DIOP_Transport_Mechanism) return Utils.Sockets.Socket_Name
   is
   begin
      return M.Address.all;
   end Address_Of;

   --------------------
   -- Bind_Mechanism --
   --------------------

   --  Factories

   Pro            : aliased PolyORB.Protocols.GIOP.DIOP.DIOP_Protocol;
   DIOP_Factories : constant Filters.Factory_Array
     := (0 => Pro'Access);

   overriding procedure Bind_Mechanism
     (Mechanism : DIOP_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container)
   is
      pragma Unreferenced (QoS);

      use PolyORB.Binding_Data;

      Sock        : Socket_Type;
      TE          : Transport.Transport_Endpoint_Access;
   begin
      if Profile.all
        not in PolyORB.Binding_Data.GIOP.DIOP.DIOP_Profile_Type
      then
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
         return;
      end if;

      Utils.Sockets.Create_Socket (Socket => Sock,
                                   Family => Family_Inet,
                                   Mode   => Socket_Datagram);

      TE := new Socket_Endpoint;
      Create
        (Socket_Endpoint (TE.all),
         Sock,
         Utils.Sockets.To_Address (Mechanism.Address.all));

      Binding_Objects.Setup_Binding_Object
        (The_ORB,
         TE,
         DIOP_Factories,
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
     (MF  : out DIOP_Transport_Mechanism_Factory;
      TAP : access Transport.Transport_Access_Point'Class)
   is
   begin
      MF.Address :=
        new Socket_Name'(Address_Of (Datagram_Socket_AP (TAP.all)));
   end Create_Factory;

   ------------------------------
   -- Create_Tagged_Components --
   ------------------------------

   overriding function Create_Tagged_Components
     (MF : DIOP_Transport_Mechanism_Factory)
      return Tagged_Components.Tagged_Component_List
   is
      pragma Unreferenced (MF);

   begin
      return Tagged_Components.Null_Tagged_Component_List;
   end Create_Tagged_Components;

   --------------------------------
   -- Create_Transport_Mechanism --
   --------------------------------

   function Create_Transport_Mechanism
     (MF : DIOP_Transport_Mechanism_Factory)
      return Transport_Mechanism_Access
   is
      Result  : constant Transport_Mechanism_Access
        := new DIOP_Transport_Mechanism;
      TResult : DIOP_Transport_Mechanism
        renames DIOP_Transport_Mechanism (Result.all);

   begin
      TResult.Address := new Socket_Name'(MF.Address.all);
      return Result;
   end Create_Transport_Mechanism;

   --------------------------------
   -- Create_Transport_Mechanism --
   --------------------------------

   function Create_Transport_Mechanism
     (Address : Utils.Sockets.Socket_Name) return Transport_Mechanism_Access
   is
      Result  : constant Transport_Mechanism_Access
        := new DIOP_Transport_Mechanism;
      TResult : DIOP_Transport_Mechanism
        renames DIOP_Transport_Mechanism (Result.all);

   begin
      TResult.Address := new Socket_Name'(Address);
      return Result;
   end Create_Transport_Mechanism;

   ------------------------
   -- Is_Local_Mechanism --
   ------------------------

   overriding function Is_Local_Mechanism
     (MF : access DIOP_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class) return Boolean
   is
   begin
      return M.all in DIOP_Transport_Mechanism
        and then DIOP_Transport_Mechanism (M.all).Address = MF.Address;
   end Is_Local_Mechanism;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents
     (M : access DIOP_Transport_Mechanism)
   is
   begin
      Free (M.Address);
   end Release_Contents;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (TMA : DIOP_Transport_Mechanism) return DIOP_Transport_Mechanism
   is
   begin
      return DIOP_Transport_Mechanism'
               (Address => new Socket_Name'(TMA.Address.all));
   end Duplicate;

   ------------------
   -- Is_Colocated --
   ------------------

   overriding function Is_Colocated
     (Left  : DIOP_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean
   is
   begin
      return Right in DIOP_Transport_Mechanism
        and then Left.Address = DIOP_Transport_Mechanism (Right).Address;
   end Is_Colocated;

end PolyORB.GIOP_P.Transport_Mechanisms.DIOP;
