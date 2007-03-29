------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TRANSPORT_MECHANISMS.DIOP                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data.GIOP.DIOP;
with PolyORB.Binding_Objects;
with PolyORB.Filters;
with PolyORB.ORB;
with PolyORB.Protocols.GIOP.DIOP;
with PolyORB.Transport.Datagram.Sockets_In;
with PolyORB.Transport.Datagram.Sockets_Out;

package body PolyORB.GIOP_P.Transport_Mechanisms.DIOP is

   use PolyORB.Components;
   use PolyORB.Errors;
   use PolyORB.Sockets;
   use PolyORB.Transport.Datagram.Sockets_In;
   use PolyORB.Transport.Datagram.Sockets_Out;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of (M : DIOP_Transport_Mechanism)
     return Sockets.Sock_Addr_Type
   is
   begin
      return M.Address;
   end Address_Of;

   --------------------
   -- Bind_Mechanism --
   --------------------

   --  Factories

   Pro            : aliased PolyORB.Protocols.GIOP.DIOP.DIOP_Protocol;
   DIOP_Factories : constant Filters.Factory_Array
     := (0 => Pro'Access);

   procedure Bind_Mechanism
     (Mechanism : DIOP_Transport_Mechanism;
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
      Remote_Addr : constant Sock_Addr_Type := Mechanism.Address;
      TE          : Transport.Transport_Endpoint_Access;

   begin
      if Profile.all
        not in PolyORB.Binding_Data.GIOP.DIOP.DIOP_Profile_Type then
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
         return;
      end if;

      Create_Socket (Socket => Sock,
                     Family => Family_Inet,
                     Mode   => Socket_Datagram);

      TE := new Socket_Out_Endpoint;

      Create (Socket_Out_Endpoint (TE.all), Sock, Remote_Addr);

      Binding_Objects.Setup_Binding_Object
        (TE,
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

   procedure Create_Factory
     (MF  : out DIOP_Transport_Mechanism_Factory;
      TAP :     Transport.Transport_Access_Point_Access)
   is
   begin
      MF.Address := Address_Of (Socket_In_Access_Point (TAP.all));
   end Create_Factory;

   ------------------------------
   -- Create_Tagged_Components --
   ------------------------------

   function Create_Tagged_Components
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
      TResult.Address := MF.Address;
      return Result;
   end Create_Transport_Mechanism;

   function Create_Transport_Mechanism
     (Address : Sockets.Sock_Addr_Type)
      return Transport_Mechanism_Access
   is
      Result  : constant Transport_Mechanism_Access
        := new DIOP_Transport_Mechanism;
      TResult : DIOP_Transport_Mechanism
        renames DIOP_Transport_Mechanism (Result.all);

   begin
      TResult.Address := Address;
      return Result;
   end Create_Transport_Mechanism;

   ------------------------
   -- Is_Local_Mechanism --
   ------------------------

   function Is_Local_Mechanism
     (MF : access DIOP_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class)
      return Boolean
   is
      use type PolyORB.Sockets.Sock_Addr_Type;

   begin
      return M.all in DIOP_Transport_Mechanism
        and then DIOP_Transport_Mechanism (M.all).Address = MF.Address;
   end Is_Local_Mechanism;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (M : access DIOP_Transport_Mechanism) is
      pragma Unreferenced (M);

   begin
      null;
   end Release_Contents;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (TMA : DIOP_Transport_Mechanism)
     return DIOP_Transport_Mechanism
   is
   begin
      return TMA;
   end Duplicate;

   ------------------
   -- Is_Colocated --
   ------------------

   function Is_Colocated
     (Left  : DIOP_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean
   is
   begin
      return Right in DIOP_Transport_Mechanism
        and then Left.Address = DIOP_Transport_Mechanism (Right).Address;
   end Is_Colocated;

end PolyORB.GIOP_P.Transport_Mechanisms.DIOP;
