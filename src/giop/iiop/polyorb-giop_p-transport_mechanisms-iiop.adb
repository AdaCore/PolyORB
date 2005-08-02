------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TRANSPORT_MECHANISMS.IIOP                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

with PolyORB.Binding_Objects;
with PolyORB.Filters.Slicers;
with PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Transport_Mechanisms.IIOP is

   use PolyORB.Components;
   use PolyORB.Errors;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
   use PolyORB.Sockets;
   use PolyORB.Transport.Connected.Sockets;

   procedure Initialize;

   function Create
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : Binding_Data.Profile_Access)
     return Transport_Mechanism_List;
   --  Create list of Transport Mechanism from list of Tagged Component

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of (M : IIOP_Transport_Mechanism)
     return Sockets.Sock_Addr_Type
   is
   begin
      return M.Address;
   end Address_Of;

   --------------------
   -- Bind_Mechanism --
   --------------------

   --  Factories

   Sli            : aliased PolyORB.Filters.Slicers.Slicer_Factory;
   Pro            : aliased PolyORB.Protocols.GIOP.IIOP.IIOP_Protocol;
   IIOP_Factories : constant PolyORB.Filters.Factory_Array
     := (0 => Sli'Access, 1 => Pro'Access);

   procedure Bind_Mechanism
     (Mechanism :     IIOP_Transport_Mechanism;
      The_ORB   :     Components.Component_Access;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container)
   is
      Sock        : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Mechanism.Address;
      TE          : constant PolyORB.Transport.Transport_Endpoint_Access
        := new Socket_Endpoint;

   begin
      Create_Socket (Sock);
      Connect_Socket (Sock, Remote_Addr);
      Create (Socket_Endpoint (TE.all), Sock);
      Set_Allocation_Class (TE.all, Dynamic);

      Binding_Objects.Setup_Binding_Object
        (ORB.ORB_Access (The_ORB),
         TE,
         IIOP_Factories,
         ORB.Client,
         BO_Ref);

   exception
      when Sockets.Socket_Error =>
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
   end Bind_Mechanism;

   ------------
   -- Create --
   ------------

   function Create
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : Binding_Data.Profile_Access)
     return Transport_Mechanism_List
   is
      pragma Unreferenced (Profile);

      Result    : Transport_Mechanism_List;
      Mechanism : constant Transport_Mechanism_Access
        := new IIOP_Transport_Mechanism;

   begin
      IIOP_Transport_Mechanism (Mechanism.all).Address :=
        TC_Alternate_IIOP_Address (TC.all).Address;

      Append (Result, Mechanism);

      return Result;
   end Create;

   --------------------
   -- Create_Factory --
   --------------------

   procedure Create_Factory
     (MF  : out IIOP_Transport_Mechanism_Factory;
      TAP :     Transport.Transport_Access_Point_Access)
   is
   begin
      MF.Address := Address_Of (Socket_Access_Point (TAP.all));
   end Create_Factory;

   ------------------------------
   -- Create_Tagged_Components --
   ------------------------------

   function Create_Tagged_Components
     (MF : IIOP_Transport_Mechanism_Factory)
      return Tagged_Components.Tagged_Component_List
   is
      Result : Tagged_Component_List;

      TC : constant Tagged_Component_Access := new TC_Alternate_IIOP_Address;

   begin
      TC_Alternate_IIOP_Address (TC.all).Address := MF.Address;
      Add (Result, TC);
      return Result;
   end Create_Tagged_Components;

   --------------------------------
   -- Create_Transport_Mechanism --
   --------------------------------

   function Create_Transport_Mechanism
     (MF : IIOP_Transport_Mechanism_Factory)
      return Transport_Mechanism_Access
   is
      Result  : constant Transport_Mechanism_Access
        := new IIOP_Transport_Mechanism;
      TResult : IIOP_Transport_Mechanism
        renames IIOP_Transport_Mechanism (Result.all);

   begin
      TResult.Address := MF.Address;
      return Result;
   end Create_Transport_Mechanism;

   function Create_Transport_Mechanism
     (Address : Sockets.Sock_Addr_Type)
      return Transport_Mechanism_Access
   is
      Result  : constant Transport_Mechanism_Access
        := new IIOP_Transport_Mechanism;
      TResult : IIOP_Transport_Mechanism
        renames IIOP_Transport_Mechanism (Result.all);

   begin
      TResult.Address := Address;
      return Result;
   end Create_Transport_Mechanism;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register (Tag_Alternate_IIOP_Address, Create'Access);
   end Initialize;

   ------------------------
   -- Is_Local_Mechanism --
   ------------------------

   function Is_Local_Mechanism
     (MF : access IIOP_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class)
      return Boolean
   is
      use type PolyORB.Sockets.Sock_Addr_Type;

   begin
      return M.all in IIOP_Transport_Mechanism
        and then IIOP_Transport_Mechanism (M.all).Address = MF.Address;
   end Is_Local_Mechanism;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (M : access IIOP_Transport_Mechanism) is
      pragma Unreferenced (M);

   begin
      null;
   end Release_Contents;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"giop_p.transport_mechanisms.iiop",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => PolyORB.Initialization.String_Lists.Empty,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access));
   end;
end PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
