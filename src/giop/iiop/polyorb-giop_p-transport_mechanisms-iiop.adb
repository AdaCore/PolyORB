------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TRANSPORT_MECHANISMS.IIOP                  --
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

with PolyORB.Binding_Data.GIOP.IIOP;
with PolyORB.Binding_Objects;
with PolyORB.Filters.Slicers;
with PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Transport_Mechanisms.IIOP is

   use PolyORB.Components;
   use PolyORB.Errors;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
   use PolyORB.Sockets;
   use PolyORB.Transport.Connected.Sockets;
   use Sock_Addr_Lists;

   procedure Initialize;

   function Create
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : Binding_Data.Profile_Access)
     return Transport_Mechanism_List;
   --  Create list of Transport Mechanism from list of Tagged Component

   --------------------
   -- Bind_Mechanism --
   --------------------

   --  Factories

   Sli            : aliased PolyORB.Filters.Slicers.Slicer_Factory;
   Pro            : aliased PolyORB.Protocols.GIOP.IIOP.IIOP_Protocol;
   IIOP_Factories : constant PolyORB.Filters.Factory_Array :=
                      (0 => Sli'Access, 1 => Pro'Access);

   procedure Bind_Mechanism
     (Mechanism : IIOP_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container)
   is
      pragma Unreferenced (QoS);

      use PolyORB.Binding_Data;
      use PolyORB.Binding_Objects;

      Iter : Iterator := First (Mechanism.Addresses);

   begin
      if Profile.all
        not in PolyORB.Binding_Data.GIOP.IIOP.IIOP_Profile_Type
      then
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
         return;
      end if;

      while not Last (Iter) loop
         declare
            Sock        : Socket_Type;
            Remote_Addr : Sock_Addr_Type := Value (Iter).all;
            TE          : constant PolyORB.Transport.Transport_Endpoint_Access
                            := new Socket_Endpoint;

         begin
            Create_Socket (Sock);
            Utils.Sockets.Connect_Socket (Sock, Remote_Addr);
            Create (Socket_Endpoint (TE.all), Sock);

            Binding_Objects.Setup_Binding_Object
              (TE,
               IIOP_Factories,
               BO_Ref,
               Profile_Access (Profile));

            ORB.Register_Binding_Object
              (ORB.ORB_Access (The_ORB),
               BO_Ref,
               ORB.Client);

            return;

         exception
            when Sockets.Socket_Error =>
               Throw (Error, Comm_Failure_E,
                      System_Exception_Members'
                      (Minor => 0, Completed => Completed_No));
         end;

         Next (Iter);

         if not Last (Iter) and then Found (Error) then
            Catch (Error);
         end if;
      end loop;
   end Bind_Mechanism;

   ------------
   -- Create --
   ------------

   function Create
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : Binding_Data.Profile_Access)
     return Transport_Mechanism_List
   is
      use PolyORB.Binding_Data.GIOP;

      Mechanism : constant Transport_Mechanism_Access
        := Get_Primary_Transport_Mechanism (GIOP_Profile_Type (Profile.all));

   begin
      Append
        (IIOP_Transport_Mechanism (Mechanism.all).Addresses,
         TC_Alternate_IIOP_Address (TC.all).Address);

      return Transport_Mechanism_List (Transport_Mechanism_Lists.Empty);
   end Create;

   --------------------
   -- Create_Factory --
   --------------------

   procedure Create_Factory
     (MF  : out IIOP_Transport_Mechanism_Factory;
      TAP :     Transport.Transport_Access_Point_Access)
   is
   begin
      Append (MF.Addresses, Address_Of (Socket_Access_Point (TAP.all)));
   end Create_Factory;

   ------------------------------
   -- Create_Tagged_Components --
   ------------------------------

   function Create_Tagged_Components
     (MF : IIOP_Transport_Mechanism_Factory)
      return Tagged_Components.Tagged_Component_List
   is
      Result : Tagged_Component_List;

      Iter : Iterator := First (MF.Addresses);

   begin
      --  If Transport Mechanism is disabled (e.g. unprotected invocation
      --  has been disabled), then don't create any tagged components for
      --  alternative addresses.

      if MF.Disabled then
         return Result;
      end if;

      Next (Iter);
      --  Skipping first address in the list because it is a primary address,
      --  declared in profile itself.

      while not Last (Iter) loop
         declare
            TC : constant Tagged_Component_Access
              := new TC_Alternate_IIOP_Address;

         begin
            TC_Alternate_IIOP_Address (TC.all).Address := Value (Iter).all;
            Add (Result, TC);
         end;

         Next (Iter);
      end loop;

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
      Iter    : Iterator := First (MF.Addresses);

   begin
      --  If Transport Mechanism is disabled (e.g. unprotected invocation
      --  has been disabled), add only primary address with zero port number
      --  and ignore all alternate addresses. Otherwise, add all addresses.

      if MF.Disabled then
         declare
            Aux : Sock_Addr_Type := Value (Iter).all;

         begin
            Aux.Port := 0;
            Append (TResult.Addresses, Aux);
         end;

      else
         while not Last (Iter) loop
            Append (TResult.Addresses, Value (Iter).all);
            Next (Iter);
         end loop;
      end if;

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
      Append (TResult.Addresses, Address);
      return Result;
   end Create_Transport_Mechanism;

   ---------------------------------
   -- Disable_Transport_Mechanism --
   ---------------------------------

   procedure Disable_Transport_Mechanism
     (MF : in out IIOP_Transport_Mechanism_Factory)
   is
   begin
      MF.Disabled := True;
   end Disable_Transport_Mechanism;

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

      Iter_1 : Iterator;

   begin
      if MF.Disabled
        or else M.all not in IIOP_Transport_Mechanism
      then
         return False;
      end if;

      Iter_1 := First (IIOP_Transport_Mechanism (M.all).Addresses);

      if M.all in IIOP_Transport_Mechanism then
         while not Last (Iter_1) loop
            declare
               Iter_2 : Iterator := First (MF.Addresses);

            begin
               while not Last (Iter_2) loop
                  if Value (Iter_1).all = Value (Iter_2).all then
                     return True;
                  end if;

                  Next (Iter_2);
               end loop;
            end;

            Next (Iter_1);
         end loop;
      end if;

      return False;
   end Is_Local_Mechanism;

   ------------------------
   -- Primary_Address_Of --
   ------------------------

   function Primary_Address_Of (M : IIOP_Transport_Mechanism)
     return Sockets.Sock_Addr_Type
   is
   begin
      return Element (M.Addresses, 0).all;
   end Primary_Address_Of;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (M : access IIOP_Transport_Mechanism) is
   begin
      Deallocate (M.Addresses);
   end Release_Contents;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (TMA : IIOP_Transport_Mechanism)
     return IIOP_Transport_Mechanism
   is
   begin
      return IIOP_Transport_Mechanism'
        (Addresses => Duplicate (TMA.Addresses));
   end Duplicate;

   ------------------
   -- Is_Colocated --
   ------------------

   function Is_Colocated
     (Left  : IIOP_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean
   is
   begin
      if Right not in IIOP_Transport_Mechanism then
         return False;
      end if;

      declare
         L_Iter : Iterator := First (Left.Addresses);
         R_Iter : Iterator;
      begin

         --  Check if Left.Addresses and Right.Addresses have an address in
         --  common.

         Left_Addresses :
         while not Last (L_Iter) loop

            R_Iter := First (IIOP_Transport_Mechanism (Right).Addresses);

            Right_Addresses :
            while not Last (R_Iter) loop
               if Value (L_Iter).all = Value (R_Iter).all then
                  return True;
               end if;
               Next (R_Iter);
            end loop Right_Addresses;

            Next (L_Iter);
         end loop Left_Addresses;
      end;

      return False;
   end Is_Colocated;

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
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
