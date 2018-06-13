------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.TRANSPORT_MECHANISMS.SSLIOP                 --
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

with Ada.Strings.Fixed;

with PolyORB.Binding_Data.GIOP.IIOP;
with PolyORB.Binding_Objects;
with PolyORB.Filters.Slicers;
with PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.Sockets;
with PolyORB.SSL;
with PolyORB.Transport.Connected.Sockets.SSL;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Transport_Mechanisms.SSLIOP is

   use PolyORB.Binding_Data.GIOP.IIOP;
   use PolyORB.Components;
   use PolyORB.Errors;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans;
   use PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
   use PolyORB.Parameters;
   use PolyORB.Sockets;
   use PolyORB.SSL;
   use PolyORB.Transport.Connected.Sockets.SSL;
   use PolyORB.Utils.Sockets;

   procedure Initialize;

   procedure Create
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : Binding_Data.Profile_Access;
      Mechs   : in out Transport_Mechanism_List);
   --  Create list of Transport Mechanism from list of Tagged Component

   Binding_Context : SSL_Context_Type;

   --  Factories

   Sli            : aliased PolyORB.Filters.Slicers.Slicer_Factory;
   Pro            : aliased PolyORB.Protocols.GIOP.IIOP.IIOP_Protocol;
   IIOP_Factories : constant PolyORB.Filters.Factory_Array
     := (0 => Sli'Access, 1 => Pro'Access);

   --------------------
   -- Bind_Mechanism --
   --------------------

   overriding procedure Bind_Mechanism
     (Mechanism : SSLIOP_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container)
   is
      pragma Unreferenced (QoS);

      use PolyORB.Binding_Data;

      Sock        : Socket_Type;
      SSL_Sock    : SSL_Socket_Type;
      TE          : constant PolyORB.Transport.Transport_Endpoint_Access :=
                      new SSL_Endpoint;

   begin
      if Profile.all
        not in PolyORB.Binding_Data.GIOP.IIOP.IIOP_Profile_Type
      then
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
         return;
      end if;

      Utils.Sockets.Create_Socket (Sock);
      Connect_Socket (Sock, Binding_Context, SSL_Sock, Mechanism.Address.all);
      Create (SSL_Endpoint (TE.all), SSL_Sock);

      Binding_Objects.Setup_Binding_Object
        (The_ORB,
         TE,
         IIOP_Factories,
         BO_Ref,
         Profile_Access (Profile));

      ORB.Register_Binding_Object
        (ORB.ORB_Access (The_ORB),
         BO_Ref,
         ORB.Client);

   exception
      when Sockets.Socket_Error =>
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_No));

      when SSL.SSL_Error =>
         Throw (Error, No_Permission_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_No));
   end Bind_Mechanism;

   ------------
   -- Create --
   ------------

   procedure Create
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : Binding_Data.Profile_Access;
      Mechs   : in out Transport_Mechanism_List)
   is
      Mechanism : constant Transport_Mechanism_Access :=
                    new SSLIOP_Transport_Mechanism;

   begin
      SSLIOP_Transport_Mechanism (Mechanism.all).Address :=
        new Socket_Name'(Primary_Address_Of
          (IIOP_Transport_Mechanism
             (Get_Primary_Transport_Mechanism
                (IIOP_Profile_Type (Profile.all)).all)));
      SSLIOP_Transport_Mechanism (Mechanism.all).Address.Port :=
        TC_SSL_Sec_Trans (TC.all).Port;

      Append (Mechs, Mechanism);
   end Create;

   --------------------
   -- Create_Factory --
   --------------------

   overriding procedure Create_Factory
     (MF  : out SSLIOP_Transport_Mechanism_Factory;
      TAP : access Transport.Transport_Access_Point'Class)
   is
   begin
      MF.Address :=
        new Socket_Name'
          (SSL_Access_Point (TAP.all).Socket_AP_Publish_Name);

      --  Detect supported and required security assocations by
      --  review of descriptions of available ciphers (conformant
      --  with CORBA 3.0 paragraph 24.5.1.3 TAG_TLS_SEC_TRANS)
      --
      --  The following algorithm are used:
      --
      --  Integrity:
      --    Supported - one of ciphers have not None Mac parameter
      --    Required  - all of ciphers have not None Mac parameter
      --
      --  Confidentiality:
      --    Supported - one of chipers have not None Enc parameter
      --    Required  - all of ciphers have not None Enc parameter
      --
      --  Establish_Trust_In_Target:
      --    Supported - one of ciphers have not None Au parameter
      --    Required  - always false
      --
      --  Establish_Trust_In_Client:
      --    Supported - verify mode is SSL_VERIFY_PEER but not
      --                SSL_VERIFY_FAIL_IF_NO_PEER_CERT
      --    Required  - both SSL_VERIFY_PEER and
      --                SSL_VERIFY_FAIL_IF_NO_PEER_CERT are enabled
      declare

         function Is_None
           (Description : String;
            Parameter   : String)
            return Boolean;
         --  Check is a Parameter have None value or not present
         --  in Description

         -------------
         -- Is_None --
         -------------

         function Is_None
           (Description : String;
            Parameter   : String)
            return Boolean
         is
            None : constant String := "None";
            Pos  : constant Natural
              := Ada.Strings.Fixed.Index (Description, Parameter & '=')
              + Parameter'Length + 1;

         begin
            --  Check if a parameter is present in description

            if Pos <= Parameter'Length then
               return False;
            end if;

            --  Check the length of parameter value less whan None

            if Description'Last < Pos + None'Length then
               return True;
            end if;

            return Description (Pos .. Pos + None'Length - 1) = None;
         end Is_None;

         List : constant SSL_Cipher_Array
           := Ciphers_Of (Get_SSL_Context (SSL_Access_Point (TAP.all)));
         Mode : constant SSL_Verification_Mode
           := Verification_Mode_Of
           (Get_SSL_Context (SSL_Access_Point (TAP.all)));

         Integrity_Supported                 : Boolean := False;
         Integrity_Required                  : Boolean := True;
         Confidentiality_Supported           : Boolean := False;
         Confidentiality_Required            : Boolean := True;
         Establish_Trust_In_Target_Supported : Boolean := False;
         Establish_Trust_In_Client_Supported : Boolean := False;
         Establish_Trust_In_Client_Required  : Boolean := False;

      begin
         for J in List'Range loop
            declare
               Desc : constant String := Description_Of (List (J));

            begin
               --  Compute Integrity option

               if Is_None (Desc, "Mac") then
                  Integrity_Required := False;
               else
                  Integrity_Supported := True;
               end if;

               --  Compute Confidentiality option

               if Is_None (Desc, "Enc") then
                  Confidentiality_Required := False;
               else
                  Confidentiality_Supported := True;
               end if;

               --  Compute Establish_Trust_In_Target option

               if not Is_None (Desc, "Au") then
                  Establish_Trust_In_Target_Supported := True;
               end if;

            end;
         end loop;

         if Mode (Peer) then
            Establish_Trust_In_Client_Supported := True;

            if Mode (Fail_If_No_Peer_Certificate) then
               Establish_Trust_In_Client_Required := True;
            end if;
         end if;

         --  Setting consolidated Target Supports accosiation options

         MF.Target_Supports := 0;

         if Integrity_Supported then
            MF.Target_Supports := MF.Target_Supports + Integrity;
         end if;

         if Confidentiality_Supported then
            MF.Target_Supports := MF.Target_Supports + Confidentiality;
         end if;

         if Establish_Trust_In_Target_Supported then
            MF.Target_Supports :=
              MF.Target_Supports + Establish_Trust_In_Target;
         end if;

         if Establish_Trust_In_Client_Supported then
            MF.Target_Supports :=
              MF.Target_Supports + Establish_Trust_In_Client;
         end if;

         --  Setting consolidated Target Requires accosiation options

         MF.Target_Requires := 0;

         if Integrity_Required then
            MF.Target_Requires := MF.Target_Requires + Integrity;
         end if;

         if Confidentiality_Required then
            MF.Target_Requires := MF.Target_Requires + Confidentiality;
         end if;

         if Establish_Trust_In_Client_Required then
            MF.Target_Requires :=
              MF.Target_Requires + Establish_Trust_In_Client;
         end if;
      end;
   end Create_Factory;

   ------------------------------
   -- Create_Tagged_Components --
   ------------------------------

   overriding function Create_Tagged_Components
     (MF : SSLIOP_Transport_Mechanism_Factory)
      return Tagged_Components.Tagged_Component_List
   is
      Result : Tagged_Component_List;

      TC : constant Tagged_Component_Access := new TC_SSL_Sec_Trans;

   begin
      TC_SSL_Sec_Trans (TC.all).Port            := MF.Address.Port;
      TC_SSL_Sec_Trans (TC.all).Target_Supports := MF.Target_Supports;
      TC_SSL_Sec_Trans (TC.all).Target_Requires := MF.Target_Requires;

      Add (Result, TC);

      return Result;
   end Create_Tagged_Components;

   ---------------
   -- Duplicate --
   ---------------

   overriding function Duplicate
     (TMA : SSLIOP_Transport_Mechanism)
     return SSLIOP_Transport_Mechanism is
   begin
      return SSLIOP_Transport_Mechanism'
        (Address         => new Socket_Name'(TMA.Address.all),
         Target_Supports => TMA.Target_Supports,
         Target_Requires => TMA.Target_Requires);
   end Duplicate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Get_Conf ("modules", "binding_data.iiop.ssliop", False) then
         Create_Context
           (Binding_Context,
            Any,
            Get_Conf
            ("ssliop",
             "polyorb.protocols.ssliop.privatekeyfile",
             ""),
            Get_Conf
            ("ssliop",
             "polyorb.protocols.ssliop.certificatefile",
             ""),
            Get_Conf
            ("ssliop",
             "polyorb.protocols.ssliop.cafile",
             ""),
            Get_Conf
            ("ssliop",
             "polyorb.protocols.ssliop.capath",
             ""),
            (Get_Conf
             ("ssliop", "polyorb.protocols.ssliop.verify", False),
             Get_Conf
             ("ssliop",
              "polyorb.protocols.ssliop.verify_fail_if_no_peer_cert",
              False),
             Get_Conf
             ("ssliop",
              "polyorb.protocols.ssliop.verify_client_once",
              False)));

         Register (Tag_SSL_Sec_Trans, Create'Access);
      end if;
   end Initialize;

   ------------------
   -- Is_Colocated --
   ------------------

   overriding function Is_Colocated
     (Left  : SSLIOP_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean
   is
   begin
      return Right in SSLIOP_Transport_Mechanism
        and then Left.Address = SSLIOP_Transport_Mechanism (Right).Address;
   end Is_Colocated;

   ------------------------
   -- Is_Local_Mechanism --
   ------------------------

   overriding function Is_Local_Mechanism
     (MF : access SSLIOP_Transport_Mechanism_Factory;
      M  : access Transport_Mechanism'Class)
      return Boolean
   is
   begin
      return M.all in SSLIOP_Transport_Mechanism
        and then SSLIOP_Transport_Mechanism (M.all).Address = MF.Address;
   end Is_Local_Mechanism;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents
     (M : access SSLIOP_Transport_Mechanism)
   is
   begin
      Free (M.Address);
   end Release_Contents;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"giop_p.transport_mechanisms.ssliop",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => +"ssl",
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Transport_Mechanisms.SSLIOP;
