------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.GIOP_P.TRANSPORT_MECHANISMS.TLS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
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

--  with Ada.Strings.Fixed;
--
with PolyORB.Binding_Data.GIOP.IIOP;
with PolyORB.Binding_Objects;
with PolyORB.Filters.Slicers;
with PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List;
--  with PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans;
--  with PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
with PolyORB.Initialization;
with PolyORB.ORB;
--  with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.QoS.Transport_Contexts;
with PolyORB.Security.Credentials.Compound;
with PolyORB.Security.Credentials.TLS;
with PolyORB.Sockets;
with PolyORB.TLS;
with PolyORB.Transport.Connected.Sockets.TLS;
with PolyORB.Utils.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.GIOP_P.Transport_Mechanisms.TLS is

   use PolyORB.Components;
   use PolyORB.Errors;
   use PolyORB.GIOP_P.Tagged_Components;
   use PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans;
   use PolyORB.GIOP_P.Tagged_Components.TLS_Sec_Trans.Socket_Name_Lists;
   use PolyORB.QoS;
   use PolyORB.QoS.Transport_Contexts;
   use PolyORB.Security.Credentials;
   use PolyORB.Security.Credentials.Compound;
   use PolyORB.Security.Credentials.TLS;
   use PolyORB.Sockets;
   use PolyORB.TLS;
   use PolyORB.Transport.Connected.Sockets.TLS;

   procedure Initialize;

   function Create
     (TC      : Tagged_Components.Tagged_Component_Access;
      Profile : Binding_Data.Profile_Access)
     return Transport_Mechanism_List;
   --  Create list of Transport Mechanism from list of Tagged Component

   function Create_QoS
     (End_Point : PolyORB.Transport.Transport_Endpoint_Access)
     return PolyORB.QoS.QoS_Parameter_Access;
   --  Create QoS parameter from transport endpoint

   function Extract_TLS_Credentials
     (Credentials : Credentials_Ref)
      return TLS_Credentials_Access;

--   Binding_Context : SSL_Context_Type;

   --------------------
   -- Bind_Mechanism --
   --------------------

   --  Factories

   Sli            : aliased PolyORB.Filters.Slicers.Slicer_Factory;
   Pro            : aliased PolyORB.Protocols.GIOP.IIOP.IIOP_Protocol;
   IIOP_Factories : constant PolyORB.Filters.Factory_Array
     := (0 => Sli'Access, 1 => Pro'Access);

   procedure Bind_Mechanism
     (Mechanism : TLS_Transport_Mechanism;
      Profile   : access PolyORB.Binding_Data.Profile_Type'Class;
      The_ORB   : Components.Component_Access;
      QoS       : PolyORB.QoS.QoS_Parameters;
      BO_Ref    : out Smart_Pointers.Ref;
      Error     : out Errors.Error_Container)
   is
      Sock        : Socket_Type;
      TLS_Sock    : TLS_Socket_Type;
      Remote_Addr : Utils.Sockets.Socket_Name_Ptr;
      TE          : PolyORB.Transport.Transport_Endpoint_Access;
      Iter        : Socket_Name_Lists.Iterator := First (Mechanism.Addresses);
      Creds       : constant TLS_Credentials_Access :=
                      Extract_TLS_Credentials
                        (QoS_Transport_Context_Parameter_Access
                           (QoS (Transport_Security)).Invocation_Credentials);

   begin
      if Profile.all
        not in PolyORB.Binding_Data.GIOP.IIOP.IIOP_Profile_Type then
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
         return;
      end if;

      while not Last (Iter) loop
         begin
            Remote_Addr := Value (Iter).all;

            Create_Socket (Sock);
            Utils.Sockets.Connect_Socket (Sock, Remote_Addr.all);

            TLS_Sock := Create_Invocation_Socket (Creds);

            Set_Socket (TLS_Sock, Sock);
            Connect_Socket (TLS_Sock);

            TE := new TLS_Endpoint;
            Create (TLS_Endpoint (TE.all), TLS_Sock);

            Binding_Objects.Setup_Binding_Object
              (TE,
               IIOP_Factories,
               BO_Ref,
               Binding_Data.Profile_Access (Profile));

            ORB.Register_Binding_Object
              (ORB.ORB_Access (The_ORB),
               BO_Ref,
               ORB.Client);

            --  XXX Accepting credentials should be computed!!!

            exit;

         exception
            when Sockets.Socket_Error =>
               Throw (Error, Comm_Failure_E,
                      System_Exception_Members'
                      (Minor => 0, Completed => Completed_No));
         end;

         Next (Iter);
      end loop;

   exception
      when TLS_Error =>
         Throw (Error, No_Permission_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_No));
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
        := new TLS_Transport_Mechanism;

   begin
      --  XXX Setup Target_Supports and Target_Requires
      TLS_Transport_Mechanism (Mechanism.all).Addresses :=
        Duplicate (TC_TLS_Sec_Trans (TC.all).Addresses);

      Append (Result, Mechanism);

      return Result;
   end Create;

--   --------------------
--   -- Create_Factory --
--   --------------------
--
--   procedure Create_Factory
--     (MF  : out TLS_Transport_Mechanism_Factory;
--      TAP :     Transport.Transport_Access_Point_Access)
--   is
--      pragma Unreferenced (MF);
--      pragma Unreferenced (TAP);
--
--   begin
--      null;
--
--      MF.Address := Address_Of (SSL_Access_Point (TAP.all));
--
--      --  Detect supported and required security assocations by
--      --  review of descriptions of available ciphers (conformant
--      --  with CORBA 3.0 paragraph 24.5.1.3 TAG_TLS_SEC_TRANS)
--      --
--      --  The following algorithm are used:
--      --
--      --  Integrity:
--      --    Supported - one of ciphers have not None Mac parameter
--      --    Required  - all of ciphers have not None Mac parameter
--      --
--      --  Confidentiality:
--      --    Supported - one of chipers have not None Enc parameter
--      --    Required  - all of ciphers have not None Enc parameter
--      --
--      --  Establish_Trust_In_Target:
--      --    Supported - one of ciphers have not None Au parameter
--      --    Required  - always false
--      --
--      --  Establish_Trust_In_Client:
--      --    Supported - verify mode is SSL_VERIFY_PEER but not
--      --                SSL_VERIFY_FAIL_IF_NO_PEER_CERT
--      --    Required  - both SSL_VERIFY_PEER and
--      --                SSL_VERIFY_FAIL_IF_NO_PEER_CERT are enabled
--      declare
--
--         function Is_None
--           (Description : in String;
--            Parameter   : in String)
--            return Boolean;
--         --  Check is a Parameter have None value or not present
--         --  in Description
--
--         -------------
--         -- Is_None --
--         -------------
--
--         function Is_None
--           (Description : in String;
--            Parameter   : in String)
--            return Boolean
--         is
--            None : constant String := "None";
--            Pos  : constant Natural
--              := Ada.Strings.Fixed.Index (Description, Parameter & '=')
--              + Parameter'Length + 1;
--
--         begin
--            --  Check if a parameter is present in description
--
--            if Pos <= Parameter'Length then
--               return False;
--            end if;
--
--            --  Check the length of parameter value less whan None
--
--            if Description'Last < Pos + None'Length then
--               return True;
--            end if;
--
--            return Description (Pos .. Pos + None'Length - 1) = None;
--         end Is_None;
--
--         List : constant SSL_Cipher_Array
--           := Ciphers_Of (Get_SSL_Context (SSL_Access_Point (TAP.all)));
--         Mode : constant SSL_Verification_Mode
--           := Verification_Mode_Of
--           (Get_SSL_Context (SSL_Access_Point (TAP.all)));
--
--         Integrity_Supported                 : Boolean := False;
--         Integrity_Required                  : Boolean := True;
--         Confidentiality_Supported           : Boolean := False;
--         Confidentiality_Required            : Boolean := True;
--         Establish_Trust_In_Target_Supported : Boolean := False;
--         Establish_Trust_In_Client_Supported : Boolean := False;
--         Establish_Trust_In_Client_Required  : Boolean := False;
--
--      begin
--         for J in List'Range loop
--            declare
--               Desc : constant String := Description_Of (List (J));
--
--            begin
--               --  Compute Integrity option
--
--               if Is_None (Desc, "Mac") then
--                  Integrity_Required := False;
--               else
--                  Integrity_Supported := True;
--               end if;
--
--               --  Compute Confidentiality option
--
--               if Is_None (Desc, "Enc") then
--                  Confidentiality_Required := False;
--               else
--                  Confidentiality_Supported := True;
--               end if;
--
--               --  Compute Establish_Trust_In_Target option
--
--               if not Is_None (Desc, "Au") then
--                  Establish_Trust_In_Target_Supported := True;
--               end if;
--
--            end;
--         end loop;
--
--         if Mode (Peer) then
--            Establish_Trust_In_Client_Supported := True;
--
--            if Mode (Fail_If_No_Peer_Certificate) then
--               Establish_Trust_In_Client_Required := True;
--            end if;
--         end if;
--
--         --  Setting consolidated Target Supports accosiation options
--
--         MF.Target_Supports := 0;
--
--         if Integrity_Supported then
--            MF.Target_Supports := MF.Target_Supports + Integrity;
--         end if;
--
--         if Confidentiality_Supported then
--            MF.Target_Supports := MF.Target_Supports + Confidentiality;
--         end if;
--
--         if Establish_Trust_In_Target_Supported then
--            MF.Target_Supports :=
--              MF.Target_Supports + Establish_Trust_In_Target;
--         end if;
--
--         if Establish_Trust_In_Client_Supported then
--            MF.Target_Supports :=
--              MF.Target_Supports + Establish_Trust_In_Client;
--         end if;
--
--         --  Setting consolidated Target Requires accosiation options
--
--         MF.Target_Requires := 0;
--
--         if Integrity_Required then
--            MF.Target_Requires := MF.Target_Requires + Integrity;
--         end if;
--
--         if Confidentiality_Required then
--            MF.Target_Requires := MF.Target_Requires + Confidentiality;
--         end if;
--
--         if Establish_Trust_In_Client_Required then
--            MF.Target_Requires :=
--              MF.Target_Requires + Establish_Trust_In_Client;
--         end if;
--      end;
--   end Create_Factory;

   ----------------
   -- Create_QoS --
   ----------------

   function Create_QoS
     (End_Point : PolyORB.Transport.Transport_Endpoint_Access)
     return PolyORB.QoS.QoS_Parameter_Access
   is
   begin
      if End_Point.all in TLS_Endpoint then
         return Create_QoS (TLS_Endpoint (End_Point.all));

      else
         return null;
      end if;
   end Create_QoS;

--   ------------------------------
--   -- Create_Tagged_Components --
--   ------------------------------
--
--   function Create_Tagged_Components
--     (MF : TLS_Transport_Mechanism_Factory)
--      return Tagged_Components.Tagged_Component_List
--   is
--      Result : Tagged_Component_List;
--
--      TC : constant Tagged_Component_Access := new TC_TLS_Sec_Trans;
--
--   begin
--      --  TC_TLS_Sec_Trans (TC.all).Target_Supports := MF.Target_Supports;
--      --  TC_TLS_Sec_Trans (TC.all).Target_Requires := MF.Target_Requires;
--      TC_TLS_Sec_Trans (TC.all).Addresses       := Duplicate (MF.Addresses);
--
--      Add (Result, TC);
--
--      return Result;
--   end Create_Tagged_Components;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (TMA : TLS_Transport_Mechanism)
     return TLS_Transport_Mechanism
   is
      Result : TLS_Transport_Mechanism;

   begin
      Result.Addresses := Duplicate (TMA.Addresses);

      return Result;
   end Duplicate;

   -----------------------------
   -- Extract_TLS_Credentials --
   -----------------------------

   function Extract_TLS_Credentials
     (Credentials : Credentials_Ref)
      return TLS_Credentials_Access
   is
      Creds : Credentials_Access
        := Credentials_Access (Entity_Of (Credentials));

   begin
      if Creds /= null then
         Creds :=
           Credentials_Access
           (Entity_Of
            (Get_Transport_Credentials
             (Compound_Credentials_Access (Creds))));

         if Creds /= null
           and then Creds.all in TLS_Credentials'Class
         then
            return TLS_Credentials_Access (Creds);
         end if;
      end if;

      return null;
   end Extract_TLS_Credentials;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register (Tag_TLS_Sec_Trans, Create'Access);
      PolyORB.GIOP_P.Tagged_Components.CSI_Sec_Mech_List.Register
        (Create_QoS'Access);
   end Initialize;

   ------------------
   -- Is_Colocated --
   ------------------

   function Is_Colocated
     (Left  : TLS_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean
   is
   begin
      if Right not in TLS_Transport_Mechanism then
         return False;
      end if;

      declare
         use type Utils.Sockets.Socket_Name;

         L_Iter : Iterator := First (Left.Addresses);
         R_Iter : Iterator :=
                    First (TLS_Transport_Mechanism (Right).Addresses);
      begin

         --  Check if Left.Addresses and Right.Addresses have an address in
         --  common.

         Left_Addresses :
         while not Last (L_Iter) loop

            Right_Addresses :
            while not Last (R_Iter) loop
               if Value (L_Iter).all.all = Value (R_Iter).all.all then
                  return True;
               end if;

               Next (R_Iter);
            end loop Right_Addresses;

            Next (L_Iter);
         end loop Left_Addresses;
      end;

      return False;
   end Is_Colocated;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent
     (Left  : TLS_Transport_Mechanism;
      Right : Transport_Mechanism'Class) return Boolean
   is
      use type Utils.Sockets.Socket_Name;
   begin
      if Right not in TLS_Transport_Mechanism then
         return False;
      end if;

      declare
         L_Iter : Iterator := First (Left.Addresses);
         R_Iter : Iterator
           := First (TLS_Transport_Mechanism (Right).Addresses);

      begin
         if Length (Left.Addresses)
           /= Length (TLS_Transport_Mechanism (Right).Addresses)
         then
            return False;
         end if;

         while not Last (L_Iter) loop
            if Value (L_Iter).all.all /= Value (R_Iter).all.all then
               return False;
            end if;

            Next (L_Iter);
            Next (R_Iter);
         end loop;

         return True;
      end;
   end Is_Equivalent;

--   ------------------------
--   -- Is_Local_Mechanism --
--   ------------------------
--
--   function Is_Local_Mechanism
--     (MF : access TLS_Transport_Mechanism_Factory;
--      M  : access Transport_Mechanism'Class)
--      return Boolean
--   is
--      use type PolyORB.Sockets.Sock_Addr_Type;
--
--      Iter_1 : Iterator;
--
--   begin
--      if M.all not in TLS_Transport_Mechanism then
--         return False;
--      end if;
--
--      Iter_1 := First (TLS_Transport_Mechanism (M.all).Addresses);
--
--      while not Last (Iter_1) loop
--         declare
--            Iter_2 : Iterator := First (MF.Addresses);
--
--         begin
--            while not Last (Iter_2) loop
--               if Value (Iter_1).all = Value (Iter_2).all then
--                  return True;
--               end if;
--
--               Next (Iter_2);
--            end loop;
--         end;
--
--         Next (Iter_1);
--      end loop;
--
--      return False;
--   end Is_Local_Mechanism;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (M : access TLS_Transport_Mechanism) is
   begin
      Deallocate (M.Addresses);
   end Release_Contents;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"giop_p.transport_mechanisms.tls",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => +"tls",
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Transport_Mechanisms.TLS;
