------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E C U R I T Y . C R E D E N T I A L S . T L S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

--  Notes: TLS credentials are usually useful at both accepting and
--  invocation credentials. But, supported functionality are very
--  dependent from credentials configuration. Following description
--  describe most significant aspects of credentials configuration.
--
--  Integrity:
--
--  Confidentiality:
--
--  Detect_Replay: Not supported.
--
--  Detect_Misordering: Not supported.
--
--  Establish_Trust_In_[Target/Client]: This function dependent from presence
--  of own certificate/private key pair and trusted CA's certificates.
--
--  If credentials have certificate/private key pair then it supports
--  Establish_Trust_In_Client invocation association option and
--  Establish_Trust_In_Target accepting association option.
--
--  If credentials have list of trusted CA's certificates then it supports
--  Establish_Trust_In_Target invocation association option and
--  Establish_Trust_In_Client accepting association option.

with Ada.Strings.Fixed;

with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Security.Identities.Distinguished_Name;
with PolyORB.Utils.Strings;

package body PolyORB.Security.Credentials.TLS is

   use PolyORB.Errors;
   use PolyORB.Parameters;
   use PolyORB.Security.Identities.Distinguished_Name;
   use PolyORB.Security.Types;
   use PolyORB.TLS;
   use PolyORB.Types;
   use PolyORB.X509;

   function Create_Credentials
     (Section_Name : String)
      return Credentials_Access;

   procedure Initialize;

   -----------------------------
   -- Create_Accepting_Socket --
   -----------------------------

   function Create_Accepting_Socket
     (Self : access TLS_Credentials) return PolyORB.TLS.TLS_Socket_Type
   is
   begin
      return Create (Self.Context);
   end Create_Accepting_Socket;

   ------------------------
   -- Create_Credentials --
   ------------------------

   function Create_Credentials
     (Section_Name : String)
      return Credentials_Access
   is
      Method_Name                 : constant String
        := Get_Conf (Section_Name, "tls.method", "any");
      Certificate_File            : constant String
        := Get_Conf (Section_Name, "tls.certificate_file", "");
      Certificate_Chain_File      : constant String
        := Get_Conf (Section_Name, "tls.certificate_chain_file", "");
      Private_Key_File            : constant String
        := Get_Conf (Section_Name, "tls.private_key_file", "");
      Certificate_Authority_File  : constant String
        := Get_Conf (Section_Name, "tls.certificate_authority_file", "");
      Certificate_Authority_Path  : constant String
        := Get_Conf (Section_Name, "tls.certificate_authority_path", "");
      Ciphers                     : constant String
        := Get_Conf (Section_Name, "tls.ciphers", "");
      Verify_Peer                 : constant Boolean
        := Get_Conf (Section_Name, "tls.verify_peer", False);
      Verify_Fail_If_No_Peer_Cert : constant Boolean
        := Get_Conf
        (Section_Name, "tls.verify_fail_if_no_peer_certificate", False);

      Error  : Error_Container;
      Result : TLS_Credentials_Access;

   begin
      Create_TLS_Credentials
        (Credentials                        => Result,
         Error                              => Error,
         Method_Name                        => Method_Name,
         Private_Key_File                   => Private_Key_File,
         Certificate_File                   => Certificate_File,
         Certificate_Chain_File             => Certificate_Chain_File,
         Certificate_Authority_File         => Certificate_Authority_File,
         Certificate_Authority_Path         => Certificate_Authority_Path,
         Ciphers                            => Ciphers,
         Verify_Peer                        => Verify_Peer,
         Verify_Fail_If_No_Peer_Certificate => Verify_Fail_If_No_Peer_Cert);

      if Found (Error) then
         raise Program_Error;
      end if;

      return Credentials_Access (Result);
   end Create_Credentials;

   ------------------------------
   -- Create_Invocation_Socket --
   ------------------------------

   function Create_Invocation_Socket
     (Self : access TLS_Credentials) return PolyORB.TLS.TLS_Socket_Type
   is
   begin
      return Create (Self.Context);
   end Create_Invocation_Socket;

   ---------------------------------
   -- Create_Peer_TLS_Credentials --
   ---------------------------------

   function Create_Peer_TLS_Credentials
     (Socket : PolyORB.TLS.TLS_Socket_Type) return Credentials_Ref
   is
      Result : Credentials_Ref;
      Aux    : constant TLS_Credentials_Access := new TLS_Credentials (False);

   begin
      Aux.Certificate := Peer_Certificate_Of (Socket);

      Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Aux));

      return Result;
   end Create_Peer_TLS_Credentials;

   ----------------------------
   -- Create_TLS_Credentials --
   ----------------------------

   procedure Create_TLS_Credentials
     (Credentials                        :    out TLS_Credentials_Access;
      Error                              : in out
        PolyORB.Errors.Error_Container;
      Method_Name                        :        String  := "";
      Private_Key_File                   :        String  := "";
      Certificate_File                   :        String  := "";
      Certificate_Chain_File             :        String  := "";
      Certificate_Authority_File         :        String  := "";
      Certificate_Authority_Path         :        String  := "";
      Ciphers                            :        String  := "";
      Verify_Peer                        :        Boolean := False;
      Verify_Fail_If_No_Peer_Certificate :        Boolean := False)
   is
      Method : TLS_Method_Type;
      Mode   : TLS_Verification_Mode := (others => False);

   begin
      if Method_Name = "" or else Method_Name = "any" then
         Method := Any;

      elsif Method_Name = "tls1" then
         Method := TLS_1;

      elsif Method_Name = "ssl3" then
         Method := SSL_3;

      elsif Method_Name = "ssl2" then
         Method := SSL_2;

      else
         Throw (Error, Bad_Param_E,
                System_Exception_Members'(0, Completed_No));
      end if;

      Credentials := new TLS_Credentials (True);

      Credentials.Context := Create (Method);

      --  Loading CA's certificates

      if Certificate_Authority_File /= ""
        or else Certificate_Authority_Path /= ""
      then
         Load_Verify_Locations
           (Credentials.Context,
            Certificate_Authority_File,
            Certificate_Authority_Path);
         Credentials.CA_Defined := True;
      end if;

      --  Load owner's certificate and private key

      if Private_Key_File /= ""
        or else Certificate_File /= ""
        or else Certificate_Chain_File /= ""
      then
         Use_Private_Key (Credentials.Context, Private_Key_File);

         if Certificate_Chain_File /= "" then
            --  XXX Certificate member should be defined!
            Use_Certificate_Chain
              (Credentials.Context, Certificate_Chain_File);

         else
            Credentials.Certificate := Read (Certificate_File);
            Use_Certificate (Credentials.Context, Credentials.Certificate);
         end if;

         Check_Private_Key (Credentials.Context);

         Credentials.Cert_Defined := True;
      end if;

      --  Setup ciphers list

      if Ciphers /= "" then
         Set_Cipher_List (Credentials.Context, Ciphers);
         Credentials.Ciphers := To_PolyORB_String (Ciphers);
      end if;

      --  Setup verification mode

      if Verify_Peer then
         Mode (Peer) := True;

         if Verify_Fail_If_No_Peer_Certificate then
            Mode (Fail_If_No_Peer_Certificate) := True;
         end if;
      end if;

      Set_Verify_Mode (Credentials.Context, Mode);

      --  Calculate Association Options

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

         List : constant TLS_Cipher_List
           := Ciphers_Of (Credentials.Context);

         Integrity_Supported                 : Boolean := False;
         Integrity_Required                  : Boolean := True;
         Confidentiality_Supported           : Boolean := False;
         Confidentiality_Required            : Boolean := True;
         Authentication_Supported            : Boolean := False;
         Authentication_Required             : Boolean := True;
         pragma Warnings (Off, Authentication_Required);
         --  XXX Should be investigated!!!

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

               --  Compute Authentication option

               if Is_None (Desc, "Au") then
                  Authentication_Required := False;

               else
                  Authentication_Supported := True;
               end if;

            end;
         end loop;

         if Integrity_Supported then
            Credentials.Accepting_Supports :=
              Credentials.Accepting_Supports or Integrity;
            Credentials.Invocation_Supports :=
              Credentials.Invocation_Supports or Integrity;

            if Integrity_Required then
               Credentials.Accepting_Requires :=
                 Credentials.Accepting_Requires or Integrity;
               Credentials.Invocation_Requires :=
                 Credentials.Invocation_Requires or Integrity;
            end if;
         end if;

         if Confidentiality_Supported then
            Credentials.Accepting_Supports :=
              Credentials.Accepting_Supports or Confidentiality;
            Credentials.Invocation_Supports :=
              Credentials.Invocation_Supports or Confidentiality;

            if Confidentiality_Required then
               Credentials.Accepting_Requires :=
                 Credentials.Accepting_Requires or Confidentiality;
               Credentials.Invocation_Requires :=
                 Credentials.Invocation_Requires or Confidentiality;
            end if;
         end if;

         --  XXX Following code should be reviewed. It incorrectly handle
         --  some configurations. Also, it is possible to raise Bad_Param
         --  in some situations.

         if Authentication_Supported then
            if Credentials.Cert_Defined then
               Credentials.Accepting_Supports :=
                 Credentials.Accepting_Supports or Establish_Trust_In_Target;
               Credentials.Invocation_Supports :=
                 Credentials.Invocation_Supports or Establish_Trust_In_Client;
            end if;

            if Credentials.CA_Defined then
               if Mode (Peer) then
                  Credentials.Accepting_Supports :=
                    Credentials.Accepting_Supports
                    or Establish_Trust_In_Client;
                  Credentials.Invocation_Supports :=
                    Credentials.Invocation_Supports
                    or Establish_Trust_In_Target;

                  if Mode (Fail_If_No_Peer_Certificate) then
                     Credentials.Accepting_Requires :=
                       Credentials.Accepting_Requires
                       or Establish_Trust_In_Client;
                  end if;
               end if;
            end if;
         end if;
      end;

   exception
      when TLS_Error =>
         Throw (Error, Bad_Param_E,
                System_Exception_Members'(0, Completed_No));
   end Create_TLS_Credentials;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out TLS_Credentials) is
   begin
      Destroy (Self.Certificate);

      if Self.Own then
         Destroy (Self.Context);
      end if;
   end Finalize;

   ------------------------------------
   -- Get_Accepting_Options_Required --
   ------------------------------------

   function Get_Accepting_Options_Required
     (Self : access TLS_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
   begin
      return Self.Accepting_Requires;
   end Get_Accepting_Options_Required;

   -------------------------------------
   -- Get_Accepting_Options_Supported --
   -------------------------------------

   function Get_Accepting_Options_Supported
     (Self : access TLS_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
   begin
      return Self.Accepting_Supports;
   end Get_Accepting_Options_Supported;

   ------------------
   -- Get_Identity --
   ------------------

   function Get_Identity
     (Self : access TLS_Credentials)
      return PolyORB.Security.Identities.Identity_Access
   is
   begin
      return Create (Duplicate (Subject_Name_Of (Self.Certificate)));
   end Get_Identity;

   -------------------------------------
   -- Get_Invocation_Options_Required --
   -------------------------------------

   function Get_Invocation_Options_Required
     (Self : access TLS_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
   begin
      return Self.Invocation_Requires;
   end Get_Invocation_Options_Required;

   --------------------------------------
   -- Get_Invocation_Options_Supported --
   --------------------------------------

   function Get_Invocation_Options_Supported
     (Self : access TLS_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
   begin
      return Self.Invocation_Supports;
   end Get_Invocation_Options_Supported;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register ("tls", Create_Credentials'Access);
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.security.credentials.tls",
          Conflicts => Empty,
          Depends   => +"tls",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Credentials.TLS;
