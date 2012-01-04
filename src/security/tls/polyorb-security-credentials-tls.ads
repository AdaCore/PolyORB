------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E C U R I T Y . C R E D E N T I A L S . T L S      --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Errors;
with PolyORB.TLS;
with PolyORB.Types;
with PolyORB.X509;

package PolyORB.Security.Credentials.TLS is

   type TLS_Credentials (<>) is new Credentials with private;

   type TLS_Credentials_Access is access all TLS_Credentials'Class;

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
      Verify_Fail_If_No_Peer_Certificate :        Boolean := False);

   function Create_Peer_TLS_Credentials
     (Socket : PolyORB.TLS.TLS_Socket_Type) return Credentials_Ref;

   function Create_Invocation_Socket
     (Self : access TLS_Credentials) return PolyORB.TLS.TLS_Socket_Type;

   function Create_Accepting_Socket
     (Self : access TLS_Credentials) return PolyORB.TLS.TLS_Socket_Type;

private

   type TLS_Credentials (Own : Boolean) is new Credentials with record
      Certificate : PolyORB.X509.Certificate;

      case Own is
         when True =>
            Context             : PolyORB.TLS.TLS_Context_Type;
            Ciphers             : PolyORB.Types.String;
            Cert_Defined        : Boolean := False;
            CA_Defined          : Boolean := False;
            Accepting_Supports  : PolyORB.Security.Types.Association_Options
              := 0;
            Accepting_Requires  : PolyORB.Security.Types.Association_Options
              := 0;
            Invocation_Supports : PolyORB.Security.Types.Association_Options
              := 0;
            Invocation_Requires : PolyORB.Security.Types.Association_Options
              := 0;

         when False =>
            null;
      end case;
   end record;

   --  Derived from Credentials

   function Get_Accepting_Options_Supported
     (Self : access TLS_Credentials)
      return PolyORB.Security.Types.Association_Options;

   function Get_Accepting_Options_Required
     (Self : access TLS_Credentials)
      return PolyORB.Security.Types.Association_Options;

   function Get_Invocation_Options_Supported
     (Self : access TLS_Credentials)
      return PolyORB.Security.Types.Association_Options;

   function Get_Invocation_Options_Required
     (Self : access TLS_Credentials)
      return PolyORB.Security.Types.Association_Options;

   function Get_Identity
     (Self : access TLS_Credentials)
      return PolyORB.Security.Identities.Identity_Access;

   --  Derived from Entity

   procedure Finalize (Self : in out TLS_Credentials);

end PolyORB.Security.Credentials.TLS;
