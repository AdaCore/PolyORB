------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E C U R I T Y . C R E D E N T I A L S . T L S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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
