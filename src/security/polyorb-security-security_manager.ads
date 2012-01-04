------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . S E C U R I T Y . S E C U R I T Y _ M A N A G E R     --
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

--  Security Manager pseudo object. It represent capsule specific security
--  information.

with PolyORB.Security.Credentials;
with PolyORB.Security.Transport_Mechanisms;
with PolyORB.Security.Types;

package PolyORB.Security.Security_Manager is

   function Own_Credentials
     return PolyORB.Security.Credentials.Credentials_List;

   function Get_Transport_Mechanism
     (Name : String)
      return
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access;

--  XXX Unimplemented features from CORBA Security Service Specification
--
--   readonly attribute Security::MechandOptionsList supported_mechanisms;
--
--   readonly attribute RequiredRights required_rights_object;
--
--   readonly attribute PrincipalAuthenticator principal_authenticator;
--
--   readonly attribute AccessDecision access_decision;
--
--   readonly attribute AuditDecision audit_decision;
--
--   TargetCredentials get_target_credentials (
--       in Object obj_ref
--   );
--
--   void remove_own_credentials (
--       in Credentials creds
--   );
--
--   CORBA::Policy get_security_policy (
--       in CORBA::PolicyType policy_type
--   );

   function Client_Requires return PolyORB.Security.Types.Association_Options;

   --  Transport mechsnism registry

   procedure Register_Transport_Mechanism
     (Name : String;
      Mech :
      PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access);

end PolyORB.Security.Security_Manager;
