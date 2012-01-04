------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . S E C U R I T Y . S E C U R I T Y _ M A N A G E R     --
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

with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Security.Credentials.Compound;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package body PolyORB.Security.Security_Manager is

   use PolyORB.Parameters;
   use PolyORB.Security.Transport_Mechanisms;
   use PolyORB.Types;

   procedure Initialize;

   type Creds_Item is record
      Creds : PolyORB.Security.Credentials.Credentials_Ref;
      Name  : PolyORB.Types.String;
   end record;

   package Creds_Lists is
     new PolyORB.Utils.Chained_Lists (Creds_Item, "=", True);
   use Creds_Lists;

   type Mechs_Item is record
      Mech : Target_Transport_Mechanism_Access;
      Name : PolyORB.Types.String;
   end record;

   package Transport_Mechanism_Lists is
     new PolyORB.Utils.Chained_Lists (Mechs_Item, "=", True);
   use Transport_Mechanism_Lists;

   Creds    : Creds_Lists.List;
   Mechs    : Transport_Mechanism_Lists.List;
   Requires : PolyORB.Security.Types.Association_Options;

   ---------------------
   -- Client_Requires --
   ---------------------

   function Client_Requires
      return PolyORB.Security.Types.Association_Options
   is
   begin
      return Requires;
   end Client_Requires;

   -----------------------------
   -- Get_Transport_Mechanism --
   -----------------------------

   function Get_Transport_Mechanism
     (Name : String)
      return
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access
   is
      Iter : Transport_Mechanism_Lists.Iterator := First (Mechs);

   begin
      while not Last (Iter) loop
         if Value (Iter).Name = Name then
            return Value (Iter).Mech;
         end if;

         Next (Iter);
      end loop;

      return null;
   end Get_Transport_Mechanism;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Calculate client requirements

      declare
         use PolyORB.Security.Types;

         Require_Integrity                 : constant Boolean
           := Get_Conf ("security_manager", "integrity_required", False);
         Require_Confidentiality           : constant Boolean
           := Get_Conf ("security_manager", "confidentiality_required", False);
         Require_Detect_Replay             : constant Boolean
           := Get_Conf ("security_manager", "detect_replay_required", False);
         Require_Detect_Misordering        : constant Boolean
           := Get_Conf
           ("security_manager", "detect_misordering_required", False);
         Require_Establish_Trust_In_Target : constant Boolean
           := Get_Conf
           ("security_manager", "establish_trust_in_target_required", False);
         Require_Establish_Trust_In_Client : constant Boolean
           := Get_Conf
           ("security_manager", "establish_trust_in_client_required", False);
         Require_Identity_Assertion        : constant Boolean
           := Get_Conf
           ("security_manager", "identity_assertion_required", False);
         Require_Delegation_By_Client      : constant Boolean
           := Get_Conf
           ("security_manager", "delegation_by_client_required", False);

      begin
         if Require_Integrity then
            Requires := Requires or Integrity;
         end if;

         if Require_Confidentiality then
            Requires := Requires or Confidentiality;
         end if;

         if Require_Detect_Replay then
            Requires := Requires or Detect_Replay;
         end if;

         if Require_Detect_Misordering then
            Requires := Requires or Detect_Misordering;
         end if;

         if Require_Establish_Trust_In_Target then
            Requires := Requires or Establish_Trust_In_Target;
         end if;

         if Require_Establish_Trust_In_Client then
            Requires := Requires or Establish_Trust_In_Client;
         end if;

         if Require_Identity_Assertion then
            Requires := Requires or Identity_Assertion;
         end if;

         if Require_Delegation_By_Client then
            Requires := Requires or Delegation_By_Client;
         end if;
      end;

      --  Creating capsule's credentials

      declare
         Own_Credentials : constant String
           := Get_Conf ("security_manager", "own_credentials", "");

         Last  : Natural := Own_Credentials'First - 1;
         First : Positive;
         Aux   : PolyORB.Security.Credentials.Credentials_Ref;

      begin
         Parse_Creds : loop
            First := Last + 1;

            exit Parse_Creds when First > Own_Credentials'Last;

            while Own_Credentials (First) = ' ' loop
               First := First + 1;
               exit Parse_Creds when First > Own_Credentials'Last;
            end loop;

            Last := First;

            while Last <= Own_Credentials'Last
              and then Own_Credentials (Last) /= ' '
            loop
               Last := Last + 1;
            end loop;

            Last := Last - 1;

            Aux :=
              PolyORB.Security.Credentials.Compound.Create_Credentials
              (Own_Credentials (First .. Last));

            if not PolyORB.Security.Credentials.Is_Null (Aux) then
               Append
                 (Creds,
                  (Aux,
                   PolyORB.Types.To_PolyORB_String
                   (Own_Credentials (First .. Last))));

            else
               raise Program_Error;
            end if;
         end loop Parse_Creds;
      end;
   end Initialize;

   ---------------------
   -- Own_Credentials --
   ---------------------

   function Own_Credentials
     return PolyORB.Security.Credentials.Credentials_List
   is
      Result :
        PolyORB.Security.Credentials.Credentials_List (1 .. Length (Creds));
      RLast  : Natural := 0;
      Iter   : Creds_Lists.Iterator := First (Creds);

   begin
      while not Last (Iter) loop
         RLast := RLast + 1;
         Result (RLast) := Value (Iter).Creds;
         Next (Iter);
      end loop;

      return Result;
   end Own_Credentials;

   ----------------------------------
   -- Register_Transport_Mechanism --
   ----------------------------------

   procedure Register_Transport_Mechanism
     (Name : String;
      Mech :
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access)
   is
   begin
      Append
        (Mechs,
         (Mech,
          PolyORB.Types.To_PolyORB_String (Name)));
   end Register_Transport_Mechanism;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.security.security_manager",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => +"orb"
      --  Neutral stuff
          & "polyorb.security.authority_mechanisms.atlas_client?"
          & "polyorb.security.authority_mechanisms.atlas_target?"
          & "polyorb.security.credentials.gssup?"
          & "polyorb.security.credentials.tls?"
          & "polyorb.security.identities.distinguished_name?",
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Security_Manager;
