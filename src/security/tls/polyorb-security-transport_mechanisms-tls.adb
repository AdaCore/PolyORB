------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.SECURITY.TRANSPORT_MECHANISMS.TLS                 --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Security.Credentials.Compound;
with PolyORB.Security.Credentials.TLS;
with PolyORB.Transport.Connected.Sockets.TLS;

package body PolyORB.Security.Transport_Mechanisms.TLS is

   use PolyORB.Security.Credentials;
   use PolyORB.Security.Credentials.Compound;
   use PolyORB.Security.Credentials.TLS;
   use PolyORB.Transport;
   use PolyORB.Transport.Connected.Sockets.TLS;
   use TAP_Lists;

   function Extract_TLS_Credentials
     (Credentials : Credentials_Ref)
      return TLS_Credentials_Access;

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

   -----------------
   -- Is_Supports --
   -----------------

   function Is_Supports
     (Mechanism   : access Client_TLS_Transport_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
      return Boolean
   is
      pragma Unreferenced (Mechanism);

      use PolyORB.Security.Types;

      Creds  : constant TLS_Credentials_Access
        := Extract_TLS_Credentials (Credentials);

   begin
      return Creds /= null;
      --  XXX Should also check satisfaction of target's requirements
   end Is_Supports;

   -------------------------------
   -- Set_Accepting_Credentials --
   -------------------------------

   procedure Set_Accepting_Credentials
     (Mechanism   : access Target_TLS_Transport_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
   is
      Iter : Iterator := First (Mechanism.TAP);

   begin
      Mechanism.Credentials := Credentials;

      while not Last (Iter) loop
         Set_Accepting_Credentials
           (TLS_Access_Point (Value (Iter).all.all), Credentials);
         Next (Iter);
      end loop;
   end Set_Accepting_Credentials;

   ------------------------------
   -- Supported_Identity_Types --
   ------------------------------

   function Supported_Identity_Types
     (Mechanism : access Target_TLS_Transport_Mechanism)
      return PolyORB.Security.Types.Identity_Token_Type
   is
      pragma Unreferenced (Mechanism);

      use PolyORB.Security.Types;

   begin
      return ITT_X509_Cert_Chain or ITT_Distinguished_Name;
   end Supported_Identity_Types;

   ---------------------------------
   -- Supported_Naming_Mechanisms --
   ---------------------------------

   function Supported_Naming_Mechanisms
     (Mechanism : access Target_TLS_Transport_Mechanism)
      return PolyORB.Security.Types.OID_Lists.List
   is
      pragma Unreferenced (Mechanism);

   begin
      return PolyORB.Security.Types.OID_Lists.Empty;
   end Supported_Naming_Mechanisms;

   ---------------------
   -- Target_Requires --
   ---------------------

   function Target_Requires
     (Mechanism : access Client_TLS_Transport_Mechanism)
      return PolyORB.Security.Types.Association_Options
   is
   begin
      return Mechanism.Target_Requires;
   end Target_Requires;

   function Target_Requires
     (Mechanism : access Target_TLS_Transport_Mechanism)
      return PolyORB.Security.Types.Association_Options
   is
      Credentials  : constant TLS_Credentials_Access
        := Extract_TLS_Credentials (Mechanism.Credentials);

   begin
      if Credentials = null then
         raise Program_Error;
      end if;

      return Get_Accepting_Options_Required (Credentials);
   end Target_Requires;

   ---------------------
   -- Target_Supports --
   ---------------------

   function Target_Supports
     (Mechanism : access Client_TLS_Transport_Mechanism)
      return PolyORB.Security.Types.Association_Options
   is
   begin
      return Mechanism.Target_Supports;
   end Target_Supports;

   function Target_Supports
     (Mechanism : access Target_TLS_Transport_Mechanism)
      return PolyORB.Security.Types.Association_Options
   is
      Credentials  : constant TLS_Credentials_Access
        := Extract_TLS_Credentials (Mechanism.Credentials);

   begin
      if Credentials = null then
         raise Program_Error;
      end if;

      return Get_Accepting_Options_Supported (Credentials);
   end Target_Supports;

end PolyORB.Security.Transport_Mechanisms.TLS;
