------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.CREDENTIALS.COMPOUND                   --
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

pragma Ada_2005;

with PolyORB.Parameters;

package body PolyORB.Security.Credentials.Compound is

   use PolyORB.Parameters;
   use PolyORB.Security.Types;

   ------------------------
   -- Create_Credentials --
   ------------------------

   function Create_Credentials
     (Section_Name : String)
      return Credentials_Ref
   is
      Transport_Credentials_Type      : constant String
        := Get_Conf (Section_Name, "transport_credentials_type", "");
      Authentication_Credentials_Type : constant String
        := Get_Conf (Section_Name, "authentication_credentials_type", "");
      Aux                             : constant Compound_Credentials_Access
        := new Compound_Credentials;
      Result                          : Credentials_Ref;

   begin
      if Transport_Credentials_Type /= "" then
         Aux.Transport :=
           PolyORB.Security.Credentials.Credentials_Ref
           (PolyORB.Security.Credentials.Create_Credentials
            (Transport_Credentials_Type, Section_Name));
      end if;

      if Authentication_Credentials_Type /= "" then
         Aux.Authentication :=
           PolyORB.Security.Credentials.Credentials_Ref
           (PolyORB.Security.Credentials.Create_Credentials
            (Authentication_Credentials_Type, Section_Name));
      end if;

      Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Aux));

      return Result;
   end Create_Credentials;

   ------------------------------------------
   -- Create_Received_Compound_Credentials --
   ------------------------------------------

   function Create_Received_Compound_Credentials
     (Accepting : Credentials_Ref;
      Transport : Credentials_Ref) return Credentials_Ref
   is
      Result : Credentials_Ref;
      Aux    : constant Received_Compound_Credentials_Access
        := new Received_Compound_Credentials;

   begin
      Aux.Accepting := Accepting;
      Aux.Transport := Transport;

      Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Aux));

      return Result;
   end Create_Received_Compound_Credentials;

   ----------------------
   -- Credentials_Type --
   ----------------------

   function Credentials_Type
     (Self : access Compound_Credentials)
      return Invocation_Credentials_Type
   is
      pragma Unreferenced (Self);

   begin
      return Own_Credentials;
   end Credentials_Type;

   ------------------------------------
   -- Get_Accepting_Options_Required --
   ------------------------------------

   overriding function Get_Accepting_Options_Required
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
      T      : constant Credentials_Access
        := Credentials_Access (Entity_Of (Self.Transport));
      A      : constant Credentials_Access
        := Credentials_Access (Entity_Of (Self.Authentication));
      Result : PolyORB.Security.Types.Association_Options := 0;

   begin
      if T /= null then
         Result := Result or Get_Accepting_Options_Required (T);
      end if;

      if A /= null then
         Result := Result or Get_Accepting_Options_Required (A);
      end if;

      return Result;
   end Get_Accepting_Options_Required;

   -------------------------------------
   -- Get_Accepting_Options_Supported --
   -------------------------------------

   overriding function Get_Accepting_Options_Supported
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
      T      : constant Credentials_Access
        := Credentials_Access (Entity_Of (Self.Transport));
      A      : constant Credentials_Access
        := Credentials_Access (Entity_Of (Self.Authentication));
      Result : PolyORB.Security.Types.Association_Options := 0;

   begin
      if T /= null then
         Result := Result or Get_Accepting_Options_Supported (T);
      end if;

      if A /= null then
         Result := Result or Get_Accepting_Options_Supported (A);
      end if;

      return Result;
   end Get_Accepting_Options_Supported;

   ------------------------------------
   -- Get_Authentication_Credentials --
   ------------------------------------

   function Get_Authentication_Credentials
     (Self : access Compound_Credentials) return Credentials_Ref
   is
   begin
      return Self.Authentication;
   end Get_Authentication_Credentials;

   ------------------
   -- Get_Identity --
   ------------------

   overriding function Get_Identity
     (Self : access Compound_Credentials)
      return PolyORB.Security.Identities.Identity_Access
   is
      pragma Unreferenced (Self);

   begin
      raise Program_Error;

      return null;
   end Get_Identity;

   --------------------------------------
   -- Get_Invocation_Options_Required --
   --------------------------------------

   overriding function Get_Invocation_Options_Required
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
      T      : constant Credentials_Access
        := Credentials_Access (Entity_Of (Self.Transport));
      A      : constant Credentials_Access
        := Credentials_Access (Entity_Of (Self.Authentication));
      Result : PolyORB.Security.Types.Association_Options := 0;

   begin
      if T /= null then
         Result := Result or Get_Invocation_Options_Required (T);
      end if;

      if A /= null then
         Result := Result or Get_Invocation_Options_Required (A);
      end if;

      return Result;
   end Get_Invocation_Options_Required;

   --------------------------------------
   -- Get_Invocation_Options_Supported --
   --------------------------------------

   overriding function Get_Invocation_Options_Supported
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
      T      : constant Credentials_Access
        := Credentials_Access (Entity_Of (Self.Transport));
      A      : constant Credentials_Access
        := Credentials_Access (Entity_Of (Self.Authentication));
      Result : PolyORB.Security.Types.Association_Options := 0;

   begin
      if T /= null then
         Result := Result or Get_Invocation_Options_Supported (T);
      end if;

      if A /= null then
         Result := Result or Get_Invocation_Options_Supported (A);
      end if;

      return Result;
   end Get_Invocation_Options_Supported;

   -------------------------------
   -- Get_Transport_Credentials --
   -------------------------------

   function Get_Transport_Credentials
     (Self : access Compound_Credentials) return Credentials_Ref
   is
   begin
      return Self.Transport;
   end Get_Transport_Credentials;

   ----------------------------
   -- Get_Transport_Identity --
   ----------------------------

   function Get_Transport_Identity
     (Self : access Compound_Credentials)
      return PolyORB.Security.Identities.Identity_Access
   is
   begin
      return Get_Identity (Credentials_Access (Entity_Of (Self.Transport)));
   end Get_Transport_Identity;

end PolyORB.Security.Credentials.Compound;
