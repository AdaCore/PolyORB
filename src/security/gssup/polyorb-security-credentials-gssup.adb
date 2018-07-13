------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E C U R I T Y . C R E D E N T I A L S . G S S U P    --
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

pragma Ada_2012;

with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Security.Exported_Names.GSSUP;
with PolyORB.Utils.Strings;

package body PolyORB.Security.Credentials.GSSUP is

   function Create_Credentials
     (Section_Name : String)
      return Credentials_Access;

   procedure Initialize;

   ------------------------
   -- Create_Credentials --
   ------------------------

   function Create_Credentials
     (Section_Name : String)
      return Credentials_Access
   is
      use PolyORB.Parameters;

      User_Name   : constant String
        := Get_Conf (Section_Name, "gssup.username", "");
      Password    : constant String
        := Get_Conf (Section_Name, "gssup.password", "");
      Target_Name : constant String
        := Get_Conf (Section_Name, "gssup.target_name", "");

      Result : constant GSSUP_Credentials_Access := new GSSUP_Credentials;

   begin
      Result.User_Name   := PolyORB.Types.To_PolyORB_String (User_Name);
      Result.Password    := PolyORB.Types.To_PolyORB_String (Password);
      Result.Target_Name :=
        PolyORB.Security.Exported_Names.GSSUP.Create_GSSUP_Exported_Name
        (Target_Name);

      return Credentials_Access (Result);
   end Create_Credentials;

--   ----------------------
--   -- Credentials_Type --
--   ----------------------
--
--   function Credentials_Type
--     (Self : access GSSUP_Credentials)
--      return Invocation_Credentials_Type
--   is
--      pragma Unreferenced (Self);
--
--   begin
--      return Own_Credentials;
--   end Credentials_Type;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out GSSUP_Credentials) is
   begin
      PolyORB.Security.Exported_Names.Destroy (Self.Target_Name);
   end Finalize;

   ------------------------------------
   -- Get_Accepting_Options_Required --
   ------------------------------------

   overriding function Get_Accepting_Options_Required
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
      pragma Unreferenced (Self);

   begin
      return 0;
   end Get_Accepting_Options_Required;

   -------------------------------------
   -- Get_Accepting_Options_Supported --
   -------------------------------------

   overriding function Get_Accepting_Options_Supported
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
      pragma Unreferenced (Self);

   begin
      return 0;
   end Get_Accepting_Options_Supported;

   -----------------
   -- Get_Idenity --
   -----------------

   overriding function Get_Identity
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Identities.Identity_Access
   is
      pragma Unreferenced (Self);

   begin
      return null;
   end Get_Identity;

   -------------------------------------
   -- Get_Invocation_Options_Required --
   -------------------------------------

   overriding function Get_Invocation_Options_Required
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
      pragma Unreferenced (Self);

   begin
      return 0;
   end Get_Invocation_Options_Required;

   --------------------------------------
   -- Get_Invocation_Options_Supported --
   --------------------------------------

   overriding function Get_Invocation_Options_Supported
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options
   is
      pragma Unreferenced (Self);

   begin
      return PolyORB.Security.Types.Establish_Trust_In_Client;
   end Get_Invocation_Options_Supported;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password
     (Self : access GSSUP_Credentials)
      return UTF8_String
   is
   begin
      return PolyORB.Types.To_Standard_String (Self.Password);
   end Get_Password;

   ---------------------
   -- Get_Target_Name --
   ---------------------

   function Get_Target_Name
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Exported_Names.Exported_Name_Access
   is
   begin
      return Self.Target_Name;
   end Get_Target_Name;

   -------------------
   -- Get_User_Name --
   -------------------

   function Get_User_Name
     (Self : access GSSUP_Credentials)
      return UTF8_String
   is
   begin
      return PolyORB.Types.To_Standard_String (Self.User_Name);
   end Get_User_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register ("gssup", Create_Credentials'Access);
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.security.credentials.gssup",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Security.Credentials.GSSUP;
