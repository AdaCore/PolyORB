------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E C U R I T Y . C R E D E N T I A L S . G S S U P    --
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

with PolyORB.Security.Exported_Names;
with PolyORB.Types;

package PolyORB.Security.Credentials.GSSUP is

   type GSSUP_Credentials is new Credentials with private;

   type GSSUP_Credentials_Access is access all GSSUP_Credentials'Class;

   subtype UTF8_String is String;

   function Get_User_Name (Self : access GSSUP_Credentials) return UTF8_String;

   function Get_Password (Self : access GSSUP_Credentials) return UTF8_String;

   function Get_Target_Name
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Exported_Names.Exported_Name_Access;

private

   type GSSUP_Credentials is new Credentials with record
      User_Name   : PolyORB.Types.String;
      Password    : PolyORB.Types.String;
      Target_Name : PolyORB.Security.Exported_Names.Exported_Name_Access;
   end record;

   --  Derived from Credentials

--   function Credentials_Type
--     (Self : access GSSUP_Credentials)
--      return Invocation_Credentials_Type;

   overriding function Get_Accepting_Options_Supported
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Accepting_Options_Supported
--     (Self    : access GSSUP_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   overriding function Get_Accepting_Options_Required
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Accepting_Options_Required
--     (Self    : access GSSUP_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   overriding function Get_Invocation_Options_Supported
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Invocation_Options_Supported
--     (Self    : access GSSUP_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   overriding function Get_Invocation_Options_Required
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Invocation_Options_Required
--     (Self    : access GSSUP_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   overriding function Get_Identity
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Identities.Identity_Access;

   --  Derived from Non_Controlled_Entity

   overriding procedure Finalize (Self : in out GSSUP_Credentials);

end PolyORB.Security.Credentials.GSSUP;
