------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.CREDENTIALS.COMPOUND                   --
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

pragma Ada_2005;

package PolyORB.Security.Credentials.Compound is

   --  Compound Credentials

   type Compound_Credentials is new Credentials with private;

   type Compound_Credentials_Access is access all Compound_Credentials'Class;

   function Get_Transport_Credentials
     (Self : access Compound_Credentials) return Credentials_Ref;

   function Get_Transport_Identity
     (Self : access Compound_Credentials)
      return PolyORB.Security.Identities.Identity_Access;

   function Get_Authentication_Credentials
     (Self : access Compound_Credentials) return Credentials_Ref;

   --  Received Credentials

   type Received_Compound_Credentials is new Compound_Credentials with private;

   type Received_Compound_Credentials_Access is
     access all Received_Compound_Credentials'Class;

--   --  Target Credentials
--
--   type Target_Compound_Credentials is new Compound_Credentials with private;

   function Create_Credentials (Section_Name : String) return Credentials_Ref;

   function Create_Received_Compound_Credentials
     (Accepting : Credentials_Ref;
      Transport : Credentials_Ref) return Credentials_Ref;

private

   type Compound_Credentials is new Credentials with record
      Transport      : Credentials_Ref;
      Authentication : Credentials_Ref;
   end record;

   --  Derived from Credentials

   function Credentials_Type
     (Self : access Compound_Credentials)
      return Invocation_Credentials_Type;

   overriding function Get_Accepting_Options_Supported
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Accepting_Options_Supported
--     (Self    : access Compound_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   overriding function Get_Accepting_Options_Required
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Accepting_Options_Required
--     (Self    : access Compound_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   overriding function Get_Invocation_Options_Supported
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Invocation_Options_Supported
--     (Self    : access Compound_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   overriding function Get_Invocation_Options_Required
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Invocation_Options_Required
--     (Self    : access Compound_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   overriding function Get_Identity
     (Self : access Compound_Credentials)
      return PolyORB.Security.Identities.Identity_Access;

   type Received_Compound_Credentials is new Compound_Credentials with record
      Accepting : Credentials_Ref;
   end record;

--   type Target_Compound_Credentials is new Compound_Credentials with record
--      Initiating : Credentials_Ref;
--   end record;

end PolyORB.Security.Credentials.Compound;
