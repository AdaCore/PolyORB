------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . S E C U R I T Y . C R E D E N T I A L S          --
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

with PolyORB.Security.Identities;
with PolyORB.Security.Types;
with PolyORB.Smart_Pointers;

package PolyORB.Security.Credentials is

   type Credentials is
     abstract new PolyORB.Smart_Pointers.Non_Controlled_Entity with
       null record;

   type Credentials_Access is access all Credentials'Class;

   type Credentials_Ref is new PolyORB.Smart_Pointers.Ref with null record;

   type Invocation_Credentials_Type is
     (Own_Credentials,
      Received_Credentials,
      Target_Credentials);  --  ???

--   function Credentials_Type
--     (Self : access Credentials)
--      return Invocation_Credentials_Type
--      is abstract;

   --  Server side Association Options

   function Get_Accepting_Options_Supported
     (Self : access Credentials)
      return PolyORB.Security.Types.Association_Options
      is abstract;

--   procedure Set_Accepting_Options_Supported
--     (Self    : access Credentials;
--      Options : PolyORB.Security.Types.Association_Options)
--      is abstract;

   function Get_Accepting_Options_Required
     (Self : access Credentials)
      return PolyORB.Security.Types.Association_Options
      is abstract;

--   procedure Set_Accepting_Options_Required
--     (Self    : access Credentials;
--      Options : PolyORB.Security.Types.Association_Options)
--      is abstract;

   --  Client side Association Options

   function Get_Invocation_Options_Supported
     (Self : access Credentials)
      return PolyORB.Security.Types.Association_Options
      is abstract;

--   procedure Set_Invocation_Options_Supported
--     (Self    : access Credentials;
--      Options : PolyORB.Security.Types.Association_Options)
--      is abstract;

   function Get_Invocation_Options_Required
     (Self : access Credentials)
      return PolyORB.Security.Types.Association_Options
      is abstract;

--   procedure Set_Invocation_Options_Required
--     (Self    : access Credentials;
--      Options : PolyORB.Security.Types.Association_Options)
--      is abstract;

   function Get_Identity
     (Self : access Credentials)
      return PolyORB.Security.Identities.Identity_Access
      is abstract;

--   function Is_Valid (Self : Ref; Expiry_Time : out Time) return Boolean;

--   type Security_Feature is
--      (No_Protection, Integrity, Confidentiality,
--       Integrity_And_Confidentiality, Detect_Replay, Detect_Misordering,
--       Establish_Trust_In_Target, Establish_Trust_In_Client);
--   --  from Security Service: No_Delegation, Simple_Delegation,
--   --                         Composite_Delegation
--   --  from CSIv2: Identity_Assertion, Delegation_By_Client

--   function Get_Security_Feature
--     (Self : Ref; Feature : Security_Feature) return Boolean;

--   type Received_Credentials is abstract new Ref with null record;

--   function Accepting_Credentials (Self : Ref) return Credentials
--     is abstract;

--   function Association_Options_Used
--     (Self : Received_Credentials)
--      return PolyORB.Security.Types.Association_Options
--      is abstract;

--   procedure Set_Invocation_Options_Required
--     (Self    : Received_Credentials;
--      Options : PolyORB.Security.Types.Association_Options)
--      is abstract;

--    Delegation_State
--    Delegation_Mode

   --  Credentials Lists

   type Credentials_List is
     array (Positive range <>) of PolyORB.Security.Credentials.Credentials_Ref;

private

   --  Credentials Type Registry

   type Credentials_Constructor is
     access function (Section_Name : String) return Credentials_Access;

   procedure Register
     (Credentials_Type : String;
      Constructor      : Credentials_Constructor);

   function Create_Credentials
     (Credentials_Type : String;
      Section_Name     : String)
      return Credentials_Ref'Class;

end PolyORB.Security.Credentials;
