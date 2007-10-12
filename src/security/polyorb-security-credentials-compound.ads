------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.CREDENTIALS.COMPOUND                   --
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

   function Get_Accepting_Options_Supported
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Accepting_Options_Supported
--     (Self    : access Compound_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   function Get_Accepting_Options_Required
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Accepting_Options_Required
--     (Self    : access Compound_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   function Get_Invocation_Options_Supported
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Invocation_Options_Supported
--     (Self    : access Compound_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   function Get_Invocation_Options_Required
     (Self : access Compound_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Invocation_Options_Required
--     (Self    : access Compound_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   function Get_Identity
     (Self : access Compound_Credentials)
      return PolyORB.Security.Identities.Identity_Access;

   type Received_Compound_Credentials is new Compound_Credentials with record
      Accepting : Credentials_Ref;
   end record;

--   type Target_Compound_Credentials is new Compound_Credentials with record
--      Initiating : Credentials_Ref;
--   end record;

end PolyORB.Security.Credentials.Compound;
