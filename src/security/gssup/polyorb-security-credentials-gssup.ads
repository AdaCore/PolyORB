------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E C U R I T Y . C R E D E N T I A L S . G S S U P    --
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

   function Get_Accepting_Options_Supported
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Accepting_Options_Supported
--     (Self    : access GSSUP_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   function Get_Accepting_Options_Required
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Accepting_Options_Required
--     (Self    : access GSSUP_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   function Get_Invocation_Options_Supported
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Invocation_Options_Supported
--     (Self    : access GSSUP_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   function Get_Invocation_Options_Required
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Types.Association_Options;

--   procedure Set_Invocation_Options_Required
--     (Self    : access GSSUP_Credentials;
--      Options : PolyORB.Security.Types.Association_Options);

   function Get_Identity
     (Self : access GSSUP_Credentials)
      return PolyORB.Security.Identities.Identity_Access;

   --  Derived from Non_Controlled_Entity

   procedure Finalize (Self : in out GSSUP_Credentials);

end PolyORB.Security.Credentials.GSSUP;
