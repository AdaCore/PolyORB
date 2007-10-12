------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.SECURITY.AUTHENTICATION_MECHANISMS                 --
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

with Ada.Streams;

with PolyORB.ASN1;
with PolyORB.Security.Credentials;
with PolyORB.Security.Identities;
with PolyORB.Security.Exported_Names;
with PolyORB.Security.Types;

package PolyORB.Security.Authentication_Mechanisms is

   type Client_Authentication_Mechanism is abstract tagged private;

   type Client_Authentication_Mechanism_Access is
     access all Client_Authentication_Mechanism'Class;

   function Is_Supports
     (Mechanism   : access Client_Authentication_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
      return Boolean is abstract;
   --  Return True iff Credentials supports the Mechanism

   function Init_Security_Context
     (Mechanism   : access Client_Authentication_Mechanism;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
      return Ada.Streams.Stream_Element_Array is abstract;
   --  Return authentication token

   procedure Release_Contents
     (Mechanism : access Client_Authentication_Mechanism);
   --  Release used resources

   type Target_Authentication_Mechanism is abstract tagged private;

   type Target_Authentication_Mechanism_Access is
     access all Target_Authentication_Mechanism'Class;

   function Get_Mechanism_OID
     (Mechanism : access Target_Authentication_Mechanism)
      return PolyORB.ASN1.Object_Identifier;
   --  Return authentication mechanism object identifier

   function Get_Target_Name
     (Mechanism : access Target_Authentication_Mechanism)
      return PolyORB.Security.Exported_Names.Exported_Name_Access;
   --  Return Target Name

   function Get_Supported_Identity_Types
     (Mechanism : access Target_Authentication_Mechanism)
      return PolyORB.Security.Types.Identity_Token_Type;
   --  Return set of supported identity types

   function Get_Supported_Naming_Mechanisms
     (Mechanism : access Target_Authentication_Mechanism)
      return PolyORB.Security.Types.OID_Lists.List;
   --  Return list of supported naming mechanisms. If authentication mechanism
   --  not support principal name identity type then returned list is always
   --  empty.

   procedure Accept_Security_Context
     (Mechanism    : access Target_Authentication_Mechanism;
      Token        :        PolyORB.Security.Types.Stream_Element_Array_Access;
      Success      : out    Boolean;
      Return_Token : out
        PolyORB.Security.Types.Stream_Element_Array_Access;
      Identity     : out    PolyORB.Security.Identities.Identity_Access)
      is abstract;
   --  Accept security context (do authentication)

   procedure Release_Contents
     (Mechanism : access Target_Authentication_Mechanism);
   --  Release used resources

   procedure Destroy
     (Mechanism : in out Client_Authentication_Mechanism_Access);

   procedure Destroy
     (Mechanism : in out Target_Authentication_Mechanism_Access);

   function Create_Client_Mechanism
     (Mechanism_OID : PolyORB.ASN1.Object_Identifier;
      Target_Name   : PolyORB.Security.Exported_Names.Exported_Name_Access)
      return Client_Authentication_Mechanism_Access;

   function Create_Target_Mechanism
     (Section_Name : String)
      return Target_Authentication_Mechanism_Access;

private

   type Client_Authentication_Mechanism is abstract tagged record
      Target_Name : PolyORB.Security.Exported_Names.Exported_Name_Access;
   end record;

   type Target_Authentication_Mechanism is abstract tagged record
      Mechanism_OID     : PolyORB.ASN1.Object_Identifier;
      Target_Name       : PolyORB.Security.Exported_Names.Exported_Name_Access;
      Identity_Types    : PolyORB.Security.Types.Identity_Token_Type;
      Naming_Mechanisms : PolyORB.Security.Types.OID_Lists.List;
   end record;

   --  Registry for known Authentication Mechanisms

   type Client_Mechanism_Constructor is access function
     (Target_Name : PolyORB.Security.Exported_Names.Exported_Name_Access)
      return Client_Authentication_Mechanism_Access;

   procedure Register
     (Mechanism_OID : PolyORB.ASN1.Object_Identifier;
      Constructor   : Client_Mechanism_Constructor);

   type Target_Mechanism_Constructor is access function
     (Section_Name : String)
      return Target_Authentication_Mechanism_Access;

   procedure Register
     (Mechanism_Name : String;
      Constructor    : Target_Mechanism_Constructor);

end PolyORB.Security.Authentication_Mechanisms;
