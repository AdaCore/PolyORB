------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SECURITY.AUTHORITY_MECHANISMS                   --
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

with Ada.Streams;

with PolyORB.Security.Authorization_Elements;
--  with PolyORB.Security.Credentials;
with PolyORB.Security.Identities;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Security.Authority_Mechanisms is

   type Service_Configuration_Syntax is new PolyORB.Types.Unsigned_Long;

   type Client_Authority_Mechanism is abstract tagged null record;

   type Client_Authority_Mechanism_Access is
     access all Client_Authority_Mechanism'Class;

--   function Is_Same
--     (Self                : access Privilege_Authority_Type;
--      Privilege_Authority :        Privilege_Authority_Access)
--      return Boolean
--      is abstract;

--   procedure Get_Authorization_Token
--     (Self                           : access Privilege_Authority_Type;
--      Invocation_Credentials         :
--        PolyORB.Security.Credentials.Credentials_Ref;
--      Identity                       :
--        PolyORB.Security.Identities.Identity_Access;
--      Authorization_Token            :
--        Authorization_Elements.Authorization_Element_Lists.List;
--      Privilege_Authority            :        Privilege_Authority_Access;
--      Invocation_Identity            :    out
--        PolyORB.Security.Identities.Identity_Access;
--      Invocation_Authorization_Token :    out
--        Authorization_Elements.Authorization_Element_Lists.List;
--      Success                        :    out Boolean)
--      is abstract;

   procedure Get_Authorization_Token
     (Self                           : access Client_Authority_Mechanism;
--      Invocation_Credentials         :
--        PolyORB.Security.Credentials.Credentials_Ref;
--      Identity                       :
--        PolyORB.Security.Identities.Identity_Access;
--      Authorization_Token            :
--        Authorization_Elements.Authorization_Element_Lists.List;
--      Privilege_Authority            :        Privilege_Authority_Access;
      Invocation_Identity            :    out
        PolyORB.Security.Identities.Identity_Access;
      Invocation_Authorization_Token :    out
        Authorization_Elements.Authorization_Element_Lists.List;
      Success                        :    out Boolean)
      is abstract;

   procedure Release_Contents
     (Self : access Client_Authority_Mechanism)
      is abstract;
   --  Release used resources

   type Target_Authority_Mechanism is abstract tagged null record;

   type Target_Authority_Mechanism_Access is
     access all Target_Authority_Mechanism'Class;

   function Get_Service_Configuration_Syntax
     (Self : access Target_Authority_Mechanism)
      return Service_Configuration_Syntax
      is abstract;
   --  Return serivce configuration syntax

   function Verify
     (Self    : access Target_Authority_Mechanism;
      Element :        Authorization_Elements.Authorization_Element_Access)
      return Boolean
      is abstract;
   --  Check is autorization element signed by privilege authority

   function Encode
     (Self : access Target_Authority_Mechanism)
      return Ada.Streams.Stream_Element_Array
      is abstract;

   procedure Release_Contents
     (Self : access Target_Authority_Mechanism)
      is abstract;
   --  Release used resources

   procedure Destroy (Item : in out Client_Authority_Mechanism_Access);

   procedure Destroy (Item : in out Target_Authority_Mechanism_Access);

   function Create_Client_Authority_Mechanism
     (Syntax : Service_Configuration_Syntax;
      Name   : Ada.Streams.Stream_Element_Array)
      return Client_Authority_Mechanism_Access;
   --  Create client side privilege authority

   function Create_Target_Authority_Mechanism
     (Section_Name : Standard.String)
      return Target_Authority_Mechanism_Access;
   --  Create target side privilege authority

   package Client_Authority_Mechanism_Lists is
     new PolyORB.Utils.Chained_Lists (Client_Authority_Mechanism_Access);

   package Target_Authority_Mechanism_Lists is
     new PolyORB.Utils.Chained_Lists (Target_Authority_Mechanism_Access);

   --  Client and target privilege authority mechanisms registry

   type Target_Constructor is
     access function (Section_Name : Standard.String)
       return Target_Authority_Mechanism_Access;

   procedure Register (Name        : Standard.String;
                       Constructor : Target_Constructor);

   type Client_Constructor is
     access function (Name : Ada.Streams.Stream_Element_Array)
       return Client_Authority_Mechanism_Access;

   procedure Register (Syntax      : Service_Configuration_Syntax;
                       Constructor : Client_Constructor);

end PolyORB.Security.Authority_Mechanisms;
