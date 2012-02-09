------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . S O A P             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Binding data concrete implementation for SOAP over HTTP.

with PolyORB.Types;
with PolyORB.Utils.Sockets;

package PolyORB.Binding_Data.SOAP is

   pragma Elaborate_Body;

   type SOAP_Profile_Type is new Profile_Type with private;
   --  A profile that designates an object accessible through
   --  SOAP RPC over HTTP.

   overriding procedure Release    (P : in out SOAP_Profile_Type);

   ----------------------------------------------------
   -- Overloaded abstract operations of Profile_Type --
   ----------------------------------------------------

   overriding procedure Bind_Profile
     (Profile : access SOAP_Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container);

   overriding function Get_Profile_Tag
     (Profile : SOAP_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   overriding function Get_Profile_Preference
     (Profile : SOAP_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   overriding function Is_Colocated
     (Left  : SOAP_Profile_Type;
      Right : Profile_Type'Class) return Boolean;

   -----------------------------------------------
   -- Supplementary operations of SOAP profiles --
   -----------------------------------------------

   function Get_URI_Path (Profile : SOAP_Profile_Type) return Types.String;
   --  Return the 'path' component of the object URI.

   function To_URI (Prof : SOAP_Profile_Type) return String;
   --  Convert a SOAP profile to an equivalent URI.

   function Create_Profile (URI : Types.String) return Profile_Access;
   --  Create a SOAP RPC/HTTP profile from an absolute URI.

   overriding function Image (Prof : SOAP_Profile_Type) return String;
   --  Represent Prof as a string, for debugging purposes.

   ----------------------------
   -- SOAP profile factories --
   ----------------------------

   type SOAP_Profile_Factory is new Profile_Factory with private;

   overriding procedure Create_Factory
     (PF  : out SOAP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access);

   overriding function Create_Profile
     (PF  : access SOAP_Profile_Factory;
      Oid : Objects.Object_Id)
     return Profile_Access;

   overriding function Duplicate_Profile
     (P : SOAP_Profile_Type)
     return Profile_Access;

   overriding function Is_Local_Profile
     (PF : access SOAP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean;

private

   type SOAP_Profile_Type is new Profile_Type with record
      Address  : Utils.Sockets.Socket_Name_Ptr;
      URI_Path : Types.String;
   end record;

   type SOAP_Profile_Factory is new Profile_Factory with record
      Address : Utils.Sockets.Socket_Name_Ptr;
   end record;

end PolyORB.Binding_Data.SOAP;
