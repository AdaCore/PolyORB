------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . S O A P             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

--  Binding data concrete implementation for SOAP over HTTP.

with PolyORB.Sockets;
with PolyORB.Types;

package PolyORB.Binding_Data.SOAP is

   pragma Elaborate_Body;

   type SOAP_Profile_Type is new Profile_Type with private;
   --  A profile that designates an object accessible through
   --  SOAP RPC over HTTP.

   procedure Release    (P : in out SOAP_Profile_Type);

   ----------------------------------------------------
   -- Overloaded abstract operations of Profile_Type --
   ----------------------------------------------------

   procedure Bind_Profile
     (Profile : access SOAP_Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container);

   function Get_Profile_Tag
     (Profile : SOAP_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : SOAP_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   function Is_Colocated
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

   function Image (Prof : SOAP_Profile_Type) return String;
   --  Represent Prof as a string, for debugging purposes.

   function Get_OA
     (Profile : SOAP_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr;
   pragma Inline (Get_OA);

   ----------------------------
   -- SOAP profile factories --
   ----------------------------

   type SOAP_Profile_Factory is new Profile_Factory with private;

   procedure Create_Factory
     (PF  : out SOAP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access);

   function Create_Profile
     (PF  : access SOAP_Profile_Factory;
      Oid : Objects.Object_Id)
     return Profile_Access;

   function Duplicate_Profile
     (P : SOAP_Profile_Type)
     return Profile_Access;

   function Is_Local_Profile
     (PF : access SOAP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean;

private

   type SOAP_Profile_Type is new Profile_Type with record
      Address  : Sockets.Sock_Addr_Type;
      URI_Path : Types.String;
   end record;

   type SOAP_Profile_Factory is new Profile_Factory with record
      Address : Sockets.Sock_Addr_Type;
   end record;

end PolyORB.Binding_Data.SOAP;
