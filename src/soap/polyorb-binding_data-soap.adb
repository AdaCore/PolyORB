------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . S O A P             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Components;
with PolyORB.Protocols;
with PolyORB.Protocols.SOAP;
with PolyORB.Sockets;
with PolyORB.Filters;
with PolyORB.Filters.HTTP;
with PolyORB.Objects;
with PolyORB.Types;
with PolyORB.Transport.Sockets;
with PolyORB.Protocols.HTTP;
with PolyORB.Utils.HTTP;


package body PolyORB.Binding_Data.SOAP is

   use PolyORB.Transport.Sockets;
   use PolyORB.Objects;
   use PolyORB.Types;
   use PolyORB.Sockets;
   use PolyORB.Protocols.HTTP;

   procedure Initialize (P : in out SOAP_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   procedure Adjust (P : in out SOAP_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   procedure Finalize (P : in out SOAP_Profile_Type) is
   begin
      Free (P.Object_Id);
   end Finalize;

   function Get_Object_Key
     (Profile : SOAP_Profile_Type)
     return Objects.Object_Id is
   begin
      return Profile.Object_Id.all;
   end Get_Object_Key;

   function Get_URL
     (Profile : SOAP_Profile_Type)
     return URL_Object is
   begin
      return URL_Parse (To_Standard_String (Profile.Target_URL));
   end Get_URL;

   procedure Bind_Profile
     (Profile   : SOAP_Profile_Type;
      TE        : out Transport.Transport_Endpoint_Access;
      Filter    : out Components.Component_Access)
   is
      use PolyORB.Components;
      use PolyORB.Protocols;
      use PolyORB.Protocols.SOAP;
      use PolyORB.Sockets;
      use PolyORB.Filters;
      use PolyORB.Filters.HTTP;

      Sock : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;

      Pro  : aliased SOAP_Protocol;
      Sli  : aliased HTTP_Filter_Factory;
      Prof : Profile_Access := new SOAP_Profile_Type;
      TProf : SOAP_Profile_Type
        renames SOAP_Profile_Type (Prof.all);
      Connection : HTTP_Connection_Access :=
          new HTTP_Connection'(Create_Connection
           (To_Standard_String (Profile.Target_URL)));

   begin

      Create_Socket (Sock);
      Connect_Socket (Sock, Remote_Addr);

      TE := new Socket_Endpoint;
      Create (Socket_Endpoint (TE.all), Sock);

      Chain_Factories ((0 => Sli'Unchecked_Access,
                        1 => Pro'Unchecked_Access));

      Filter := Component_Access (Create_Filter_Chain (Sli'Unchecked_Access));

      TProf.Address := Remote_Addr;
      TProf.Target_URL := Profile.Target_URL;
      TProf.Object_Id := Profile.Object_Id;

      Store_Profile (SOAP_Session (Upper (Filter_Access
             (Filter)).all)'Access, Prof);

      Store_Connection (SOAP_Session (Upper (Filter_Access
             (Filter)).all)'Access, Connection);


      --  XXX Session must be an access to the lowest filter in
      --  the stack (=> the Slicer).

      --  The caller will invoke Register_Endpoint on TE.
   end Bind_Profile;


   function Get_Profile_Tag
     (Profile : SOAP_Profile_Type)
     return Profile_Tag is
   begin
      return Tag_SOAP;
   end Get_Profile_Tag;

   function Get_Profile_Preference
     (Profile : SOAP_Profile_Type)
     return Profile_Preference is
   begin
      return Preference_Default;
   end Get_Profile_Preference;

   procedure Create_Factory
     (PF  : out SOAP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access) is
   begin
      PF.Address := Address_Of (Socket_Access_Point (TAP.all));
   end Create_Factory;

   function Create_Profile
     (PF  : access SOAP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access
   is
   begin
      return null;
   end Create_Profile;


   function Create_Profile
     (PF  : access SOAP_Profile_Factory;
      URL : Types.String)
      return Profile_Access
   is
      IP_Addr     : Inet_Addr_Type;
      Result : constant Profile_Access
        := new SOAP_Profile_Type;

      TResult : SOAP_Profile_Type
        renames SOAP_Profile_Type (Result.all);

      URL_Obj : URL_Object := URL_Parse (To_Standard_String (URL));

   begin
      IP_Addr := Addresses (Get_Host_By_Name
            (To_Standard_String (URL_Obj.Server_Name)));
      TResult.Address := Sock_Addr_Type'(Family => Family_Inet,
           Addr => IP_Addr, Port => Port_Type (URL_Obj.Port));
      TResult.Target_URL := URL;
      TResult.Object_Id := new Object_Id'(
           Mapping_String_To_Object (URL_Obj.URI));
      return Result;
   end Create_Profile;



   function Is_Local_Profile
     (PF : access SOAP_Profile_Factory;
      P : Profile_Access) return Boolean is
   begin
      return P.all in SOAP_Profile_Type
        and then SOAP_Profile_Type (P.all).Target_URL = PF.Target_URL;
   end Is_Local_Profile;


   function Image (Prof : SOAP_Profile_Type)
     return String
   is
   begin
      return " ";
   end Image;


   function Mapping_String_To_Object
     (S : Types.String)
    return Objects.Object_Id
   is
      use PolyORB.Utils.HTTP;
   begin
      return Object_Id (Base64_Decode (To_Standard_String (S)));
   end Mapping_String_To_Object;


   function Mapping_Object_To_String
     (O : Objects.Object_Id)
    return Types.String
   is
      use PolyORB.Utils.HTTP;
   begin
      return To_PolyORB_String (Base64_Encode
       (Stream_Element_Array (O)));
   end Mapping_Object_To_String;



end PolyORB.Binding_Data.SOAP;
