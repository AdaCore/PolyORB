------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . B I N D I N G _ D A T A . S R P              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  Example binding data concrete implementation.

--  $Id$

with PolyORB.Filters;

with PolyORB.Protocols.SRP;
--  The SRP profile is bound to the SRP invocation protocol.

with PolyORB.Transport.Sockets;
--  The TEST profile denotes an SRP protocol stack instanciated
--  over a TCP socket.

package body PolyORB.Binding_Data.SRP is

   use PolyORB.Objects;
   use PolyORB.Sockets;
   use PolyORB.Transport.Sockets;

   procedure Initialize (P : in out SRP_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   procedure Adjust (P : in out SRP_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   procedure Finalize (P : in out SRP_Profile_Type) is
   begin
      Free (P.Object_Id);
   end Finalize;

   function Get_Object_Key
     (Profile : SRP_Profile_Type)
     return Objects.Object_Id is
   begin
      return Profile.Object_Id.all;
   end Get_Object_Key;

   procedure Bind_Profile
     (Profile : SRP_Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Session : out Components.Component_Access)
   is
      use PolyORB.Protocols.SRP;
      use PolyORB.Sockets;
      use PolyORB.Transport.Sockets;

      S : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      P : aliased SRP_Protocol;

   begin
      Create_Socket (S);
      Connect_Socket (S, Remote_Addr);
      TE := new Transport.Sockets.Socket_Endpoint;
      Create (Socket_Endpoint (TE.all), S);
      Create (P'Access, Filters.Filter_Access (Session));
   end Bind_Profile;

   function Get_Profile_Tag
     (Profile : SRP_Profile_Type)
     return Profile_Tag is
   begin
      return Tag_SRP;
   end Get_Profile_Tag;

   function Get_Profile_Preference
     (Profile : SRP_Profile_Type)
     return Profile_Preference is
   begin
      return Preference_Default;
   end Get_Profile_Preference;

   procedure Create_Factory
     (PF : out SRP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access) is
   begin
      PF.Address := Address_Of (Socket_Access_Point (TAP.all));
   end Create_Factory;

   function Create_Profile
     (PF  : access SRP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access
   is
      Result : constant Profile_Access
        := new SRP_Profile_Type;

      TResult : SRP_Profile_Type
        renames SRP_Profile_Type (Result.all);
   begin
      TResult.Object_Id := new Object_Id'(Oid);
      TResult.Address   := PF.Address;
      return  Result;
   end Create_Profile;

   function Is_Local_Profile
     (PF : access SRP_Profile_Factory;
      P : Profile_Access) return Boolean is
   begin
      return P.all in SRP_Profile_Type
        and then SRP_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   function Image (Prof : SRP_Profile_Type) return String is
   begin
      return "Address : " & Image (Prof.Address) &
        ", Object_Id : " & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

end PolyORB.Binding_Data.SRP;
