------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . B I N D I N G _ D A T A . S R P              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2004 Free Software Foundation, Inc.          --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Binding data for the Simple Request Protocol over TCP.

--  $Id$

with PolyORB.Binding_Objects;
with PolyORB.Filters;
with PolyORB.ORB;
with PolyORB.Protocols.SRP;
with PolyORB.Setup;
with PolyORB.Transport.Connected.Sockets;

package body PolyORB.Binding_Data.SRP is

   use PolyORB.Objects;
   use PolyORB.Sockets;
   use PolyORB.Transport;
   use PolyORB.Transport.Connected.Sockets;

   procedure Duplicate
     (P1 : SRP_Profile_Type; P2 : out SRP_Profile_Type) is
   begin
      P2.Continuation := P1.Continuation;
      if P1.Object_Id /= null then
         P2.Object_Id := new Object_Id'(P1.Object_Id.all);
      else
         P2.Object_Id := null;
      end if;
   end Duplicate;

   procedure Release (P : in out SRP_Profile_Type)
   is
   begin
      Free (P.Object_Id);
   end Release;

   Pro : aliased Protocols.SRP.SRP_Protocol;
   SRP_Factories : constant Filters.Factory_Array
     := (0 => Pro'Access);

   procedure Bind_Profile
     (Profile :     SRP_Profile_Type;
      The_ORB :     Components.Component_Access;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Exceptions.Error_Container)
   is
      use PolyORB.Components;
      use PolyORB.Exceptions;
      use PolyORB.ORB;
      use PolyORB.Sockets;

      S : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      TE : constant Transport_Endpoint_Access
        := new Socket_Endpoint;
   begin
      Create_Socket (S);
      Connect_Socket (S, Remote_Addr);
      Create (Socket_Endpoint (TE.all), S);
      Set_Allocation_Class (TE.all, Dynamic);

      --  Create (P'Access, Filters.Filter_Access (Session));

      Binding_Objects.Setup_Binding_Object
        (ORB.ORB_Access (The_ORB),
         TE,
         SRP_Factories,
         ORB.Client,
         BO_Ref);

   exception
      when Sockets.Socket_Error =>
         Throw (Error, Comm_Failure_E, System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
   end Bind_Profile;

   function Get_Profile_Tag
     (Profile : SRP_Profile_Type)
     return Profile_Tag is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);
      return Tag_SRP;
   end Get_Profile_Tag;

   function Get_Profile_Preference
     (Profile : SRP_Profile_Type)
     return Profile_Preference is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);
      return Preference_Default;
   end Get_Profile_Preference;

   procedure Create_Factory
     (PF : out SRP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      PF.Address := Address_Of (Socket_Access_Point (TAP.all));
   end Create_Factory;

   function Create_Profile
     (PF  : access SRP_Profile_Factory;
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
      P  : access Profile_Type'Class)
      return Boolean is
   begin
      return P.all in SRP_Profile_Type
        and then SRP_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   function Image (Prof : SRP_Profile_Type) return String is
   begin
      return "Address : " & Image (Prof.Address) &
        ", Object_Id : " & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   function Get_OA
     (Profile : SRP_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Profile);
      pragma Warnings (On); --  WAG:3.15
   begin
      return PolyORB.Smart_Pointers.Entity_Ptr
        (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB));
   end Get_OA;

end PolyORB.Binding_Data.SRP;
