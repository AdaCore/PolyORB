------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . B I N D I N G _ D A T A . S R P              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2013, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Objects;
with PolyORB.Filters;
with PolyORB.ORB;
with PolyORB.Protocols.SRP;
with PolyORB.Sockets;
with PolyORB.Transport.Connected.Sockets;

package body PolyORB.Binding_Data.SRP is

   use PolyORB.Objects;
   use PolyORB.Sockets;
   use PolyORB.Transport;
   use PolyORB.Transport.Connected.Sockets;
   use PolyORB.Utils.Sockets;

   ---------------
   -- Duplicate --
   ---------------

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

   -------------
   -- Release --
   -------------

   overriding procedure Release (P : in out SRP_Profile_Type) is
   begin
      Free (P.Address);
      Free (P.Object_Id);
   end Release;

   Pro : aliased Protocols.SRP.SRP_Protocol;
   SRP_Factories : constant Filters.Factory_Array
     := (0 => Pro'Access);

   ------------------
   -- Bind_Profile --
   ------------------

   overriding procedure Bind_Profile
     (Profile : access SRP_Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container)
   is
      pragma Unreferenced (QoS);

      use PolyORB.Binding_Objects;
      use PolyORB.Components;
      use PolyORB.Errors;
      use PolyORB.ORB;

      S : Socket_Type;
      TE : constant Transport_Endpoint_Access := new Socket_Endpoint;
   begin
      Utils.Sockets.Create_Socket (S);
      Utils.Sockets.Connect_Socket (S, Profile.Address.all);
      Create (Socket_Endpoint (TE.all), S);

      --  Create (P'Access, Filters.Filter_Access (Session));

      Binding_Objects.Setup_Binding_Object
        (The_ORB,
         TE,
         SRP_Factories,
         BO_Ref,
         Profile_Access (Profile));

      ORB.Register_Binding_Object
        (ORB.ORB_Access (The_ORB),
         BO_Ref,
         ORB.Client);

   exception
      when Sockets.Socket_Error =>
         Throw (Error, Comm_Failure_E,
                System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
   end Bind_Profile;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   overriding function Get_Profile_Tag
     (Profile : SRP_Profile_Type)
     return Profile_Tag is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);
      return Tag_SRP;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   overriding function Get_Profile_Preference
     (Profile : SRP_Profile_Type)
     return Profile_Preference is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);
      return Preference_Default;
   end Get_Profile_Preference;

   --------------------
   -- Create_Factory --
   --------------------

   overriding procedure Create_Factory
     (PF : out SRP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      PF.Address :=
        new Socket_Name'(Address_Of (Socket_Access_Point (TAP.all)));
   end Create_Factory;

   --------------------
   -- Create_Profile --
   --------------------

   overriding function Create_Profile
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
      TResult.Address   := new Socket_Name'(PF.Address.all);
      return  Result;
   end Create_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   overriding function Duplicate_Profile
     (P : SRP_Profile_Type)
     return Profile_Access
   is
      Result : constant Profile_Access := new SRP_Profile_Type;

      TResult : SRP_Profile_Type
        renames SRP_Profile_Type (Result.all);

      PP : SRP_Profile_Type renames P;

   begin
      TResult.Object_Id := new Object_Id'(PP.Object_Id.all);
      TResult.Address   := new Socket_Name'(PP.Address.all);

      return Result;
   end Duplicate_Profile;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   overriding function Is_Local_Profile
     (PF : access SRP_Profile_Factory;
      P  : not null access Profile_Type'Class)
      return Boolean is
   begin
      if P.all in SRP_Profile_Type
        and then SRP_Profile_Type (P.all).Address = PF.Address
      then
         P.Known_Local := True;
         return True;
      end if;
      return False;
   end Is_Local_Profile;

   -----------
   -- Image --
   -----------

   overriding function Image (Prof : SRP_Profile_Type) return String is
   begin
      return "Address : " & Image (Prof.Address.all) &
        ", Object_Id : " & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   ------------------
   -- Is_Colocated --
   ------------------

   overriding function Is_Colocated
     (Left  : SRP_Profile_Type;
      Right : Profile_Type'Class) return Boolean
   is
      use PolyORB.Types;
   begin
      return Right in SRP_Profile_Type'Class
        and then Left.Address = SRP_Profile_Type (Right).Address;
   end Is_Colocated;

end PolyORB.Binding_Data.SRP;
