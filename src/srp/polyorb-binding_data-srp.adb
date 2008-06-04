------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . B I N D I N G _ D A T A . S R P              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

--  Binding data for the Simple Request Protocol over TCP.

with PolyORB.Binding_Objects;
with PolyORB.Filters;
with PolyORB.ORB;
with PolyORB.Protocols.SRP;
with PolyORB.Setup;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Sockets;

package body PolyORB.Binding_Data.SRP is

   use PolyORB.Objects;
   use PolyORB.Sockets;
   use PolyORB.Transport;
   use PolyORB.Transport.Connected.Sockets;

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

   procedure Release (P : in out SRP_Profile_Type)
   is
   begin
      Free (P.Object_Id);
   end Release;

   Pro : aliased Protocols.SRP.SRP_Protocol;
   SRP_Factories : constant Filters.Factory_Array
     := (0 => Pro'Access);

   ------------------
   -- Bind_Profile --
   ------------------

   procedure Bind_Profile
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
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      TE : constant Transport_Endpoint_Access := new Socket_Endpoint;
   begin
      Create_Socket (S);
      Utils.Sockets.Connect_Socket (S, Remote_Addr);
      Create (Socket_Endpoint (TE.all), S);

      --  Create (P'Access, Filters.Filter_Access (Session));

      Binding_Objects.Setup_Binding_Object
        (TE,
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

   function Get_Profile_Tag
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

   function Get_Profile_Preference
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

   --------------------
   -- Create_Profile --
   --------------------

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

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   function Duplicate_Profile
     (P : SRP_Profile_Type)
     return Profile_Access
   is
      Result : constant Profile_Access := new SRP_Profile_Type;

      TResult : SRP_Profile_Type
        renames SRP_Profile_Type (Result.all);

      PP : SRP_Profile_Type renames P;

   begin
      TResult.Object_Id := new Object_Id'(PP.Object_Id.all);
      TResult.Address   := PP.Address;

      return Result;
   end Duplicate_Profile;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile
     (PF : access SRP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean is
   begin
      return P.all in SRP_Profile_Type
        and then SRP_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   -----------
   -- Image --
   -----------

   function Image (Prof : SRP_Profile_Type) return String is
   begin
      return "Address : " & Image (Prof.Address) &
        ", Object_Id : " & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : SRP_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Unreferenced (Profile);
   begin
      return PolyORB.Smart_Pointers.Entity_Ptr
        (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB));
   end Get_OA;

   ------------------
   -- Is_Colocated --
   ------------------

   function Is_Colocated
     (Left  : SRP_Profile_Type;
      Right : Profile_Type'Class) return Boolean
   is
      use PolyORB.Types;
   begin
      return Right in SRP_Profile_Type'Class
        and then Left.Address = SRP_Profile_Type (Right).Address;
   end Is_Colocated;

end PolyORB.Binding_Data.SRP;
