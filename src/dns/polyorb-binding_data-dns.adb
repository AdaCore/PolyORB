------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . B I N D I N G _ D A T A . D N S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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

with Ada.Tags;

with PolyORB.Utils.Sockets;

package body PolyORB.Binding_Data.DNS is

   use PolyORB.Errors;
   use PolyORB.DNS.Transport_Mechanisms;
   use PolyORB.Objects;
   use PolyORB.Types;

   ------------------
   -- Bind_Profile --
   ------------------

   procedure Bind_Profile
     (Profile : access DNS_Profile_Type;
      The_ORB : Components.Component_Access;
      QoS     : PolyORB.QoS.QoS_Parameters;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container)
   is
   begin
      Bind_Mechanism
        (Profile.Mechanism.all, Profile, The_ORB, QoS, BO_Ref, Error);
   end Bind_Profile;

   -----------
   -- Image --
   -----------

   function Image (Prof : DNS_Profile_Type) return String is
   begin
      return Ada.Tags.External_Tag (DNS_Profile_Type'Class (Prof)'Tag)
        & " Address : " & Utils.Sockets.Image (Address_Of (Prof.Mechanism.all))
        & ", Object_Id : " & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   ------------------
   -- Is_Colocated --
   ------------------

   function Is_Colocated
     (Left  : DNS_Profile_Type;
      Right : Profile_Type'Class) return Boolean
   is
   begin
      return
        Right in DNS_Profile_Type'Class
        and then Is_Colocated
                   (Left.Mechanism.all,
                    DNS_Profile_Type (Right).Mechanism.all);
   end Is_Colocated;

   ----------------------
   -- Is_Local_Profile --
   ----------------------

   function Is_Local_Profile
     (PF : access DNS_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean
   is
   begin
      if P.all in DNS_Profile_Type'Class
        and then Is_Local_Mechanism
                   (PF.Mechanism, DNS_Profile_Type (P.all).Mechanism)
      then
         P.Known_Local := True;
         return True;

      else
         return False;
      end if;
   end Is_Local_Profile;

   -------------------------------------
   -- Get_Primary_Transport_Mechanism --
   -------------------------------------

   function Get_Primary_Transport_Mechanism
     (P : DNS_Profile_Type)
      return PolyORB.DNS.Transport_Mechanisms.Transport_Mechanism_Access
   is
   begin
      return P.Mechanism;
   end Get_Primary_Transport_Mechanism;

   ---------------------------------------------
   -- Get_Primary_Transport_Mechanism_Factory --
   ---------------------------------------------

   function Get_Primary_Transport_Mechanism_Factory
     (P : DNS_Profile_Factory)
      return
        PolyORB.DNS.Transport_Mechanisms.Transport_Mechanism_Factory_Access
   is
   begin
      return P.Mechanism;
   end Get_Primary_Transport_Mechanism_Factory;

   -------------
   -- Release --
   -------------

   procedure Release (P : in out DNS_Profile_Type) is
   begin
      Free (P.Object_Id);
      PolyORB.Annotations.Destroy (P.Notepad);
      --  XXX This is a temporary fix
      --  Release_Contents (P.Mechanisms);
   end Release;

end PolyORB.Binding_Data.DNS;
