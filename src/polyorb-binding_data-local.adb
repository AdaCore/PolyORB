------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . B I N D I N G _ D A T A . L O C A L            --
--                                                                          --
--                                 B o d y                                  --
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

pragma Ada_2012;

package body PolyORB.Binding_Data.Local is

   use PolyORB.Objects;

   -------------
   -- Release --
   -------------

   overriding procedure Release (P : in out Local_Profile_Type)
   is
   begin
      Free (P.Object_Id);
   end Release;

   --------------------------
   -- Create_Local_Profile --
   --------------------------

   procedure Create_Local_Profile
     (Oid : Objects.Object_Id;
      P   : out Local_Profile_Type) is
   begin
      P.Object_Id := new Object_Id'(Oid);
      P.Known_Local := True;
      pragma Assert (P.Object_Id /= null);
   end Create_Local_Profile;

   ------------------
   -- Is_Colocated --
   ------------------

   overriding function Is_Colocated
     (Left  : Local_Profile_Type;
      Right : Profile_Type'Class) return Boolean
   is
      pragma Unreferenced (Left);
   begin
      return Right in Local_Profile_Type'Class;
   end Is_Colocated;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   overriding function Duplicate_Profile
     (P : Local_Profile_Type)
     return Profile_Access
   is
      Result : constant Profile_Access := new Local_Profile_Type;
      TResult : Local_Profile_Type renames Local_Profile_Type (Result.all);

   begin
      TResult.Object_Id := new Object_Id'(P.Object_Id.all);
      TResult.Known_Local := True;
      return Result;
   end Duplicate_Profile;

   ------------------
   -- Bind_Profile --
   -------------------

   overriding procedure Bind_Profile
     (Profile : access Local_Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container)
   is
      pragma Unreferenced (Profile, The_ORB, QoS, BO_Ref, Error);

   begin
      raise Program_Error;
      --  May not happen currently, because the local case
      --  is handled specially in PolyORB.References.Bind,
      --  but could be implemented as (mostly):

      --  Servant := Components.Component_Access
      --    (Find_Servant
      --     (Object_Adapter (Local_ORB), Profile.Object_Id));
      --  Set (BO_Ref, Servant_To_Binding_Object (Servant));
   end Bind_Profile;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   overriding function Get_Profile_Tag
     (Profile : Local_Profile_Type)
     return Profile_Tag
   is
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);

   begin
      return Tag_Local;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   overriding function Get_Profile_Preference
     (Profile : Local_Profile_Type)
     return Profile_Preference
   is
      pragma Warnings (Off);
      pragma Unreferenced (Profile);
      pragma Warnings (On);

   begin
      return Profile_Preference'Last;
      --  A local profile is always preferred to any other.

   end Get_Profile_Preference;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Prof : Local_Profile_Type)
     return String is
   begin
      return "Object_Id: " & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

end PolyORB.Binding_Data.Local;
