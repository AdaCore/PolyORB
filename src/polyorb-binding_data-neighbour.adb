------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . N E I G H B O U R        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with PolyORB.Binding_Objects;
with PolyORB.ORB;
with PolyORB.Setup;

package body PolyORB.Binding_Data.Neighbour is

   use PolyORB.Objects;

   -------------
   -- Release --
   -------------

   procedure Release (P : in out Neighbour_Profile_Type) is
   begin
      Free (P.Object_Id);
   end Release;

   ------------------------------
   -- Create_Neighbour_Profile --
   ------------------------------

   procedure Create_Neighbour_Profile
     (BO  : Smart_Pointers.Ref;
      Oid : Objects.Object_Id;
      P   : out Neighbour_Profile_Type)
   is
   begin
      P.Target_Binding_Object := BO;
      P.Object_Id := new Object_Id'(Oid);
   end Create_Neighbour_Profile;

   -----------------------
   -- Duplicate_Profile --
   -----------------------

   function Duplicate_Profile
     (P : Neighbour_Profile_Type) return Profile_Access
   is
      Result  : constant Profile_Access := new Neighbour_Profile_Type;
      TResult : Neighbour_Profile_Type renames
                  Neighbour_Profile_Type (Result.all);
   begin
      TResult.Object_Id := new Object_Id'(P.Object_Id.all);
      TResult.Target_Binding_Object := P.Target_Binding_Object;
      return Result;
   end Duplicate_Profile;

   ------------------
   -- Bind_Profile --
   ------------------

   procedure Bind_Profile
     (Profile : access Neighbour_Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container)
   is
      pragma Unreferenced (The_ORB, QoS, Error);

   begin

      --  Always bind to the target binding object

      BO_Ref := Profile.Target_Binding_Object;
      pragma Assert (not Smart_Pointers.Is_Null (BO_Ref));
   end Bind_Profile;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : Neighbour_Profile_Type) return Profile_Tag
   is
      pragma Unreferenced (Profile);
   begin
      return Tag_Neighbour;
   end Get_Profile_Tag;

   ----------------------------
   -- Get_Profile_Preference --
   ----------------------------

   function Get_Profile_Preference
     (Profile : Neighbour_Profile_Type) return Profile_Preference
   is
      pragma Unreferenced (Profile);
   begin
      return Profile_Preference'Last;

      --  A neighbour profile is always preferred to any other. We can return
      --  any value because we never actually bind a neighbour profile.
      --  (Instead we select it in the context of a binding object reuse
      --  operation.)

   end Get_Profile_Preference;

   -----------
   -- Image --
   -----------

   function Image (Prof : Neighbour_Profile_Type) return String is
   begin
      return "Neighbour Profile - Object_Id: "
        & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : Neighbour_Profile_Type)
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
     (Left  : Neighbour_Profile_Type;
      Right : Profile_Type'Class) return Boolean
   is
      use PolyORB.Binding_Objects;
      use PolyORB.Smart_Pointers;

      BO_Acc : constant Binding_Object_Access :=
                 Binding_Object_Access
                   (Entity_Of (Left.Target_Binding_Object));
   begin

      --  The profile of the target binding object is the real profile that was
      --  used when the BO was created. Neighbours profiles will never provoke
      --  the creation of a new BO because by construction they already have
      --  one. Therefore this recursion is safe.

      return Is_Colocated (Get_Profile (BO_Acc).all, Right);
   end Is_Colocated;

end PolyORB.Binding_Data.Neighbour;
