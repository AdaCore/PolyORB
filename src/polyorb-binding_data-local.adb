------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . B I N D I N G _ D A T A . L O C A L            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Contact information for an object that exists within the local ORB.

with PolyORB.ORB;
with PolyORB.Setup;

package body PolyORB.Binding_Data.Local is

   use PolyORB.Objects;

   -------------
   -- Release --
   -------------

   procedure Release (P : in out Local_Profile_Type)
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
      pragma Assert (P.Object_Id /= null);
   end Create_Local_Profile;

   ------------------
   -- Bind_Profile --
   -------------------

   procedure Bind_Profile
     (Profile :     Local_Profile_Type;
      The_ORB :     Components.Component_Access;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container)
   is
      pragma Warnings (Off); -- WAG:3.15
      pragma Unreferenced (Profile, The_ORB, BO_Ref, Error);
      pragma Warnings (On); -- WAG:3.15

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

   function Get_Profile_Tag
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

   function Get_Profile_Preference
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

   function Image
     (Prof : Local_Profile_Type)
     return String is
   begin
      return "Object_Id: " & PolyORB.Objects.Image (Prof.Object_Id.all);
   end Image;

   ------------
   -- Get_OA --
   ------------

   function Get_OA
     (Profile : Local_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Profile);
      pragma Warnings (On); --  WAG:3.15
   begin
      return PolyORB.Smart_Pointers.Entity_Ptr
        (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB));
   end Get_OA;

end PolyORB.Binding_Data.Local;
