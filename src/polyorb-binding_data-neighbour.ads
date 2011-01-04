------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . N E I G H B O U R        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2011, Free Software Foundation, Inc.          --
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

--  Neighbour profiles are used to reach partitions for which a local client
--  binding object linking to them exists, which we call the target binding
--  object. Neighbour profiles allow us to make requests to remote object in
--  one of these partitions using this target binding object and an Object_Id.
--  Per construction, neighbour profiles always bind to their target BO.

with PolyORB.Objects;

package PolyORB.Binding_Data.Neighbour is

   pragma Elaborate_Body;

   type Neighbour_Profile_Type is new Profile_Type with private;

   procedure Create_Neighbour_Profile
     (BO  : Smart_Pointers.Ref;
      Oid : Objects.Object_Id;
      P   : out Neighbour_Profile_Type);
   --  Create a neighbour profile: BO is the target binding object, and Oid the
   --  id of the object this profile designates.

   ---------------------------------------------
   -- Overridden primitives from Profile_Type --
   ---------------------------------------------

   procedure Release (P : in out Neighbour_Profile_Type);

   function Duplicate_Profile
     (P : Neighbour_Profile_Type) return Profile_Access;

   procedure Bind_Profile
     (Profile : access Neighbour_Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container);

   function Get_Profile_Tag
     (Profile : Neighbour_Profile_Type) return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : Neighbour_Profile_Type) return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   function Image (Prof : Neighbour_Profile_Type) return String;

   function Is_Colocated
     (Left  : Neighbour_Profile_Type;
      Right : Profile_Type'Class) return Boolean;

private

   type Neighbour_Profile_Type is new Profile_Type with
      record
         Target_Binding_Object : Smart_Pointers.Ref;
         --  The binding object used to create the profile. This BO links to
         --  the partition the profile designates. The profile always bind to
         --  this BO.
      end record;

end PolyORB.Binding_Data.Neighbour;
