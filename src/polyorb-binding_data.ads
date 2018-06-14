------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . B I N D I N G _ D A T A                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
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

--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

with PolyORB.Annotations;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Objects;
with PolyORB.QoS;
with PolyORB.Smart_Pointers;
pragma Elaborate_All (PolyORB.Smart_Pointers);
with PolyORB.Transport;
with PolyORB.Types;

package PolyORB.Binding_Data is

   ----------------------------------------------
   -- Abstract inter-ORB protocol profile type --
   ----------------------------------------------

   type Profile_Type is abstract tagged limited private;
   type Profile_Access is access all Profile_Type'Class;
   --  A profile is an element of information that contains:
   --    - a profile tag identifying a communication system and a
   --      method invocation protocol stack;
   --    - an unambiguous name of one TSAP within the communication
   --      system;
   --    - a key that identifies one object among all those accessible
   --      on that TSAP;
   --    - a priority, locally assigned, that denotes the preferrence
   --      expressed by the user for the choice of a profile type
   --      among a set of profiles.

   procedure Release (P : in out Profile_Type) is abstract;
   --  Free profile data

   subtype Profile_Tag is Types.Unsigned_Long;

   Tag_Internet_IOP        : constant Profile_Tag;
   Tag_UIPMC               : constant Profile_Tag;
   Tag_Multiple_Components : constant Profile_Tag;
   Tag_Local               : constant Profile_Tag;
   Tag_SRP                 : constant Profile_Tag;
   Tag_SOAP                : constant Profile_Tag;
   Tag_DIOP                : constant Profile_Tag;
   Tag_Neighbour           : constant Profile_Tag;
   Tag_UDNS                : constant Profile_Tag;
   Tag_MDNS                : constant Profile_Tag;

   Tag_Test                : constant Profile_Tag;
   type Profile_Preference is new Integer range 0 .. Integer'Last;
   --  Profile_Preference'First means "unsupported profile type".

   Preference_Default : constant Profile_Preference;
   --  Default value for profile preference.

   function Get_OA
     (Profile : Profile_Type) return PolyORB.Smart_Pointers.Entity_Ptr;
   --  For object group profiles, return the group object adapter that controls
   --  Profile's OID. For other profiles, return null.

   function Get_Object_Key
     (Profile : Profile_Type) return Objects.Object_Id_Access;
   --  Retrieve the opaque object key from Profile.

   procedure Bind_Profile
     (Profile : access Profile_Type;
      The_ORB :        Components.Component_Access;
      QoS     :        PolyORB.QoS.QoS_Parameters;
      BO_Ref  :    out Smart_Pointers.Ref;
      Error   :    out Errors.Error_Container)
      is abstract;
   --  Retrieve a transport endpoint and an attached protocol stack instance
   --  (or create new ones) that match this profile, in order to send a message
   --  to the middleware that hosts the designated object. The Filter at the
   --  top of the protocol stack (i.e. the Session) is returned. Concrete
   --  implementations are responsible for registering the TE with the ORB if
   --  necessary.

   function Get_Profile_Tag (Profile : Profile_Type) return Profile_Tag
      is abstract;
   pragma Inline (Get_Profile_Tag);
   --  Return the profile tag associated with this profile type

   function Get_Profile_Preference (Profile : Profile_Type)
     return Profile_Preference
      is abstract;
   pragma Inline (Get_Profile_Preference);
   --  Return the profile priority associated with this profile type

   type Profile_Factory is abstract tagged limited private;
   type Profile_Factory_Access is access all Profile_Factory'Class;

   function Create_Factory
     (TAP : not null access Transport.Transport_Access_Point'Class)
      return Profile_Factory is abstract;
   --  Initialize PF to act as profile factory for transport access point TAP

   function Create_Profile
     (PF  : access Profile_Factory;
      Oid : Objects.Object_Id) return Profile_Access
      is abstract;
   --  Create a profile of the type determined by PF, using Oid as the object
   --  specification.

   function Duplicate_Profile (P : Profile_Type) return Profile_Access
      is abstract;
   --  Return a copy of the user-provided data used to build P, it
   --  does not duplicate any internal structure.

   procedure Destroy_Profile (P : in out Profile_Access);
   pragma Inline (Destroy_Profile);

   function Is_Multicast_Profile (P : Profile_Type) return Boolean;
   --  True if this profile designates a group of objects that may exist on
   --  different nodes. False by default, overridden for group profiles.

   function Is_Local_Profile
     (PF : access Profile_Factory;
      P  : not null access Profile_Type'Class) return Boolean is abstract;
   --  True iff P designates an object that can be contacted at the access
   --  point associated with PF.

   function Is_Local_Profile (P : Profile_Type'Class) return Boolean;
   --  True if a previous call to Is_Local_Profile (two-argument version)
   --  has previously returned True (optimization, used to avoid traversing
   --  the list of all profile factories again).

   function Image (Prof : Profile_Type) return String is abstract;
   --  Used for debugging purposes

   function Is_Colocated
     (Left  : Profile_Type;
      Right : Profile_Type'Class) return Boolean is abstract;
   --  True if, knowing Left, we determine that Right (a profile of any type)
   --  designates an object that resides on the same node.

   function Same_Node (Left, Right : Profile_Type'Class) return Boolean;
   --  True if we can determine that Left and Right are profiles
   --  targetting the same node.

   function Same_Object_Key (Left, Right  : Profile_Type'Class) return Boolean;
   --  True if Left and Right have the same object key. Note that some profile
   --  types (e.g. Multiple_Components) have null object keys, in which case
   --  this function cannot match and returns False.

   procedure Set_Continuation
     (Prof         : access Profile_Type;
      Continuation :        PolyORB.Smart_Pointers.Ref);
   --  Associate profile Profile (a profile designating an object on the local
   --  ORB) with the designated object as its actual Continuation. Used for
   --  proxy profiles (which are actually indirect pointers to remote objects).

   function Notepad_Of
     (Prof : access Profile_Type)
      return Annotations.Notepad_Access;

private

   use type Types.Unsigned_Long;

   --  Standard tags defined by CORBA

   Tag_Internet_IOP        : constant Profile_Tag := 0;
   Tag_Multiple_Components : constant Profile_Tag := 1;
   Tag_UIPMC               : constant Profile_Tag := 3;
   --  TAO value :
   --  Tag_UIPMC               : constant Profile_Tag := 1413566220;

   --  Tags defined by PolyORB (see docs/OMG_TAGS for assigned ranges)

   Tag_PolyORB_First       : constant Profile_Tag := 16#504f0000#;
   --  "PO\x00\x00"

   Tag_Local               : constant Profile_Tag := Tag_PolyORB_First + 0;
   Tag_SRP                 : constant Profile_Tag := Tag_PolyORB_First + 1;
   Tag_SOAP                : constant Profile_Tag := Tag_PolyORB_First + 2;
   Tag_DIOP                : constant Profile_Tag := Tag_PolyORB_First + 3;
   Tag_Neighbour           : constant Profile_Tag := Tag_PolyORB_First + 4;
   Tag_UDNS                : constant Profile_Tag := Tag_PolyORB_First + 5;
   Tag_MDNS                : constant Profile_Tag := Tag_PolyORB_First + 6;

   Tag_Test                : constant Profile_Tag := Tag_PolyORB_First + 255;

   Tag_PolyORB_Last        : constant Profile_Tag := 16#504f00ff#;
   --  "PO\x00\xff"

   Preference_Default : constant Profile_Preference :=
                          (Profile_Preference'First
                         + Profile_Preference'Last) / 2;

   type Profile_Type is abstract tagged limited record
      Object_Id    : Objects.Object_Id_Access;
      --  The object identifier for this object, relative to a node's
      --  name space.

      Notepad      : aliased PolyORB.Annotations.Notepad;
      --  Profile's notepad. It is the user's responsibility to protect this
      --  component against invalid concurrent accesses.

      Continuation : PolyORB.Smart_Pointers.Ref;
      --  If the profile has been bound, this component designates its
      --  continuation (which is either a local servant, or a binding object).

      Known_Local : Boolean := False;
      --  Set True by Is_Local_Profile when it is determined that this profile
      --  is local.

   end record;

   type Profile_Factory is abstract tagged limited null record;

end PolyORB.Binding_Data;
