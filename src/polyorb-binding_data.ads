------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . B I N D I N G _ D A T A                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

--  $Id: //droopi/main/src/polyorb-binding_data.ads#9 $

with Ada.Finalization;

with PolyORB.Components;
with PolyORB.Objects;
with PolyORB.Smart_Pointers;
with PolyORB.Transport;
with PolyORB.Types;

package PolyORB.Binding_Data is

   pragma Elaborate_Body;

   ----------------------------------------------
   -- Abstract inter-ORB protocol profile type --
   ----------------------------------------------

   type Profile_Type is abstract
     new Ada.Finalization.Limited_Controlled with private;
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

   --  subtype Profile_Tag is CORBA.Unsigned_Long;
   subtype Profile_Tag is Types.Unsigned_Long;

   Tag_Internet_IOP        : constant Profile_Tag;
   Tag_Multiple_Components : constant Profile_Tag;
   Tag_Local               : constant Profile_Tag;
   Tag_SRP                 : constant Profile_Tag;
   Tag_SOAP                : constant Profile_Tag;
   Tag_Test                : constant Profile_Tag;

   type Profile_Preference is new Integer range 0 .. Integer'Last;
   --  Profile_Preference'First means "unsupported profile type"

   Preference_Default : constant Profile_Preference;
   --  Default value for profile preference.

   function Get_Object_Key
     (Profile : Profile_Type)
     return Objects.Object_Id_Access;
   --  Retrieve the opaque object key from Profile.

   procedure Bind_Non_Local_Profile
     (Profile : Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Filter  : out Components.Component_Access)
      is abstract;
   --  Find or create a transport endpoint and an attached protocol
   --  stack instance (or create new ones) that match this profile,
   --  in order to send a message to the designated middleware.
   --  The Transport_Endpoint at the bottom of the transport stack
   --  and the Filter just above (ie the base of the protocol stack).

   function Get_Profile_Tag
     (Profile : Profile_Type)
     return Profile_Tag
      is abstract;
   pragma Inline (Get_Profile_Tag);
   --  Return the profile tag associated with this profile type.

   function Get_Profile_Preference
     (Profile : Profile_Type)
     return Profile_Preference
      is abstract;
   pragma Inline (Get_Profile_Preference);
   --  Return the profile priority associated with this profile type.

   type Profile_Factory is abstract tagged limited private;
   type Profile_Factory_Access is access all Profile_Factory'Class;

   procedure Create_Factory
     (PF : out Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access)
      is abstract;
   --  Initialize PF to act as profile factory for transport
   --  access point TAP managed by ORB.

   function Create_Profile
     (PF  : access Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access
      is abstract;
   --  Create a profile of the type determined by PF, using
   --  the address of TAP, and Oid as the object specification.

   procedure Destroy_Profile (P : in out Profile_Access);
   pragma Inline (Destroy_Profile);

   function Is_Local_Profile
     (PF : access Profile_Factory;
      P  : access Profile_Type'Class)
     return Boolean is abstract;
   --  True iff P designates an object that can be contacted
   --  at the access point associated with PF.

   function Image (Prof : Profile_Type) return String is abstract;
   --  Used for debugging purposes

   procedure Set_Continuation
     (Prof         : access Profile_Type;
      Continuation :        PolyORB.Smart_Pointers.Ref);
   --  Associate profile Profile (a profile designating an object
   --  on the local ORB) with the designated object as its actual
   --  Continuation. Used for proxy profiles (which are actually
   --  indirect pointers to remote objects).

private

   --  Standard tags defined by CORBA

   Tag_Internet_IOP        : constant Profile_Tag := 0;
   Tag_Multiple_Components : constant Profile_Tag := 1;

   --  Tags defined by PolyORB

   Tag_Local               : constant Profile_Tag := 16#7fffff00#;
   Tag_SRP                 : constant Profile_Tag := 16#7fffff02#;
   Tag_SOAP                : constant Profile_Tag := 16#7fffff03#;
   Tag_Test                : constant Profile_Tag := 16#7fffff0f#;

   Preference_Default : constant Profile_Preference
     := (Profile_Preference'First + Profile_Preference'Last) / 2;

   type Profile_Type is
     abstract new Ada.Finalization.Limited_Controlled with record
        Object_Id    : Objects.Object_Id_Access;
        Continuation : PolyORB.Smart_Pointers.Ref;
     end record;

   type Profile_Factory is abstract tagged limited null record;

end PolyORB.Binding_Data;
