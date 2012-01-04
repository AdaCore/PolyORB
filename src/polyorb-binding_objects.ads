------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . B I N D I N G _ O B J E C T S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

--  Binding objects: protocol stacks seen globally as a reference-counted
--  entity.

pragma Ada_2005;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Smart_Pointers;
with PolyORB.Transport;
with PolyORB.Utils.Ilists;

package PolyORB.Binding_Objects is

   type Binding_Object is
     new Smart_Pointers.Non_Controlled_Entity with private;
   type Binding_Object_Access is access all Binding_Object'Class;
   --  A protocol session and associated transport and filter stack,
   --  seen globally as a reference-counted entity.

   function Link
     (X     : access Binding_Object'Class;
      Which : Utils.Ilists.Link_Type) return access Binding_Object_Access;
   pragma Inline (Link);
   --  Accessor for chaining pointers in binding objects allowing them to be
   --  attached to the ORB's binding objects list.

   function Get_Component
     (X : Smart_Pointers.Ref) return Components.Component_Access;
   --  Return the top component of the Binding_Object
   --  designated by reference X.

   function Get_Endpoint
     (X : Smart_Pointers.Ref) return Transport.Transport_Endpoint_Access;
   --  Return the transport endpoint of the Binding_Object
   --  designated by reference X.

   function Get_Profile
     (BO : Binding_Object_Access) return Binding_Data.Profile_Access;
   --  Return profile associated with Binding Object BO

   procedure Set_Profile
     (BO : Binding_Object_Access; P : Binding_Data.Profile_Access);
   --  Set the profile associated with Binding Object BO

   procedure Set_Referenced
     (BO         : Binding_Object_Access;
      Referenced : Boolean);
   --  Record that BO is attached to the ORB's BO list

   function Referenced (BO : Binding_Object_Access) return Boolean;
   --  Test whether BO is attached to the ORB's BO list

   procedure Setup_Binding_Object
     (ORB     : Components.Component_Access;
      TE      : Transport.Transport_Endpoint_Access;
      FFC     : Filters.Factory_Array;
      BO_Ref  : out Smart_Pointers.Ref;
      Pro     : Binding_Data.Profile_Access);
   --  Create a binding object associating TE with a protocol stack
   --  instantiated using FFC.

   function Notepad_Of
     (BO : Binding_Object_Access) return Annotations.Notepad_Access;
   --  Returns the notepad of given Binding Object

   function Valid (BO : Binding_Object_Access) return Boolean;
   --  True if BO can be used to forward requests to an object

private
   type Links_Type is
     array (Utils.Ilists.Link_Type) of aliased Binding_Object_Access;

   type Binding_Object is new Smart_Pointers.Non_Controlled_Entity with record
      ORB : Components.Component_Access;
      --  The ORB owning this BO

      Transport_Endpoint : Transport.Transport_Endpoint_Access;
      --  Bottom of the binding object: a transport endpoint

      Top : Filters.Filter_Access;
      --  Top of the binding object: a protocol session

      Profile : Binding_Data.Profile_Access;
      --  The Profile associated with this Binding Object. This profile is
      --  used to determine if the Binding Object can be reused for another
      --  profile.

      Links : aliased Links_Type;
      --  Pointers for chaining of this Binding Object in the
      --  Binding_Objects list of the ORB.
      --  Note: this component must be accessed under the protection of the
      --  critical section of the Referenced_In ORB.

      Referenced : Boolean := False;
      --  True when attached to the ORB's BO list

      Notepad : aliased Annotations.Notepad;
      --  Binding_Object's notepad. The user is responsible for ensuring
      --  proper protection against incorrect concurrent accesses.
   end record;

   procedure Finalize (X : in out Binding_Object);

end PolyORB.Binding_Objects;
