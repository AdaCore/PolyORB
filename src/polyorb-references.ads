------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . R E F E R E N C E S                    --
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

--  Object references.

--  $Id$

with Ada.Unchecked_Deallocation;
with PolyORB.Sequences.Unbounded;

with PolyORB.Binding_Data; use PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package PolyORB.References is

   pragma Elaborate_Body;

   package Profile_Seqs is
      new PolyORB.Sequences.Unbounded (Binding_Data.Profile_Access);
   subtype Profile_Array is Profile_Seqs.Element_Array;

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;
   --  An object reference of any kind.

   procedure Create_Reference
     (Profiles : Profile_Array;
      Type_Id  : String;
      R        : out Ref);
   --  Create a reference with Profiles as its profiles.
   --  The returned ref R is nil iff Profiles'Length = 0.

   function Profiles_Of (R : Ref) return Profile_Array;
   --  Return the list of profiles constituting Ref.

   function Type_Id_Of  (R : Ref) return String;
   --  Return the type identifier of Ref.

   --  function Is_Nil (R : Ref) return Boolean;
   --  True iff R is a Nil reference, i.e. a reference that
   --  does not designate any object.

   procedure Get_Binding_Info
     (R   :     Ref;
      BOC : out Components.Component_Access;
      Pro : out Binding_Data.Profile_Access);
   --  Retrieve the binding object associated with R, if R is bound.
   --  Otherwise, return null.

   procedure Set_Binding_Info
     (R   : Ref;
      BOC : Components.Component_Access;
      Pro : Binding_Data.Profile_Access);
   --  Set BOC to be the binding object associated with R.
   --  R must not be already bound.

   procedure Share_Binding_Info
     (Dest   : Ref;
      Source : Ref);

   function Image (R : Ref) return String;
   --  For debugging purposes.

   type Ref_Ptr is access all Ref;
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Ref, Ref_Ptr);

private

   subtype Profile_Seq is Profile_Seqs.Sequence;

   type Reference_Info is new PolyORB.Smart_Pointers.Entity with
      record
         Type_Id  : Utils.Strings.String_Ptr;
         Profiles : Profile_Seq;
         --  The collection of tagged profiles that designate
         --  transport access points where this object can be
         --  contacted, together with the object ids to be used.

         --  A reference constitutes a surrogate for an object.
         --  When the surrogate is free, it is not linked
         --  to a binding object, and this component is null.
         --  When the profile (and thus the surrogate) is bound,
         --  this component denotes the associated binding object
         --  on the local ORB (= the Session).

         Binding_Object_Ref : Smart_Pointers.Ref;
         Binding_Object_Profile : Binding_Data.Profile_Access;
         --  If a reference is already bound, the Binding_Object_Ref
         --  will designate the component that serves as the
         --  binding object, and Binding_Object_Profile will be
         --  the profile (among Profiles above) associated with
         --  the protocol of that component.
         --  The profile is stored here, in the reference, rather
         --  than in the designated Binding_Object to allow sharing
         --  of Binding_Objects among references to different objects
         --  residing on the same node.
      end record;

   procedure Finalize (RI : in out Reference_Info);

end PolyORB.References;
