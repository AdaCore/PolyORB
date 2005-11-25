------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . R E F E R E N C E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  References on object exported by the ORB.

with Ada.Unchecked_Deallocation;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package PolyORB.References is

   type Profile_Array is array (Integer range <>) of
     Binding_Data.Profile_Access;

   type Profile_Array_Access is access all Profile_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Profile_Array, Profile_Array_Access);

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;
   --  An object reference of any kind.

   Nil_Ref : constant Ref;
   --  Nil reference.

   function Is_Exported_Reference (The_Ref : in Ref) return Boolean;
   --  True iff The_Ref is a non null reference on an object
   --  exported by the ORB.

   function Is_Same_Object (Left, Right : Ref) return Boolean;
   --  True iff it is determined that Left Right designate the
   --  same object.

   procedure Create_Reference
     (Profiles :     Profile_Array;
      Type_Id  :     String;
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

   function Image (R : Ref) return String;
   --  For debugging purposes.

   procedure String_To_Object (Str : String; The_Ref : out Ref);
   --  Note: String_To_Object must be a procedure so it need not
   --  be overridden when Ref is derived.

   ----------------------------
   -- Annotations management --
   ----------------------------

   function Notepad_Of
     (R : in Ref)
     return Annotations.Notepad_Access;

   type Ref_Ptr is access all Ref;
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Ref, Ref_Ptr);

private

   procedure Get_Binding_Info
     (R   :     Ref'Class;
      BOC : out Components.Component_Access;
      Pro : out Binding_Data.Profile_Access);
   --  Retrieve the binding object associated with R, if R is bound.
   --  Otherwise, return null.

   procedure Share_Binding_Info
     (Dest   : Ref'Class;
      Source : Ref'Class);

   Nil_Ref : constant Ref := (PolyORB.Smart_Pointers.Ref with null record);

   type Reference_Info is
     new PolyORB.Smart_Pointers.Non_Controlled_Entity
     with record
         Type_Id  : Utils.Strings.String_Ptr;
         Profiles : Profile_Array_Access;
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

         Notepad : aliased Annotations.Notepad;
         --  Reference_Info's notepad. The user must ensure there is
         --  no race condition when accessing it.
     end record;
   type Reference_Info_Access is access all Reference_Info'Class;

   function Ref_Info_Of (R : Ref'Class) return Reference_Info_Access;
   --  Obtain the object reference information from R.

   --  When an object reference is bound (i.e. associated at
   --  runtime with a transport service endpoint and a messaging
   --  protocol stack), it becomes associated with a Binding_Object
   --  which will remain in existence until all references to
   --  the object have been finalized (at which time the transport
   --  connection and protocol stack will be torn down, as a
   --  result of finalizing the binding object).

   procedure Finalize (RI : in out Reference_Info);

   --  Note that Reference_Info must not be an Entity, because the
   --  Finalize operation would then be called *after* (not *before*)
   --  the controlled components of Reference_Info (including
   --  Profiles and Binding_Object_Ref) have been finalized.

   --  XXX the following declarations must be documented.

   type String_To_Object_Func is
     access function (Str : String) return Ref;

   procedure Register_String_To_Object
     (Prefix : String;
      Func   : String_To_Object_Func);

end PolyORB.References;
