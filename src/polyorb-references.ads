------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . R E F E R E N C E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2014, Free Software Foundation, Inc.          --
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

--  References on object exported by the ORB.

pragma Ada_2012;

with Ada.Streams;
with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Binding_Objects;
with PolyORB.QoS;
with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

package PolyORB.References is

   type Profile_Array is array (Integer range <>) of
     Binding_Data.Profile_Access;

   type Profile_Array_Access is access all Profile_Array;
   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Profile_Array, Name => Profile_Array_Access);

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;
   --  An object reference of any kind.

   Nil_Ref : constant Ref;
   --  Nil reference

   function Is_Exported_Reference (The_Ref : Ref'Class) return Boolean;
   --  True iff The_Ref is a non null reference on an object
   --  exported by the ORB

   function Is_Equivalent (Left, Right : Ref'Class) return Boolean;
   --  True if we can conclusively determine locally that Left and Right
   --  are two references to the same object.

   function Same_Node (Left, Right : Ref'Class) return Boolean;
   --  True if we can determine that Left and Right designate entities on the
   --  same node.

   procedure Create_Reference
     (Profiles : Profile_Array;
      Type_Id  : String;
      R        : out Ref);
   --  Create a reference with Profiles as its profiles.
   --  The returned ref R is nil iff Profiles'Length = 0.

   function Profiles_Of (R : Ref) return Profile_Array;
   --  Return the list of profiles constituting Ref

   function Type_Id_Of (R : Ref) return String;
   --  Return the type identifier of Ref

   procedure Set_Type_Id (R : Ref; Type_Id : String);
   --  Set the Type_Id of the reference

   function Image (R : Ref) return String;
   --  For debugging purposes

   procedure String_To_Object (Str : String; The_Ref : out Ref);
   --  Convert a stringified representation of an object reference into an
   --  actual reference.
   --  Note: String_To_Object is a procedure so that it can be inherited
   --  when Ref is derived without requiring overload (Ada 95).

   procedure Get_Binding_Info
     (R   :     Ref'Class;
      QoS :     PolyORB.QoS.QoS_Parameters;
      BO  : in out Binding_Objects.Ref;
      Pro : out Binding_Data.Profile_Access);
   --  Retrieve the binding object associated with R, if R is bound.
   --  Otherwise, return null. The caller is responsible for taking R's
   --  mutex.

   function Mutex_Of (R : Ref) return access Tasking.Mutexes.Mutex_Type'Class;
   --  Return the mutex protecting R's critical section

   --------------------------------------
   -- Stream attributes for references --
   --------------------------------------

   --  PolyORB.References does not mandate any particular external
   --  representation for references. Instead, the stream attributes make
   --  indirect calls through a Ref_Streamer object, which must provide
   --  appropriate primitives.

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Ref);
   for Ref'Read use Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Ref);
   for Ref'Write use Write;

   ----------------------------
   -- Annotations management --
   ----------------------------

   function Notepad_Of (R : Ref) return Annotations.Notepad_Access;

   type Ref_Ptr is access all Ref;
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Ref,
      Name => Ref_Ptr);

private

   type Binding_Info is record
      Binding_Object_Ref : Smart_Pointers.Ref;
      Binding_Profile    : Binding_Data.Profile_Access;
      --  If a reference is already bound, the Binding_Object_Ref will
      --  designate the component that serves as the binding object, and
      --  Binding_Profile will designate the profile used to bind this
      --  reference. Binding_Profile is kept so we can determine the
      --  Object_Id of the object designated by the reference.
   end record;

   package Binding_Info_Lists is
     new Utils.Chained_Lists (Binding_Info, Doubly_Chained => True);

   procedure Share_Binding_Info
     (Dest   : Ref'Class;
      Source : Ref'Class);
   --  Needs comment???

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

         Mutex : Tasking.Mutexes.Mutex_Access;
         --  Mutex protecting concurrent accesses to ref info

         Binding_Info : Binding_Info_Lists.List;
         --  The list of binding objects used for reference bound

         Notepad : aliased Annotations.Notepad;
         --  Reference_Info's notepad. The user is responsible for ensuring
         --  proper protection against incorrect concurrent accesses.
     end record;
   type Reference_Info_Access is access all Reference_Info'Class;

   function Ref_Info_Of (R : Ref'Class) return Reference_Info_Access;
   --  Obtain the object reference information from R.

   overriding procedure Finalize (RI : in out Reference_Info);
   --  When an object reference is bound (i.e. associated at runtime with a
   --  transport service endpoint and a protocol stack), it becomes associated
   --  with a Binding_Object which will remain in existence until all
   --  references to the object have been finalized (at which time the
   --  transport connection and protocol stack will be torn down, as a result
   --  of finalizing the binding object).

   --  Note that Reference_Info must not be an Entity, because the Finalize
   --  operation would then be called *after*, not *before*, the controlled
   --  components of Reference_Info (including Profiles and
   --  Binding_Object_Ref) have been finalized.

   --  XXX the following declarations must be documented.

   type String_To_Object_Func is
     access function (Str : String) return Ref;

   procedure Register_String_To_Object
     (Prefix : String;
      Func   : String_To_Object_Func);

   type Ref_Streamer is abstract tagged limited null record;
   type Ref_Streamer_Access is access all Ref_Streamer'Class;

   procedure Read
     (R : access Ref_Streamer;
      S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Ref'Class) is abstract;

   procedure Write
     (R : access Ref_Streamer;
      S : access Ada.Streams.Root_Stream_Type'Class;
      V : Ref'Class) is abstract;

   The_Ref_Streamer : Ref_Streamer_Access;

end PolyORB.References;
