------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S M A R T _ P O I N T E R S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2021, Free Software Foundation, Inc.          --
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

with Ada.Finalization;
with Interfaces;

package PolyORB.Smart_Pointers is

   pragma Preelaborate;

   ------------------------
   -- Task-unsafe entity --
   ------------------------

   type Unsafe_Entity is abstract tagged limited private;

   function Is_Controlled (X : Unsafe_Entity) return Boolean;
   --  False expect for derived type from Smart_Pointers.Controlled_Entities

   procedure Finalize (X : in out Unsafe_Entity);
   --  Unsafe_Entity is the base type of all objects that can be referenced.
   --  It contains a Counter, which is the number of references to this
   --  object, and is automatically destroyed when the counter reaches 0.
   --  Before the entity is destroyed, the Finalize operation is called.
   --  NOTE however that Unsafe_Entity is *not* a controlled type: Finalize
   --  is *only* called when an Entity is destroyed as a result of its
   --  reference counter dropping to 0.

   type Entity_Ptr is access all Unsafe_Entity'Class;

   procedure Disable_Ref_Counting (Obj : in out Unsafe_Entity'Class);
   --  Disable reference counting on Obj. No attempt will then be made to keep
   --  track of references, and no automatic deallocation will occur after the
   --  last reference is used. This is intended primarily for library-level
   --  entities.

   ----------------------
   -- Task-safe entity --
   ----------------------

   type Non_Controlled_Entity is abstract new Unsafe_Entity with private;
   --  Same as Unsafe_Entity

   ---------
   -- Ref --
   ---------

   type Ref is new Ada.Finalization.Controlled with private;
   pragma Preelaborable_Initialization (Ref);
   --  The base type of all references. This type is often derived but never
   --  extended. It contains one field, which designates the referenced object.

   overriding procedure Adjust   (The_Ref : in out Ref);
   overriding procedure Finalize (The_Ref : in out Ref);

   procedure Set
     (The_Ref    : in out Ref;
      The_Entity : Entity_Ptr);
   --  Make The_Ref designate The_Entity, and increment The_Entity's usage
   --  counter. The_Entity's reference counter is allowed to be 0 only when
   --  creating the first reference to it. This procedure guarantees that
   --  The_Ref does not transiently become Nil during the operation.

   procedure Reuse_Entity
     (The_Ref    : in out Ref;
      The_Entity : Entity_Ptr);
   --  Equivalent to Set (The_Ref, The_Entity) if The_Entity's usage counter
   --  is strictly greater than 0. Otherwise, The_Ref is left unchanged.
   --  It is the caller's responsibility to ensure that The_Entity points
   --  to a valid Entity object (even in the latter case). This allows a
   --  reference to be reconstructed from a saved Entity_Ptr value, ensuring
   --  that the designated entity is not being finalized. The_Ref is expected
   --  to be nil before the call. Caller must ensure that no concurrent calls
   --  to Reuse_Entity are made on the same Entity_Ptr.

   procedure Use_Entity
     (The_Ref    : in out Ref;
      The_Entity : Entity_Ptr);
   --  Equivalent to Set (The_Ref, The_Entity), but requires The_Entity's usage
   --  counter to be zero, and The_Ref to be a null reference. Does not require
   --  the ORB to have been initialized. The caller is responsible to ensure
   --  task safety (this subprogram is expected to be used only to associate
   --  a reference to a newly allocated object).

   procedure Unref (The_Ref : in out Ref) renames Finalize;
   procedure Release (The_Ref : in out Ref) renames Finalize;

   function Is_Nil (The_Ref : Ref) return Boolean;
   --  True iff The_Ref is a nil reference

   function Is_Null (The_Ref : Ref) return Boolean renames Is_Nil;

   function Entity_Of (The_Ref : Ref) return Entity_Ptr;
   --  Return the entity designated by The_Ref

   function Same_Entity (Left, Right : Ref) return Boolean;
   --  True if Left and Right designate the same entity

   --  The following two low-level functions are exposed for cases where
   --  controlled types cannot be directly used in a personality. Great care
   --  must be taken when using them outside of this unit!

   procedure Inc_Usage (Obj : Entity_Ptr);
   --  Increment Obj's reference counter

   procedure Dec_Usage (Obj : in out Entity_Ptr);
   --  Decremement Obj's reference counter; if it drops to zero, deallocate
   --  the designated object, and reset Obj to null.

private

   type Unsafe_Entity is abstract tagged limited record
      Counter : aliased Interfaces.Unsigned_32 := 0;
      --  Reference counter.
      --  If set to Unsigned_32'Last, no reference counting is performed for
      --  this entity: Inc_Usage and Dec_Usage are both no-ops in that case.
   end record;

   type Non_Controlled_Entity is abstract new Unsafe_Entity with null record;

   type Ref is new Ada.Finalization.Controlled with record
      A_Ref : Entity_Ptr := null;
      --  The entity designated by this reference
   end record;

   ---------------------
   -- Debugging hooks --
   ---------------------

   --  For debugging purposes, the body of this unit needs to call
   --  Ada.Tags.External_Tag for entities and references. However, we do not
   --  want any dependence on Ada.Tags, because that would prevent this unit
   --  from being preelaborable. So, we call External_Tag indirectly through
   --  a hook that is set during PolyORB initialization.
   --
   --  Note: Ada.Tags is preelaborable in Ada 2005, we need to review this
   --  dependency.

   type Entity_External_Tag_Hook is access
     function (X : Unsafe_Entity'Class) return String;
   --  A function returning External_Tag (X'Tag)

   type Ref_External_Tag_Hook is access
     function (X : Ref'Class) return String;
   --  A function returning External_Tag (Entity_Of (X)'Tag)

   procedure Initialize
     (The_Entity_External_Tag : Entity_External_Tag_Hook;
      The_Ref_External_Tag    : Ref_External_Tag_Hook;
      The_Default_Trace       : Boolean);
   --  Initialize internal structures and set debugging hooks (to be called by
   --  child elaboration package)

   --  Determination of whether to trace smart pointers event for a specific
   --  entity type: in [smart_pointers] section, whether type T is traced
   --  is determined by parameter T.trace, or if not set, by default.trace.
   --  By default event is traced.

   Trace_Section : constant String := "smart_pointers";
   Trace_Suffix  : constant String := ".trace";

   function Get_Trace (Entity_Type : String) return Boolean;
   --  Return indication of whether to trace events for the given entity type

end PolyORB.Smart_Pointers;
