------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S M A R T _ P O I N T E R S                --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Parameters;

package body PolyORB.Smart_Pointers is

   use Interfaces;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.smart_pointers");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   type Event_Kind_Type is (Inc_Usage, Dec_Usage);
   --  Smart pointer events that can be traced

   procedure Trace_Event
     (Event_Kind : Event_Kind_Type;
      Obj        : Entity_Ptr);
   --  Produce debugging trace for the indicated event on Obj, if applicable
   --  (must be called just before updating Obj's reference counter).

   function Dummy_Entity_External_Tag (X : Unsafe_Entity'Class) return String;
   function Dummy_Ref_External_Tag (X : Ref'Class) return String;
   --  Dummy version of functions returning External_Tag (X'Tag) to prevent
   --  crashes at elaboration time for early initialization of references
   --  (before complete ORB initialization).

   Entity_External_Tag : Entity_External_Tag_Hook :=
     Dummy_Entity_External_Tag'Access;
   Ref_External_Tag    : Ref_External_Tag_Hook    :=
     Dummy_Ref_External_Tag'Access;
   --  Debugging hooks, set at initialization

   Default_Trace : Boolean := True;
   --  Needs comment???

   package Sync_Counters is
      --  Support for atomic addition/subtraction

      procedure Initialize;
      function Sync_Add_And_Fetch
        (Ptr   : access Interfaces.Unsigned_32;
         Value : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
      function Sync_Sub_And_Fetch
        (Ptr   : access Interfaces.Unsigned_32;
         Value : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;

      pragma Inline (Initialize);
      pragma Inline (Sync_Add_And_Fetch);
      pragma Inline (Sync_Sub_And_Fetch);
   end Sync_Counters;

   package body Sync_Counters is separate;

   use Sync_Counters;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust
     (The_Ref : in out Ref) is
   begin
      pragma Debug (C, O ("Adjust: enter"));

      if The_Ref.A_Ref /= null then
         Inc_Usage (The_Ref.A_Ref);
      else
         pragma Debug (C, O ("Adjust: null ref"));
         null;
      end if;

      pragma Debug (C, O ("Adjust: leave"));
   end Adjust;

   ---------------
   -- Dec_Usage --
   ---------------

   procedure Dec_Usage (Obj : in out Entity_Ptr) is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Unsafe_Entity'Class,
         Name => Entity_Ptr);

      Counter : Interfaces.Unsigned_32;

   begin
      if Obj.Counter = Interfaces.Unsigned_32'Last then
         --  Entity is not reference-counted

         return;
      end if;

      pragma Debug (C, Trace_Event (Dec_Usage, Obj));
      Counter := Sync_Sub_And_Fetch (Obj.Counter'Access, 1);

      if Counter = 0 then

         pragma Debug (C, O ("Dec_Usage: deallocating "
                          & Entity_External_Tag (Obj.all)));

         if not Is_Controlled (Obj.all) then
            --  This entity is not controlled: finalize it ourselves

            Finalize (Obj.all);
         end if;

         Free (Obj);

         pragma Debug (C, O ("Dec_Usage: deallocation done"));
      end if;
   end Dec_Usage;

   --------------------------
   -- Disable_Ref_Counting --
   --------------------------

   procedure Disable_Ref_Counting (Obj : in out Unsafe_Entity'Class) is
   begin
      Obj.Counter := Interfaces.Unsigned_32'Last;
   end Disable_Ref_Counting;

   -------------------------------
   -- Dummy_Entity_External_Tag --
   -------------------------------

   function Dummy_Entity_External_Tag
     (X : Unsafe_Entity'Class) return String
   is
      pragma Unreferenced (X);
   begin
      return "";
   end Dummy_Entity_External_Tag;

   ----------------------------
   -- Dummy_Ref_External_Tag --
   ----------------------------

   function Dummy_Ref_External_Tag (X : Ref'Class) return String is
      pragma Unreferenced (X);
   begin
      return "";
   end Dummy_Ref_External_Tag;

   ---------------
   -- Entity_Of --
   ---------------

   function Entity_Of (The_Ref : Ref) return Entity_Ptr is
   begin
      return The_Ref.A_Ref;
   end Entity_Of;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Unsafe_Entity) is
      pragma Warnings (Off);
      pragma Unreferenced (X);
      pragma Warnings (On);
   begin
      null;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (The_Ref : in out Ref) is

      function Return_Ref_External_Tag return String;
      --  Encapsulate the call to Ref_External_Tag. This function avoids
      --  run-time overhead if debug is turned off.

      function Return_Ref_External_Tag return String is
      begin
         if Ref_External_Tag /= null then
            return "Finalize: enter, The_Ref is a "
              & Ref_External_Tag (The_Ref);

         else
            return "Finalize: enter, The_Ref is a <UNAVAILABLE>";
         end if;
      end Return_Ref_External_Tag;

      Obj : Entity_Ptr := The_Ref.A_Ref;

   --  Start of processing for Finalize

   begin
      pragma Debug (C, O (Return_Ref_External_Tag));

      --  Invalidate A_Ref early because such access may subsequently become
      --  erroneous, see below.

      The_Ref.A_Ref := null;

      if Obj /= null then
         Dec_Usage (Obj);

         --  From this point on, we may not assume that The_Ref is still valid,
         --  because in the case of auto-referential structures, it may be
         --  a member of Obj.all, which has been destroyed above if its ref
         --  counter dropped to 0.

      else
         pragma Debug (C, O ("Finalize: null ref"));
         null;
      end if;

      pragma Debug (C, O ("Finalize: leave"));
   end Finalize;

   ---------------
   -- Get_Trace --
   ---------------

   function Get_Trace (Entity_Type : String) return Boolean is
   begin
      return Parameters.Get_Conf
        (Section => Trace_Section,
         Key     => Entity_Type & Trace_Suffix,
         Default => Default_Trace);
   end Get_Trace;

   ---------------
   -- Inc_Usage --
   ---------------

   procedure Inc_Usage (Obj : Entity_Ptr) is
      Discard : Interfaces.Unsigned_32;
      pragma Unreferenced (Discard);
   begin
      if Obj.Counter =  Interfaces.Unsigned_32'Last then
         --  Entity is not reference-counted

         return;
      end if;

      pragma Debug (C, Trace_Event (Inc_Usage, Obj));
      Discard := Sync_Add_And_Fetch (Obj.Counter'Access, 1);
   end Inc_Usage;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (The_Entity_External_Tag : Entity_External_Tag_Hook;
      The_Ref_External_Tag    : Ref_External_Tag_Hook;
      The_Default_Trace       : Boolean)
   is
   begin
      Sync_Counters.Initialize;
      Entity_External_Tag := The_Entity_External_Tag;
      Ref_External_Tag    := The_Ref_External_Tag;
      Default_Trace       := The_Default_Trace;
   end Initialize;

   -------------------
   -- Is_Controlled --
   -------------------

   function Is_Controlled (X : Unsafe_Entity) return Boolean is
      pragma Unreferenced (X);
   begin
      return False;
   end Is_Controlled;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (The_Ref : Ref) return Boolean is
   begin
      return The_Ref.A_Ref = null;
   end Is_Nil;

   ------------------
   -- Reuse_Entity --
   ------------------

   procedure Reuse_Entity
     (The_Ref    : in out Ref;
      The_Entity : Entity_Ptr)
   is
      Counter : Interfaces.Unsigned_32;
   begin
      pragma Assert (The_Ref.A_Ref = null);
      pragma Assert (The_Entity.Counter /= Interfaces.Unsigned_32'Last);
      pragma Debug (C, Trace_Event (Inc_Usage, The_Entity));
      Counter := Sync_Add_And_Fetch (The_Entity.Counter'Access, 1);

      --  Note: race condition here, one must absolutely avoid concurrent
      --  calls to Reuse_Entity, otherwise both calls might increment
      --  The_Entity.Counter, and one might thus fail to notice that
      --  it was null prior to the two calls.

      if Counter = 1 then
         --  Was 0, can't reuse entity, reset counter.

         pragma Debug (C, Trace_Event (Dec_Usage, The_Entity));
         Counter := Sync_Sub_And_Fetch (The_Entity.Counter'Access, 1);

      else
         The_Ref.A_Ref := The_Entity;
      end if;
   end Reuse_Entity;

   -----------------
   -- Same_Entity --
   -----------------

   function Same_Entity (Left, Right : Ref) return Boolean is
   begin
      return Entity_Of (Left) = Entity_Of (Right);
   end Same_Entity;

   ---------
   -- Set --
   ---------

   procedure Set
     (The_Ref    : in out Ref;
      The_Entity : Entity_Ptr)
   is
      Prev_Entity : Entity_Ptr := The_Ref.A_Ref;
   begin
      if The_Ref.A_Ref = The_Entity then
         --  Same entity: no-op

         return;
      end if;

      if The_Entity /= null then
         Inc_Usage (The_Entity);
      end if;
      The_Ref.A_Ref := The_Entity;

      if Prev_Entity /= null then
         Dec_Usage (Prev_Entity);
      end if;
   end Set;

   -----------------
   -- Trace_Event --
   -----------------

   procedure Trace_Event
     (Event_Kind : Event_Kind_Type;
      Obj        : Entity_Ptr)
   is
      Entity_Kind : constant String := Entity_External_Tag (Obj.all);
   begin
      if Get_Trace (Entity_Kind) then
         O (Event_Kind'Img & ": "
              & Entity_Kind
              & Unsigned_32'Image (Obj.Counter)
              & " ->"
              & Unsigned_32'Image ((if Event_Kind = Inc_Usage
                                    then Obj.Counter + 1
                                    else Obj.Counter - 1)));
      end if;
   end Trace_Event;

   ----------------
   -- Use_Entity --
   ----------------

   procedure Use_Entity
     (The_Ref    : in out Ref;
      The_Entity : Entity_Ptr)
   is
   begin
      pragma Assert (The_Ref.A_Ref = null and then The_Entity.Counter = 0);

      pragma Debug (C, Trace_Event (Inc_Usage, The_Entity));
      The_Entity.Counter := 1;
      The_Ref.A_Ref := The_Entity;
   end Use_Entity;

end PolyORB.Smart_Pointers;
