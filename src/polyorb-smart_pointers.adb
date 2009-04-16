------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S M A R T _ P O I N T E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2009, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Tasking.Mutexes;

package body PolyORB.Smart_Pointers is

   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log ("polyorb.smart_pointers");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Counter_Lock : Mutex_Access;
   --  Global lock used to protect concurrent accesses to reference counters

   procedure Unchecked_Inc_Usage (Obj : Entity_Ptr);
   --  Internal procedure to increment Obj's usage counter. This must be
   --  called with the proper lock held.

   type Event_Kind_Type is (Inc_Usage, Dec_Usage);
   --  Smart pointer events that can be traced

   procedure Trace_Event
     (Event_Kind : Event_Kind_Type;
      Obj        : Entity_Ptr);
   --  Produce debugging trace for the indicated event on Obj, if applicable

   Entity_External_Tag : Entity_External_Tag_Hook := null;
   Ref_External_Tag    : Ref_External_Tag_Hook := null;
   --  Debugging hooks, set at initialization

   Default_Trace : Boolean := True;
   --  needs comment???

   ------------
   -- Adjust --
   ------------

   procedure Adjust
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
      procedure Free is new Ada.Unchecked_Deallocation
        (Unsafe_Entity'Class, Entity_Ptr);

   begin
      if Obj.Counter = -1 then
         --  Entity is not reference-counted

         return;
      end if;

      pragma Assert (Counter_Lock /= null);
      Entity_Lock (Obj.all);

      pragma Debug (C, Trace_Event (Dec_Usage, Obj));
      Obj.Counter := Obj.Counter - 1;

      if Obj.Counter = 0 then

         pragma Debug (C, O ("Dec_Usage: deallocating "
                          & Entity_External_Tag (Obj.all)));

         Entity_Unlock (Obj.all);
         --  Releasing Obj lock at this stage is sufficient to ensure
         --  that only one task finalizes Obj.all and frees Obj.

         if not Is_Controlled (Obj.all) then
            --  This entity is not controlled: finalize it ourselves

            Finalize (Obj.all);
         end if;

         Free (Obj);

         pragma Debug (C, O ("Dec_Usage: deallocation done"));
      else
         Entity_Unlock (Obj.all);
      end if;
   end Dec_Usage;

   --------------------------------
   -- Disable_Reference_Counting --
   --------------------------------

   procedure Disable_Reference_Counting (Obj : in out Unsafe_Entity'Class) is
   begin
      Obj.Counter := -1;
   end Disable_Reference_Counting;

   -----------------
   -- Entity_Lock --
   -----------------

   procedure Entity_Lock (X : in out Unsafe_Entity) is
      pragma Unreferenced (X);
   begin
      null;
   end Entity_Lock;

   -----------------
   -- Entity_Lock --
   -----------------

   procedure Entity_Lock (X : in out Non_Controlled_Entity) is
      pragma Unreferenced (X);

   begin
      Enter (Counter_Lock);
   end Entity_Lock;

   ---------------
   -- Entity_Of --
   ---------------

   function Entity_Of (The_Ref : Ref) return Entity_Ptr is
   begin
      return The_Ref.A_Ref;
   end Entity_Of;

   -------------------
   -- Entity_Unlock --
   -------------------

   procedure Entity_Unlock (X : in out Unsafe_Entity) is
      pragma Unreferenced (X);

   begin
      null;
   end Entity_Unlock;

   -------------------
   -- Entity_Unlock --
   -------------------

   procedure Entity_Unlock (X : in out Non_Controlled_Entity) is
      pragma Unreferenced (X);

   begin
      Leave (Counter_Lock);
   end Entity_Unlock;

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

   procedure Finalize (The_Ref : in out Ref) is

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
   begin
      if Obj.Counter = -1 then
         --  Entity is not reference-counted

         return;
      end if;

      Entity_Lock (Obj.all);
      Unchecked_Inc_Usage (Obj);
      Entity_Unlock (Obj.all);
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
      Create (Counter_Lock);
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

   -----------------------
   -- Reference_Counter --
   -----------------------

   function Reference_Counter (Obj : Unsafe_Entity'Class) return Integer is
   begin
      return Obj.Counter;
   end Reference_Counter;

   ------------------
   -- Reuse_Entity --
   ------------------

   procedure Reuse_Entity
     (The_Ref    : in out Ref;
      The_Entity : Entity_Ptr)
   is
   begin
      Entity_Lock (The_Entity.all);

      if The_Entity.Counter > 0 then
         Unchecked_Inc_Usage (The_Entity);
         The_Ref.A_Ref := The_Entity;
      end if;

      Entity_Unlock (The_Entity.all);
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
      The_Entity :        Entity_Ptr) is
   begin
      Finalize (The_Ref);
      The_Ref.A_Ref := The_Entity;
      Adjust (The_Ref);
   end Set;

   -----------------
   -- Trace_Event --
   -----------------

   procedure Trace_Event
     (Event_Kind : Event_Kind_Type;
      Obj        : Entity_Ptr)
   is
      Entity_Kind : constant String := Entity_External_Tag (Obj.all);
      Event_Values : constant array (Event_Kind_Type) of Integer :=
                       (Inc_Usage => +1, Dec_Usage => -1);
   begin
      if Get_Trace (Entity_Kind) then
         O (Event_Kind'Img & ": "
              & Entity_Kind
              & Natural'Image (Obj.Counter)
              & " ->"
              & Natural'Image (Obj.Counter + Event_Values (Event_Kind)));
      end if;
   end Trace_Event;

   -------------------------
   -- Unchecked_Inc_Usage --
   -------------------------

   procedure Unchecked_Inc_Usage (Obj : Entity_Ptr) is
   begin
      pragma Assert (Obj.Counter /= -1);
      pragma Debug (C, Trace_Event (Inc_Usage, Obj));
      Obj.Counter := Obj.Counter + 1;
   end Unchecked_Inc_Usage;

end PolyORB.Smart_Pointers;
