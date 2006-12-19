------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S M A R T _ P O I N T E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

package body PolyORB.Smart_Pointers is

   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log ("polyorb.smart_pointers");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   procedure Unchecked_Inc_Usage (Obj : Entity_Ptr);
   --  Internal procedure to increment Obj's usage counter. This must be
   --  called with the proper lock held.

   ------------
   -- Adjust --
   ------------

   procedure Adjust
     (The_Ref : in out Ref) is
   begin
      pragma Debug (O ("Adjust: enter"));

      if The_Ref.A_Ref /= null then
         Inc_Usage (The_Ref.A_Ref);
      else
         pragma Debug (O ("Adjust: null ref"));
         null;
      end if;

      pragma Debug (O ("Adjust: leave"));
   end Adjust;

   ---------------
   -- Dec_Usage --
   ---------------

   procedure Dec_Usage
     (Obj : in out Entity_Ptr)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Unsafe_Entity'Class, Entity_Ptr);

   begin
      pragma Assert (Obj.Counter /= -1);
      pragma Debug (O ("Dec_Usage: Obj is a "
                       & Entity_External_Tag (Obj.all)));

      pragma Assert (Counter_Lock /= null);
      Entity_Lock (Obj.all);
      pragma Debug (O ("Dec_Usage: Counter"
                       & Natural'Image (Obj.Counter)
                       & " ->"
                       & Natural'Image (Obj.Counter - 1)));
      Obj.Counter := Obj.Counter - 1;

      if Obj.Counter = 0 then

         pragma Debug (O ("Dec_Usage: deallocating "
                          & Entity_External_Tag (Obj.all)));

         Entity_Unlock (Obj.all);
         --  Releasing Obj lock at this stage is sufficient to ensure
         --  that only one task finalizes Obj.all and frees Obj.

         if Obj.all not in Entity'Class then
            --  This entity is not controlled: finalize it ourselves

            Finalize (Obj.all);
         end if;

         Free (Obj);
      else
         Entity_Unlock (Obj.all);
      end if;

      pragma Debug (O ("Leaving Dec_Usage"));
   end Dec_Usage;

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

   procedure Finalize
     (X : in out Unsafe_Entity)
   is
      pragma Warnings (Off);
      pragma Unreferenced (X);
      pragma Warnings (On);

   begin
      null;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (X : in out Entity_Controller) is
   begin
      Finalize (X.E.all);
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

   begin
      pragma Debug (O (Return_Ref_External_Tag));

      if The_Ref.A_Ref /= null then
         Dec_Usage (The_Ref.A_Ref);
      else
         pragma Debug (O ("Finalize: null ref"));
         null;
      end if;

      The_Ref.A_Ref := null;
      pragma Debug (O ("Finalize: leave"));
   end Finalize;

   ---------------
   -- Inc_Usage --
   ---------------

   procedure Inc_Usage (Obj : Entity_Ptr) is
   begin
      Entity_Lock (Obj.all);
      Unchecked_Inc_Usage (Obj);
      Entity_Unlock (Obj.all);
   end Inc_Usage;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (X : in out Entity_Controller) is
   begin
      pragma Debug (O ("Initializing Entity"));
      Initialize (X.E.all);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (X : in out Entity)
   is
      pragma Warnings (Off);
      pragma Unreferenced (X);
      pragma Warnings (On);

   begin
      pragma Assert (Counter_Lock /= null);
      null;
   end Initialize;

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

   function Reference_Counter (Obj : Unsafe_Entity) return Integer is
   begin
      return Obj.Counter;
   end Reference_Counter;

   -------------
   -- Release --
   -------------

   procedure Release
     (The_Ref : in out Ref) is
   begin
      The_Ref := (Ada.Finalization.Controlled with A_Ref => null);
   end Release;

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
      pragma Debug (O ("Set: enter."));

      Finalize (The_Ref);
      The_Ref.A_Ref := The_Entity;
      Adjust (The_Ref);

      pragma Debug (O ("Set: leave."));
   end Set;

   -------------------------
   -- Unchecked_Inc_Usage --
   -------------------------

   procedure Unchecked_Inc_Usage (Obj : Entity_Ptr) is
   begin
      pragma Assert (Obj.Counter /= -1);

      pragma Debug (O ("Inc_Usage: Obj is a "
                       & Entity_External_Tag (Obj.all)));

      pragma Debug (O ("Inc_Usage: Counter"
                       & Natural'Image (Obj.Counter)
                       & " ->"
                       & Natural'Image (Obj.Counter + 1)));
      Obj.Counter := Obj.Counter + 1;
   end Unchecked_Inc_Usage;

end PolyORB.Smart_Pointers;
