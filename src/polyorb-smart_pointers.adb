------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S M A R T _ P O I N T E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  $Id: //droopi/release/1.0/src/polyorb-smart_pointers.adb#3 $

with Ada.Unchecked_Deallocation;
with Ada.Tags;

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Strings;

package body PolyORB.Smart_Pointers is

   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   Counter_Lock : Mutex_Access;

   package L is new PolyORB.Log.Facility_Log ("polyorb.smart_pointers");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------
   -- Inc_Usage --
   ---------------

   procedure Inc_Usage
     (Obj : Entity_Ptr) is
   begin
      pragma Assert (Obj.Counter /= -1);

      pragma Debug (O ("Inc_Usage: Obj is a "
                       & Ada.Tags.External_Tag (Obj.all'Tag)));

      Enter (Counter_Lock);
      pragma Debug (O ("Inc_Usage: Counter"
                       & Natural'Image (Obj.Counter)
                       & " ->"
                       & Natural'Image (Obj.Counter + 1)));
      Obj.Counter := Obj.Counter + 1;
      Leave (Counter_Lock);

   end Inc_Usage;

   ---------------
   -- Dec_Usage --
   ---------------

   procedure Dec_Usage
     (Obj : in out Entity_Ptr)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Non_Controlled_Entity'Class, Entity_Ptr);

   begin
      pragma Assert (Obj.Counter /= -1);
      pragma Debug (O ("Dec_Usage: Obj is a "
                       & Ada.Tags.External_Tag (Obj.all'Tag)));

      Enter (Counter_Lock);
      pragma Debug (O ("Dec_Usage: Counter"
                       & Natural'Image (Obj.Counter)
                       & " ->"
                       & Natural'Image (Obj.Counter - 1)));
      Obj.Counter := Obj.Counter - 1;

      if Obj.Counter = 0 then

         pragma Debug (O ("Dec_Usage: deallocating."));

         Leave (Counter_Lock);
         --  Releasing Counter_Lock at this stage is sufficient to
         --  ensure that only one task finalizes 'Obj.all' and
         --  frees 'Obj'.

         if Obj.all not in Entity'Class then
            --  This entity is not controlled: finalize it ourselves

            Finalize (Obj.all);
         end if;

         Free (Obj);
      else
         Leave (Counter_Lock);
      end if;

      pragma Debug (O ("Leaving Dec_Usage"));
   end Dec_Usage;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (X : in out Entity_Controller) is
   begin
      pragma Debug (O ("Initializing Entity"));
      Initialize (X.E.all);
   end Initialize;

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

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (X : in out Entity_Controller) is
   begin
      Finalize (X.E.all);
   end Finalize;

   procedure Finalize
     (X : in out Non_Controlled_Entity)
   is
      pragma Warnings (Off);
      pragma Unreferenced (X);
      pragma Warnings (On);

   begin
      null;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (The_Ref : in out Ref) is
   begin
      pragma Assert (The_Ref.A_Ref = null);
      pragma Debug (O ("Initialized a Ref"));
      null;
   end Initialize;

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

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (The_Ref : in out Ref) is
   begin
      pragma Debug (O ("Finalize: enter, The_Ref is a "
                       & Ada.Tags.External_Tag
                       (Ref'Class (The_Ref)'Tag)));

      if The_Ref.A_Ref /= null then
         Dec_Usage (The_Ref.A_Ref);
      else
         pragma Debug (O ("Finalize: null ref"));
         null;
      end if;

      The_Ref.A_Ref := null;
      pragma Debug (O ("Finalize: leave"));
   end Finalize;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil
     (The_Ref : Ref)
     return Boolean is
   begin
      return The_Ref.A_Ref = null;
   end Is_Nil;

   -------------
   -- Release --
   -------------

   procedure Release
     (The_Ref : in out Ref) is
   begin
      The_Ref := (Ada.Finalization.Controlled with A_Ref => null);
   end Release;

   ---------------
   -- Entity_Of --
   ---------------

   function Entity_Of
     (The_Ref : Ref)
     return Entity_Ptr is
   begin
      pragma Debug (O ("Entity_Of"));
      return The_Ref.A_Ref;
   end Entity_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Create (Counter_Lock);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"smart_pointers",
       Conflicts => Empty,
       Depends   => +"tasking.mutexes",
       Depends   => +"soft_links" & "tasking.mutexes",
       Provides  => Empty,
       Init      => Initialize'Access));
end PolyORB.Smart_Pointers;
