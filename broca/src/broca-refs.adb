------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . R E F S                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Tags;
with Broca.Locks;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Refs is

   use Broca.Buffers;

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.refs");
   procedure O is new Broca.Debug.Output (Flag);

   Counter_Global_Lock : Broca.Locks.Mutex_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Entity'Class, Ref_Ptr);

   function Img (I : Integer) return String;
   function Img (I : Integer) return String
   is
      S : constant String
        := Integer'Image (I);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   ------------------
   --  Deallocate  --
   ------------------
   procedure Deallocate (Obj : access Entity) is
   begin
      null;
   end Deallocate;

   ---------------
   -- Inc_Usage --
   ---------------

   procedure Inc_Usage (Obj : Ref_Ptr) is
   begin
      pragma Assert (Obj.Counter /= -1);
      pragma Debug (O ("Inc_Usage: Obj is a "
                       & Ada.Tags.External_Tag (Obj.all'Tag)));

      Counter_Global_Lock.Lock;
      pragma Debug (O ("Inc_Usage: Counter"
                       & Obj.Counter'Img
                       & " -> "
                       & Img (Obj.Counter + 1)));
      Obj.Counter := Obj.Counter + 1;
      Counter_Global_Lock.Unlock;

   end Inc_Usage;

   ---------------
   -- Dec_Usage --
   ---------------

   procedure Dec_Usage (Obj : in out Ref_Ptr) is
   begin
      pragma Assert (Obj.Counter /= -1);
      pragma Debug (O ("Dec_Usage: Obj is a "
                       & Ada.Tags.External_Tag (Obj.all'Tag)));

      Counter_Global_Lock.Lock;
      pragma Debug (O ("Dec_Usage: Counter"
                       & Obj.Counter'Img
                       & " -> "
                       & Img (Obj.Counter - 1)));
      Obj.Counter := Obj.Counter - 1;
      Counter_Global_Lock.Unlock;

      if Obj.Counter = 0 then
         pragma Debug
           (O ("Dec_Usage: deallocating."));
         Deallocate (Obj);
         Free (Obj);
      end if;

   end Dec_Usage;

   procedure Set
     (The_Ref : in out Ref;
      The_Entity : Ref_Ptr) is
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

   procedure Initialize (The_Ref : in out Ref) is
   begin
      if The_Ref.A_Ref /= null then
         pragma Debug (O ("Initialize: Stray pointer."));
         raise Program_Error;
      end if;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (The_Ref : in out Ref) is
   begin
      if The_Ref.A_Ref /= null then
         Inc_Usage (The_Ref.A_Ref);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (The_Ref : in out Ref) is
   begin
      if The_Ref.A_Ref /= null then
         Dec_Usage (The_Ref.A_Ref);
      end if;
      The_Ref.A_Ref := null;

   end Finalize;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (The_Ref : Ref) return Boolean is
   begin
      return The_Ref.A_Ref = null;
   end Is_Nil;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Entity) is
   begin
      raise Program_Error;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Value  : out Entity) is
   begin
      raise Program_Error;
   end Unmarshall;

   -------------
   -- Release --
   -------------

   procedure Release (The_Ref : in out Ref) is
   begin
      The_Ref := Nil_Ref;
   end Release;

   ---------------
   -- Entity_Of --
   ---------------

   function Entity_Of (The_Ref : Ref) return Ref_Ptr is
   begin
      return The_Ref.A_Ref;
   end Entity_Of;

end Broca.Refs;
