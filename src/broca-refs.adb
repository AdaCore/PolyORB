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
with Broca.Exceptions;
with Broca.Object;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Refs is

   use Broca.Buffers;

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.refs");
   procedure O is new Broca.Debug.Output (Flag);

   Counter_Global_Lock : Broca.Locks.Mutex_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Ref_Type'Class, Ref_Ptr);

   -------------------
   -- Disable_Usage --
   -------------------

   procedure Disable_Usage (Obj : in out Ref_Type) is
   begin
      if Obj.Counter /= 0 then
         Broca.Exceptions.Raise_Internal (100);
      else
         Obj.Counter := -1;
      end if;
   end Disable_Usage;

   ---------------
   -- Inc_Usage --
   ---------------

   procedure Inc_Usage (Obj : in Ref_Ptr) is
   begin
      if Obj.Counter /= -1 then
         Counter_Global_Lock.Lock;
         Obj.Counter := Obj.Counter + 1;
         Counter_Global_Lock.Unlock;
      end if;
   end Inc_Usage;

   ---------------
   -- Dec_Usage --
   ---------------

   procedure Dec_Usage (Obj : in out Ref_Ptr) is
   begin
      if Obj.Counter /= -1 then
         Counter_Global_Lock.Lock;
         Obj.Counter := Obj.Counter - 1;
         Counter_Global_Lock.Unlock;

         if Obj.Counter = 0 then
            pragma Debug
              (O ("dec_usage: deallocate " &
                  Ada.Tags.External_Tag (Obj.all'Tag)));
            Free (Obj);
         end if;
      else
         pragma Debug (O ("dec_usage: Counter = -1"));
         null;
      end if;
   end Dec_Usage;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Ref) is
   begin
      null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Ref) is
   begin
      if Object.A_Ref /= null then
         Inc_Usage (Object.A_Ref);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Ref) is
   begin
      if Object.A_Ref /= null then
         Dec_Usage (Object.A_Ref);
      end if;
   end Finalize;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : access Buffer_Type;
      Value  : in Ref_Type) is
   begin
      Broca.Exceptions.Raise_Marshal;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Value  : out Ref_Type) is
   begin
      raise Program_Error;
   end Unmarshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall_Reference
     (Buffer : access Buffer_Type;
      Value  : in Ref) is
   begin
      if Value.A_Ref = null then
         Broca.Exceptions.Raise_Marshal;
      end if;
      Marshall (Buffer, Value.A_Ref.all);
   end Marshall_Reference;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall_Reference
     (Buffer : access Buffer_Type;
      Value  : out Ref)
   is
      New_Ref : Ref;
      Obj : constant Ref_Ptr
        := new Broca.Object.Object_Type;
   begin
      Unmarshall (Buffer, Obj.all);
      Set (New_Ref, Obj);
      Value := New_Ref;
   end Unmarshall_Reference;

   ---------
   -- Get --
   ---------

   function Get (Self : Ref) return Ref_Ptr is
   begin
      return Self.A_Ref;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Ref; Referenced : Ref_Ptr) is
   begin
      if Self.A_Ref /= null then
         Dec_Usage (Self.A_Ref);
      end if;
      Self.A_Ref := Referenced;
      if Referenced /= null then
         Inc_Usage (Referenced);
      end if;
   end Set;

end Broca.Refs;
