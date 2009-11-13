------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SMART_POINTERS.SYNC_COUNTERS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2009, Free Software Foundation, Inc.             --
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

with PolyORB.Tasking.Mutexes; use PolyORB.Tasking.Mutexes;

separate (PolyORB.Smart_Pointers) package body Sync_Counters is

   Counter_Lock : Mutex_Access;
   --  Global lock used to protect concurrent accesses to reference counters

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Counter_Lock);
   end Initialize;

   ------------------------
   -- Sync_Add_And_Fetch --
   ------------------------

   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32
   is
      Result : Interfaces.Integer_32;
   begin
      Enter (Counter_Lock);
      Ptr.all := Ptr.all + Value;
      Result := Ptr.all;
      Leave (Counter_Lock);
      return Result;
   end Sync_Add_And_Fetch;

end Sync_Counters;
