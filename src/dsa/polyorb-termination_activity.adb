------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . T E R M I N A T I O N _ A C T I V I T Y          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;

package body PolyORB.Termination_Activity is

   use PolyORB.Tasking.Mutexes;

   procedure Initialize;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active return Boolean
   is
      Result : Boolean := False;
   begin
      Enter (Lock);
      Result := Activity_Counter > 0;
      Leave (Lock);
      return Result;
   end Is_Active;

   ------------------------
   -- Increment_Activity --
   ------------------------

   procedure Increment_Activity is
   begin
      Enter (Lock);
      Activity_Counter := Activity_Counter + 1;
      Leave (Lock);
   end Increment_Activity;

   --------------------
   -- Reset_Activity --
   --------------------

   procedure Reset_Activity is
   begin
      Enter (Lock);
      Activity_Counter := 0;
      Leave (Lock);
   end Reset_Activity;

   ------------------------
   -- Decrement_Activity --
   ------------------------

   procedure Decrement_Activity is
   begin
      Enter (Lock);
      if Activity_Counter > 0 then
         Activity_Counter := Activity_Counter - 1;
      end if;
      Leave (Lock);
   end Decrement_Activity;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Lock);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"termination.activity",
       Conflicts => Empty,
       Depends   => +"tasking.mutexes",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Termination_Activity;
