------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . T E R M I N A T I O N _ A C T I V I T Y          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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
