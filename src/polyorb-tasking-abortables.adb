------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . A B O R T A B L E S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

package body PolyORB.Tasking.Abortables is

   Initialized : Boolean      := False;

   ---------------
   -- Abort_Run --
   ---------------

   procedure Abort_Run (AR : not null access Abortable) is
   begin
      --  By default abortion is not supported and this opeartion has no effect
      null;
   end Abort_Run;

   ------------
   -- Create --
   ------------

   function Create (R : not null access PTT.Runnable'Class) return Abortable is
   begin
      return Abortable'(R => PTT.Runnable_Access (R));
   end Create;

   ----------------------------
   -- Register_Abortable_Tag --
   ----------------------------

   procedure Register_Abortable_Tag (T : Ada.Tags.Tag) is
   begin
      pragma Assert (not Initialized);

      if not Initialized then
         Abortable_Tag := T;
         Initialized := True;
      end if;
   end Register_Abortable_Tag;

   ---------
   -- Run --
   ---------

   overriding procedure Run (AR : not null access Abortable) is
   begin
      PTT.Run (AR.R);
   end Run;

   ----------------------
   -- Run_With_Timeout --
   ----------------------

   procedure Run_With_Timeout
     (AR      : not null access Abortable;
      Timeout : Duration;
      Expired : out Boolean)
   is
      pragma Unreferenced (Timeout);
   begin
      Expired := False;
      AR.Run;
   end Run_With_Timeout;

end PolyORB.Tasking.Abortables;
