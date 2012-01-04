------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . T E R M I N A T I O N _ A C T I V I T Y          --
--                                                                          --
--                                 S p e c                                  --
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

--  This package manages the termination manager activity counter

with PolyORB.Tasking.Mutexes;

package PolyORB.Termination_Activity is

   function Is_Active return Boolean;
   --  Returns true iff local node has sent messages since last termination
   --  wave.

   procedure Increment_Activity;
   --  Increment activity counter by one

   procedure Reset_Activity;
   --  Set to zero activity counter

   procedure Decrement_Activity;
   --  Decrement activity counter by one

private
   Lock : PolyORB.Tasking.Mutexes.Mutex_Access;
   --  The lock ensuring integrity of the Activity Counter

   Activity_Counter : Natural := 0;
   --  Number of sent messages since the last wave by this node (not counting
   --  messages sent by the Termination Manager).

end PolyORB.Termination_Activity;
