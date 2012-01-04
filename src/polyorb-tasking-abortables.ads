------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . A B O R T A B L E S            --
--                                                                          --
--                                 S p e c                                  --
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

--  Runnables with optional support for abortion

pragma Ada_2005;

with Ada.Tags.Generic_Dispatching_Constructor;

with PolyORB.Tasking.Threads;

package PolyORB.Tasking.Abortables is

   --  pragma Preelaborate;
   --  WAG:64
   --  pragma Preelaborate_05 in Ada.Tags is not always obeyed

   package PTT renames PolyORB.Tasking.Threads;

   ---------------
   -- Abortable --
   ---------------

   --  A Runnable that can be asynchronously aborted (if supported by the
   --  underlying tasking profile).

   type Abortable (R : not null access PTT.Runnable'Class) is
     new PTT.Runnable with null record;
   function Create (R : not null access PTT.Runnable'Class) return Abortable;

   overriding procedure Run (AR : not null access Abortable);
   --  Runs R, but abort if Abort_Run is called

   procedure Run_With_Timeout
     (AR      : not null access Abortable;
      Timeout : Duration;
      Expired : out Boolean);
   --  Like Run but additionally abort if Timeout expires (if supported by the
   --  underlying tasking profile). Timeout = Constants.Forever means no
   --  timeout.

   procedure Abort_Run (AR : not null access Abortable);
   --  Abort current call to Run

   -----------------------
   -- Abortable factory --
   -----------------------

   Abortable_Tag : Ada.Tags.Tag := Abortable'Tag;
   procedure Register_Abortable_Tag (T : Ada.Tags.Tag);

   function Make_Abortable is
     new Ada.Tags.Generic_Dispatching_Constructor
       (T           => Abortable,
        Parameters  => PTT.Runnable'Class,
        Constructor => Create);

   --  WAG:64
   --  Ideally, variable Abortable_Tag should be hidden in the body of this
   --  package, and the instantiation and call to the generic dispatching
   --  constructor hidden in a subprogram. However in GNAT 6.4 a bug causes
   --  this architecture to cause an unwanted early finalization of the
   --  returned Abortable.

end PolyORB.Tasking.Abortables;
