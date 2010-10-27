------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T A S K I N G . A B O R T A B L E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2010, Free Software Foundation, Inc.          --
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

--  Runnables with optional support for abortion

pragma Ada_2005;

with Ada.Tags;

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

   procedure Run (AR : not null access Abortable);
   --  Runs R, but abort if Abort_Run is called

   procedure Abort_Run (AR : not null access Abortable);
   --  Abort current call to Run

   -----------------------
   -- Abortable factory --
   -----------------------

   procedure Register_Abortable_Tag (T : Ada.Tags.Tag);
   function Make_Abortable
     (R : access PTT.Runnable'Class) return Abortable'Class;

end PolyORB.Tasking.Abortables;
