------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . W A T C H E R S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides watchers.
--  Watchers are objects that manage a version number, that can be incremented.
--  A task can wait on a watcher until its version number is changed.

--  $Id$

with PolyORB.Tasking.Monitors;

package PolyORB.Tasking.Watchers is

   pragma Preelaborate;

   type Watcher_Type is limited private;
   --  A watcher is advanced barrier; version numbers are associated
   --  to signals.  A task can get a version number and wait on the
   --  watcher until their versions differs . Another task can then
   --  release the waiting tasks by calling Update.

   type Watcher_Access is access all Watcher_Type;

   type Version_Id is mod 2 ** 8;
   --  Type of the version stored in the Watcher.
   --  XXX This type is not private for the moment, but it
   --      will as soon as PolyORB.Soft_Links will not be used anymore.
   --      No computation is allowed on Version_Ids outside of implementations
   --      of the Watcher interface.

   procedure Differ
     (W : in out Watcher_Type;
      V : in Version_Id);
   --  Await until W's version differs from V. V must be a Version
   --  value obtained from a previous call to Lookup on the same watcher.

   procedure Lookup
     (W : in Watcher_Type;
      V : out Version_Id);
   --  Fetch W's version.

   procedure Update
     (W : in out Watcher_Type);
   --  Increment  version

   procedure Create
     (W : in out Watcher_Type);
   --  Create a watcher.
   --  The object must have been allocated by the client of this package.

   procedure Destroy
     (W : in out Watcher_Type);
   --  Destroy the watcher.
   --  The deallocation, if needed, after "Destroy" and is
   --  the responsability of the client of this package.

private

   use PolyORB.Tasking.Monitors;

   No_Version : constant Version_Id := 0;

   type Watcher_Condition_Type is new Condition_Type with record
      Passing : Boolean := False;
   end record;
   --  Type of the Conditions used by the algorithm of Differ.
   --  Simple boolean condition.

   procedure Evaluate (C : in out Watcher_Condition_Type;
                       B : out Boolean);
   --  Return the value of the internal boolean.

   type Watcher_Type is record
      Version  : Version_Id := No_Version;
      --  The current version of the Watcher.

      Monitor  : Monitor_Access;
      --  The monitors used for the synchronisation in the watcher.

      Await_Count           : Integer := 0;
      --  Number of tasks waiting on Differ.


      --  Several conditions used by the algorithm :

      Passing_Condition     : aliased Watcher_Condition_Type;

      Not_Passing_Condition : aliased Watcher_Condition_Type;

   end record;

end PolyORB.Tasking.Watchers;
