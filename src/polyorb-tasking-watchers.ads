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

with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

package PolyORB.Tasking.Watchers is

   pragma Preelaborate;

   type Watcher_Type is limited private;
   --  A watcher is advanced barrier; version numbers are associated
   --  to signals.  A task can get a version number and wait on the
   --  watcher until their versions differs . Another task can then
   --  release the waiting tasks by calling Update.

   type Watcher_Access is access all Watcher_Type;

   type Version_Id is private;
   --  Type of the version stored in the Watcher.

   function "<" (L, R : Version_Id) return Boolean;

   procedure Create
     (W : in out Watcher_Type);
   --  The object must have been allocated by the client of this package.
   --  XXX ????

   procedure Create (W : out Watcher_Access);
   --  Create a watcher.

   procedure Destroy
     (W : in out Watcher_Type);
   --  The deallocation, if needed, after "Destroy" and is
   --  the responsability of the client of this package.
   --  XXX ???

   procedure Destroy (W : in out Watcher_Access);
   pragma Inline (Destroy);
   --  Destroy the watcher.

   procedure Differ
     (W : in out Watcher_Type;
      V : in Version_Id);

   procedure Differ (W : in Watcher_Access; V : in Version_Id);
   pragma Inline (Differ);

   --  Await until W's version differs from V. V must be a Version
   --  value obtained from a previous call to Lookup on the same watcher.

   procedure Lookup
     (W : in Watcher_Type;
      V : out Version_Id);

   procedure Lookup (W : in Watcher_Access; V : out Version_Id);
   pragma Inline (Lookup);
   --  Fetch W's version.

   procedure Update
     (W : in out Watcher_Type);

   procedure Update (W : in Watcher_Access);
   pragma Inline (Update);
   --  Increment  version

private

   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   type Version_Id is mod 2 ** 8;
   --  No computation is allowed on Version_Ids outside of implementations
   --  of the Watcher interface.

   No_Version : constant Version_Id := 0;

   type Watcher_Type is record
      Version     : Version_Id := No_Version;
      --  The current version of the Watcher.

      WMutex     : Mutex_Access;
      --  The mutex used for the synchronisation in the watcher.

      Await_Count : Integer;
      --  Number of tasks waiting on Differ.


      --  Several conditions used by the algorithm :

      Passing     : Boolean;
      WCondition  : Condition_Access;

   end record;

end PolyORB.Tasking.Watchers;
