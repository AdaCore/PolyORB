------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B - T A S K I N G - S O F T _ L I N K S            --
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

--  The package provides an implementation for PolyORB.Soft_Links, using
--  PolyORB.Tasking

--  $Id$

with PolyORB.Soft_Links;
with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Watchers;
with PolyORB.Tasking.Advanced_Mutexes;

package PolyORB.Tasking.Soft_Links is

   pragma Elaborate_Body;

   package PS renames PolyORB.Soft_Links;

   procedure Initialize;

   -------------------------------------------
   -- Critical Section for ORB with Tasking --
   -------------------------------------------

   procedure Enter_Critical_Section;

   procedure Leave_Critical_Section;

   --------------------------------
   -- Mutex for PCS with Tasking --
   --------------------------------

   type Tasking_Mutex_Type is new PS.Mutex_Type with private;

   function Create return PS.Mutex_Access;

   procedure Enter (M : in Tasking_Mutex_Type);

   procedure Destroy (M : in out Tasking_Mutex_Type);

   procedure Leave (M : in Tasking_Mutex_Type);

   ----------------------------------
   -- Watcher for PCS with Tasking --
   ----------------------------------

   type Tasking_Watcher_Type is new PS.Watcher_Type with private;

   function Create return PS.Watcher_Access;

   procedure Destroy (W : in out Tasking_Watcher_Type);

   procedure Differ
     (W : in Tasking_Watcher_Type;
      V : in PS.Version_Id);

   procedure Lookup
     (W : in Tasking_Watcher_Type;
      V : out PS.Version_Id);

   procedure Update (W : in Tasking_Watcher_Type);

   -----------------------------------------
   -- Advanced Mutex for PCS with Tasking --
   -----------------------------------------

   type Tasking_Adv_Mutex_Type is new PS.Adv_Mutex_Type with private;

   function Create return PS.Adv_Mutex_Access;

   procedure Enter (M : in Tasking_Adv_Mutex_Type);

   procedure Destroy (M : in out Tasking_Adv_Mutex_Type);

   procedure Leave (M : in Tasking_Adv_Mutex_Type);

   ---------------------
   -- Task allocation --
   ---------------------

   procedure Create_Task
     (Main : PS.Parameterless_Procedure);

   -------------------------
   -- Task identification --
   -------------------------

   type Tasking_Task_Id is new PS.Task_Id with private;

   function Get_Current_Task return PS.Task_Id'Class;
   function Get_Null_Task return PS.Task_Id'Class;

   function Image (T : Tasking_Task_Id) return String;
   pragma Inline (Image);

   function To_Integer (T : Tasking_Task_Id) return Integer;
   pragma Inline (To_Integer);

private
   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Watchers;
   use PolyORB.Tasking.Advanced_Mutexes;

   type Tasking_Mutex_Type is new PS.Mutex_Type
     with record
        M : Mutex_Access;
     end record;

   type Tasking_Watcher_Type is new PS.Watcher_Type
     with record
        W : Watcher_Access;
     end record;

   type Tasking_Adv_Mutex_Type is new PS.Adv_Mutex_Type
     with record
        X : Adv_Mutex_Access;
     end record;

   type Tasking_Task_Id is new PS.Task_Id
     with record
        X : Thread_Id_Access;
     end record;

end PolyORB.Tasking.Soft_Links;
