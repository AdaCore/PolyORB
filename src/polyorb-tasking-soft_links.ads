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

with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Watchers;
with PolyORB.Tasking.Advanced_Mutexes;

package PolyORB.Tasking.Soft_Links is

   pragma Elaborate_Body;

   type Version_Id is mod 2 ** 8;
   No_Version : constant Version_Id;

   function "<" (L, R : Version_Id) return Boolean;

   -------------------
   -- General types --
   -------------------

   type Parameterless_Procedure is access procedure;

   -------------------------------------------
   -- Critical Section for ORB with Tasking --
   -------------------------------------------

   procedure Enter_Critical_Section;

   procedure Leave_Critical_Section;

   --------------------
   -- Advanced_Mutex --
   --------------------

   type Adv_Mutex_Type is tagged private;
   type Adv_Mutex_Access is access all Adv_Mutex_Type'Class;

   function Create return Adv_Mutex_Access;

   procedure Create (M : out Adv_Mutex_Access);
   pragma Inline (Create);

   procedure Destroy (M : in out Adv_Mutex_Type);

   procedure Destroy (M : in out Adv_Mutex_Access);
   pragma Inline (Destroy);

   procedure Enter (M : in Adv_Mutex_Type);

   procedure Enter (M : in Adv_Mutex_Access);
   pragma Inline (Enter);

   procedure Leave (M : in Adv_Mutex_Type);

   procedure Leave (M : in Adv_Mutex_Access);
   pragma Inline (Leave);

   -----------
   -- Mutex --
   -----------

   type Mutex_Type is tagged private;
   type Mutex_Access is access all Mutex_Type'Class;

   function Create return Mutex_Access;

   procedure Create (M : out Mutex_Access);
   pragma Inline (Create);

   procedure Destroy (M : in out Mutex_Type);

   procedure Destroy (M : in out Mutex_Access);
   pragma Inline (Destroy);

   procedure Enter (M : in Mutex_Type);

   procedure Enter (M : in Mutex_Access);
   pragma Inline (Enter);

   procedure Leave (M : in Mutex_Type);

   procedure Leave (M : in Mutex_Access);
   pragma Inline (Leave);

   -------------
   -- Watcher --
   -------------

   type Watcher_Type is tagged private;
   type Watcher_Access is access all Watcher_Type'Class;

   function Create return Watcher_Access;

   procedure Create (W : out Watcher_Access);
   pragma Inline (Create);

   procedure Destroy (W : in out Watcher_Type);

   procedure Destroy (W : in out Watcher_Access);
   pragma Inline (Destroy);

   procedure Differ (W : in Watcher_Type; V : in Version_Id);

   procedure Differ (W : in Watcher_Access; V : in Version_Id);
   pragma Inline (Differ);
   --  Await until W version differs from V

   procedure Lookup (W : in Watcher_Type; V : out Version_Id);

   procedure Lookup (W : in Watcher_Access; V : out Version_Id);
   pragma Inline (Lookup);
   --  Fetch W version

   procedure Update (W : in Watcher_Type);

   procedure Update (W : in Watcher_Access);
   pragma Inline (Update);
   --  Increment W version

   ---------------------
   -- Task management --
   ---------------------

   type Task_Id is tagged private;

   procedure Create_Task
     (Main : Parameterless_Procedure);

   function Current_Task return Task_Id'Class;
   function Null_Task return Task_Id'Class;

   function Image (T : Task_Id) return String;
   pragma Inline (Image);

   function To_Integer (T : Task_Id) return Integer;
   pragma Inline (To_Integer);

private

   No_Version : constant Version_Id := 0;

   type  Mutex_Type is tagged record
     M : PolyORB.Tasking.Mutexes.Mutex_Access;
   end record;

   type Watcher_Type is tagged record
     W : PolyORB.Tasking.Watchers.Watcher_Access;
   end record;

   type Adv_Mutex_Type is tagged record
     X : PolyORB.Tasking.Advanced_Mutexes.Adv_Mutex_Access;
   end record;

   type Task_Id is tagged record
     X : PolyORB.Tasking.Threads.Thread_Id_Access;
   end record;

end PolyORB.Tasking.Soft_Links;
