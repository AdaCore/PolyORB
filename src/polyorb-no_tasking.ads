------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . N O _ T A S K I N G                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  $Id$

with PolyORB.Soft_Links;

package PolyORB.No_Tasking is

   pragma Elaborate_Body;

   procedure Initialize;

   ----------------------------------------------
   -- Critical Section for ORB with no tasking --
   ----------------------------------------------

   procedure Enter_Critical_Section;

   procedure Leave_Critical_Section;

   ----------------------------------
   -- Barrier for ORB with Tasking --
   ----------------------------------

   type Unprotected_Barrier_Type is new Soft_Links.Barrier_Type with private;

   function Create return Soft_Links.Barrier_Access;

   procedure Destroy (B : in out Unprotected_Barrier_Type);

   procedure Signal
     (B : in Unprotected_Barrier_Type;
      N : in Positive := 1);

   procedure Signal_All
     (B : in Unprotected_Barrier_Type;
      P : in Boolean := True);

   procedure Wait (B : in Unprotected_Barrier_Type);

   --------------------------------
   -- Mutex for PCS with Tasking --
   --------------------------------

   type Unprotected_Mutex_Type is new Soft_Links.Mutex_Type with private;

   function Create return Soft_Links.Mutex_Access;

   procedure Enter (M : in Unprotected_Mutex_Type);

   procedure Destroy (M : in out Unprotected_Mutex_Type);

   procedure Leave (M : in Unprotected_Mutex_Type);

   ----------------------------------
   -- Watcher for PCS with Tasking --
   ----------------------------------

   type Unprotected_Watcher_Type is new Soft_Links.Watcher_Type with private;

   function Create return Soft_Links.Watcher_Access;

   procedure Destroy (W : in out Unprotected_Watcher_Type);

   procedure Differ
     (W : in Unprotected_Watcher_Type;
      V : in Soft_Links.Version_Id);

   procedure Lookup
     (W : in Unprotected_Watcher_Type;
      V : out Soft_Links.Version_Id);

   procedure Update (W : in Unprotected_Watcher_Type);

   -----------------------------------------
   -- Advanced Mutex for PCS with Tasking --
   -----------------------------------------

   type Unprotected_Adv_Mutex_Type is
     new Soft_Links.Adv_Mutex_Type with private;

   function Create return Soft_Links.Adv_Mutex_Access;

   procedure Enter (M : in Unprotected_Adv_Mutex_Type);

   procedure Destroy (M : in out Unprotected_Adv_Mutex_Type);

   procedure Leave (M : in Unprotected_Adv_Mutex_Type);

   -------------------------
   -- Task identification --
   -------------------------

   type No_Task_Id is new Soft_Links.Task_Id with private;

   function Get_Current_Task return Soft_Links.Task_Id'Class;
   function Get_Null_Task return Soft_Links.Task_Id'Class;

   function Image (T : No_Task_Id) return String;
   pragma Inline (Image);

   function To_Integer (T : No_Task_Id) return Integer;
   pragma Inline (To_Integer);

private

   type Unprotected_Mutex_Type is
     new Soft_Links.Mutex_Type with null record;

   type Unprotected_Adv_Mutex_Type is
     new Soft_Links.Adv_Mutex_Type with null record;

   type Unprotected_Barrier_Data is record
      Free : Natural := 0;
      Perm : Boolean := False;
   end record;

   type Unprotected_Barrier_Data_Access is
     access all Unprotected_Barrier_Data;

   type Unprotected_Barrier_Type is
     new Soft_Links.Barrier_Type with
      record
         X : Unprotected_Barrier_Data_Access;
      end record;

   type Unprotected_Watcher_Data is record
      Version : Soft_Links.Version_Id;
   end record;

   type Unprotected_Watcher_Data_Access is
     access all Unprotected_Watcher_Data;

   type Unprotected_Watcher_Type is
     new Soft_Links.Watcher_Type with
      record
         X : Unprotected_Watcher_Data_Access;
      end record;

   type No_Task_Id is new Soft_Links.Task_Id with record
      Is_Null : Boolean := True;
   end record;

end PolyORB.No_Tasking;
