------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . T A S K I N G . W A T C H E R S              --
--                                                                          --
--                                 B o d y                                  --
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

--  This package implements Watchers.

--  $Id$

with PolyORB.Log;

package body PolyORB.Tasking.Watchers is

   use PolyORB.Log;

   package PTM renames PolyORB.Tasking.Mutexes;

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.tasking.watchers");

   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   ------------
   -- Create --
   ------------

   procedure Create (W : in out Watcher_Type) is
   begin
      pragma Debug (O ("Create a watcher"));
      W.Version := Version_Id'First;
      PTM.Create (W.WMutex);
      PTCV.Create (W.WCondition);
      W.Passing := True;
      W.Await_Count := 0;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (W : in out Watcher_Type) is
   begin
      pragma Debug (O ("Destroy a watcher"));
      PTM.Destroy (W.WMutex);
      PTCV.Destroy (W.WCondition);
   end Destroy;

   ------------
   -- Differ --
   ------------

   procedure Differ
     (W : in out Watcher_Type;
      V : in Version_Id) is
   begin
      PTM.Enter (W.WMutex);
      pragma Debug (O ("Call to differ"));

      while W.Version = V loop
         while not W.Passing loop
            pragma Debug (O ("Wait for update"));
            PTCV.Wait (W.WCondition, W.WMutex);
            pragma Debug (O ("Reevaluation"));
         end loop;

         if W.Version = V then
            W.Await_Count := W.Await_Count + 1;
            while W.Passing loop
               PTCV.Wait (W.WCondition, W.WMutex);
            end loop;
            W.Await_Count := W.Await_Count - 1;

            if W.Await_Count = 0 then
               W.Passing := True;
               PTCV.Broadcast (W.WCondition);
            end if;

         end if;

      end loop;
      pragma Debug (O ("end of Differ"));
      PTM.Leave (W.WMutex);
   end Differ;

   ------------
   -- Lookup --
   ------------

   procedure Lookup
     (W : in Watcher_Type;
      V : out Version_Id) is
   begin
      Enter (W.WMutex);
      pragma Debug (O ("Call to Lookup"));
      V := W.Version;
      Leave (W.WMutex);
   end Lookup;

   ------------
   -- Update --
   ------------

   procedure Update
     (W : in out Watcher_Type) is
   begin
      Enter (W.WMutex);
      pragma Debug (O ("Update"));
      W.Version := W.Version + 1;
      if W.Await_Count /= 0 then
         pragma Debug (O ("Update provokes a reevaluation"));
         W.Passing := False;
         Broadcast (W.WCondition);
      end if;
      Leave (W.WMutex);
   end Update;

end PolyORB.Tasking.Watchers;
