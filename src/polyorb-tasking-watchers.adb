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

package body PolyORB.Tasking.Watchers is

   package PTM renames PolyORB.Tasking.Mutexes;

   package PTCV renames PolyORB.Tasking.Condition_Variables;

   ------------
   -- Create --
   ------------

   procedure Create
     (W : in out Watcher_Type) is
      My_Mutex_Factory : constant Mutex_Factory_Access
        := PTM.Get_Mutex_Factory;
      My_Condition_Factory : constant Condition_Factory_Access
        := PTCV.Get_Condition_Factory;
   begin
      W.Version := Version_Id'First;
      W.WMutex := PTM.Create (My_Mutex_Factory);
      W.WCondition := PTCV.Create (My_Condition_Factory);
      W.Passing := True;
      W.Await_Count := 0;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (W : in out Watcher_Type) is
      My_Mutex_Factory : constant Mutex_Factory_Access
        := PTM.Get_Mutex_Factory;
      My_Condition_Factory : constant Condition_Factory_Access
        := PTCV.Get_Condition_Factory;
   begin
      PTM.Destroy (My_Mutex_Factory.all, W.WMutex);
      PTCV.Destroy (My_Condition_Factory.all, W.WCondition);
   end Destroy;

   ------------
   -- Differ --
   ------------

   procedure Differ
     (W : in out Watcher_Type;
      V : in Version_Id) is
   begin
      PTM.Enter (W.WMutex.all);

      while W.Version = V loop
         while not W.Passing loop
            PTCV.Wait
              (W.WCondition.all,
               W.WMutex);
         end loop;

         if W.Version = V then
            W.Await_Count := W.Await_Count + 1;
            while W.Passing loop
               PTCV.Wait (W.WCondition.all, W.WMutex);
            end loop;
            W.Await_Count := W.Await_Count - 1;

            if W.Await_Count = 0 then
               W.Passing := True;
               PTCV.Broadcast (W.WCondition.all);
            end if;

         end if;

      end loop;
      PTM.Leave (W.WMutex.all);
   end Differ;

   ------------
   -- Lookup --
   ------------

   procedure Lookup
     (W : in Watcher_Type;
      V : out Version_Id) is
   begin
      Enter (W.WMutex.all);
      V := W.Version;
      Leave (W.WMutex.all);
   end Lookup;

   ------------
   -- Update --
   ------------

   procedure Update
     (W : in out Watcher_Type) is
   begin
      Enter (W.WMutex.all);
      W.Version := W.Version + 1;
      if W.Await_Count /= 0 then
         W.Passing := False;
         Broadcast (W.WCondition.all);
      end if;
      Leave (W.WMutex.all);
   end Update;

end PolyORB.Tasking.Watchers;
