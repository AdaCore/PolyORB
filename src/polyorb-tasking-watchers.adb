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

   package PTM renames PolyORB.Tasking.Monitors;

   --  Initial value of the condition :

   Passing               : constant Boolean := True;

   ------------
   -- Create --
   ------------

   procedure Create
     (W : in out Watcher_Type) is
      My_Monitor_Factory : constant Monitor_Factory_Access
        := PTM.Get_Monitor_Factory;
   begin
      W.Version := Version_Id'First;
      W.Monitor := PTM.Create (My_Monitor_Factory);
      W.Passing_Condition.Passing := Passing;
      W.Not_Passing_Condition.Passing := not Passing;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (W : in out Watcher_Type) is
      My_Monitor_Factory : constant Monitor_Factory_Access
        := PTM.Get_Monitor_Factory;
   begin
      PTM.Destroy (My_Monitor_Factory.all, W.Monitor);
   end Destroy;

   ------------
   -- Differ --
   ------------

   procedure Differ
     (W : in out Watcher_Type;
      V : in Version_Id) is
   begin
      PTM.Enter (W.Monitor.all);
      while W.Version = V loop
         --  Real_Wait :
         pragma Assert (W.Passing_Condition.Passing
                        xor W.Not_Passing_Condition.Passing);
         PTM.Wait
           (W.Monitor.all,
            W.Passing_Condition'Access);
         pragma Assert (W.Passing_Condition.Passing
                        xor W.Not_Passing_Condition.Passing);
         if W.Version = V then
            W.Await_Count := W.Await_Count + 1;
            PTM.Wait (W.Monitor.all, W.Not_Passing_Condition'Access);
            pragma Assert (W.Passing_Condition.Passing
                           xor W.Not_Passing_Condition.Passing);
            W.Await_Count := W.Await_Count - 1;
            if W.Await_Count = 0 then
               W.Passing_Condition.Passing := True;
               W.Not_Passing_Condition.Passing := False;
               PTM.Signal (W.Monitor.all);
            end if;
         end if;
      end loop;
      PTM.Leave (W.Monitor.all);
   end Differ;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (C : in out Watcher_Condition_Type;
      B : out Boolean) is
      pragma Warnings (Off);
      pragma Unreferenced (C);
      pragma Warning (On);
   begin
      --  B := C.First /= C.W.Version;
      B := C.Passing;
   end Evaluate;

   ------------
   -- Lookup --
   ------------

   procedure Lookup
     (W : in Watcher_Type;
      V : out Version_Id) is
   begin
      Enter (W.Monitor.all);
      V := W.Version;
      Leave (W.Monitor.all);
   end Lookup;

   ------------
   -- Update --
   ------------

   procedure Update
     (W : in out Watcher_Type) is
   begin
      Enter (W.Monitor.all);
      W.Version := W.Version + 1;
      if W.Await_Count /= 0 then
         W.Passing_Condition.Passing := False;
         W.Not_Passing_Condition.Passing := True;
         Signal (W.Monitor.all);
      end if;
      Leave (W.Monitor.all);
   end Update;

end PolyORB.Tasking.Watchers;
