------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B - T A S K I N G - A D V A N C E D _ M U T E X E S      --
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

--  This package provides an implementation of advanced mutexes.

--  $Id$

with Unchecked_Deallocation;

package body PolyORB.Tasking.Advanced_Mutexes is

   package PTT renames PolyORB.Tasking.Threads;


   --  Initial value of the condition :
   --  Passing               : constant Boolean := True;


   procedure Free is
      new Unchecked_Deallocation (PTT.Thread_Id'Class,
                                  PTT.Thread_Id_Access);

   ------------
   -- Create --
   ------------

   procedure Create (M : in out Adv_Mutex_Type) is
      use PolyORB.Tasking.Threads;

      My_Mutex_Factory : constant PTM.Mutex_Factory_Access
         := PTM.Get_Mutex_Factory;
      My_Condition_Factory : constant PTCV.Condition_Factory_Access
         := PTCV.Get_Condition_Factory;
      My_Thread_Factory : constant PTT.Thread_Factory_Access
        := PTT.Get_Thread_Factory;
      Self              : constant PTT.Thread_Id'Class
        := PTT.Get_Current_Thread_Id (My_Thread_Factory);
   begin
      M.Current := new PTT.Thread_Id'Class'(Self);
      M.Empty := True;
      M.MMutex := PTM.Create (My_Mutex_Factory);
      M.MCondition := PTCV.Create (My_Condition_Factory);
      M.Level := 0;
      M.Await_Count := 0;
      M.Passing := True;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (M : in out Adv_Mutex_Type) is
      use PolyORB.Tasking.Threads;
      My_Mutex_Factory : constant PTM.Mutex_Factory_Access
        := PTM.Get_Mutex_Factory;
      My_Condition_Factory : constant PTCV.Condition_Factory_Access
        := PTCV.Get_Condition_Factory;
   begin
      Free (M.Current);
      PTM.Destroy (My_Mutex_Factory.all, M.MMutex);
      PTCV.Destroy (My_Condition_Factory.all, M.MCondition);
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in out Adv_Mutex_Type) is
      use PolyORB.Tasking.Threads;

      My_Thread_Factory : constant PTT.Thread_Factory_Access
        := PTT.Get_Thread_Factory;
      Self              : constant PTT.Thread_Id'Class
        := PTT.Get_Current_Thread_Id (My_Thread_Factory);
   begin
      PTM.Enter (M.MMutex.all);
      while not M.Empty
        and then M.Current.all /= Self loop
         if not M.Passing then
            PTCV.Wait
              (M.MCondition.all,
               M.MMutex);
         end if;
         M.Passing := False;
      end loop;

      M.Empty := False;
      M.Level := M.Level + 1;
      Copy_Thread_Id (My_Thread_Factory, Self, M.Current);
      PTM.Leave (M.MMutex.all);
   end Enter;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in out Adv_Mutex_Type) is
      use PolyORB.Tasking.Threads;
      My_Thread_Factory  : constant PTT.Thread_Factory_Access
        := PTT.Get_Thread_Factory;
      Self               : constant PTT.Thread_Id'Class
        := PTT.Get_Current_Thread_Id (My_Thread_Factory);
   begin
      PTM.Enter (M.MMutex.all);
      pragma Assert (M.Current.all = Self);
      pragma Assert (M.Level > 0);
      M.Level := M.Level - 1;

      if M.Level = 0 then
         M.Empty := True;
         M.Passing := True;
         PTCV.Signal (M.MCondition.all);
      end if;

      PTM.Leave (M.MMutex.all);

   end Leave;

end PolyORB.Tasking.Advanced_Mutexes;
