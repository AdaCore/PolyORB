------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . F U L L _ T A S K I N G . M O N I T O R S         --
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

--  Implementation of monitors under the Full_Tasking profile.

--  $Id$

with Unchecked_Deallocation;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;


package body PolyORB.Full_Tasking.Monitors is

   procedure Initialize;

   ----------
   -- Free --
   ----------

   procedure Free is new Unchecked_Deallocation
     (Full_Tasking_Monitor_Type'Class,
      Full_Tasking_Monitor_Access);

   protected type Monitor_PO is
      --  Protected object which is the real implementation of
      --  Monitor_Type

      entry Enter;
      --  Real implementation of Enter (Monitor_Type).

      procedure Leave;
      --  Real implementation of Leave (Monitor_Type).

      entry Wait_Entry (C : access PTM.Condition_Type'Class);
      --  Real implementation of Wait.
      --  It just test if the condition is fulfilled, and
      --  deleguate the management of the waiting to Real_Wait
      --  and Test_Wait.

      entry Real_Wait (C : access PTM.Condition_Type'Class);
      --  Wait for the monitor to be signaled.

      entry Test_Wait (C : access PTM.Condition_Type'Class);
      --  Test the condition, and free the waiting task or requeue it
      --  to Real_Wait.

      procedure Signal;
      --  Real implementation of Signal (Monitor_Type).

   private
      Locked   : Boolean := False;
      --  False when the lock is free; else True;

      Signaled : Boolean := False;
      --  True when the monitor is signaled, and when the
      --  waiting tasks have not all reevaluated their Condition.
   end Monitor_PO;

   ------------
   -- Create --
   ------------

   function Create
     (MF   : access Full_Tasking_Monitor_Factory_Type;
      Name : String := "")
     return PTM.Monitor_Access is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      --  XXX The use of Name is not yet implemented
      M : Full_Tasking_Monitor_Access := new Full_Tasking_Monitor_Type;
   begin
      M.The_PO := new Monitor_PO;
      return PTM.Monitor_Access (M);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (MF : in out Full_Tasking_Monitor_Factory_Type;
      M  : in out PTM.Monitor_Access) is
      pragma Warnings (Off);
      pragma Unreferenced (MF);
      pragma Warnings (On);
   begin
      Free (Full_Tasking_Monitor_Access (M));
   end Destroy;

   -----------
   -- Enter --
   -----------

   procedure Enter (M : in out Full_Tasking_Monitor_Type) is
   begin
      M.The_PO.Enter;
   end Enter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PTM.Register_Monitor_Factory (PTM.Monitor_Factory_Access
                                    (The_Monitor_Factory));
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (M : in out Full_Tasking_Monitor_Type) is
   begin
      M.The_PO.Leave;
   end Leave;

   ----------------
   -- Monitor_PO --
   ----------------

   protected body Monitor_PO is

      ----------------------
      -- Monitor_PO.Enter --
      ----------------------

      entry Enter when not Locked is
      begin
         Locked := True;
      end Enter;

      ----------------------
      -- Monitor_PO.Leave --
      ----------------------

      procedure Leave is
      begin
         Locked := False;
      end Leave;

      --------------------------
      -- Monitor_PO.Real_Wait --
      --------------------------

      entry Real_Wait (C : access PTM.Condition_Type'Class) when Signaled is
         pragma Warnings (Off);
         pragma Unreferenced (C);
         pragma Warnings (On);
      begin

         if Real_Wait'Count = 0 then
            Signaled := False;
         end if;

         requeue Test_Wait with abort;
      end Real_Wait;

      -----------------------
      -- Monitor_PO.Signal --
      -----------------------

      procedure Signal is
      begin
         if Real_Wait'Count /= 0 then
            Signaled := True;
         end if;
      end Signal;

      --------------------------
      -- Monitor_PO.Test_Wait --
      --------------------------

      entry Test_Wait
        (C : access PTM.Condition_Type'Class) when not Signaled is
         Exit_Condition : Boolean;
      begin
         PTM.Evaluate (C.all, Exit_Condition);

         if  not Exit_Condition then
            requeue Real_Wait with abort;
         end if;

         requeue Enter with abort;
      end Test_Wait;

      ---------------------------
      -- Monitor_PO.Wait_Entry --
      ---------------------------

      entry Wait_Entry (C : access PTM.Condition_Type'Class) when True is
         Exit_Condition : Boolean;
      begin
         if not Locked then
            raise Program_Error;
         end if;

         PTM.Evaluate (C.all, Exit_Condition);

         if  not Exit_Condition then
            Locked := False;
            requeue Test_Wait with abort;
         end if;
      end Wait_Entry;

   end Monitor_PO;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (M : in out Full_Tasking_Monitor_Type) is
   begin
      M.The_PO.Signal;
   end Signal;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (M : in out Full_Tasking_Monitor_Type;
      C : access PTM.Condition_Type'Class) is
   begin
      M.The_PO.Wait_Entry (C);
   end Wait;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"full_tasking-monitors",
       Conflicts => Empty,
       Depends => Empty,
       Provides => +"tasking-monitors",
       Init => Initialize'Access));
end PolyORB.Full_Tasking.Monitors;
