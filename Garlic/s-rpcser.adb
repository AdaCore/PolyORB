------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                    S Y S T E M . R P C . S E R V E R                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions);
with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces;

with System.Garlic;              use System.Garlic;
with System.Garlic.Debug;        use System.Garlic.Debug;
with System.Garlic.Exceptions;   use System.Garlic.Exceptions;
with System.Garlic.Heart;        use System.Garlic.Heart;
with System.Garlic.Options;
with System.Garlic.Priorities;   use System.Garlic.Priorities;
with System.Garlic.Soft_Links;
with System.Garlic.Streams;
with System.Garlic.Tasking;
with System.Garlic.Types;

with System.Garlic.Startup;
pragma Elaborate_All (System.Garlic.Startup);
pragma Warnings (Off, System.Garlic.Startup);

package body System.RPC.Server is

   use type System.Garlic.Types.Partition_ID;
   use type System.Garlic.Streams.Params_Stream_Type;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_RPCSER", "(s-rpcser): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   type Inner_Abort_Handler_Type is
     new Ada.Finalization.Controlled with
      record
         Outer : System.Garlic.Soft_Links.Abort_Handler_Type;
      end record;

   type Outer_Abort_Handler_Type is
     new System.Garlic.Soft_Links.Abort_Handler_Type with
      record
         Inner : Inner_Abort_Handler_Type;
      end record;

   procedure Finalize
     (Handler : in out Inner_Abort_Handler_Type);

   procedure Adjust
     (Self : in out Outer_Abort_Handler_Type);

   function Convert is
      new Ada.Unchecked_Conversion (System.Address, Streams.RPC_Receiver);

   --  This package handles a pool of anonymous tasks which will be used
   --  by System.RPC to handle incoming calls.

   procedure Allocate_Task
     (PID    : in System.Garlic.Types.Partition_ID;
      RPC    : in System.RPC.RPC_Id;
      Params : in System.Garlic.Streams.Params_Stream_Access;
      Async  : in Boolean);
   --  Start a new anonymous task to handle the request

   procedure Abort_Task
     (PID : in System.Garlic.Types.Partition_ID;
      RPC : in System.RPC.RPC_Id);
   --  Abort a running task

   procedure Initialize;
   --  Initialize this package

   procedure Shutdown;
   --  Called on shutdown

   type Task_Identifier;
   type Task_Identifier_Access is access Task_Identifier;

   function Create_Anonymous_Task
     return Task_Identifier_Access;

   procedure Destroy_Anonymous_Task
     (Identifier : in out Task_Identifier_Access);

   task type Anonymous_Task is
      entry  Initialize (Identifier : in Task_Identifier_Access);
      entry  Execute;
      entry  Shutdown;
      pragma Priority (Default_Priority);
      pragma Storage_Size (3_000_000);
   end Anonymous_Task;
   type Anonymous_Task_Access is access Anonymous_Task;
   --  An anonymous task will serve a request. Is the pragma Storage_Size
   --  still needed there ???

   type Task_Identifier is record
      Self   : Anonymous_Task_Access;
      RPC    : RPC_Id;
      PID    : Types.Partition_ID;
      Stop   : System.Garlic.Tasking.Mutex_PO_Access;
      Params : Streams.Params_Stream_Access;
      Async  : Boolean;
      Next   : Task_Identifier_Access;
      Prev   : Task_Identifier_Access;
   end record;
   --  Since it is impossible for a task to get a pointer on itself, it
   --  is transmitted through this structure. Moreover, this allows to
   --  handle a list of free tasks very easily.

   procedure Free is
      new Ada.Unchecked_Deallocation (Task_Identifier, Task_Identifier_Access);

   Low_Mark  : Positive renames System.Garlic.Options.Task_Pool_Low_Bound;
   High_Mark : Positive renames System.Garlic.Options.Task_Pool_High_Bound;
   Max_Mark  : Positive renames System.Garlic.Options.Task_Pool_Max_Bound;

   Allocated_Tasks    : Natural := 0;
   Deallocated_Tasks  : Natural := 0;
   Tasks_Pool_Count   : Natural := 0;
   Idle_Tasks_Count   : Natural := 0;
   Tasks_Pool_Mutex   : System.Garlic.Soft_Links.Mutex_Access;
   Tasks_Pool_Watcher : System.Garlic.Soft_Links.Watcher_Access;
   Idle_Tasks_Queue   : Task_Identifier_Access;
   Used_Tasks_Queue   : Task_Identifier_Access;

   Terminated : Boolean := False;

   procedure Show_Tasks_Pool;
   --  This procedure will print a tasks pool status in debug mode

   procedure Stop_Tasks_Pool;
   --  This procedure will be called upon shutdown

   task type Background_Creation is
      pragma Priority (Background_Creation_Priority);
   end Background_Creation;
   --  This task will have a low priority and create tasks in the background
   --  when they are needed.

   type Background_Creation_Access is access Background_Creation;
   Background_Task : Background_Creation_Access;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task
     (PID : in Types.Partition_ID;
      RPC : in RPC_Id)
   is
      Identifier : Task_Identifier_Access;
   begin
      System.Garlic.Soft_Links.Enter (Tasks_Pool_Mutex);
      Identifier := Used_Tasks_Queue;
      while Identifier /= null
        and then (Identifier.PID /= PID or else Identifier.RPC /= RPC)
      loop
         Identifier := Identifier.Next;
      end loop;
      if Identifier /= null then
         Identifier.Stop.Leave;
      end if;
      System.Garlic.Soft_Links.Leave (Tasks_Pool_Mutex);
   end Abort_Task;

   ------------
   -- Adjust --
   ------------

   procedure Adjust
     (Self : in out Outer_Abort_Handler_Type) is
   begin
      Self.Inner.Outer.PID  := Self.PID;
      Self.Inner.Outer.Wait := Self.Wait;
      Self.Inner.Outer.Key  := Self.Key;
   end Adjust;

   -------------------
   -- Allocate_Task --
   -------------------

   procedure Allocate_Task
     (PID    : in Types.Partition_ID;
      RPC    : in RPC_Id;
      Params : in Streams.Params_Stream_Access;
      Async  : in Boolean)
   is
      Identifier : Task_Identifier_Access;
      Version    : System.Garlic.Types.Version_Id;

   begin
      while not Terminated loop
         System.Garlic.Soft_Links.Enter (Tasks_Pool_Mutex);
         if Idle_Tasks_Count > 0 then
            Identifier       := Idle_Tasks_Queue;
            Idle_Tasks_Queue := Identifier.Next;
            Idle_Tasks_Count := Idle_Tasks_Count - 1;

         elsif Tasks_Pool_Count < Max_Mark then
            Identifier       := Create_Anonymous_Task;

         else
            System.Garlic.Soft_Links.Lookup (Tasks_Pool_Watcher, Version);
         end if;

         if Identifier /= null then
            Identifier.Next       := Used_Tasks_Queue;
            if Identifier.Next /= null then
               Identifier.Next.Prev  := Identifier;
            end if;
            Identifier.Prev       := null;
            Used_Tasks_Queue      := Identifier;

            Identifier.RPC    := RPC;
            Identifier.PID    := PID;
            Identifier.Params := Params;
            Identifier.Async  := Async;

            Identifier.Self.Execute;
         end if;
         System.Garlic.Soft_Links.Leave (Tasks_Pool_Mutex);

         exit when Identifier /= null;

         System.Garlic.Soft_Links.Differ (Tasks_Pool_Watcher, Version);
      end loop;

      if Terminated then
         raise System.RPC.Communication_Error;
      end if;

      Show_Tasks_Pool;
   end Allocate_Task;

   --------------------
   -- Anonymous_Task --
   --------------------

   task body Anonymous_Task is
      Callee    : Types.Partition_ID;
      Receiver  : Streams.RPC_Receiver;
      Result    : Streams.Params_Stream_Access;
      Cancelled : Boolean;
      Priority  : Natural;
      Self      : Task_Identifier_Access;
      Aborted   : Boolean := False;

      use Ada.Exceptions;
   begin
      pragma Debug (D ("Anonymous task starting"));
      select
         accept Initialize (Identifier : in Task_Identifier_Access) do
            Self := Identifier;
         end Initialize;
      or
         terminate;
      end select;

      while Self /= null loop
         pragma Debug (D ("Waiting for a job"));
         select
            accept Execute;
         or
            accept Shutdown do
               Aborted := True;
            end Shutdown;
         or
            terminate;
         end select;

         exit when Aborted;

         --  Before executing anything, make sure that our elaboration is
         --  finished.

         Wait_For_Elaboration_Completion;

         Result    := new Streams.Params_Stream_Type (0);
         Cancelled := False;
         Types.Partition_ID'Read (Self.Params, Callee);
         if not Callee'Valid then
            pragma Debug (D ("Invalid PID received"));
            raise Constraint_Error;
         end if;
         Natural'Read (Self.Params, Priority);
         System.Garlic.Soft_Links.Set_Priority (Priority);
         When_Established;
         select
            Self.Stop.Enter;
            declare
               Empty  : aliased Streams.Params_Stream_Type (0);
               Header : constant RPC_Header := (Abortion_Reply, Self.RPC);
               Error  : aliased Error_Type;
            begin
               pragma Debug (D ("Abortion queried by caller"));
               Insert_RPC_Header (Empty'Access, Header);
               Send (Self.PID, Remote_Call, Empty'Access, Error);
               if Found (Error) then
                  Raise_Exception (Communication_Error'Identity,
                                   Content (Error'Access));
               end if;
               Cancelled := True;
            end;
         then abort
            pragma Debug (D ("Job to achieve"));
            Receiver := Convert
               (System.Address (Interfaces.Unsigned_64'Input (Self.Params)));
            Receiver (Self.Params, Result);
            pragma Debug (D ("Job achieved without abortion"));
         end select;

         declare
            Copy : Streams.Params_Stream_Access := Self.Params;
         begin

            --  Yes, we deallocate a copy, because Params is readonly (it's
            --  a discriminant). We must *not* use Params later in this task.

            Streams.Deallocate (Copy);
         end;
         if Self.Async or else Cancelled then
            pragma Debug (D ("Result not sent"));
            Streams.Deallocate (Result);
         else
            declare
               Header : constant RPC_Header := (RPC_Reply, Self.RPC);
               Error  : aliased Error_Type;
            begin
               pragma Debug (D ("Result will be sent"));
               Insert_RPC_Header (Result, Header);
               Send (Self.PID, Remote_Call, Result, Error);
               if Found (Error) then
                  Raise_Exception (Communication_Error'Identity,
                                   Content (Error'Access));
               end if;
               Streams.Free (Result);
            end;
         end if;
         pragma Debug (D ("Job finished, queuing"));

         System.Garlic.Soft_Links.Enter (Tasks_Pool_Mutex);
         if Self.Prev = null then
            Used_Tasks_Queue := Self.Next;
         else
            Self.Prev.Next   := Self.Next;
         end if;
         if Self.Next /= null then
            Self.Next.Prev := Self.Prev;
         end if;

         if Idle_Tasks_Count < High_Mark then
            Self.Prev        := null;
            Self.Next        := Idle_Tasks_Queue;
            Idle_Tasks_Queue := Self;
            Idle_Tasks_Count := Idle_Tasks_Count + 1;

         else
            Destroy_Anonymous_Task (Self);
         end if;

         System.Garlic.Soft_Links.Update (Tasks_Pool_Watcher);
         System.Garlic.Soft_Links.Leave  (Tasks_Pool_Mutex);
      end loop;
   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D ("Error in anonymous task " &
                          "(exception " & Exception_Name (E) & ")"));
         Destroy_Anonymous_Task (Self);
   end Anonymous_Task;

   -------------------------
   -- Background_Creation --
   -------------------------

   task body Background_Creation is
      Identifier : Task_Identifier_Access;

   begin
      System.Garlic.Soft_Links.Add_Non_Terminating_Task;
      while not Terminated loop
         System.Garlic.Soft_Links.Enter (Tasks_Pool_Mutex);
         if Tasks_Pool_Count < Low_Mark then
            Identifier       := Create_Anonymous_Task;
            Identifier.Next  := Idle_Tasks_Queue;
            Idle_Tasks_Queue := Identifier;

         else
            Identifier := null;
         end if;
         System.Garlic.Soft_Links.Leave (Tasks_Pool_Mutex);

         exit when Identifier = null;
      end loop;
      System.Garlic.Soft_Links.Sub_Non_Terminating_Task;
   end Background_Creation;

   ---------------------------
   -- Create_Anonymous_Task --
   ---------------------------

   function Create_Anonymous_Task return Task_Identifier_Access is
      Identifier : constant Task_Identifier_Access := new Task_Identifier;
   begin
      Allocated_Tasks  := Allocated_Tasks + 1;
      Tasks_Pool_Count := Tasks_Pool_Count + 1;
      Identifier.Self  := new Anonymous_Task;
      Identifier.Stop  := new System.Garlic.Tasking.Mutex_PO;
      Identifier.Stop.Enter;
      Identifier.Self.Initialize (Identifier);
      return Identifier;
   end Create_Anonymous_Task;

   ----------------------------
   -- Destroy_Anonymous_Task --
   ----------------------------

   procedure Destroy_Anonymous_Task
     (Identifier : in out Task_Identifier_Access) is
   begin
      Deallocated_Tasks := Deallocated_Tasks + 1;
      Tasks_Pool_Count  := Tasks_Pool_Count - 1;
      System.Garlic.Tasking.Free (Identifier.Stop);
      Free (Identifier);
   end Destroy_Anonymous_Task;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Handler : in out Inner_Abort_Handler_Type) is
   begin
      Finalize (Handler.Outer.PID, Handler.Outer.Wait, Handler.Outer.Key);
   end Finalize;

   ---------------------
   -- Show_Tasks_Pool --
   ---------------------

   procedure Show_Tasks_Pool is
   begin
      return;
      pragma Debug (D ("Idle Tasks Count :" & Idle_Tasks_Count'Img));
      pragma Debug (D ("Tasks Pool Count :" & Tasks_Pool_Count'Img));
      pragma Debug (D ("Allocated   Tasks:" & Allocated_Tasks'Img));
      pragma Debug (D ("Deallocated Tasks:" & Deallocated_Tasks'Img));
      null;
   end Show_Tasks_Pool;

   ---------------------
   -- Stop_Tasks_Pool --
   ---------------------

   procedure Stop_Tasks_Pool is
   begin
      pragma Debug (D ("Stop tasks pool"));
      Terminated := True;
      System.Garlic.Soft_Links.Update (Tasks_Pool_Watcher);
   end Stop_Tasks_Pool;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Handler : System.Garlic.Soft_Links.Abort_Handler_Access
        := new Outer_Abort_Handler_Type;

   begin
      Background_Task := new Background_Creation;

      --  This handler will be finalized. We must initialized its
      --  internal values correctly.

      Handler.PID  := 0;
      Handler.Wait := False;
      Handler.Key  := 0;

      System.Garlic.Soft_Links.Adjust (Handler.all);
      System.Garlic.Soft_Links.Register_Abort_Handler (Handler);
   end Initialize;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      pragma Debug (D ("Shutdown called"));
      Show_Tasks_Pool;
      Terminated := True;
      System.Garlic.Soft_Links.Enter (Tasks_Pool_Mutex);
      while Idle_Tasks_Queue /= null loop
         Idle_Tasks_Queue.Self.Shutdown;
         Idle_Tasks_Queue := Idle_Tasks_Queue.Next;
      end loop;
      System.Garlic.Soft_Links.Update (Tasks_Pool_Watcher);
      System.Garlic.Soft_Links.Leave (Tasks_Pool_Mutex);
   end Shutdown;

begin
   System.Garlic.Soft_Links.Create (Tasks_Pool_Mutex);
   System.Garlic.Soft_Links.Create (Tasks_Pool_Watcher);

   Register_Task_Pool
     (Allocate_Task => Allocate_Task'Access,
      Abort_Task    => Abort_Task'Access,
      Initialize    => Initialize'Access,
      Shutdown      => Shutdown'Access);
end System.RPC.Server;
