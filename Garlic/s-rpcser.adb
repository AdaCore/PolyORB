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
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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
with System.Garlic.Priorities;
with System.Garlic.Priorities.Mapping;
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
     (Partition : in System.Garlic.Types.Partition_ID;
      Session   : in System.RPC.Session_Type;
      Stamp     : in System.Garlic.Types.Stamp_Type;
      Params    : in System.Garlic.Streams.Params_Stream_Access;
      Async     : in Boolean);
   --  Start a new anonymous task to handle the request

   procedure Abort_Task
     (Partition : in System.Garlic.Types.Partition_ID;
      Session   : in System.RPC.Session_Type);
   --  Abort a running task

   procedure Initialize;
   --  Initialize this package

   procedure Shutdown;
   --  Called on shutdown

   type Task_Identifier;
   type Task_Identifier_Access is access Task_Identifier;

   function Create_RPC_Handler
     return Task_Identifier_Access;

   procedure Destroy_RPC_Handler
     (Identifier : in out Task_Identifier_Access);

   task type RPC_Handler is
      entry  Initialize (Identifier : in Task_Identifier_Access);
      entry  Execute;
      entry  Shutdown;
      pragma Priority (System.Priority'Last);
      pragma Storage_Size (2_000_000);
   end RPC_Handler;
   type RPC_Handler_Access is access RPC_Handler;
   --  An anonymous task will serve a request. Is the pragma Storage_Size
   --  still needed there ???

   type Task_Identifier is record
      Handler   : RPC_Handler_Access;
      Session   : Session_Type;
      Partition : Types.Partition_ID;
      Stop      : System.Garlic.Tasking.Mutex_PO_Access;
      Params    : Streams.Params_Stream_Access;
      Stamp     : System.Garlic.Types.Stamp_Type;
      Async     : Boolean;
      Next      : Task_Identifier_Access;
      Prev      : Task_Identifier_Access;
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

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task
     (Partition : in Types.Partition_ID;
      Session   : in Session_Type)
   is
      Identifier : Task_Identifier_Access;
   begin
      System.Garlic.Soft_Links.Enter (Tasks_Pool_Mutex);
      Identifier := Used_Tasks_Queue;
      while Identifier /= null
        and then (Identifier.Partition /= Partition
                  or else Identifier.Session /= Session)
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
      Self.Inner.Outer.Partition     := Self.Partition;
      Self.Inner.Outer.Waiting    := Self.Waiting;
      Self.Inner.Outer.Session := Self.Session;
   end Adjust;

   -------------------
   -- Allocate_Task --
   -------------------

   procedure Allocate_Task
     (Partition : in Types.Partition_ID;
      Session   : in Session_Type;
      Stamp     : in System.Garlic.Types.Stamp_Type;
      Params    : in Streams.Params_Stream_Access;
      Async     : in Boolean)
   is
      Identifier : Task_Identifier_Access;
      Version    : System.Garlic.Types.Version_Id;

   begin

      --  Recycle an anonymous task from the task pool or allocate a
      --  new one depending on the anonymous task pool.

      while not Terminated loop
         System.Garlic.Soft_Links.Enter (Tasks_Pool_Mutex);
         if Idle_Tasks_Count > 0 then
            Identifier       := Idle_Tasks_Queue;
            Idle_Tasks_Queue := Identifier.Next;
            Idle_Tasks_Count := Idle_Tasks_Count - 1;

         elsif Tasks_Pool_Count < Max_Mark then
            Identifier       := Create_RPC_Handler;

         else
            System.Garlic.Soft_Links.Lookup (Tasks_Pool_Watcher, Version);
         end if;

         if Identifier /= null then
            Identifier.Next       := Used_Tasks_Queue;
            if Identifier.Next /= null then
               Identifier.Next.Prev  := Identifier;
            end if;
            Identifier.Prev      := null;
            Used_Tasks_Queue     := Identifier;

            Identifier.Session   := Session;
            Identifier.Partition := Partition;
            Identifier.Params    := Params;
            Identifier.Stamp     := Stamp;
            Identifier.Async     := Async;

            Identifier.Handler.Execute;
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

   -----------------
   -- RPC_Handler --
   -----------------

   task body RPC_Handler is
      Callee    : Types.Partition_ID;
      Receiver  : Streams.RPC_Receiver;
      Result    : Streams.Params_Stream_Access;
      Cancelled : Boolean;
      Priority  : Priorities.Global_Priority;
      Self      : Task_Identifier_Access;
      Aborted   : Boolean := False;

      use Ada.Exceptions;
      use System.Garlic.Priorities;
      use System.Garlic.Priorities.Mapping;

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

         Soft_Links.Set_Stamp (Self.Stamp);

         exit when Aborted;

         --  Before executing anything, make sure that our elaboration is
         --  finished.

         Wait_For_Elaboration_Completion;

         Result    := new Streams.Params_Stream_Type (0);
         Cancelled := False;
         Types.Partition_ID'Read (Self.Params, Callee);
         if not Callee'Valid then
            pragma Debug (D ("Invalid Partition received"));
            raise Constraint_Error;
         end if;
         Global_Priority'Read (Self.Params, Priority);
         if RPC_Handler_Priority_Policy = Client_Propagated then
            System.Garlic.Soft_Links.Set_Priority
              (To_Native_Priority (Priority));
         else
            System.Garlic.Soft_Links.Set_Priority
              (To_Native_Priority (RPC_Handler_Priority));
         end if;
         When_Established;

         select
            Self.Stop.Enter;

            --  This RPC is aborted. Send an abortion reply to recycle
            --  properly Session_Type.

            declare
               Empty  : aliased Streams.Params_Stream_Type (0);
               Header : constant RPC_Header := (Abortion_Reply, Self.Session);
               Error  : aliased Error_Type;
            begin
               pragma Debug (D ("Abortion queried by caller"));
               Insert_RPC_Header (Empty'Access, Header);
               Send (Self.Partition, Remote_Call, Empty'Access, Error);
               if Found (Error) then
                  Raise_Exception (Communication_Error'Identity,
                                   Content (Error'Access));
               end if;
               Cancelled := True;
            end;

         then abort
            pragma Debug (D ("Job to achieve"));

            --  Execute locally remote procedure call. Extract RPC_Receiver
            --  of the package and then dereference it.

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
               Header : constant RPC_Header := (RPC_Reply, Self.Session);
               Error  : aliased Error_Type;
            begin
               pragma Debug (D ("Result will be sent"));
               Insert_RPC_Header (Result, Header);
               Send (Self.Partition, Remote_Call, Result, Error);
               if Found (Error) then
                  Raise_Exception (Communication_Error'Identity,
                                   Content (Error'Access));
               end if;
               Streams.Free (Result);
            end;
         end if;

         pragma Debug (D ("Job finished, queuing"));

         pragma Debug (Soft_Links.Set_Stamp (Types.No_Stamp));

         --  Set RPC handler back to its initial priority.

         System.Garlic.Soft_Links.Set_Priority (System.Priority'Last);

         --  Recycle anonymous task or destroy depending on the
         --  configuration of the anonymous task pool.

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
            Destroy_RPC_Handler (Self);
         end if;

         System.Garlic.Soft_Links.Update (Tasks_Pool_Watcher);
         System.Garlic.Soft_Links.Leave  (Tasks_Pool_Mutex);
      end loop;
   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D ("Error in anonymous task " &
                          "(exception " & Exception_Name (E) & ")"));
         Destroy_RPC_Handler (Self);
   end RPC_Handler;

   ------------------------
   -- Create_RPC_Handler --
   ------------------------

   function Create_RPC_Handler return Task_Identifier_Access is
      Identifier : constant Task_Identifier_Access := new Task_Identifier;
   begin
      Allocated_Tasks    := Allocated_Tasks + 1;
      Tasks_Pool_Count   := Tasks_Pool_Count + 1;
      Identifier.Handler := new RPC_Handler;
      Identifier.Stop    := new System.Garlic.Tasking.Mutex_PO;
      Identifier.Stop.Enter;
      Identifier.Handler.Initialize (Identifier);
      return Identifier;
   end Create_RPC_Handler;

   -------------------------
   -- Destroy_RPC_Handler --
   -------------------------

   procedure Destroy_RPC_Handler
     (Identifier : in out Task_Identifier_Access) is
   begin
      Deallocated_Tasks := Deallocated_Tasks + 1;
      Tasks_Pool_Count  := Tasks_Pool_Count - 1;
      System.Garlic.Tasking.Free (Identifier.Stop);
      Free (Identifier);
   end Destroy_RPC_Handler;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Handler : in out Inner_Abort_Handler_Type) is
   begin
      Finalize
        (Handler.Outer.Partition,
         Handler.Outer.Waiting,
         Session_Type (Handler.Outer.Session));
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Identifier : Task_Identifier_Access;
      Handler    : System.Garlic.Soft_Links.Abort_Handler_Access
        := new Outer_Abort_Handler_Type;

   begin

      --  Preallocate a pool of anonymous tasks

      System.Garlic.Soft_Links.Enter (Tasks_Pool_Mutex);
      while Tasks_Pool_Count < Low_Mark loop
         Identifier       := Create_RPC_Handler;
         Identifier.Next  := Idle_Tasks_Queue;
         Idle_Tasks_Queue := Identifier;
         Idle_Tasks_Count := Idle_Tasks_Count + 1;
      end loop;
      System.Garlic.Soft_Links.Leave (Tasks_Pool_Mutex);

      --  This handler will be finalized. We must initialize its
      --  internal values correctly.

      Handler.Partition := System.Garlic.Types.Null_Partition_ID;
      Handler.Waiting   := False;
      Handler.Session   := Natural (No_Session);

      System.Garlic.Soft_Links.Adjust (Handler.all);
      System.Garlic.Soft_Links.Register_Abort_Handler (Handler);
   end Initialize;

   ---------------------
   -- Show_Tasks_Pool --
   ---------------------

   procedure Show_Tasks_Pool is
   begin
      pragma Debug (D ("Idle Tasks Count :" & Idle_Tasks_Count'Img));
      pragma Debug (D ("Tasks Pool Count :" & Tasks_Pool_Count'Img));
      pragma Debug (D ("Allocated   Tasks:" & Allocated_Tasks'Img));
      pragma Debug (D ("Deallocated Tasks:" & Deallocated_Tasks'Img));
      null;
   end Show_Tasks_Pool;

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
         Idle_Tasks_Queue.Handler.Shutdown;
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
