------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                      S Y S T E M . R P C . P O O L                       --
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

with Ada.Dynamic_Priorities;
with Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions);
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Interfaces;
with System.Garlic;              use System.Garlic;
with System.Garlic.Debug;        use System.Garlic.Debug;
with System.Garlic.Heart;        use System.Garlic.Heart;
with System.Garlic.Options;
with System.Garlic.Priorities;   use System.Garlic.Priorities;
with System.Garlic.Soft_Links;
with System.Garlic.Streams;
with System.Garlic.Types;
with System.Garlic.Utils;        use System.Garlic.Utils;

with System.Garlic.Startup;
pragma Elaborate_All (System.Garlic.Startup);
pragma Warnings (Off, System.Garlic.Startup);

package body System.RPC.Pool is

   use type System.Garlic.Types.Partition_ID;
   use type System.Garlic.Streams.Params_Stream_Type;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_RPCPOO", "(s-rpcpoo): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Max_Tasks : constant := 512;
   --  This one must match the maximum value of Task_Pool_Max_Bound defined
   --  in s-garopt.ads. Use a named constant somewhere to make sure that
   --  they match ???

   function Convert is
      new Ada.Unchecked_Conversion
     (System.Address, Streams.RPC_Receiver);

   type Cancel_Type is record
      PID  : Types.Partition_ID;
      RPC  : RPC_Id;
      Free : Boolean := True;
   end record;

   type Cancel_Array is array (1 .. Max_Tasks) of Cancel_Type;

   protected Task_Manager is
      entry Get_One;

      procedure Free_One;

      procedure Abort_One
        (PID : in Types.Partition_ID;
         RPC : in RPC_Id);

      procedure Unabort_One
        (PID : in Types.Partition_ID;
         RPC : in RPC_Id);

      entry Is_Aborted
        (PID : in Types.Partition_ID;
         RPC : in RPC_Id);

      procedure Shutdown;

   private

      entry Is_Aborted_Waiting
        (PID : in Types.Partition_ID;
         RPC : in RPC_Id);

      Cancel_Map  : Cancel_Array;
      In_Progress : Boolean := False;
      Count       : Natural := 0;
      Count_Abort : Natural := 0;
      Terminated  : Boolean := False;
   end Task_Manager;
   --  This protected object requeues on Is_Aborted_Waiting; this may look
   --  inefficient, but we hope that remote abortion won't occur too much
   --  (or at least that remote abortion won't occur too often when there is
   --  a lot of other remote calls in progress). Count_Abort contains the
   --  number of abortion in progress.

   type Task_Identifier;
   type Task_Identifier_Access is access Task_Identifier;

   task type Anonymous_Task is
      entry Set_Identifier (Identifier : in Task_Identifier_Access);
      entry Set_Job (The_PID    : in Types.Partition_ID;
                     The_RPC    : in RPC_Id;
                     The_Params : in Streams.Params_Stream_Access;
                     The_Async  : in Boolean);
      entry Shutdown;
      pragma Priority (Default_Priority);
      pragma Storage_Size (3_000_000);
   end Anonymous_Task;
   type Anonymous_Task_Access is access Anonymous_Task;
   --  An anonymous task will serve a request. Is the pragma Storage_Size
   --  still needed there ???

   type Task_Identifier is record
      Task_Pointer : Anonymous_Task_Access;
      Next         : Task_Identifier_Access;
   end record;
   --  Since it is impossible for a task to get a pointer on itself, it
   --  is transmitted through this structure. Moreover, this allows to
   --  handle a list of free tasks very easily.

   procedure Free is
      new Ada.Unchecked_Deallocation
     (Task_Identifier, Task_Identifier_Access);

   function Create_New_Task return Task_Identifier_Access;
   --  Create a new task

   Low_Mark  : Positive renames System.Garlic.Options.Task_Pool_Low_Bound;
   High_Mark : Positive renames System.Garlic.Options.Task_Pool_High_Bound;
   Max_Mark  : Positive renames System.Garlic.Options.Task_Pool_Max_Bound;

   protected Free_Tasks is

      entry Get_Task
        (Identifier : out Task_Identifier_Access;
         Shutdown   : out Boolean);
      --  Call this to create a task. If Identifier is Null, then you have
      --  to create the task yourself before using it (calling the
      --  Create_New_Task function).
      --  This entry is potentially blocking because in some cases you
      --  do not want to have more than a maximum number of running tasks
      --  in your system.

      procedure Queue (Identifier : in Task_Identifier_Access;
                       Accepted   : out Boolean);
      --  A task will call Queue when it has terminated its job. If Accepted
      --  is false on return, then the task must terminate itself as soon
      --  as possible in order to limit the number of running tasks in the
      --  system.

      entry Wait (Shutdown : out Boolean);
      --  This procedure will be blocked until there is a need for more
      --  anonymous tasks. It will also be unblocked by the shutdown
      --  operation and will set Shutdown_In_Progress to True if there
      --  is a shutdown in progress.

      procedure Shutdown;
      --  This procedure will be called upon shutdown

      procedure Status;
      --  This procedure will print a status when in debug mode.
      --  Warning: it is *not* legal to do so in normal mode since the
      --  traces are potentially blocking operations.

   private
      Terminated        : Boolean := False;
      Free_Tasks_List   : Task_Identifier_Access;
      Free_Tasks_Count  : Natural := 0;
      Total_Tasks_Count : Natural := 0;
      Allocations       : Natural := 0;
      Deallocations     : Natural := 0;
   end Free_Tasks;

   task type Background_Creation is
      entry Shutdown;
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
      RPC : in RPC_Id) is
   begin
      Task_Manager.Abort_One (PID, RPC);
   end Abort_Task;

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
      Terminated : Boolean;
   begin
      Free_Tasks.Get_Task (Identifier, Terminated);
      if Terminated then
         raise System.RPC.Communication_Error;
      end if;
      if Identifier = null then
         Identifier := Create_New_Task;
      end if;
      Identifier.Task_Pointer.Set_Job (PID, RPC, Params, Async);
   end Allocate_Task;

   --------------------
   -- Anonymous_Task --
   --------------------

   task body Anonymous_Task is
      Dest      : Types.Partition_ID;
      Receiver  : Streams.RPC_Receiver;
      Result    : Streams.Params_Stream_Access;
      Cancelled : Boolean;
      Prio      : Any_Priority;
      PID       : Types.Partition_ID;
      RPC       : RPC_Id;
      Params    : Streams.Params_Stream_Access;
      Async     : Boolean;
      Self      : Task_Identifier_Access;
      Aborted   : Boolean := False;

      use Ada.Exceptions;
   begin
      pragma Debug (D ("Anonymous task starting"));
      select
         accept Set_Identifier (Identifier : in Task_Identifier_Access) do
            Self := Identifier;
         end Set_Identifier;
      or
         terminate;
      end select;
      loop
         pragma Debug (D ("Waiting for a job"));
         select
            accept Set_Job
              (The_PID    : in Types.Partition_ID;
               The_RPC    : in RPC_Id;
               The_Params : in Streams.Params_Stream_Access;
               The_Async  : in Boolean) do
               PID    := The_PID;
               RPC    := The_RPC;
               Params := The_Params;
               Async  := The_Async;
            end Set_Job;
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
         Task_Manager.Get_One;
         Task_Manager.Unabort_One (PID, RPC);
         Types.Partition_ID'Read (Params, Dest);
         if not Dest'Valid then
            pragma Debug (D ("Invalid destination received"));
            raise Constraint_Error;
         end if;
         Any_Priority'Read (Params, Prio);
         if not Prio'Valid then
            pragma Debug (D ("Invalid priority received"));
            raise Constraint_Error;
         end if;
         Ada.Dynamic_Priorities.Set_Priority (Prio);
         When_Established;
         select
            Task_Manager.Is_Aborted (PID, RPC);
            declare
               Empty  : aliased Streams.Params_Stream_Type (0);
               Header : constant RPC_Header := (Abortion_Reply, RPC);
               Error  : aliased Error_Type;
            begin
               pragma Debug (D ("Abortion queried by caller"));
               Insert_RPC_Header (Empty'Access, Header);
               Send (PID, Remote_Call, Empty'Access, Error);
               if Found (Error) then
                  Raise_Exception (Communication_Error'Identity,
                                   Content (Error'Access));
               end if;
               Cancelled := True;
            end;
         then abort
            pragma Debug (D ("Job to achieve"));
            Receiver := Convert
               (System.Address (Interfaces.Unsigned_64'Input (Params)));
            Receiver (Params, Result);
            pragma Debug (D ("Job achieved without abortion"));
         end select;

         declare
            Copy : Streams.Params_Stream_Access := Params;
         begin

            --  Yes, we deallocate a copy, because Params is readonly (it's
            --  a discriminant). We must *not* use Params later in this task.

            Streams.Deallocate (Copy);
         end;
         if Async or else Cancelled then
            pragma Debug (D ("Result not sent"));
            Streams.Deallocate (Result);
         else
            declare
               Header : constant RPC_Header := (RPC_Reply, RPC);
               Error  : aliased Error_Type;
            begin
               pragma Debug (D ("Result will be sent"));
               Insert_RPC_Header (Result, Header);
               Send (PID, Remote_Call, Result, Error);
               if Found (Error) then
                  Raise_Exception (Communication_Error'Identity,
                                   Content (Error'Access));
               end if;
               Streams.Free (Result);
            end;
         end if;
         Task_Manager.Free_One;
         pragma Debug (D ("Job finished, queuing"));
         declare
            Queued : Boolean;
         begin
            Free_Tasks.Queue (Self, Queued);
            if not Queued then
               pragma Debug (D ("Too many tasks, queuing refused"));
               Free (Self);
               exit;
            end if;
         end;
      end loop;
      pragma Debug (D ("Anonymous task finishing"));

   exception
      when E : others =>
         pragma Warnings (Off, E);
         pragma Debug (D ("Error in anonymous task " &
                          "(exception " & Exception_Name (E) & ")"));
         null;

   end Anonymous_Task;

   -------------------------
   -- Background_Creation --
   -------------------------

   task body Background_Creation is
      Terminated : Boolean;
      Accepted   : Boolean;
      Identifier : Task_Identifier_Access;
   begin
      System.Garlic.Soft_Links.Add_Non_Terminating_Task;
      loop
         Free_Tasks.Wait (Terminated);
         exit when Terminated;
         Identifier := Create_New_Task;
         Free_Tasks.Queue (Identifier, Accepted);
         if not Accepted then
            Identifier.Task_Pointer.Shutdown;
         end if;
      end loop;
      System.Garlic.Soft_Links.Sub_Non_Terminating_Task;
      accept Shutdown;
   end Background_Creation;

   ---------------------
   -- Create_New_Task --
   ---------------------

   function Create_New_Task return Task_Identifier_Access is
      Identifier : constant Task_Identifier_Access :=
       new Task_Identifier'(Task_Pointer => new Anonymous_Task,
                            Next         => null);
   begin
      Identifier.Task_Pointer.Set_Identifier (Identifier);
      return Identifier;
   end Create_New_Task;

   ----------------
   -- Free_Tasks --
   ----------------

   protected body Free_Tasks is

      --------------
      -- Get_Task --
      --------------

      entry Get_Task
        (Identifier : out Task_Identifier_Access;
         Shutdown   : out Boolean)
      when Terminated
        or else Free_Tasks_Count > 0
        or else Total_Tasks_Count < Max_Mark
      is
      begin
         Shutdown := Terminated;
         if Free_Tasks_Count > 0 then
            Identifier       := Free_Tasks_List;
            Free_Tasks_List  := Identifier.Next;
            Free_Tasks_Count := Free_Tasks_Count - 1;

         else
            Identifier := null;
            if not Terminated then
               Allocations       := Allocations + 1;
               Total_Tasks_Count := Total_Tasks_Count + 1;
            end if;
         end if;
         Status;
      end Get_Task;

      -----------
      -- Queue --
      -----------

      procedure Queue
        (Identifier : in Task_Identifier_Access;
         Accepted   : out Boolean) is
      begin
         if Terminated then
            Accepted := False;

         elsif Total_Tasks_Count <= High_Mark then
            Accepted         := True;
            Identifier.Next  := Free_Tasks_List;
            Free_Tasks_List  := Identifier;
            Free_Tasks_Count := Free_Tasks_Count + 1;

         else
            Accepted          := False;
            Deallocations     := Deallocations + 1;
            Total_Tasks_Count := Total_Tasks_Count - 1;
         end if;
         Status;
      end Queue;

      ------------
      -- Status --
      ------------

      procedure Status is
      begin
         pragma Debug (D ("Free tasks:" & Free_Tasks_Count'Img));
         pragma Debug (D ("Total tasks:" & Total_Tasks_Count'Img));
         pragma Debug (D ("Allocations :" & Allocations'Img));
         pragma Debug (D ("Deallocations :" & Deallocations'Img));
         null;
      end Status;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown is
      begin
         pragma Debug (D ("Free_Tasks.Shutdown called"));
         Terminated := True;
      end Shutdown;

      ----------
      -- Wait --
      ----------

      entry Wait
        (Shutdown : out Boolean)
      when Terminated or
        else Total_Tasks_Count < High_Mark is
      begin
         Shutdown := Terminated;
         if not Terminated then
            Total_Tasks_Count := Total_Tasks_Count + 1;
         end if;
         Status;
      end Wait;

   end Free_Tasks;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if System.Garlic.Options.Has_RCI_Pkg_Or_RACW_Var then
         Background_Task := new Background_Creation;
      end if;
   end Initialize;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
      Identifier : Task_Identifier_Access;
      Terminated : Boolean;
   begin
      pragma Debug (D ("Shutdown called"));
      Free_Tasks.Status;
      Free_Tasks.Shutdown;
      Task_Manager.Shutdown;
      if Background_Task /= null then
         Background_Task.Shutdown;
      end if;
      loop
         Free_Tasks.Get_Task (Identifier, Terminated);
         exit when Identifier = null;
         Identifier.Task_Pointer.Shutdown;
      end loop;
   end Shutdown;

   ------------------
   -- Task_Manager --
   ------------------

   protected body Task_Manager is

      ---------------
      -- Abort_One --
      ---------------

      procedure Abort_One
        (PID : in Types.Partition_ID;
         RPC : in RPC_Id) is
      begin
         for I in Cancel_Map'Range loop
            if Cancel_Map (I).Free then
               Cancel_Map (I) := (PID, RPC, False);
               Count_Abort := Count_Abort + 1;
               if Is_Aborted_Waiting'Count > 0 then
                  In_Progress := True;
                  pragma Debug (D ("Will signal abortion"));
               end if;
               return;
            end if;
         end loop;
      end Abort_One;

      --------------
      -- Free_One --
      --------------

      procedure Free_One is
      begin
         Count := Count - 1;
      end Free_One;

      -------------
      -- Get_One --
      -------------

      entry Get_One when Count < Max_Tasks is
      begin
         Count := Count + 1;
      end Get_One;

      ----------------
      -- Is_Aborted --
      ----------------

      entry Is_Aborted
        (PID : in Types.Partition_ID;
         RPC : in RPC_Id)
      when Terminated
        or else (Count_Abort > 0 and then not In_Progress)
      is
      begin
         if Terminated then
            return;
         end if;
         for I in Cancel_Map'Range loop
            declare
               Ent : Cancel_Type renames Cancel_Map (I);
            begin
               if not Ent.Free
                 and then Ent.RPC = RPC
                 and then Ent.PID = PID
               then
                  Count_Abort := Count_Abort - 1;
                  Ent.Free    := True;
                  return;
               end if;
            end;
         end loop;
         requeue Is_Aborted_Waiting with abort;
      end Is_Aborted;

      ------------------------
      -- Is_Aborted_Waiting --
      ------------------------

      entry Is_Aborted_Waiting
        (PID : in Types.Partition_ID;
         RPC : in RPC_Id)
      when In_Progress is
      begin
         if Is_Aborted_Waiting'Count = 0 then
            In_Progress := False;
         end if;
         requeue Is_Aborted with abort;
      end Is_Aborted_Waiting;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown is
      begin
         Terminated := True;
      end Shutdown;

      -----------------
      -- Unabort_One --
      -----------------

      procedure Unabort_One
        (PID : in Types.Partition_ID;
         RPC : in RPC_Id) is
      begin
         for I in Cancel_Map'Range loop
            declare
               Ent : Cancel_Type renames Cancel_Map (I);
            begin
               if not Ent.Free
                 and then Ent.RPC = RPC
                 and then Ent.PID = PID
               then
                  Ent.Free    := True;
                  Count_Abort := Count_Abort - 1;
               end if;
            end;
         end loop;
      end Unabort_One;

   end Task_Manager;

end System.RPC.Pool;
