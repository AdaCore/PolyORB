------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . T E R M I N A T I O N             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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
with System.Garlic.Debug;         use System.Garlic.Debug;
with System.Garlic.Heart;         use System.Garlic.Heart;
with System.Garlic.Options;
with System.Garlic.Partitions;    use System.Garlic.Partitions;
with System.Garlic.Soft_Links;    use System.Garlic.Soft_Links;
with System.Garlic.Streams;       use System.Garlic.Streams;
with System.Garlic.Types;         use System.Garlic.Types;

with System.Task_Primitives.Operations;
with System.Tasking.Debug;    use System.Tasking, System.Tasking.Debug;
with System.Tasking.Utilities;    use System.Tasking, System.Tasking.Utilities;
pragma Elaborate_All (System.Tasking);
pragma Elaborate_All (System.Tasking.Utilities);

package body System.Garlic.Termination is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARTER", "(s-garter): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   package Partitions renames System.Garlic.Partitions.Partitions;
   use Partitions;

   procedure Add_Non_Terminating_Task;
   --  Let Garlic know that a task is not going to terminate and that
   --  it should not be taken into account during distributed termination.

   procedure Sub_Non_Terminating_Task;
   --  Let Garlic know that a task is no longer a non terminating task.

   procedure Shutdown;
   --  Shutdown any active task

   procedure Initialize;
   --  Initialization

   procedure Activity_Detected;
   --  Some activity has been detected. This means that the current
   --  shutdown procedure (if any) must be terminated.

   procedure Local_Termination;
   --  Terminate when Garlic tasks and the environment task are the only
   --  active tasks. Don't bother with other partitions.

   procedure Global_Termination;
   --  Terminate when global termination detected (on main partition)

   protected Count is
      procedure Increment;
      procedure Decrement;
      function Get return Natural;
   private
      Counter : Integer := 0;
   end Count;
   --  Count non-terminating tasks. Counter is an Integer instead of a
   --  Natural because it may well go below 0 in case of termination
   --  (some select then abort statements may be protected by calling
   --  Sub_Non_Terminating_Task inconditionnally after the end select).

   type Stamp_Type is mod 2 ** 8;
   --  A Stamp value is assigned at each round of the termination protocol
   --  to distinguish between different rounds.

   protected Termination_Watcher is
      procedure Set_Stamp (Stamp : in Stamp_Type);
      function  Get_Stamp return Stamp_Type;
      procedure Increment_Stamp;
      procedure Messages_Sent (N : in Natural);
      procedure Activity_Detected;
      procedure Positive_Ack_Received (Stamp : in Stamp_Type);
      procedure Negative_Ack_Received (Stamp : in Stamp_Type);
      procedure Force_Termination;
      entry     Termination_Accepted (Stamp : in Stamp_Type; B : out Boolean);
      function  Result_Is_Available return Boolean;
   private
      Current   : Stamp_Type := Stamp_Type'Last;
      Count     : Natural;
      Result    : Boolean;
      Available : Boolean := False;
   end Termination_Watcher;
   --  This watcher may be used:
   --   1) By the server: Increment_Stamp
   --                     Get_Stamp
   --                     Messages_Sent
   --                     Positive_ or Negative_Ack_Received
   --      This will unblock Termination_Accepted
   --   2) By a client:   Set_Stamp
   --      This will unblock Termination_Accepted, set to True only
   --      if no Activity_Detected was called.

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type);
   --  Receive a message from Garlic

   type Termination_Code is
      (Set_Stamp, Check_Stamp, Positive_Ack, Negative_Ack);
   --  The termination code used in negociation. It is always followed by
   --  a stamp.

   procedure Initiate_Synchronization;
   --  This procedure sends the two messages to everyone

   function Get_Active_Task_Count return Natural;
   --  Active task count (i.e. tasks in a non-terminating state -
   --  non-terminating tasks).

   Time_Between_Checks : constant Duration := 3.0;
   Time_To_Synchronize : constant Duration := 10.0;
   Polling_Interval    : constant Duration := 0.5;
   --  Constants which change the behaviour of this package.

   Environment_Task : constant System.Tasking.Task_ID := System.Tasking.Self;
   --  The environment task. Self will be set to it at elaboration time.

   -----------------------
   -- Activity_Detected --
   -----------------------

   procedure Activity_Detected is
   begin
      Termination_Watcher.Activity_Detected;
   end Activity_Detected;

   ------------------------------
   -- Add_Non_Terminating_Task --
   ------------------------------

   procedure Add_Non_Terminating_Task is
   begin
      Count.Increment;
   end Add_Non_Terminating_Task;

   -----------
   -- Count --
   -----------

   protected body Count is

      ---------------
      -- Decrement --
      ---------------

      procedure Decrement is
      begin
         Counter := Counter - 1;
      end Decrement;

      ---------
      -- Get --
      ---------

      function Get return Natural is
      begin
         return Counter;
      end Get;

      ---------------
      -- Increment --
      ---------------

      procedure Increment is
      begin
         Counter := Counter + 1;
      end Increment;

   end Count;

   ---------------------------
   -- Get_Active_Task_Count --
   ---------------------------

   function Get_Active_Task_Count return Natural is
      Total : Integer;
   begin
      Total := Environment_Task.Awake_Count - Count.Get
        - Independent_Task_Count;
      if Debug_Mode (D_Debug, Private_Debug_Key) then
         List_Tasks;
      end if;
      pragma Debug (D (D_Debug, "awake =" & Environment_Task.Awake_Count'Img));
      pragma Debug (D (D_Debug, "count =" & Count.Get'Img));
      pragma Debug (D (D_Debug, "indep =" & Independent_Task_Count'Img));
      pragma Debug (D (D_Debug, "total =" & Total'Img));
      return Total;
   end Get_Active_Task_Count;

   ------------------------------
   -- Initiate_Synchronization --
   ------------------------------

   procedure Initiate_Synchronization is
      Stamp : Stamp_Type := Termination_Watcher.Get_Stamp;
      Count : Natural    := 0;
      PID   : Partition_ID;
      Info  : Partition_Info;
   begin
      PID := Null_PID;
      loop
         Next_Partition (PID);
         exit when PID = Null_PID;
         Info := Partitions.Get_Component (PID);

         if PID /= Self_PID
           and then Info.Status = Done
           and then Info.Termination /= Local_Termination
         then
            declare
               Query : aliased Params_Stream_Type (0);
            begin
               pragma Debug
                 (D (D_Debug, "Send shutdown query to partition" & PID'Img));
               Termination_Code'Write (Query'Access, Set_Stamp);
               Stamp_Type'Write (Query'Access, Stamp);
               Send (PID, Shutdown_Service, Query'Access);
               Count := Count + 1;
            exception
               when Communication_Error => null;
            end;
         end if;
      end loop;

      Termination_Watcher.Messages_Sent (Count);

      PID := Null_PID;
      loop
         Next_Partition (PID);
         exit when PID = Null_PID;
         Info := Partitions.Get_Component (PID);

         if PID /= Self_PID
           and then Info.Status = Done
           and then Info.Termination /= Local_Termination
         then
            declare
               Query : aliased Params_Stream_Type (0);
            begin
               Termination_Code'Write (Query'Access, Check_Stamp);
               Stamp_Type'Write (Query'Access, Stamp);
               Send (PID, Shutdown_Service, Query'Access);
            exception
               when Communication_Error => null;
            end;
         end if;
      end loop;
   end Initiate_Synchronization;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Options.Termination /= Local_Termination then
         Register_Handler (Shutdown_Service, Handle_Request'Access);
      end if;
   end Initialize;

   -----------------------
   -- Local_Termination --
   -----------------------

   procedure Local_Termination is
   begin
      loop
         --  This procedure is executed by the env. task. So, we terminate
         --  when the env. task is the only active task.

         exit when Get_Active_Task_Count = 1;

         delay Time_Between_Checks;
      end loop;
      pragma Debug (D (D_Debug, "Local termination detected"));
      Heart.Soft_Shutdown;
   end Local_Termination;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type)
   is
      Code  : Termination_Code;
      Stamp : Stamp_Type;
   begin
      Termination_Code'Read (Query, Code);

      if not Opcode'Valid then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Received invalid termination operation");
      end if;

      Stamp_Type'Read (Query, Stamp);
      if not Stamp'Valid then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Received invalid termination stamp");
      end if;

      case Code is
         when Set_Stamp =>
            Termination_Watcher.Set_Stamp (Stamp);

         when Check_Stamp =>
            declare
               Ready : Boolean;
               Reply : aliased Params_Stream_Type (0);
            begin
               Termination_Watcher.Termination_Accepted (Stamp, Ready);

               --  To terminate, Get_Active_Task_Count should be 2 because
               --  the env. task is still active (awake) and the task
               --  executing this code is also active.

               if Ready
                 and then Get_Active_Task_Count = 2
                 and then Options.Termination /= Deferred_Termination
               then
                  pragma Debug (D (D_Debug, "Partition can terminate"));
                  Termination_Code'Write (Reply'Access, Positive_Ack);
               else
                  pragma Debug (D (D_Debug, "Partition cannot terminate"));
                  Termination_Code'Write (Reply'Access, Negative_Ack);
               end if;
               Stamp_Type'Write (Reply'Access, Stamp);
               Send (Partition, Shutdown_Service, Reply'Access);
            end;

         when Positive_Ack =>
            Termination_Watcher.Positive_Ack_Received (Stamp);

         when Negative_Ack =>
            Termination_Watcher.Negative_Ack_Received (Stamp);

      end case;
   end Handle_Request;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Termination_Watcher.Force_Termination;
   end Shutdown;

   ------------------------------
   -- Sub_Non_Terminating_Task --
   ------------------------------

   procedure Sub_Non_Terminating_Task is
   begin
      Count.Decrement;
   end Sub_Non_Terminating_Task;

   ------------------------
   -- Global_Termination --
   ------------------------

   procedure Global_Termination is
   begin

      --  This partition is involved in the global termination algorithm.
      --  But only the main partition will have something to do. If
      --  shutdown is already in progress, we do not have anything to
      --  negotiate.

      if Is_Shutdown_In_Progress or else not Options.Boot_Partition then
         return;
      end if;

      Main_Loop : loop

         --  Wait for a given time

         pragma Debug (D (D_Debug, "Waiting for some time"));

         --  The following block may cause an additionnal delay of
         --  Time_Between_Checks before the shutdown, but it will only
         --  occur whenever an error has been signaled causing the regular
         --  shutdown algorithm to be unused.

         exit Main_Loop when Shutdown_In_Progress;
         delay Time_Between_Checks;
         exit Main_Loop when Shutdown_In_Progress;

         --  To terminate, Get_Active_Task_Count should be 1 because the
         --  env. task is still active because it is executing this code.

         if Get_Active_Task_Count = 1 then
            declare
               function Clock return Duration
                 renames System.Task_Primitives.Operations.Clock;
               Ready    : Boolean := True;
               Stamp    : Stamp_Type;
               Success  : Boolean;
               Deadline : Duration;
               PID      : Partition_ID;
               Info     : Partition_Info;
            begin
               --  First of all, check if there is any alive partition whose
               --  termination is local. If this is the case, that means
               --  that these partitions have not terminated yet.

               PID := Null_PID;
               loop
                  Next_Partition (PID);
                  exit when PID = Null_PID;
                  Info := Partitions.Get_Component (PID);

                  if PID /= Self_PID
                    and then Info.Status = Done
                    and then Info.Termination = Local_Termination
                  then
                     pragma Debug
                       (D (D_Debug,
                           "Partition" & PID'Img & " still active"));
                     Ready := False;
                     exit;
                  end if;
               end loop;

               if Ready then

                  Termination_Watcher.Increment_Stamp;
                  Stamp := Termination_Watcher.Get_Stamp;
                  Initiate_Synchronization;

                  Success  := False;
                  Deadline := Clock + Time_To_Synchronize;
                  while Clock < Deadline loop

                     --  The following construction is against all the
                     --  quality and style guidelines; but they cannot be
                     --  applied here: we do NOT care if this is not
                     --  executed in time, since that means that some other
                     --  activity took place. If this is the case, then it
                     --  is likely that we do not want to terminate anymore.

                     delay Polling_Interval;

                     if Termination_Watcher.Result_Is_Available then
                        Termination_Watcher.Termination_Accepted
                          (Stamp, Success);
                        exit;
                     end if;
                  end loop;

               else
                  --  Success is impossible because some partitions with
                  --  local termination are still alive.

                  Success := False;
               end if;

               --  To terminate, Get_Active_Task_Count should be 1 because
               --  the env. task is still active because it is executing
               --  this code.

               if Success and then Get_Active_Task_Count = 1 then

                  --  Everyone agrees it's time to die, so let's initiate
                  --  this if nothing runs here.

                  Heart.Soft_Shutdown;
                  exit Main_Loop;
               end if;
            end;
         end if;

      end loop Main_Loop;
   end Global_Termination;

   -------------------------
   -- Termination_Watcher --
   -------------------------

   protected body Termination_Watcher is

      -----------------------
      -- Activity_Detected --
      -----------------------

      procedure Activity_Detected is
      begin
         Result    := False;
         Available := True;
      end Activity_Detected;

      -----------------------
      -- Force_Termination --
      -----------------------

      procedure Force_Termination is
      begin
         Result    := True;
         Available := True;
      end Force_Termination;

      ---------------
      -- Get_Stamp --
      ---------------

      function Get_Stamp return Stamp_Type is
      begin
         return Current;
      end Get_Stamp;

      ---------------------
      -- Increment_Stamp --
      ---------------------

      procedure Increment_Stamp is
      begin
         Current := Current + 1;
         Available := False;
      end Increment_Stamp;

      -------------------
      -- Messages_Sent --
      -------------------

      procedure Messages_Sent (N : in Natural) is
      begin
         Count := N;
         if Count = 0 then

            --  There are no other active partitions

            Result    := True;
            Available := True;
         end if;
      end Messages_Sent;

      ---------------------------
      -- Negative_Ack_Received --
      ---------------------------

      procedure Negative_Ack_Received (Stamp : in Stamp_Type) is
      begin
         if Stamp = Current then
            Result    := False;
            Available := True;
         end if;
      end Negative_Ack_Received;

      ---------------------------
      -- Positive_Ack_Received --
      ---------------------------

      procedure Positive_Ack_Received (Stamp : in Stamp_Type) is
      begin
         if Stamp = Current then
            if not Available then
               Count := Count - 1;
               if Count = 0 then
                  Result    := True;
                  Available := True;
               end if;
            end if;
         end if;
      end Positive_Ack_Received;

      -------------------------
      -- Result_Is_Available --
      -------------------------

      function Result_Is_Available return Boolean is
      begin
         return Available;
      end Result_Is_Available;

      ---------------
      -- Set_Stamp --
      ---------------

      procedure Set_Stamp (Stamp : in Stamp_Type) is
      begin
         Current   := Stamp;
         Result    := True;
         Available := True;
      end Set_Stamp;

      --------------------------
      -- Termination_Accepted --
      --------------------------

      entry Termination_Accepted (Stamp : in Stamp_Type; B : out Boolean)
      when Available is
      begin
         B := Result and then Stamp = Current;
      end Termination_Accepted;

   end Termination_Watcher;

begin
   Register_Add_Non_Terminating_Task (Add_Non_Terminating_Task'Access);
   Register_Sub_Non_Terminating_Task (Sub_Non_Terminating_Task'Access);
   Register_Termination_Shutdown (Shutdown'Access);
   Register_Termination_Initialize (Initialize'Access);
   Register_Activity_Detected (Activity_Detected'Access);
   Register_Local_Termination (Local_Termination'Access);
   Register_Global_Termination (Global_Termination'Access);
end System.Garlic.Termination;
