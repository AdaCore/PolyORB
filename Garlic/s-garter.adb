------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . T E R M I N A T I O N             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
with System.Garlic.Utils;         use System.Garlic.Utils;

with System.Task_Primitives.Operations;
with System.Tasking.Debug;        use System.Tasking, System.Tasking.Debug;
with System.Tasking.Utilities;    use System.Tasking, System.Tasking.Utilities;
pragma Elaborate_All (System.Tasking);
pragma Elaborate_All (System.Tasking.Utilities);

package body System.Garlic.Termination is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARTER", "(s-garter): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Non_Terminating_Tasks : Natural := 0;
   pragma Atomic (Non_Terminating_Tasks);
   --  Count non-terminating tasks. Counter is an Integer instead of a
   --  Natural because it may well go below 0 in case of termination
   --  (some select then abort statements may be protected by calling
   --  Sub_Non_Terminating_Task inconditionnally after the end select).

   type Stamp_Type is mod 2 ** 8;
   --  A Stamp value is assigned at each round of the termination protocol
   --  to distinguish between different rounds.

   Termination_Stamp : Stamp_Type := Stamp_Type'Last;
   pragma Atomic (Termination_Stamp);
   Acknowledge_Count : Natural;
   Vote_Result_Ready : Boolean;
   Vote_Result_Value : Boolean;

   type Termination_Code is
      (Set_Stamp, Check_Stamp, Positive_Ack, Negative_Ack);
   --  The termination code used in negociation. It is always followed by
   --  a stamp.

   Time_Between_Checks : constant Duration := 3.0;
   Time_To_Synchronize : constant Duration := 10.0;
   Polling_Interval    : constant Duration := 0.5;
   --  Constants which change the behaviour of this package.

   Environment_Task : constant System.Tasking.Task_ID := System.Tasking.Self;
   --  The environment task. Self will be set to it at elaboration time.

   procedure Add_Non_Terminating_Task;
   --  Let Garlic know that a task is not going to terminate and that
   --  it should not be taken into account during distributed termination.

   function Get_Active_Task_Count return Natural;
   --  Active task count (i.e. tasks in a non-terminating state -
   --  non-terminating tasks).

   procedure Activity_Detected;
   --  Some activity has been detected. This means that the current
   --  shutdown procedure (if any) must be terminated.

   procedure Global_Termination;
   --  Terminate when global termination detected (on main partition)

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);
   --  Receive a message from Garlic

   procedure Initialize;
   --  Initialization

   procedure Local_Termination;
   --  Terminate when Garlic tasks and the environment task are the only
   --  active tasks. Don't bother with other partitions.

   procedure Dump_Task_Table;

   procedure Send
     (PID   : in Partition_ID;
      Code  : in Termination_Code;
      Stamp : in Stamp_Type;
      Error : in out Error_Type);

   procedure Shutdown;
   --  Shutdown any active task

   procedure Sub_Non_Terminating_Task;
   --  Let Garlic know that a task is no longer a non terminating task.

   -----------------------
   -- Activity_Detected --
   -----------------------

   procedure Activity_Detected is
   begin
      Enter_Critical_Section;
      Vote_Result_Ready := True;
      Vote_Result_Value := False;
      Leave_Critical_Section;
   end Activity_Detected;

   ------------------------------
   -- Add_Non_Terminating_Task --
   ------------------------------

   procedure Add_Non_Terminating_Task is
   begin
      Enter_Critical_Section;
      Non_Terminating_Tasks := Non_Terminating_Tasks + 1;
      Leave_Critical_Section;
   end Add_Non_Terminating_Task;

   ---------------------------
   -- Get_Active_Task_Count --
   ---------------------------

   function Get_Active_Task_Count return Natural is
      Total : Integer;
   begin
      Total := Environment_Task.Awake_Count
        - Non_Terminating_Tasks
        - Independent_Task_Count;
      pragma Debug (Dump_Task_Table);
      return Total;
   end Get_Active_Task_Count;

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
      pragma Debug (D ("Local termination detected"));
      Heart.Soft_Shutdown;
   end Local_Termination;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
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
            Enter_Critical_Section;
            Termination_Stamp := Stamp;
            Vote_Result_Ready := True;
            Vote_Result_Value := True;
            Leave_Critical_Section;

         when Check_Stamp =>
            declare
               Ready : Boolean;
            begin
               pragma Assert (Vote_Result_Ready);
               Enter_Critical_Section;
               Ready := Vote_Result_Value and then Stamp = Termination_Stamp;
               Leave_Critical_Section;

               --  To terminate, Get_Active_Task_Count should be 2 because
               --  the env. task is still active (awake) and the task
               --  executing this code is also active.

               if Ready
                 and then Get_Active_Task_Count = 2
                 and then Options.Termination /= Deferred_Termination
               then
                  pragma Debug (D ("Partition can terminate"));
                  Termination_Code'Write (Reply, Positive_Ack);
               else
                  pragma Debug (D ("Partition cannot terminate"));
                  Termination_Code'Write (Reply, Negative_Ack);
               end if;
               Stamp_Type'Write (Reply, Stamp);
            end;

         when Positive_Ack =>
            Enter_Critical_Section;
            if Stamp = Termination_Stamp then
               if not Vote_Result_Ready then
                  Acknowledge_Count := Acknowledge_Count - 1;
                  if Acknowledge_Count = 0 then
                     Vote_Result_Ready := True;
                     Vote_Result_Value := True;
                  end if;
               end if;
            end if;
            Leave_Critical_Section;

         when Negative_Ack =>
            Enter_Critical_Section;
            if Stamp = Termination_Stamp then
               Vote_Result_Ready := True;
               Vote_Result_Value := False;
            end if;
            Leave_Critical_Section;

      end case;
   end Handle_Request;

   ----------------------
   -- Dump_Task_Table --
   ----------------------

   procedure Dump_Task_Table is
   begin
      if Debug_Mode (Private_Debug_Key) then
         List_Tasks;
         D ("awake =" & Environment_Task.Awake_Count'Img);
         D ("count =" & Non_Terminating_Tasks'Img);
         D ("indep =" & Independent_Task_Count'Img);
      end if;
   end Dump_Task_Table;

   ----------
   -- Send --
   ----------

   procedure Send
     (PID   : in Partition_ID;
      Code  : in Termination_Code;
      Stamp : in Stamp_Type;
      Error : in out Error_Type)
   is
      Query : aliased Params_Stream_Type (0);
   begin
      Termination_Code'Write (Query'Access, Code);
      Stamp_Type'Write (Query'Access, Stamp);
      Send (PID, Shutdown_Service, Query'Access, Error);
   end Send;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Enter_Critical_Section;
      Vote_Result_Ready := True;
      Vote_Result_Value := True;
      Leave_Critical_Section;
   end Shutdown;

   ------------------------------
   -- Sub_Non_Terminating_Task --
   ------------------------------

   procedure Sub_Non_Terminating_Task is
   begin
      Enter_Critical_Section;
      Non_Terminating_Tasks := Non_Terminating_Tasks - 1;
      Leave_Critical_Section;
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

      if Shutdown_In_Progress or else not Options.Is_Boot_Server then
         return;
      end if;

      Main_Loop : loop

         --  Wait for a given time

         pragma Debug (D ("Waiting for some time"));

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
               Stamp       : Stamp_Type;
               Success     : Boolean;
               Deadline    : Duration;
               Count       : Natural := 0;
               Error       : Error_Type;
            begin
               --  First of all, check if there is any alive partition whose
               --  termination is local. If this is the case, that means
               --  that these partitions have not terminated yet.

               if Local_Termination_Partitions'Length = 0 then
                  --  Update termination stamp to start a new vote.

                  Enter_Critical_Section;
                  Termination_Stamp := Termination_Stamp + 1;
                  Vote_Result_Ready := False;
                  Stamp := Termination_Stamp;
                  Leave_Critical_Section;

                  --  Send a first wave of requests to indicate the new
                  --  stamp.

                  declare
                     PIDs : Partition_List := Global_Termination_Partitions;
                  begin
                     Count := 0;
                     for I in PIDs'Range loop
                        if PIDs (I) /= Self_PID then
                           Count := Count + 1;
                           Send (PIDs (I), Set_Stamp, Stamp, Error);
                        end if;
                     end loop;

                     --  Note the number of voters.

                     Enter_Critical_Section;
                     Acknowledge_Count := Count;
                     if Count = 0 then
                        Vote_Result_Ready := True;
                        Vote_Result_Value := True;
                     end if;
                     Leave_Critical_Section;

                     --  Send a second wave of requests to see if the
                     --  partition is ready to terminate or if the
                     --  partition has received a request from another
                     --  partition since the first wave.


                     for I in PIDs'Range loop
                        if PIDs (I) /= Self_PID then
                           Send (PIDs (I), Check_Stamp, Stamp, Error);
                        end if;
                     end loop;
                  end;

                  Success  := False;
                  Deadline := Clock + Time_To_Synchronize;
                  while Clock < Deadline loop

                     --  The following construction is against all the
                     --  quality and style guidelines; but they cannot
                     --  be applied here: we do NOT care if this is
                     --  not executed in time, since that means that
                     --  some other activity took place. If this is
                     --  the case, then it is likely that we do not
                     --  want to terminate anymore.

                     delay Polling_Interval;

                     Enter_Critical_Section;
                     if Vote_Result_Ready then
                        Success := Vote_Result_Value;
                        Leave_Critical_Section;
                        exit;
                     else
                        Leave_Critical_Section;
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

begin
   Register_Add_Non_Terminating_Task (Add_Non_Terminating_Task'Access);
   Register_Sub_Non_Terminating_Task (Sub_Non_Terminating_Task'Access);
   Register_Termination_Shutdown (Shutdown'Access);
   Register_Termination_Initialize (Initialize'Access);
   Register_Activity_Detected (Activity_Detected'Access);
   Register_Local_Termination (Local_Termination'Access);
   Register_Global_Termination (Global_Termination'Access);
end System.Garlic.Termination;
