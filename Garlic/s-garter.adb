------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . T E R M I N A T I O N             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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

with Ada.Calendar;                use Ada.Calendar;
with Ada.Exceptions;              use Ada.Exceptions;
with System.Garlic.Debug;         use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Elaboration;
pragma Warnings (Off, System.Garlic.Elaboration);
pragma Elaborate_All (System.Garlic.Elaboration);
with System.Garlic.Exceptions;    use System.Garlic.Exceptions;
with System.Garlic.Heart;         use System.Garlic.Heart;
with System.Garlic.Options;
with System.Garlic.Partitions;    use System.Garlic.Partitions;
with System.Garlic.Soft_Links;    use System.Garlic.Soft_Links;
with System.Garlic.Streams;       use System.Garlic.Streams;
with System.Garlic.Types;         use System.Garlic.Types;

with System.Tasking;              use System.Tasking;

package body System.Garlic.Termination is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARTER", "(s-garter): ");
   procedure D
     (Message : String;
      Key     : Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Mutex : Mutex_Access;

   Non_Terminating_Tasks : Natural := 0;
   pragma Atomic (Non_Terminating_Tasks);
   --  Count non-terminating tasks. These tasks are runnable when
   --  blocked on system calls (recv() or select() for instance). They
   --  should be removed from the active task count.

   Serialized_Replies         : Mutex_Access;
   Last_Task_To_Process_Reply : Task_Id := Null_Task;
   --  When we ask N neighbors whether they can terminate, we shall
   --  receive N positive replies in case of distributed
   --  termination. During the Nth reply processing, we check whether
   --  the number of active tasks corresponds to 2 (env. task and task
   --  processing result). But it is possible that the tasks
   --  processing the previous replies did not yet declare themselves
   --  as non-terminating tasks. In this case, Get_Active_Task_Count
   --  will return an incorrect result. Therefore, Serialized_Replies
   --  ensures that a new reply is not processed before that previous
   --  task processing the previous result has not declare itself as
   --  non-terminating task.

   type Stamp_Type is mod 2 ** 8;
   --  A new stamp value is assigned to each termination detection wave.

   function ">" (S1, S2 : Stamp_Type) return Boolean;
   --  Compare two stamps. S1 > S2 means that S1 is very likely to have been
   --  issued prior to S2.

   Time_Between_Checks : constant Duration := 1.0;
   Time_To_Synchronize : constant Duration := 5.0;
   Polling_Interval    : constant Duration := 0.5;
   --  Constants which affect the algorithm temporal behaviour.

   Minimum_Active_Tasks : constant := 2;
   --  When we receive the result of a termination detection wave and
   --  when we get the result from the last neighbor, there are only
   --  two active tasks, the environment task and the task processing
   --  the result. Termination is detected when the number of active
   --  tasks is equal to this value.

   procedure Add_Non_Terminating_Task;
   --  Let Garlic know that a task is not going to terminate and that
   --  it should not be taken into account during distributed termination.

   function Get_Active_Task_Count return Natural;
   --  Return the active task count (i.e. tasks in a non-terminating
   --  state - non-terminating tasks).

   procedure Activity_Detected;
   --  A message activity has been detected. This means that the
   --  current termination procedure (if any) must be aborted.

   procedure Global_Termination;
   --  Process global termination detection (on main partition). When
   --  the main partition detects task activity, it sends a
   --  termination detection wave to all its neighbors (with a
   --  stamp). Each neighbor checks whether it has any activity and if
   --  not propagate the wave to its neighbors. This propagation stops
   --  when the partition has already received the wave (checking the
   --  stamp) [result = true], when there is any activity [result =
   --  false], when a neighbor sends a negative reply [result = false]
   --  or when all neighbors have positively replied [result = true].
   --  Channels must be FIFO.

   procedure Handle_Request
     (Partition : Partition_ID;
      Opcode    : External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);
   --  Receive a message from Garlic

   procedure Local_Termination;
   --  Terminate when Garlic tasks and the environment task are the only
   --  active tasks. Don't bother with other partitions.

   procedure Dump_Task_Table;

   Current_Father : Types.Partition_ID;
   --  Our father for the current wave

   Current_Stamp  : Stamp_Type := 0;
   --  The stamp of the current request

   Partition_Neighbors : Natural;
   --  Number of neighbors which this partition is connected to and
   --  which have not replied to the current wave. This number is
   --  brought back to zero. This can occur before receiving all the
   --  replies when we receive a negative reply.

   Message_Activity : Boolean := False;
   --  Record the fact that messages have been sent since the last wave

   Termination_Detected : Boolean := False;
   --  Set to True when termination is detected

   Termination_Rejected : Boolean;
   --  Set to True when termination is rejected

   type Control_Type is (Detection_Request,
                         Detection_Result,
                         Termination_Validation);

   function Send_Termination_Detection_Wave
     return Natural;
   --  Send a wave to all partition neighbors but our father and
   --  return the number of partitions involved in the procedure ie
   --  the excepted replies.

   procedure Send_Termination_Detection_Result
     (Recipient : Partition_ID; Ready : Boolean);
   --  Send a termination detection result to Recipient saying whether
   --  we are ready or not to terminate. As a special case, if the
   --  Recipient is the partition itself, then it means that we
   --  initiated the termination algorithm. In this case, the result
   --  is stored in Termination_Detected.

   procedure Send_Termination_Validation_Wave;
   --  Send a termination validation wave to have all the neighbors.

   procedure Sub_Non_Terminating_Task;
   --  Let Garlic know that a task is no longer a non terminating task.

   ---------
   -- ">" --
   ---------

   function ">" (S1, S2 : Stamp_Type) return Boolean is
      D : Integer;
   begin
      D := Integer (S1) - Integer (S2);
      if D > Integer (Stamp_Type'Last) / 2 then
         return False;
      elsif D < -Integer (Stamp_Type'Last / 2) then
         return True;
      else
         return D > 0;
      end if;
   end ">";

   -----------------------
   -- Activity_Detected --
   -----------------------

   procedure Activity_Detected is
   begin
      Message_Activity := True;
   end Activity_Detected;

   ------------------------------
   -- Add_Non_Terminating_Task --
   ------------------------------

   procedure Add_Non_Terminating_Task is
   begin
      Enter_Critical_Section;
      Non_Terminating_Tasks := Non_Terminating_Tasks + 1;
      if Last_Task_To_Process_Reply /= Null_Task
        and then Last_Task_To_Process_Reply = Self
      then
         Last_Task_To_Process_Reply := Null_Task;
         Leave (Serialized_Replies);
      end if;
      Leave_Critical_Section;
   end Add_Non_Terminating_Task;

   ----------------------
   -- Dump_Task_Table --
   ----------------------

   procedure Dump_Task_Table is
   begin
      if Debug_Mode (Private_Debug_Key) then
         Soft_Links.List_Tasks;
         D ("env task awaken tasks count =" &
            Soft_Links.Env_Task_Awake_Count'Img);
         D ("non terminating task count  =" &
            Non_Terminating_Tasks'Img);
         D ("independent task count      =" &
            Soft_Links.Independent_Task_Count'Img);
      end if;
   end Dump_Task_Table;

   ---------------------------
   -- Get_Active_Task_Count --
   ---------------------------

   function Get_Active_Task_Count return Natural is
      Total : Integer;
   begin
      Total := Soft_Links.Env_Task_Awake_Count
        - Non_Terminating_Tasks
        - Soft_Links.Independent_Task_Count;
      return Total;
   end Get_Active_Task_Count;

   ------------------------
   -- Global_Termination --
   ------------------------

   procedure Global_Termination is
      Deadline  : Time;
      Neighbors : Natural;

   begin
      pragma Debug (D ("Global termination"));

      --  Only the main partition initiates the global termination
      --  algorithm.

      if Options.Is_Boot_Server then

         --  Wait for others to connect to this partition. Do not
         --  terminate immediatly since partition elaboration and
         --  first connections may not be immediate.

         for I in 1 .. 10 loop
            exit when Known_Partitions'Length > 1;
            delay Time_Between_Checks;
         end loop;

         --  We have no father. This special case is identified in
         --  Send_Termination_Detection_Result.

         Current_Father := Self_PID;

         Main_Loop : loop

            --  Termination wave can be activated when
            --  Get_Active_Task_Count = 1 (no active task except
            --  env. task) and when local termination partitions are
            --  not running (because they do not response to the
            --  termination detection algorithm and can re-activate
            --  the distributed system).

            if Get_Active_Task_Count <= 1
              and then Local_Termination_Partitions'Length = 0
            then
               --  Increment termination stamp to start a new wave

               Enter (Mutex);
               Current_Stamp        := Current_Stamp + 1;
               Message_Activity     := False;
               Termination_Rejected := False;

               pragma Debug
                 (D ("Start new wave" & Stamp_Type'Image (Current_Stamp)));

               pragma Debug (Dump_Task_Table);

               --  Send a new wave. Without neighbou, we terminate safely

               Neighbors := Send_Termination_Detection_Wave;
               Leave (Mutex);

               exit Main_Loop when Neighbors = 0;

               --  Wait for all the replies but do not wait forever
               --  since some partitions may die without replying to
               --  our request.

               Deadline := Clock + Time_To_Synchronize;

               while Clock < Deadline loop
                  delay Polling_Interval;

                  exit when Termination_Rejected;
                  --  The termination detection algorithm has been
                  --  aborted. Try later.

                  exit Main_Loop when Termination_Detected;
                  --  The termination detection has been
                  --  detected. Process to the termination validation.
               end loop;
            else
               delay Polling_Interval;
            end if;
         end loop Main_Loop;

      else
         while not Termination_Detected
           and then not Shutdown_Activated
         loop
            delay Time_Between_Checks;
            pragma Debug (D ("Slave waiting for master shutdown"));
         end loop;
      end if;

      Enter (Mutex);
      pragma Debug (D ("Shutdown neighbors"));
      Send_Termination_Validation_Wave;
      Leave (Mutex);

      pragma Debug (D ("Shutdown myself"));
      Activate_Shutdown;
   end Global_Termination;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : Partition_ID;
      Opcode    : External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
      pragma Unreferenced (Opcode);
      pragma Unreferenced (Reply);

      Control : Control_Type;
      Stamp   : Stamp_Type;
      Ready   : Boolean;

   begin
      Stamp_Type'Read (Query, Stamp);
      if not Stamp'Valid then
         pragma Debug (D ("Invalid stamp received"));
         Raise_Exception (Constraint_Error'Identity, "Invalid stamp");
      end if;

      Control_Type'Read (Query, Control);
      if not Control'Valid then
         pragma Debug (D ("Invalid control received"));
         Raise_Exception (Constraint_Error'Identity, "Invalid control");
      end if;

      pragma Debug (D ("Request from" & Partition'Img &
                       " about " & Control'Img &
                       " stamp" & Stamp'Img));
      pragma Debug (D ("Status father" & Current_Father'Img &
                       " stamp" & Current_Stamp'Img));

      case Control is
         when Detection_Request =>
            Enter (Mutex);
            if Stamp > Current_Stamp then
               pragma Debug (D ("Propagate new wave"));

               Current_Stamp  := Stamp;
               Current_Father := Partition;

               pragma Debug (Dump_Task_Table);

               --  If we have no neighbor, we just reply to the father.

               if Send_Termination_Detection_Wave = 0 then
                  pragma Debug (D ("Reply immediately (no neighbor)"));

                  Ready := Get_Active_Task_Count = Minimum_Active_Tasks
                    and then not Message_Activity;
                  Send_Termination_Detection_Result (Current_Father, Ready);
               end if;

            elsif Stamp = Current_Stamp then
               pragma Debug (D ("Always reply true"));
               Send_Termination_Detection_Result (Partition, True);

            else
               pragma Debug (D ("Ignore obsolete stamp"));
               null;
            end if;
            Leave (Mutex);

         when Detection_Result =>
            Boolean'Read (Query, Ready);

            if not Ready'Valid then
               pragma Debug (D ("Invalid boolean"));
               Raise_Exception (Constraint_Error'Identity, "Invalid control");
            end if;

            Enter (Mutex);
            if Stamp = Current_Stamp
              and then Partition_Neighbors > 0
            then
               pragma Debug (D ("Receive termination detection " & Ready'Img));

               Partition_Neighbors := Partition_Neighbors - 1;
               if not Ready then
                  pragma Debug
                    (D ("Forward result to father" & Current_Father'Img &
                        " stamp" & Current_Stamp'Img));
                  Send_Termination_Detection_Result (Current_Father, False);

               else
                  --  Wait for the previous task that processed a
                  --  termination reply to become a non-terminating
                  --  task. Otherwise, when the last neighbor replies,
                  --  Get_Active_Task_Count may return an inaccurate
                  --  result.

                  Enter (Serialized_Replies);
                  Last_Task_To_Process_Reply := Self;

                  if Partition_Neighbors = 0 then
                     pragma Debug (D ("Check whether we can terminate"));
                     Ready := Get_Active_Task_Count = Minimum_Active_Tasks
                       and then not Message_Activity;
                     Send_Termination_Detection_Result (Current_Father, Ready);
                  end if;
               end if;

            else
               null;
               pragma Debug (D ("Ignore request with bad stamp"));
            end if;
            Leave (Mutex);

         when Termination_Validation =>
            Termination_Detected := True;
            Activate_Shutdown;

      end case;
   exception when others =>
      Throw (Error, "Data error in Termination.Handle_Request");
   end Handle_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Mutex);
      Create (Serialized_Replies);
      Register_Add_Non_Terminating_Task (Add_Non_Terminating_Task'Access);
      Register_Sub_Non_Terminating_Task (Sub_Non_Terminating_Task'Access);
      Register_Activity_Detected (Activity_Detected'Access);
      Register_Local_Termination (Local_Termination'Access);
      Register_Global_Termination (Global_Termination'Access);
      if Options.Termination /= Local_Termination then
         Register_Handler (Shutdown_Service, Handle_Request'Access);
      end if;
   end Initialize;

   -----------------------
   -- Local_Termination --
   -----------------------

   procedure Local_Termination is
   begin
      pragma Debug (D ("Local termination"));

      loop
         --  This procedure is executed by the env. task. So, we
         --  terminate when the env. task is the only active task.

         exit when Get_Active_Task_Count <= 1
           or else Shutdown_Activated;

         delay Time_Between_Checks;
      end loop;

      pragma Debug (D ("Local termination detected"));
      Activate_Shutdown;
   end Local_Termination;

   ---------------------------------------
   -- Send_Termination_Detection_Result --
   ---------------------------------------

   procedure Send_Termination_Detection_Result
     (Recipient : Partition_ID;
      Ready     : Boolean)
   is
      Message : aliased Params_Stream_Type (0);
      Error   : Error_Type;

   begin

      --  ???
      if Recipient = Current_Father then
         Message_Activity := False;
      end if;

      --  Do not send a message to the partition itself. Just
      --  acknowledge the termination detection.

      if Recipient = Self_PID then
         Termination_Detected := Ready;
         Termination_Rejected := not Ready;
         return;
      end if;

      Stamp_Type'Write (Message'Access, Current_Stamp);
      Control_Type'Write (Message'Access, Detection_Result);
      Boolean'Write (Message'Access, Ready);
      Send (Recipient, Shutdown_Service, Message'Access, Error);
   end Send_Termination_Detection_Result;

   --------------------------------------
   -- Send_Termination_Validation_Wave --
   --------------------------------------

   procedure Send_Termination_Validation_Wave is
      Neighbors   : constant Partition_List := Online_Partitions;
      Message      : aliased Params_Stream_Type (0);
      Message_Copy : aliased Params_Stream_Type (0);
      Error        : Error_Type;
      Partition    : Partition_ID;

   begin
      Stamp_Type'Write (Message'Access, Current_Stamp);
      Control_Type'Write (Message'Access, Termination_Validation);

      for I in Neighbors'Range loop
         Partition := Neighbors (I);
         if Partition /= Self_PID then
            Copy (Message, Message_Copy);
            Send (Partition, Shutdown_Service, Message'Access, Error);
         end if;
      end loop;
      Deallocate (Message);
   end Send_Termination_Validation_Wave;

   -------------------------------------
   -- Send_Termination_Detection_Wave --
   -------------------------------------

   function Send_Termination_Detection_Wave return Natural is
      Neighbors    : constant Partition_List := Online_Partitions;
      Message      : aliased Params_Stream_Type (0);
      Message_Copy : aliased Params_Stream_Type (0);
      Error        : Error_Type;
      Partition    : Partition_ID;
      N_Neighbors : Natural;

   begin
      Stamp_Type'Write (Message'Access, Current_Stamp);
      Control_Type'Write (Message'Access, Detection_Request);
      N_Neighbors := 0;

      for I in Neighbors'Range loop
         Partition := Neighbors (I);
         if Partition /= Self_PID
           and then Partition /= Current_Father
           and then not Has_Local_Termination (Partition)
         then
            Copy (Message, Message_Copy);
            Send (Neighbors (I), Shutdown_Service, Message_Copy'Access, Error);
            if Found (Error) then
               Catch (Error);
            else
               N_Neighbors := N_Neighbors + 1;
            end if;
         end if;
      end loop;
      Deallocate (Message);
      --  pragma Debug
      --    (D ("Send request to" & N_Neighbors'Img & " neighbors"));
      Partition_Neighbors := N_Neighbors;
      return N_Neighbors;
   end Send_Termination_Detection_Wave;

   ------------------------------
   -- Sub_Non_Terminating_Task --
   ------------------------------

   procedure Sub_Non_Terminating_Task is
   begin
      Enter_Critical_Section;
      Non_Terminating_Tasks := Non_Terminating_Tasks - 1;
      Leave_Critical_Section;
   end Sub_Non_Terminating_Task;

end System.Garlic.Termination;
