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

with System.Garlic.Debug;         use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Heart;         use System.Garlic.Heart;
with System.Garlic.Options;
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
   --  Terminate when global termination detected (on main partition).

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

   type Stamp is mod 2 ** 8;
   --  A Stamp value is assigned at each round of the termination protocol
   --  to distinguish between different rounds.

   protected Termination_Watcher is
      procedure Set_Stamp (S : in Stamp);
      function  Get_Stamp return Stamp;
      procedure Increment_Stamp;
      procedure Messages_Sent (N : in Natural);
      procedure Activity_Detected;
      procedure Positive_Ack_Received (S : in Stamp);
      procedure Negative_Ack_Received (S : in Stamp);
      procedure Force_Termination;
      entry     Termination_Accepted (S : in Stamp; B : out Boolean);
      function  Result_Is_Available return Boolean;
   private
      Current   : Stamp := Stamp'Last;
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

   procedure Receive_Message
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type);
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
      Id     : Stamp                 := Termination_Watcher.Get_Stamp;
      Count  : Natural               := 0;
      Latest : constant Partition_ID := Latest_Allocated_Partition_ID;
   begin
      for Partition in Get_Boot_Server + 1 .. Latest loop
         if Termination_Policy (Partition) /= Local_Termination then
            declare
               Params : aliased Params_Stream_Type (0);
            begin
               pragma Debug
                 (D (D_Debug,
                     "Sending shutdown query to partition" & Partition'Img));
               Termination_Code'Write (Params'Access, Set_Stamp);
               Stamp'Write (Params'Access, Id);
               Send (Partition, Shutdown_Synchronization, Params'Access);
               Count := Count + 1;
            exception
               when Communication_Error => null;
            end;
         end if;
      end loop;
      pragma Debug (D (D_Debug, "Sent" & Count'Img & " messages"));
      Termination_Watcher.Messages_Sent (Count);
      for Partition in Get_Boot_Server + 1 .. Latest loop
         if Termination_Policy (Partition) /= Local_Termination then
            declare
               Params : aliased Params_Stream_Type (0);
            begin
               Termination_Code'Write (Params'Access, Check_Stamp);
               Stamp'Write (Params'Access, Id);
               Send (Partition, Shutdown_Synchronization, Params'Access);
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
         Receive (Shutdown_Synchronization, Receive_Message'Access);
      end if;
   end Initialize;

   -----------------------
   -- Local_Termination --
   -----------------------

   procedure Local_Termination is
   begin
      loop
         pragma Debug (D (D_Debug, "Count =" & Get_Active_Task_Count'Img));

         --  This procedure is executed by the env. task. So, we terminate
         --  when the env. task is the only active task.
         exit when Get_Active_Task_Count = 1;

         delay Time_Between_Checks;
      end loop;
      pragma Debug (D (D_Debug, "Local termination detected"));
      Heart.Soft_Shutdown;
   end Local_Termination;

   ---------------------
   -- Receive_Message --
   ---------------------

   procedure Receive_Message
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type)
   is
      Termination_Operation : Termination_Code;
      Id                    : Stamp;
   begin
      Termination_Code'Read (Params, Termination_Operation);

      if not Termination_Operation'Valid then
         pragma Debug (D (D_Debug, "Received invalid termination operation"));
         raise Constraint_Error;
      end if;

      Stamp'Read (Params, Id);
      if not Id'Valid then
         pragma Debug (D (D_Debug, "Received invalid stamp"));
         raise Constraint_Error;
      end if;

      pragma Debug
        (D (D_Debug, "Received operation of " & Termination_Operation'Img));

      case Termination_Operation is

         when Set_Stamp =>
            Termination_Watcher.Set_Stamp (Id);

         when Check_Stamp =>
            declare
               OK     : Boolean;
               Answer : aliased Params_Stream_Type (0);
            begin
               Termination_Watcher.Termination_Accepted (Id, OK);

               --  Send a positive ack if there has been no activity and
               --  no task is active but the current one.

               pragma Debug (D (D_Debug,
                                "Active task count is" &
                                Natural'Image (Get_Active_Task_Count)));

               --  To terminate, Get_Active_Task_Count should be 2 because
               --  the env. task is still active (awake) and the task
               --  executing this code is also active.

               if OK and then Get_Active_Task_Count = 2 and then
                 Options.Termination /= Deferred_Termination then
                  Termination_Code'Write (Answer'Access, Positive_Ack);
               else
                  Termination_Code'Write (Answer'Access, Negative_Ack);
               end if;
               Stamp'Write (Answer'Access, Id);
               Send (Partition, Shutdown_Synchronization, Answer'Access);
            end;

         when Positive_Ack =>
            Termination_Watcher.Positive_Ack_Received (Id);

         when Negative_Ack =>
            Termination_Watcher.Negative_Ack_Received (Id);

      end case;
   end Receive_Message;

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
      --  But only the main partition will have something to do.

      if Get_My_Partition_ID /= Get_Boot_Server then
         return;
      end if;

      Main_Loop : loop

         --  Wait for a given time

         pragma Debug (D (D_Debug, "Waiting for some time"));

         select
            Shutdown_Keeper.Wait;
            exit Main_Loop;
         then abort
            delay Time_Between_Checks;
         end select;

         --  If there is only one active task (me!), we can initiate
         --  the algorithm.

         pragma Debug (D (D_Debug,
                          "Checking for active tasks:" &
                          Natural'Image (Get_Active_Task_Count)));

         --  To terminate, Get_Active_Task_Count should be 1 because the
         --  env. task is still active because it is executing this code.

         if Get_Active_Task_Count = 1 then
            declare
               function Clock return Duration
                 renames System.Task_Primitives.Operations.Clock;
               Ready    : Boolean := True;
               Id       : Stamp;
               Success  : Boolean;
               End_Time : Duration;
            begin
               --  First of all, check if there is any alive partition whose
               --  termination is local. If this is the case, that means
               --  that these partitions have not terminated yet.

               for Partition in Get_Boot_Server + 1 ..
                 Latest_Allocated_Partition_ID loop
                  if Blocking_Partition (Partition) then
                     pragma Debug (D (D_Debug,
                                      "Partition" & Partition'Img &
                                      " has not terminated"));
                     Ready := False;
                     exit;
                  end if;
               end loop;

               if Ready then

                  Termination_Watcher.Increment_Stamp;
                  Id := Termination_Watcher.Get_Stamp;
                  Initiate_Synchronization;

                  Success  := False;
                  End_Time := Clock + Time_To_Synchronize;
                  while Clock < End_Time loop

                     --  The following construction is against all the
                     --  quality and style guidelines; but they cannot be
                     --  applied here: we do NOT care if this is not
                     --  executed in time, since that means that some other
                     --  activity took place. If this is the case, then it
                     --  is likely that we do not want to terminate anymore.

                     delay Polling_Interval;

                     if Termination_Watcher.Result_Is_Available then
                        Termination_Watcher.Termination_Accepted (Id, Success);
                        exit;
                     end if;
                  end loop;

               else
                  --  Success is impossible because some partitions with
                  --  local termination are still alive.

                  Success := False;
               end if;

               pragma Debug
                 (D (D_Debug,
                     "Get_Active_Task_Count is" & Get_Active_Task_Count'Img));

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

      function Get_Stamp return Stamp is
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

      procedure Negative_Ack_Received (S : in Stamp) is
      begin
         if S = Current then
            Result    := False;
            Available := True;
         end if;
      end Negative_Ack_Received;

      ---------------------------
      -- Positive_Ack_Received --
      ---------------------------

      procedure Positive_Ack_Received (S : in Stamp) is
      begin
         if S = Current then
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

      procedure Set_Stamp (S : in Stamp) is
      begin
         Current   := S;
         Result    := True;
         Available := True;
      end Set_Stamp;

      --------------------------
      -- Termination_Accepted --
      --------------------------

      entry Termination_Accepted (S : in Stamp; B : out Boolean)
      when Available is
      begin
         B := Result and then S = Current;
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
