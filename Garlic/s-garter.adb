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

with Ada.Calendar;                use Ada.Calendar;
with Ada.Exceptions;              use Ada.Exceptions;
with System.Garlic.Debug;         use System.Garlic.Debug;
pragma Elaborate_All (System.Garlic.Debug);
with System.Garlic.Heart;         use System.Garlic.Heart;
with System.Garlic.Options;
with System.Garlic.Partitions;    use System.Garlic.Partitions;
with System.Garlic.Soft_Links;    use System.Garlic.Soft_Links;
with System.Garlic.Streams;       use System.Garlic.Streams;
with System.Garlic.Types;         use System.Garlic.Types;
with System.Garlic.Utils;         use System.Garlic.Utils;

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

   Mutex : Mutex_Access;

   Non_Terminating_Tasks : Natural := 0;
   pragma Atomic (Non_Terminating_Tasks);
   --  Count non-terminating tasks. Counter is an Integer instead of a
   --  Natural because it may well go below 0 in case of termination
   --  (some select then abort statements may be protected by calling
   --  Sub_Non_Terminating_Task inconditionnally after the end select).

   type Stamp_Type is mod 2 ** 8;
   --  A Stamp value is assigned at each round of the termination protocol
   --  to distinguish between different rounds.

   function ">" (S1, S2 : Stamp_Type) return Boolean;
   --  Compare two stamps. S1 > S2 means that S1 is very likely to have been
   --  issued prior to S2.

   Time_Between_Checks : constant Duration := 1.0;
   Time_To_Synchronize : constant Duration := 5.0;
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

   procedure Local_Termination;
   --  Terminate when Garlic tasks and the environment task are the only
   --  active tasks. Don't bother with other partitions.

   procedure Dump_Task_Table;

   Current_Father : Types.Partition_ID;
   --  Our father for the current wave

   Current_Stamp  : Stamp_Type := 0;
   --  The stamp of the current request

   Neighbours_Contacted : Natural;
   --  Number of neighbours that have been contacted so far and have not
   --  sent any answer. This number is brought back to zero as soon as
   --  we have answered our father, which may happen in case of a negative
   --  answer from a neighbour.

   Messages_Sent : Boolean := False;
   --  Record the fact that messages have been sent since the last wave

   Termination_Activated : Boolean := False;
   --  Will be set to True when the termination will be activated

   Termination_Detected : Boolean := False;
   --  Will be set to True when the termination will be detected

   Shutdown_Rejected : Boolean;
   --  Will be set when termination is rejected early

   type Control_Type is (Query, Answer, Perform_Shutdown);

   function Send_Wave return Natural;
   --  Send a wave to all our neighbours but our father and return the
   --  number of partitions that have been contacted.

   procedure Send_Answer (Recipient : in Partition_ID; Ready : in Boolean);
   --  Send an answer to Recipient saying whether we are ready or not to
   --  terminate. As a special case, if the Recipient is equal to our own
   --  Partition_ID, then it means that we are on the initiator of the
   --  termination algorithm. In this case, the result will be stored
   --  in Termination_Detected.

   procedure Send_Shutdown;
   --  Send a shutdown wave to have all the neighbours terminate.

   procedure Sub_Non_Terminating_Task;
   --  Let Garlic know that a task is no longer a non terminating task.

   ---------
   -- ">" --
   ---------

   function ">" (S1, S2 : Stamp_Type) return Boolean is
      Signed_Diff : Integer;
   begin
      Signed_Diff := Integer (S1) - Integer (S2);
      if Signed_Diff > Integer (Stamp_Type'Last) / 2 then
         return False;
      elsif Signed_Diff < -Integer (Stamp_Type'Last / 2) then
         return True;
      else
         return Signed_Diff > 0;
      end if;
   end ">";

   -----------------------
   -- Activity_Detected --
   -----------------------

   procedure Activity_Detected is
   begin
      Messages_Sent := True;
   end Activity_Detected;

   ------------------------------
   -- Add_Non_Terminating_Task --
   ------------------------------

   procedure Add_Non_Terminating_Task is
   begin
      Enter (Mutex);
      Non_Terminating_Tasks := Non_Terminating_Tasks + 1;
      Leave (Mutex);
   end Add_Non_Terminating_Task;

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

   ---------------------------
   -- Get_Active_Task_Count --
   ---------------------------

   function Get_Active_Task_Count return Natural is
      Total : Integer;
   begin
      Total := Environment_Task.Awake_Count
        - Non_Terminating_Tasks
        - Independent_Task_Count;
      return Total;
   end Get_Active_Task_Count;

   ------------------------
   -- Global_Termination --
   ------------------------

   procedure Global_Termination is
      Flip_Flop  : Boolean := False;
      Deadline   : Time;
      Neighbours : Natural;

   begin
      pragma Debug (D ("Global termination"));

      --  This partition is involved in the global termination
      --  algorithm. But only the main partition will have something
      --  to do.

      if Options.Is_Boot_Server then
         --  Wait some time for other partitions to connect

         for I in 1 .. 10 loop
            exit when Known_Partitions'Length > 1;
            delay Time_Between_Checks;
         end loop;

         --  We have no father. This special case will be recognized
         --  by Send_Answer.

         Current_Father := Self_PID;

         Main_Loop : loop

            --  The following block may cause an additionnal delay of
            --  Time_Between_Checks before the shutdown, but it will
            --  only occur whenever an error has been signaled causing
            --  the regular shutdown algorithm to be unused.

            if Flip_Flop then
               pragma Debug (D ("Waiting for some time"));

               delay Time_Between_Checks;
            end if;

            exit Main_Loop when Termination_Activated;

            Flip_Flop := not Flip_Flop;

            --  To terminate, Get_Active_Task_Count should be 1
            --  because the env. task is still active because it is
            --  executing this code.

            if Get_Active_Task_Count <= 1
              and then Local_Termination_Partitions'Length = 0
            then
               --  Update termination stamp to start a new vote

               Enter (Mutex);
               Current_Stamp     := Current_Stamp + 1;
               Messages_Sent     := False;
               Shutdown_Rejected := False;

               pragma Debug
                 (D ("Starting round" & Stamp_Type'Image (Current_Stamp)));

               pragma Debug (Dump_Task_Table);

               --  Send a wave. If we have no neighbour, then we can
               --  safely terminate

               Neighbours := Send_Wave;
               Leave (Mutex);

               exit Main_Loop when Neighbours = 0;

               --  Wait until we get all the answers

               Deadline := Clock + Time_To_Synchronize;

               while Clock < Deadline and then not Shutdown_Rejected loop
                  delay Polling_Interval;

                  exit Main_Loop when Termination_Detected;
               end loop;
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
      pragma Debug (D ("Shutdown neighbours"));
      Send_Shutdown;
      Leave (Mutex);

      pragma Debug (D ("Shudown myself"));
      Activate_Shutdown;
   end Global_Termination;

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
      Control : Control_Type;
      Stamp   : Stamp_Type;
      B       : Boolean;
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

      pragma Debug (D ("receive query " & Control'Img &
                       " stamp " & Stamp'Img));

      case Control is
         when Termination.Query =>
            Enter (Mutex);
            if Stamp > Current_Stamp then
               Current_Stamp  := Stamp;
               Current_Father := Partition;

               pragma Debug
                 (D ("New round" & Current_Stamp'Img & Current_Father'Img));

               pragma Debug (Dump_Task_Table);

               --  If we have no neighbour, then we can answer the
               --  father right now.

               if Send_Wave = 0 then
                  B := Get_Active_Task_Count = 2 and then not Messages_Sent;
                  Send_Answer (Current_Father, B);
               end if;

            elsif Stamp = Current_Stamp then
               pragma Debug
                 (D ("Got slave message" & Current_Stamp'Img & Partition'Img));
               Send_Answer (Partition, True);

            else
               pragma Debug (D ("Got obsolete stamp" & Stamp'Img));
               null;
            end if;
            Leave (Mutex);

         when Answer =>
            Boolean'Read (Query, B);
            if not B'Valid then
               pragma Debug (D ("Invalid boolean received for answer"));
               Raise_Exception (Constraint_Error'Identity, "Invalid control");
            end if;

            Enter (Mutex);
            pragma Debug (D ("Got answer " &
                             B'Img & Current_Stamp'Img & Partition'Img));

            if Stamp = Current_Stamp
              and then Neighbours_Contacted > 0
            then
               Neighbours_Contacted := Neighbours_Contacted - 1;
               if not B then
                  --  pragma Debug
                  --    (D ("Sending a negative answer back to father" &
                  --        Partition_ID'Image (Current_Father) &
                  --        Stamp_Type'Image (Current_Stamp)));
                  Send_Answer (Current_Father, False);

               elsif Neighbours_Contacted = 0 then
                  --  pragma Debug
                  --    (D ("Answering neighbour" &
                  --        Partition_ID'Image (Partition) &
                  --        Stamp_Type'Image (Current_Stamp)));
                  B := Get_Active_Task_Count = 2 and then not Messages_Sent;
                  Send_Answer (Current_Father, B);
               end if;

            else
               null;
               pragma Debug (D ("Ignoring answer with bad stamp"));
            end if;
            Leave (Mutex);

         when Perform_Shutdown =>
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

   -----------------
   -- Send_Answer --
   -----------------

   procedure Send_Answer
     (Recipient : in Partition_ID;
      Ready     : in Boolean)
   is
      Message : aliased Params_Stream_Type (0);
      Error   : Error_Type;
   begin
      if Recipient = Current_Father then
         Messages_Sent := False;
      end if;
      if Recipient = Self_PID then
         Termination_Detected := Ready;
         Shutdown_Rejected    := not Ready;
         return;
      end if;
      Stamp_Type'Write (Message'Access, Current_Stamp);
      Control_Type'Write (Message'Access, Answer);
      Boolean'Write (Message'Access, Ready);
      Send (Recipient, Shutdown_Service, Message'Access, Error);
   end Send_Answer;

   -------------------
   -- Send_Shutdown --
   -------------------

   procedure Send_Shutdown is
      Neighbours   : constant Partition_List := Online_Partitions;
      Message      : aliased Params_Stream_Type (0);
      Message_Copy : aliased Params_Stream_Type (0);
      Error        : Error_Type;
      Partition    : Partition_ID;

   begin
      Stamp_Type'Write (Message'Access, Current_Stamp);
      Control_Type'Write (Message'Access, Perform_Shutdown);
      for I in Neighbours'Range loop
         Partition := Neighbours (I);
         if Partition /= Self_PID then
            Copy (Message, Message_Copy);
            Send (Partition, Shutdown_Service, Message'Access, Error);
         end if;
      end loop;
      Deallocate (Message);
   end Send_Shutdown;

   ---------------
   -- Send_Wave --
   ---------------

   function Send_Wave return Natural is
      Neighbours   : constant Partition_List := Online_Partitions;
      Message      : aliased Params_Stream_Type (0);
      Message_Copy : aliased Params_Stream_Type (0);
      Error        : Error_Type;
      Partition    : Partition_ID;
      N_Neighbours : Natural;

   begin
      Stamp_Type'Write (Message'Access, Current_Stamp);
      Control_Type'Write (Message'Access, Query);
      N_Neighbours := 0;
      for I in Neighbours'Range loop
         Partition := Neighbours (I);
         if Partition /= Self_PID
           and then Partition /= Current_Father
           and then not Has_Local_Termination (Partition)
         then
            Copy (Message, Message_Copy);
            Send
              (Neighbours (I), Shutdown_Service, Message_Copy'Access, Error);
            if Found (Error) then
               Catch (Error);
            else
               N_Neighbours := N_Neighbours + 1;
            end if;
         end if;
      end loop;
      Deallocate (Message);
      --  pragma Debug
      --    (D ("Send request to" & N_Neighbours'Img & " neighbours"));
      Neighbours_Contacted := N_Neighbours;
      return N_Neighbours;
   end Send_Wave;

   ------------------------------
   -- Sub_Non_Terminating_Task --
   ------------------------------

   procedure Sub_Non_Terminating_Task is
   begin
      Enter (Mutex);
      Non_Terminating_Tasks := Non_Terminating_Tasks - 1;
      Leave (Mutex);
   end Sub_Non_Terminating_Task;

end System.Garlic.Termination;
