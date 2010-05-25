------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T E R M I N A T I O N _ M A N A G E R           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with PolyORB.Binding_Objects;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.ORB_Controller;
with PolyORB.References;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Threads;
with PolyORB.Tasking.Mutexes;
with PolyORB.Termination_Activity;
with PolyORB.Termination_Manager.Bootstrap;
with System.RPC;

package body PolyORB.Termination_Manager is

   use PolyORB.Binding_Objects;
   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.ORB_Controller;
   use PolyORB.Setup;
   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Termination_Activity;
   use PolyORB.Termination_Manager.Bootstrap;

   procedure Termination_Loop;
   --  Main loop of the task created by the termination manager

   procedure In_Initiator_Loop;
   --  Procedure executed in the termination loop for the initiator node

   procedure In_Slave_Loop;
   --  Procedure executed in the termination loop for non-initiator nodes

   type Action is access
     function (TM : Term_Manager_Access; Stamp : Stamp_Type) return Boolean;
   --  Comment needed???

   function Call_On_Neighbours
     (A      : Action;
      A_Name : String;
      Stamp  : Stamp_Type) return Boolean;
   --  Call action A on all the neighbours of the local partition, and return
   --  the global AND of every neighbour return value to A.

   function ">" (S1, S2 : Stamp_Type) return Boolean;
   --  Compare two stamps. S1 > S2 means that S1 is very likely to have been
   --  issued prior to S2. (Borrowed from GLADE s-garter).

   function Is_Locally_Terminated
     (Expected_Running_Tasks : Natural) return Boolean;
   --  Wrapper for the Is_Locally_Terminated function defined in ORB_Controller

   type Request_Status is (Outdated, Not_From_Father, Valid);
   --  Comment needed???

   ----------------------
   -- Critical Section --
   ----------------------

   Critical_Section : Mutex_Access;

   function Check_Stamp (S : Stamp_Type) return Request_Status;
   --  Checks the stamp S against the local TM Current_Stamp to decide if
   --  request is Valid, Outdated or not from father.
   --  Also updates the TM Current_Stamp if the request is valid.

   function Get_Stamp return Stamp_Type;
   --  Returns the TM Current_Stamp

   -------------
   -- Logging --
   -------------

   package L is new Log.Facility_Log ("polyorb.termination_manager");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
                renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;

   -------------
   -- Actions --
   -------------

   function Do_Is_Terminated (TM : Term_Manager_Access; Stamp : Stamp_Type)
                              return Boolean;

   function Do_Terminate_Now (TM : Term_Manager_Access; Stamp : Stamp_Type)
                              return Boolean;

   ----------------------
   -- Do_Is_Terminated --
   ----------------------

   function Do_Is_Terminated (TM : Term_Manager_Access; Stamp : Stamp_Type)
     return Boolean is
   begin
      return Is_Terminated (TM, Stamp);
   end Do_Is_Terminated;

   ----------------------
   -- Do_Terminate_Now --
   ----------------------

   function Do_Terminate_Now (TM : Term_Manager_Access; Stamp : Stamp_Type)
     return Boolean is
   begin
      return Terminate_Now (TM, Stamp);
   end Do_Terminate_Now;

   ------------------------
   -- Call_On_Neighbours --
   ------------------------

   function Call_On_Neighbours
     (A      : Action;
      A_Name : String;
      Stamp  : Stamp_Type) return Boolean
   is
      use BO_Ref_Lists;
      use References;
      use Smart_Pointers;

      L        : BO_Ref_List := Get_Binding_Objects (Setup.The_ORB);
      R        : References.Ref;
      NK       : Node_Kind;
      RACW     : Term_Manager_Access;
      Status   : Boolean := True;
      N_Status : Boolean;
   begin
      pragma Debug (C,
        O ("Call_On_Neighbours (" & A_Name & "," & Stamp'Img & "): enter"));

      All_Binding_Objects :
      while not Is_Empty (L) loop
         declare
            use Ada.Exceptions;
            BO_Ref : Smart_Pointers.Ref;
         begin
            Extract_First (L, BO_Ref);
            Extract_TM_Reference_From_BO
              (BO  => Binding_Object_Access (Entity_Of (BO_Ref)),
               Ref => R,
               NK  => NK);

            case NK is
               when DSA_Node | Unknown =>
                  RACW := Ref_To_Term_Manager_Access (R);

                  pragma Debug
                    (C, O ("Calling "
                           & A_Name & " (" & Stamp'Img & ")"
                           & " on neighbour..."));
                  N_Status := A (RACW, Stamp);
                  pragma Debug (C, O ("-> " & N_Status'Img));
                  Status := Status and N_Status;

               when DSA_Node_Without_TM =>
                  pragma Debug
                    (C, O ("DSA neighbour without TM"));
                  Status := False;

               when Non_DSA_Node =>
                  --  Non-DSA nodes do not take part in the global decision

                  null;
            end case;

            --  XXX A server only, no tasking partition, will not take part
            --  in the global decision. Indeed we cannot determine the
            --  kind of node from a client BO. So the server only node kind
            --  will be marked as Unknown. Because it cannot have a running
            --  TM (no tasking), request will fail and Status won't be marked
            --  as false, so others partitions won't wait for it to finish.

            Decrement_Activity;
         exception
            when System.RPC.Communication_Error =>
               Decrement_Activity;
         end;
      end loop All_Binding_Objects;

      pragma Debug (C,
        O ("Call_On_Neighbours (" & A_Name & ", " & Stamp'Img & "): leave -> "
           & Status'Img));
      return Status;
   end Call_On_Neighbours;

   -----------------
   -- Check_Stamp --
   -----------------

   function Check_Stamp (S : Stamp_Type) return Request_Status
   is
      Result : Request_Status;
   begin
      Enter (Critical_Section);
      pragma Debug (C, O ("Check_Stamp: stamp =" & S'Img
                            & ", Current =" & The_TM.Current_Stamp'Img));

      if S < The_TM.Current_Stamp then
         --  If stamp is older than current stamp, this is an outdated message

         Result := Outdated;

      elsif S = The_TM.Current_Stamp then
         --  If stamp is equal to the current stamp then the request is not
         --  from a father node.

         Result := Not_From_Father;

      elsif S > The_TM.Current_Stamp then
         --  If stamp is more recent than current stamp, this is a new wave,
         --  update the current stamp.

         Result := Valid;
         The_TM.Current_Stamp := S;
      end if;

      Leave (Critical_Section);
      pragma Debug (C, O ("Check_Stamp: -> " & Result'Img));
      return Result;
   end Check_Stamp;

   ---------------
   -- Get_Stamp --
   ---------------

   function Get_Stamp return Stamp_Type
   is
      Result : Stamp_Type;
   begin
      Enter (Critical_Section);
      Result := The_TM.Current_Stamp;
      Leave (Critical_Section);
      if Result = Stamp_Type'Last then
         raise Program_Error with "termination timestamp wrapped!";
      end if;
      return Result;
   end Get_Stamp;

   -------------------------
   -- In_Inititiator_Loop --
   -------------------------

   procedure In_Initiator_Loop is
   begin
      --  ??? This should be tested only once, not at each term loop iteration
      if The_TM.Termination_Policy = Local_Termination then
         pragma Debug (C, O ("A partition cannot be the initiator"
                         &" and have a local termination policy."));
         raise Program_Error;
      end if;

      pragma Debug (C, O ("In_Initiator_Loop: start wave"));
      if Is_Terminated (The_TM, Get_Stamp + 1) then
         The_TM.Terminated := Terminate_Now (The_TM, Get_Stamp + 1);
      end if;
   end In_Initiator_Loop;

   ---------------------
   --  In_Slave_Loop --
   ---------------------

   procedure In_Slave_Loop is
   begin
      case The_TM.Termination_Policy is
         when Local_Termination =>
            The_TM.Terminated := Is_Locally_Terminated
                                   (The_TM.Non_Terminating_Tasks);

         when Global_Termination | Deferred_Termination =>
            null;
      end case;
   end In_Slave_Loop;

   ---------------------------
   -- Is_Locally_Terminated --
   ---------------------------

   function Is_Locally_Terminated
     (Expected_Running_Tasks : Natural) return Boolean
   is
      Result : Boolean;
   begin
      --  Theoretically we should just test Is_Locally_Terminated once.
      --  However in some cases the I/O task that received the message for a
      --  wave might still be running (about to be rescheduled) at the first
      --  try, so we wait a tiny bit and check again if at first we don't get
      --  a positive result.

      pragma Debug (C, O ("Is_Locally_Terminated: enter"));
      for J in 1 .. 3 loop
         Enter_ORB_Critical_Section (The_ORB.ORB_Controller);
         pragma Debug (C, O ("Is_Locally_Terminated: in critical section, "
                             & "iteration" & J'Img));
         Result := Is_Locally_Terminated
                     (The_ORB.ORB_Controller, Expected_Running_Tasks);
         Leave_ORB_Critical_Section (The_ORB.ORB_Controller);
         exit when Result;
         Relative_Delay (The_TM.Time_Between_Waves / 10);
      end loop;

      pragma Debug (C, O ("Is_Locally_Terminated: leave, Result = "
        & Result'Img));
      return Result;
   end Is_Locally_Terminated;

   -------------------
   -- Is_Terminated --
   -------------------

   function Is_Terminated (TM : access Term_Manager; Stamp : Stamp_Type)
     return Boolean
   is
      Local_Decision        : Boolean := True;
      Neighbours_Decision   : Boolean := True;
      Non_Terminating_Tasks : Natural;
   begin
      case Check_Stamp (Stamp) is
         when Not_From_Father =>
            --  If the request is not from a father node, we immediatly answer
            --  True as this does not change the computation.

            return True;

         when Outdated =>
            return False;

         when Valid =>
            null;
            pragma Debug (C, O ("New wave (Is_Terminated) received"));
      end case;

      --  Compute the number of expected non terminating tasks

      pragma Debug (C, O ("TM.Is_Initiator = " & TM.Is_Initiator'Img));
      pragma Debug (C, O ("TM.Non_Terminating_Tasks ="
                            & TM.Non_Terminating_Tasks'Img));

      if not TM.Is_Initiator then

         --  If the termination manager is not the initiator, local termination
         --  will be checked inside a request job so one of the ORB tasks will
         --  be running at that time, so we have one more non terminating task.

         Non_Terminating_Tasks := TM.Non_Terminating_Tasks + 1;
      else
         Non_Terminating_Tasks := TM.Non_Terminating_Tasks;
      end if;

      --  If node is not locally terminated or active, return False

      pragma Debug (C, O ("Expect" & Non_Terminating_Tasks'Img
        & " remaining tasks"));

      if not Is_Locally_Terminated (Non_Terminating_Tasks) then
         pragma Debug
           (C, O ("Node is not locally terminated, refusing termination."));
         Local_Decision := False;
      end if;

      if Is_Active then
         pragma Debug
           (C, O ("Node is active (has sent messages since the last wave),"
                     & " refusing termination."));
         Local_Decision := False;
      end if;

      --  If node is locally terminated and has not sent any messages the
      --  answer depends on its children.

      --  We propagate the wave even if locally we are refusing termination.
      --  This is to reset the activity counter in all child partitions.
      --  Is this desirable if we are the initiator???

      Neighbours_Decision :=
        Call_On_Neighbours
          (Do_Is_Terminated'Access, "Do_Is_Terminated", Stamp);

      pragma Debug (C, O ("Is_Terminated: Local " & Local_Decision'Img
                            & " / Neighbours " & Neighbours_Decision'Img));
      --  Reset Activity counter

      Reset_Activity;

      return Local_Decision and then Neighbours_Decision;
   end Is_Terminated;

   -----------
   -- Start --
   -----------

   procedure Start (TM                 : access Term_Manager;
                    T                  : Termination_Type;
                    Initiator          : Boolean;
                    Time_Between_Waves : Duration;
                    Time_Before_Start  : Duration)
   is
      Thread_Acc : Thread_Access;
   begin
      Create (Critical_Section);

      TM.Time_Between_Waves := Time_Between_Waves;
      TM.Time_Before_Start  := Time_Before_Start;
      TM.Termination_Policy := T;
      TM.Is_Initiator       := Initiator;

      --  Since we are running the termination loop in a new task,
      --  we should consider it as a non terminating task.

      TM.Non_Terminating_Tasks := TM.Non_Terminating_Tasks + 1;

      pragma Debug (C, O ("Starting TM: Is_Initiator = " & Initiator'Img
                            & " / NTT =" & TM.Non_Terminating_Tasks'Img));

      Thread_Acc := Run_In_Task
        (TF               => Get_Thread_Factory,
         Default_Priority => System.Any_Priority'First,
         P                => Termination_Loop'Access);

      pragma Assert (Thread_Acc /= null);
   end Start;

   ----------------------
   -- Termination_Loop --
   ----------------------

   procedure Termination_Loop is
      use Ada.Exceptions;
   begin
      Relative_Delay (The_TM.Time_Before_Start);
      loop
         if The_TM.Is_Initiator then
            In_Initiator_Loop;
         else
            In_Slave_Loop;
         end if;

         exit when The_TM.Terminated;
         Relative_Delay (The_TM.Time_Between_Waves);
      end loop;

      PolyORB.Initialization.Shutdown_World (Wait_For_Completion => True);
   exception
      when E : others =>
         pragma Debug (C, O ("Termination_Loop: got "
                            & Exception_Information (E)));
         PolyORB.Initialization.Shutdown_World (Wait_For_Completion => False);
         raise;
   end Termination_Loop;

   -------------------
   -- Terminate_Now --
   -------------------

   function Terminate_Now (TM : access Term_Manager; Stamp : Stamp_Type)
     return Boolean
   is
      Status : Boolean;
   begin
      --  Ignore the message if it is not from father or if it is outdated

      case Check_Stamp (Stamp) is
         when Valid  =>
            pragma Debug
              (C, O ("Terminate_Now: received wave with valid time stamp"));
            null;
         when others =>
            pragma Debug
              (C, O ("Terminate_Now: received wave with junk time stamp"));
            return True;
      end case;

      --  Call Terminate_Now on all of its childs

      pragma Debug (C, O ("Terminating children"));

      Status := Call_On_Neighbours
                  (Do_Terminate_Now'Access, "Do_Terminate_Now", Stamp);
      pragma Assert (Status);

      --  Terminate this partition, except if it has the Deferred_Termination
      --  policy.

      if TM.Termination_Policy /= Deferred_Termination then
         TM.Terminated := True;
      end if;

      return TM.Terminated;
   end Terminate_Now;

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

end PolyORB.Termination_Manager;
