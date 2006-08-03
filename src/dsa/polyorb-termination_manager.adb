------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T E R M I N A T I O N _ M A N A G E R           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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
with System.Partition_Interface;
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
   use System.Partition_Interface;

   procedure Global_Termination_Loop;
   --  Main loop for global and deferred termination

   procedure Local_Termination_Loop;
   --  Main loop for local termination

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
   pragma Unreferenced (C); --  For conditional pragma Debug


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
     (A : Action; TM : access Term_Manager; Stamp : Stamp_Type) return Boolean
   is
      pragma Unreferenced (TM);

      use BO_Ref_Lists;
      use References;
      use Smart_Pointers;

      L      : constant BO_Ref_List := Get_Binding_Objects (Setup.The_ORB);
      It     : Iterator;
      R      : References.Ref;
      RACW   : Term_Manager_Access;
      Status : Boolean := True;
   begin
      It := First (L);

      All_Binding_Objects :
      while not Last (It) loop
         pragma Debug (O ("Calling Action on a neighbour..."));

         declare
            use Ada.Exceptions;
         begin
            R := Extract_TM_Reference_From_BO
                   (Binding_Object_Access (Entity_Of (Value (It).all)));

            RACW := Ref_To_Term_Manager_Access (R);
            Status := A (RACW, Stamp) and then Status;
            Decrement_Activity;

         exception
            when DSA_Node_Without_TM => Status := False;
            when Not_A_DSA_Node => null;
            when System.RPC.Communication_Error =>
               Decrement_Activity;
         end;

         Next (It);
      end loop All_Binding_Objects;
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

      if S < The_TM.Current_Stamp then

         --  If stamp is older than current stamp, this is an outdated message

         Result := Outdated;

      elsif S = The_TM.Current_Stamp then

         --  If stamp is equal to the current stamp then the request is not
         --  from a Father node.

         Result := Not_From_Father;

      elsif S > The_TM.Current_Stamp then

         --  If stamp is more recent than current stamp, this is a new wave,
         --  update the current stamp.

         Result := Valid;
         The_TM.Current_Stamp := S;
      end if;

      Leave (Critical_Section);

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
      return Result;
   end Get_Stamp;

   -----------------------------
   -- Global_Termination_Loop --
   -----------------------------

   procedure Global_Termination_Loop is
      use Ada.Exceptions;
      Status : Boolean := False;
   begin
      pragma Debug (O ("Global termination loop started"));

      if The_TM.Is_Initiator then

         pragma Debug (O ("We are the initiator"));

         --  We are the initiator, loop and send termination waves until global
         --  termination is true.

         Relative_Delay (The_TM.Time_Before_Start);

         loop

            if Is_Locally_Terminated (The_TM.Non_Terminating_Tasks)
                 and then Is_Terminated (The_TM, Get_Stamp + 1)
            then
               Status := Terminate_Now (The_TM, Get_Stamp + 1);
               pragma Assert (Status);
               The_TM.Terminated := True;

               --  We have global termination, exit the loop if the termination
               --  policy is not deferred.

               exit when The_TM.Termination_Policy /= Deferred_Termination;
            end if;

            Relative_Delay (The_TM.Time_Between_Waves);
         end loop;

      else

         --  We are not the initiator loop until we are told to terminate in
         --  the case of Global termination, loop forever in the case of
         --  Deferred termination.

         loop
            exit when The_TM.Termination_Policy /= Deferred_Termination
              and then The_TM.Terminated;
            Relative_Delay (0.0);
         end loop;

      end if;

      --  Shutdown the partition

      pragma Assert (The_TM.Terminated);
      pragma Debug (O ("Terminating me"));
      Shutdown (The_ORB, False);
   exception
      when e : others =>
         pragma Debug (O (Exception_Information (e)));

         --  Something has gone wrong, raise the exception and try to shutdown
         --  PolyORB.

         Shutdown (The_ORB, False);
         raise;
   end Global_Termination_Loop;

   ----------------------------
   -- Local_Termination_Loop --
   ----------------------------

   procedure Local_Termination_Loop is
      use Ada.Exceptions;
   begin
      pragma Debug (O ("Local termination loop started"));

      if The_TM.Is_Initiator then
         pragma Debug (O ("A partition cannot be the initiator"
                         &" and have a local termination policy."));
         raise Program_Error;
      end if;

      --  Loop until the partition is locally terminated

      loop
         exit when Is_Locally_Terminated (The_TM.Non_Terminating_Tasks);
         Relative_Delay (0.0);
      end loop;

      The_TM.Terminated := True;
      pragma Debug (O ("Terminating me"));
      Shutdown (The_ORB, False);
   exception
      when e : others =>
         pragma Debug (O (Exception_Information (e)));

         --  Something has gone wrong, raise the exception and try to shutdown
         --  PolyORB.

         Shutdown (The_ORB, False);
         raise;
   end Local_Termination_Loop;

   ---------------------------
   -- Is_Locally_Terminated --
   ---------------------------

   function Is_Locally_Terminated
     (Expected_Running_Tasks : Natural) return Boolean
   is
      Result : Boolean;
   begin
      Enter_ORB_Critical_Section (The_ORB.ORB_Controller);
      Result := Is_Locally_Terminated (The_ORB.ORB_Controller,
                                       Expected_Running_Tasks);
      Leave_ORB_Critical_Section (The_ORB.ORB_Controller);

      return Result;
   end Is_Locally_Terminated;

   -------------------
   -- Is_Terminated --
   -------------------

   function Is_Terminated (TM : access Term_Manager; Stamp : Stamp_Type)
     return Boolean
   is
      Local_Decision : Boolean := True;
      Neighbours_Decision : Boolean := True;
      Non_Terminating_Tasks : Natural;
   begin

      case Check_Stamp (Stamp) is
         when Not_From_Father =>
            return True;

            --  If the request is not from a Father node, we immediatly answer
            --  True as this does not change the computation.

         when Outdated        =>
            return False;
         when Valid           =>
            null;
            pragma Debug (O ("New Wave (Is_Terminated) received"));
      end case;

      --  Compute the number of expected non terminating tasks

      if not TM.Is_Initiator then

         --  If the termination manager is not the initiator, local termination
         --  will be checked inside a request job so one of the orb tasks will
         --  be running at that time, and we have one more non terminating
         --  task.

         Non_Terminating_Tasks := TM.Non_Terminating_Tasks + 1;
      else
         Non_Terminating_Tasks := TM.Non_Terminating_Tasks;
      end if;

      --  If node is not locally terminated return False

      if not Is_Locally_Terminated (Non_Terminating_Tasks) then
         pragma
           Debug (O ("Node is not locally terminated, refusing termination"));
         Local_Decision := False;
      end if;

      --  If node is locally terminated and has not sent any messages the
      --  answer depends on its childs.

      --  We propagate the wave even if locally we are refusing termination.
      --  This is to reset the activity counter in all the childs partitions.

      Neighbours_Decision :=
        Call_On_Neighbours (Do_Is_Terminated'Access, TM, Stamp);

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
      Loop_Acc : Parameterless_Procedure;
   begin

      Create (Critical_Section);

      TM.Time_Between_Waves := Time_Between_Waves;
      TM.Time_Before_Start  := Time_Before_Start;
      TM.Termination_Policy := T;
      TM.Is_Initiator := Initiator;

      --  Since we are running local or global loop in a new task, we should
      --  consider it as a non terminating task.

      TM.Non_Terminating_Tasks := TM.Non_Terminating_Tasks + 1;

      if T /= Local_Termination then
         Loop_Acc := Global_Termination_Loop'Access;
      else
         Loop_Acc := Local_Termination_Loop'Access;
      end if;

      Thread_Acc := Run_In_Task
        (TF               => Get_Thread_Factory,
         Default_Priority => System.Any_Priority'First,
         P                => Loop_Acc);

      pragma Assert (Thread_Acc /= null);

   end Start;

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
            null;
            pragma Debug (O ("New Wave (Terminate_Now) received"));
         when others =>
            return True;
      end case;

      --  Call Terminate_Now on all of its childs

      pragma Debug (O ("Terminating Childs"));

      Status := Call_On_Neighbours (Do_Terminate_Now'Access, TM, Stamp);
      pragma Assert (Status);

      --  Terminate this partition

      TM.Terminated := True;

      return True;
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
