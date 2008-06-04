------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.THREAD_POLICY.SINGLE_THREAD              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2007, Free Software Foundation, Inc.          --
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

--  Implementation of the POA Policy 'Single Thread'.

--  Under this policy, upcalls made by a POA shall not be made
--  concurrently. The POA will still allow reentrant code from any
--  object implementation to itself, or to another object
--  implementation managed by the same POA.

with PolyORB.Annotations;
with PolyORB.Log;
with PolyORB.Servants;

with PolyORB.Tasking.Advanced_Mutexes;

package body PolyORB.POA_Policies.Thread_Policy.Single_Thread is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.poa_policies.thread_policy.single_thread");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type ST_Note is new PolyORB.Annotations.Note with record
      Lock : PolyORB.Tasking.Advanced_Mutexes.Adv_Mutex_Access;
   end record;

   Empty_Note : constant ST_Note :=
     (PolyORB.Annotations.Note with Lock => null);

   ------------
   -- Create --
   ------------

   function Create return Single_Thread_Policy_Access is
      Result : constant Single_Thread_Policy_Access
        := new Single_Thread_Policy;

   begin
      ThreadPolicy (Result.all).Executor := new Single_Thread_Executor;

      return Result;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        Single_Thread_Policy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Unreferenced (Error);
      pragma Warnings (On);

   begin
      null;
      --  No rule to test
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Single_Thread_Policy)
      return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return "THREAD_POLICY.SINGLE_THREAD";
   end Policy_Id;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   function Handle_Request_Execution
     (Self      : access Single_Thread_Executor;
      Msg       :        PolyORB.Components.Message'Class;
      Requestor :        PolyORB.Components.Component_Access)
      return PolyORB.Components.Message'Class
   is
      use PolyORB.Annotations;
      use PolyORB.Servants;
      use PolyORB.Tasking.Advanced_Mutexes;

      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      N : ST_Note;

   begin
      --  This policy only prevents us to make concurrent calls to the
      --  same servant, but allows us to make reentrant call.

      --  To implement this policy we attach an Advanced Mutex to the
      --  servant. This construction provides the above expected
      --  properties.

      --  XXX reentrant calls are not fully supported. Currently ORB.Run
      --  does not allow to specify the thread attached to request handling.
      --  This work in No_Tasking tasking policy.

      pragma Debug (O ("Handle_Request_Execution: Enter"));

      --  Test if the servant has been attached to a advanced mutex.
      --  XXX Note that this could (should ?) be done by the POA.
      --  when setting the thread policy. This would avoid a call to
      --  Get Note to test if there is Adv_Mutex already attached.
      --  but the POA will require visibility on this package ...

      Get_Note (Notepad_Of (Servant_Access (Requestor)).all,
                N,
                Empty_Note);

      if N = Empty_Note then
         declare
            AM : Adv_Mutex_Access;
            New_Note : ST_Note;
         begin
            pragma Debug (O ("Attach a mutex to the servant."));
            Create (AM);
            New_Note := (PolyORB.Annotations.Note with Lock => AM);
            Set_Note
              (Notepad_Of (Servant_Access (Requestor)).all,
               New_Note);
         end;
      end if;

      Get_Note (Notepad_Of (Servant_Access (Requestor)).all, N);

      pragma Debug (O ("Waiting on servant's lock"));
      Enter (N.Lock);
      pragma Debug (O ("Waiting done"));

      declare
         Result : constant PolyORB.Components.Message'Class :=
           Execute_Servant (Servant_Access (Requestor), Msg);
      begin
         Leave (N.Lock);
         pragma Debug (O ("Handle_Request_Execution: Leave"));
         return Result;
      end;

   end Handle_Request_Execution;

end PolyORB.POA_Policies.Thread_Policy.Single_Thread;
