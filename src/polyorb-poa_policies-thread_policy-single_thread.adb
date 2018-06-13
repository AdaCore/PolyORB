------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            POLYORB.POA_POLICIES.THREAD_POLICY.SINGLE_THREAD              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Implementation of the POA Policy 'Single Thread'.

--  Under this policy, upcalls made by a POA shall not be made concurrently.
--  The POA will still allow reentrant code from any object implementation to
--  itself, or to another object implementation managed by the same POA.

with PolyORB.Annotations;
with PolyORB.Log;
with PolyORB.Tasking.Advanced_Mutexes;

package body PolyORB.POA_Policies.Thread_Policy.Single_Thread is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.poa_policies.thread_policy.single_thread");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

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

   overriding procedure Check_Compatibility
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

   overriding function Policy_Id
     (Self : Single_Thread_Policy)
      return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return "THREAD_POLICY.SINGLE_THREAD";
   end Policy_Id;

   ------------------------
   -- Execute_In_Context --
   ------------------------

   overriding function Execute_In_Context
     (Self      : access Single_Thread_Executor;
      Req       : Requests.Request_Access;
      Requestor : Components.Component_Access) return Boolean
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

      pragma Debug (C, O ("Execute_In_Context: Enter"));

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
            pragma Debug (C, O ("Attach a mutex to the servant."));
            Create (AM);
            New_Note := (PolyORB.Annotations.Note with Lock => AM);
            Set_Note
              (Notepad_Of (Servant_Access (Requestor)).all,
               New_Note);
         end;
      end if;

      Get_Note (Notepad_Of (Servant_Access (Requestor)).all, N);

      pragma Debug (C, O ("Waiting on servant's lock"));
      Enter (N.Lock);
      pragma Debug (C, O ("Waiting done"));

      --  Now execute the request in the current task

      declare
         Res : constant Boolean :=
           Abortable_Execute_Servant (Servant_Access (Requestor), Req);
      begin
         Leave (N.Lock);
         return Res;
      end;
   end Execute_In_Context;

end PolyORB.POA_Policies.Thread_Policy.Single_Thread;
