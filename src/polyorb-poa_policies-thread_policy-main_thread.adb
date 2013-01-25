------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.THREAD_POLICY.MAIN_THREAD               --
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

pragma Ada_2005;

--  Implementation of the 'Main thread' POA Policy.

--  Under this policy, requests to *all* main-thread POAs are
--  processed sequentially.

with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

package body PolyORB.POA_Policies.Thread_Policy.Main_Thread is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.poa_policies.thread_policy.main_thread");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Main_Thread_Lock : PolyORB.Tasking.Mutexes.Mutex_Access;
   Initialized : Boolean := False;

   ------------
   -- Create --
   ------------

   function Create return Main_Thread_Policy_Access is
      Result : constant Main_Thread_Policy_Access := new Main_Thread_Policy;

   begin
      ThreadPolicy (Result.all).Executor := new Main_Thread_Executor;

      return Result;
   end Create;

   ---------------
   -- Policy_Id --
   ---------------

   overriding function Policy_Id
     (Self : Main_Thread_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      return "THREAD_POLICY.MAIN_THREAD";
   end Policy_Id;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   overriding procedure Check_Compatibility
     (Self           :        Main_Thread_Policy;
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

   ------------------------
   -- Execute_In_Context --
   ------------------------

   overriding function Execute_In_Context
     (Self      : access Main_Thread_Executor;
      Req       : Requests.Request_Access;
      Requestor : Components.Component_Access) return Boolean
   is
      use PolyORB.Servants;
      use PolyORB.Tasking.Mutexes;

      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      --  This policy only prevents us to have to concurrent calls to
      --  Main_Thread POAs.

      --  XXX This dirty implementation associates a global lock to all
      --  Main_Thread POA.

      --  XXX However, this is a waste of ressources as a number of threads
      --  would wait on a given and known lock. We should try to specialize
      --  threads, and have only one threads to handle all upcalls made on all
      --  main_thread POAs ? cf PolyORB.ORB.Thread_Per_Session for a pattern.

      pragma Debug (C, O ("Execute_In_Context: Enter"));

      if not Initialized then
         pragma Debug (C, O ("Initialize policy"));
         Create (Main_Thread_Lock);
         Initialized := True;
      end if;

      pragma Debug (C, O ("Waiting on Main Thread's lock"));
      Enter (Main_Thread_Lock);
      pragma Debug (C, O ("Waiting done"));

      declare
         Res : constant Boolean :=
           Abortable_Execute_Servant (Servant_Access (Requestor), Req);
      begin
         Leave (Main_Thread_Lock);
         pragma Debug (C, O ("Execute_In_Context: Leave"));
         return Res;
      end;
   end Execute_In_Context;

end PolyORB.POA_Policies.Thread_Policy.Main_Thread;
