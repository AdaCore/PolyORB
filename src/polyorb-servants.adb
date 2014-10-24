------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . S E R V A N T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2014, Free Software Foundation, Inc.          --
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

with PolyORB.Servants.Iface;
with PolyORB.Tasking.Abortables;
with PolyORB.Tasking.Threads;

package body PolyORB.Servants is

   type Req_Runnable is new PolyORB.Tasking.Threads.Runnable with record
      Servant   : access Servants.Servant'Class;
      Req       : Requests.Request_Access;
      Completed : Boolean := False;
      Aborted   : Boolean := True;
   end record;

   overriding procedure Run (RR : not null access Req_Runnable);

   ---------
   -- Run --
   ---------

   overriding procedure Run (RR : not null access Req_Runnable) is
   begin
      RR.Completed := Execute_Servant (RR.Servant, RR.Req);

      --  Note: Can't set RR.Req.Completed here, since this would allow the
      --  requesting task to destroy RR.Req and prevent us from obtaining
      --  the RR.Req.Upcall_Abortable_Mutex to clean RR.Req.Upcall_Abortable
      --  (see Abortable_Execute_Servant).

      RR.Aborted := False;
   end Run;

   -------------------------------
   -- Abortable_Execute_Servant --
   -------------------------------

   function Abortable_Execute_Servant
     (S   : not null access Servant'Class;
      Req : Requests.Request_Access) return Boolean
   is
      use PolyORB.Tasking.Abortables;

      R : aliased Req_Runnable := (Servant => S, Req => Req, others => <>);

      pragma Warnings (Off); --  WAG:FSF-4.5.0
      --  Hide warning "A is not referenced"
      A : aliased Abortable'Class :=
        Make_Abortable (Abortable_Tag, R'Unchecked_Access);
      pragma Warnings (On);

   begin
      Req.Upcall_Abortable := A'Unchecked_Access;
      A.Run;

      Req.Upcall_Abortable_Mutex.Enter;
      Req.Upcall_Abortable := null;
      Req.Upcall_Abortable_Mutex.Leave;

      --  If aborted, mark the request to inhibit sending of a reply

      if R.Aborted then
         Req.Aborted := True;
      end if;

      --  Generate Executed_Request if completed normally or aborted

      return R.Completed or R.Aborted;
   end Abortable_Execute_Servant;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (S : in out Servant) is
   begin
      Annotations.Destroy (S.Notepad);
   end Destroy;

   ------------------------
   -- Execute_In_Context --
   ------------------------

   function Execute_In_Context
     (Self      : access Executor;
      Req       : Requests.Request_Access;
      Requestor : Components.Component_Access) return Boolean
   is
      use PolyORB.Servants;
      pragma Unreferenced (Self);
   begin
      return Abortable_Execute_Servant (Servant_Access (Requestor), Req);
   end Execute_In_Context;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (S : Servant_Access)
     return PolyORB.Annotations.Notepad_Access is
   begin
      return S.Notepad'Access;
   end Notepad_Of;

   ------------------
   -- Set_Executor --
   ------------------

   procedure Set_Executor
     (S    : access Servant;
      Exec :        Executor_Access)
   is
   begin
      S.Exec := Exec;
   end Set_Executor;

   --------------------
   -- Handle_Message --
   --------------------

   overriding function Handle_Message
     (S   : not null access Servant;
      Msg : Components.Message'Class) return Components.Message'Class
   is
      use PolyORB.Servants.Iface;

   begin
      if Msg in Execute_Request then
         declare
            Req : constant Requests.Request_Access :=
              Execute_Request (Msg).Req;
         begin
            if Execute_In_Context
              (S.Exec, Req, PolyORB.Components.Component_Access (S))
            then
               return Executed_Request'(Req => Req);
            else
               return Components.Null_Message'(null record);
            end if;
         end;

      elsif Msg in Abort_Request then
         declare
            Req : constant Requests.Request_Access := Abort_Request (Msg).Req;
         begin
            Req.Upcall_Abortable_Mutex.Enter;
            if Req.Upcall_Abortable /= null then
               Req.Upcall_Abortable.Abort_Run;
            end if;
            Req.Upcall_Abortable_Mutex.Leave;
         end;
         return Components.Null_Message'(null record);

      else
         raise Program_Error;
      end if;
   end Handle_Message;

end PolyORB.Servants;
