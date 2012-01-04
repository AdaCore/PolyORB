------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO; use Ada.Text_IO;
with RCI;
with GNAT.OS_Lib;

procedure Client is

   task Monitor is
      entry Call (Call_Id : Integer);
      entry Check;
      entry Kill;
   end Monitor;

   task body Monitor is
      My_Call_Id : Natural;
   begin
      loop
         select
            accept Call (Call_Id : Integer) do
               My_Call_Id := Call_Id;
            end Call;
            delay 0.1;
            Put_Line ("Call" & My_Call_Id'Img & " in progress (blocked):"
                      & RCI.Blocked_Calls'Img);
         or
            accept Check do
               Put_Line ("Monitor: checking blocked calls");
               Put_Line ("Calls blocked:" & RCI.Blocked_Calls'Img);
            end Check;
         or
            accept Kill do
               null;
            end Kill;
            delay 0.1;
            RCI.Allow_Terminate;
            GNAT.OS_Lib.OS_Exit (0);
         end select;
      end loop;
   end Monitor;

   procedure Do_Call (Call_Id : Integer; S : String) is
   begin
      Put_Line ("Call" & Call_Id'Img & ": " & S);
      select
         delay 0.5;
         Put_Line ("Call" & Call_Id'Img & " timed out");
      then abort
         RCI.Block_On_Entry (Call_Id);
      end select;
   end Do_Call;

   Num_Calls : constant := 7;
   --  Vary this constant and observe behaviour to identify memory leaks

begin
   Put_Line ("Client started");

   --  Call 1: sanity check. The call returns normally,
   --  the timeout does not trigger.

   Do_Call (1, "passing");

   --  Calls 1001 .. 10xx: Aborted calls
   --  Call blocks, timeout triggers, request is cancelled.
   --  For each call the monitor must report 1 blocked call
   --  (previous calls have been cancelled).

   for J in 1001 .. 1000 + Num_Calls loop
      Put_Line ("Call" & J'Img & ": block and abort call");
      Monitor.Call (J);
      Do_Call (J, "blocking");
   end loop;

   --  Sanity check: no call remains blocked

   Put_Line ("Client idle");
   Monitor.Check;

   --  Call 3: blocking/terminating
   --  Call blocks, partition terminates and disconnects.
   --  The server must abort and clean up the request,
   --  and then cleanly terminate.

   Put_Line ("Call 3: block and terminate partition");
   Monitor.Kill;
   Do_Call (3, "blocking/terminating");
end Client;
