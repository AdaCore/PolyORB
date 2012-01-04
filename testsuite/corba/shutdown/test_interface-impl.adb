------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  T E S T _ I N T E R F A C E . I M P L                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;
with Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;

with CORBA.ORB;
pragma Elaborate_All (CORBA.ORB);

with PolyORB.Utils.Report; use PolyORB.Utils.Report;
pragma Elaborate_All (PolyORB.Utils.Report);

with Test_Interface.Skel;
pragma Warnings (Off, Test_Interface.Skel);

package body Test_Interface.Impl is

   Request_In_Progress : Boolean := False;
   Request_Completed   : Boolean := False;

   task Killer_Task is
      entry Do_Shutdown (Wait : Boolean);
   end Killer_Task;

   task body Killer_Task is
      Wait_For_Completion : Boolean;
      Got_Exception : Boolean := False;
   begin
      accept Do_Shutdown (Wait : Boolean) do
         Wait_For_Completion := Wait;
      end Do_Shutdown;

      Output ("Shutting down, Wait = " & Wait_For_Completion'Img, True);
      begin
         CORBA.ORB.Shutdown (Wait_For_Completion);
      exception
         when E : others =>
            Output ("Unexpected exception "
              & Ada.Exceptions.Exception_Name (E), False);
            Put_Line (Ada.Exceptions.Exception_Information (E));
            Got_Exception := True;
      end;

      if Got_Exception then
         null;

      elsif Wait_For_Completion then
         Output ("Shutdown completed with wait",
           Request_Completed and not Request_In_Progress);

      else
         Output ("Shutdown completed without wait",
           Request_In_Progress and not Request_Completed);
      end if;
   end Killer_Task;

   procedure Trigger_Server_Shutdown (Self : access Object; Wait : Boolean) is
      pragma Unreferenced (Self);
   begin
      Request_In_Progress := True;
      Killer_Task.Do_Shutdown (Wait);
      Output ("Triggered server shutdown", True);
      delay 0.5;
      Request_In_Progress := False;
      Request_Completed   := True;
   end Trigger_Server_Shutdown;

   type Witness is new Ada.Finalization.Controlled with null record;
   procedure Finalize (X : in out Witness) is
      pragma Unreferenced (X);
   begin
      Output ("Client has terminated", Client_Terminated);
      Output ("Witnessing completion and finalization",
        Request_Completed and not Request_In_Progress);
      End_Report;
   end Finalize;

   A_Witness : Witness;
   pragma Unreferenced (A_Witness);

end Test_Interface.Impl;
