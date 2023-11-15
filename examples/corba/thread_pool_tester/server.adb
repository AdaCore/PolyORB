------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2023, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with GNAT.OS_Lib;    use GNAT.OS_Lib;

with CORBA.ORB;
use  CORBA;
use  CORBA.ORB;
with PortableServer.POA;
with PortableServer.POAManager;

with PolyORB.Tasking.Condition_Variables;
use  PolyORB.Tasking.Condition_Variables;

with PolyORB.CORBA_P.Server_Tools;
use  PolyORB.CORBA_P.Server_Tools;

with PolyORB.ORB;             use PolyORB.ORB;
with PolyORB.Setup.Thread_Pool_Server;
pragma Unreferenced (PolyORB.Setup.Thread_Pool_Server);

with PolyORB.Asynch_Ev;       use PolyORB.Asynch_Ev;
with PolyORB.ORB.Thread_Pool; use PolyORB.ORB.Thread_Pool;
with PolyORB.ORB_Controller;  use PolyORB.ORB_Controller;
with PolyORB.Requests;
with PolyORB.Task_Info;       use PolyORB.Task_Info;

with Svc.Impl;

with Conditions;        use Conditions;
with Shell;
with Transient_Tasks;

procedure Server is
   Svc_Ref    : Svc.Ref;

   Count : Integer;

   type Transient_Info_Array is array (Natural range <>) of
     aliased PolyORB.Requests.Request;
   type Transient_Info_Array_Access is access all Transient_Info_Array;
   Transient_Infos : Transient_Info_Array_Access;

   procedure Transient_Processing (Id : Natural);

   procedure Transient_Processing (Id : Natural) is
   begin
      Transient_Infos (Id).Completed := False;
      Put_Line ("Server transient" & Id'Img & ": enter");
      PolyORB.ORB.Run
        (PolyORB.Setup.The_ORB,
         Request  => Transient_Infos (Id)'Unchecked_Access,
         May_Exit => True);
      Put_Line ("Server transient" & Id'Img & ": leave");
   end Transient_Processing;

   package Server_Tasks is new Transient_Tasks;
   use Server_Tasks;
   Servers : Transient_Task_Array_Access renames Server_Tasks.Transient_Tasks;

   type Command_Type is (Signal, Status, Add, Del, Quit);
   procedure Handle_Command (Command : Command_Type; Argument : String);
   package Server_Shell is new Shell (Command_Type);

   procedure Handle_Command (Command : Command_Type; Argument : String) is
   begin
      case Command is
         when Quit =>
            raise Server_Shell.Exit_Shell;

         when Signal =>
            if Argument = "all" then
               for J in Condition_Variables'Range loop
                  Signal (Condition_Variables (J));
               end loop;
            else
               declare
                  Condition : Natural;
               begin
                  Condition := Natural'Value (Argument);
                  Signal (Condition_Variables (Condition));
               exception
                  when Constraint_Error =>
                     Put_Line ("bad condition id: " & Argument);
               end;
            end if;

         when Status =>
            declare
               function Status (O : access ORB_Controller) return String;
               pragma Import (Ada, Status, "polyorb__orb_controller__status");
            begin
               Put_Line (Status (ORB_Controller
                 (PolyORB.Setup.The_ORB.ORB_Controller.all)'Access));
            end;

         when Add =>
            Servers (Natural'Value (Argument)).Enter;

         when Del =>
            declare
               Id : constant Natural := Natural'Value (Argument);
               TI : Task_Info renames Transient_Infos (Id).Requesting_Task.all;
            begin
               Enter_ORB_Critical_Section
                 (PolyORB.Setup.The_ORB.ORB_Controller);
               Put_Line ("awaking transient task" & Id'Img
                 & " from " & State (TI)'Img);
               Transient_Infos (Id).Completed := True;
               case State (TI) is
                  when Idle =>
                     Signal (Condition (TI));
                  when Blocked =>
                     Abort_Check_Sources (Selector (TI).all);
                  when others =>
                     null;
               end case;
               Leave_ORB_Critical_Section
                 (PolyORB.Setup.The_ORB.ORB_Controller);
            end;
      end case;
   end Handle_Command;

begin
   Put_Line ("Server: enter");
   CORBA.ORB.Initialize ("ORB");
   PortableServer.POAManager.Activate
     (PortableServer.POA.Get_The_POAManager (Get_Root_POA));
   Put_Line ("Server: ORB initialized");

   Put_Line ("min spare threads:" & Get_Minimum_Spare_Threads'Img);
   Put_Line ("max spare threads:" & Get_Maximum_Spare_Threads'Img);
   Put_Line ("max threads:      " & Get_Maximum_Threads'Img);

   Count := Get_Maximum_Threads;
   Conditions.Create_Conditions (Count);
   Put_Line ("Server:" & Natural'Image (Condition_Variables'Length)
             & " CVs created");

   Transient_Infos := new Transient_Info_Array (0 .. Count - 1);
   Server_Tasks.Start (Count);
   Put_Line ("Server:" & Natural'Image (Condition_Variables'Length)
             & " transient tasks created");

   Initiate_Servant (new Svc.Impl.Object, Svc_Ref);
   Put_Line ("Server: servant initialized");

   declare
      IOR_File : File_Type;
   begin
      Create (IOR_File, Out_File, "svc.ior");
      Put_Line (IOR_File, To_Standard_String (Object_To_String (Svc_Ref)));
      Close (IOR_File);
   end;
   Put_Line ("IOR written to svc.ior");

   Server_Shell.Interact;

   CORBA.ORB.Shutdown (Wait_For_Completion => False);
   Put_Line ("Server: leave");
   GNAT.OS_Lib.OS_Exit (0);
exception
   when E : others =>
      Put_Line ("Server main loop got exception: "
        & Ada.Exceptions.Exception_Information (E));
end Server;
