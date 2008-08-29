------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2008, Free Software Foundation, Inc.             --
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

with PolyORB.ORB.Thread_Pool; use PolyORB.ORB.Thread_Pool;
with PolyORB.ORB_Controller;  use PolyORB.ORB_Controller;
with PolyORB.Task_Info;       use PolyORB.Task_Info;
with PolyORB.Asynch_Ev;       use PolyORB.Asynch_Ev;

with Svc.Impl;

with Conditions;        use Conditions;
with Shell;
with Transient_Tasks;

procedure Server is
   Svc_Object : aliased Svc.Impl.Object;
   Svc_Ref    : Svc.Ref;

   Count : Integer;

   type Transient_Info is record
      Condition   : aliased Boolean;
      Info_Access : aliased Task_Info_Access;
   end record;
   type Transient_Info_Array is array (Natural range <>) of Transient_Info;
   type Transient_Info_Array_Access is access all Transient_Info_Array;
   Transient_Infos : Transient_Info_Array_Access;

   procedure Transient_Processing (Id : Natural) is
   begin
      Transient_Infos (Id).Condition := False;
      Put_Line ("Server transient" & Id'Img & ": enter");
      PolyORB.ORB.Run
        (PolyORB.Setup.The_ORB,
         Exit_Condition => (Transient_Infos (Id).Condition'Unchecked_Access,
                            Transient_Infos (Id).Info_Access'Unchecked_Access),
         May_Poll       => True);
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
               TI : Task_Info renames Transient_Infos (Id).Info_Access.all;
            begin
               Enter_ORB_Critical_Section
                 (PolyORB.Setup.The_ORB.ORB_Controller);
               Put_Line ("awaking transient task" & Id'Img
                 & " from " & State (TI)'Img);
               Transient_Infos (Id).Condition := True;
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

   Initiate_Servant (Svc_Object'Unchecked_Access, Svc_Ref);
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
