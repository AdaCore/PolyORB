------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
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

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with CORBA.ORB;
use  CORBA;
use  CORBA.ORB;

with PolyORB.Setup.Thread_Pool_Server;
pragma Unreferenced (PolyORB.Setup.Thread_Pool_Server);

with PolyORB.ORB.Thread_Pool; use PolyORB.ORB.Thread_Pool;

with Svc;

with Shell;
with Transient_Tasks;

procedure Client is
   Svc_Ref    : Svc.Ref;

   Count : Integer;

   procedure Transient_Processing (Id : Natural) is
   begin
      Svc.Wait (Svc_Ref, CORBA.Short (Id));
   end Transient_Processing;

   package Client_Tasks is new Transient_Tasks;
   use Client_Tasks;
   Clients : Transient_Task_Array_Access renames Client_Tasks.Transient_Tasks;

   type Command_Type is (Call, Quit);
   procedure Handle_Command (Command : Command_Type; Argument : String);
   package Client_Shell is new Shell (Command_Type);

   procedure Handle_Command (Command : Command_Type; Argument : String) is
   begin
      case Command is
         when Quit =>
            raise Client_Shell.Exit_Shell;

         when Call =>
            Clients (Natural'Value (Argument)).Enter;
      end case;
   end Handle_Command;

begin
   Put_Line ("Client: enter");
   CORBA.ORB.Initialize ("ORB");

   Put_Line ("min spare threads:" & Get_Minimum_Spare_Threads'Img);
   Put_Line ("max spare threads:" & Get_Maximum_Spare_Threads'Img);
   Put_Line ("max threads:      " & Get_Maximum_Threads'Img);

   Count := Get_Maximum_Threads;

   Put_Line ("Client: ORB initialized");

   Count := Count * 2;
   Client_Tasks.Start (Count);
   Put_Line ("Client:" & Natural'Image (Clients'Length)
             & " tasks created");

   declare
      IOR_File : File_Type;
      IOR : String (1 .. 1024);
      Last : Integer;
   begin
      Open (IOR_File, In_File, "svc.ior");
      Get_Line (IOR_File, IOR, Last);
      Close (IOR_File);
      String_To_Object (To_CORBA_String (IOR (1 .. Last)), Svc_Ref);
   end;
   Put_Line ("IOR read from svc.ior");

   Client_Shell.Interact;

   CORBA.ORB.Shutdown (Wait_For_Completion => False);
   Put_Line ("Client: leave");
   GNAT.OS_Lib.OS_Exit (0);
end Client;
