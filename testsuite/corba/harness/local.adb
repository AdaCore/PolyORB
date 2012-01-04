------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                L O C A L                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;

with CORBA.Impl;
with CORBA.ORB;
with PortableServer;
with PolyORB.Utils.Report;
with Harness.Impl;

with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

procedure Local is
   use Ada.Real_Time;
   use CORBA;
   use PolyORB.Utils.Report;
   use Harness;
   use PolyORB.CORBA_P.Server_Tools;

begin
   New_Test ("Harness");
   CORBA.ORB.Initialize ("ORB");
   Output ("Initialization", True);

   declare
      Obj : constant CORBA.Impl.Object_Ptr := new Harness.Impl.Object;
      Ref : Harness.Ref;

      Ok : Boolean := True;

      T0, T1, T2 : Time;
      Delta1 : Duration;

      How_Many : Integer := 1_000;

   begin
      Initiate_Servant (PortableServer.Servant (Obj), Ref);
      Initiate_Server (True);

      if Ada.Command_Line.Argument_Count >= 1 then
         begin
            How_Many := Integer'Value (Ada.Command_Line.Argument (1));
         exception
            when others =>
               null;
         end;
      end if;

      T0 := Clock;
      for J in 1 .. How_Many loop
         Ok := Ok and (echoULong (Ref, 1234) = 1234);
      end loop;
      T1 := Clock;
      T2 := Clock;

      Output ("Test success", Ok);

      Delta1 := To_Duration (T1 - T0 - (T2 - T1));
      Ada.Text_IO.Put_Line ("Time: " & Duration'Image (Delta1) & "s");

      CORBA.ORB.Shutdown (Wait_For_Completion => False);

      End_Report;
   end;
end Local;
