------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C L I E N T _ C O M M O N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2012, Free Software Foundation, Inc.             --
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

with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;

with CORBA.ORB;

with PolyORB.Utils.Report;

with Harness;

package body Client_Common is

   -------------------
   -- Launch_Client --
   -------------------

   procedure Launch_Client is
      use Ada.Real_Time;
      use CORBA;
      use PolyORB.Utils.Report;
      use Harness;

      IOR : CORBA.String;
      MyHarness : Harness.Ref;
      Ok : Boolean := True;

      T0, T1, T2 : Time;
      Delta1 : Duration;

      How_Many : Integer;

   begin
      New_Test ("Harness");

      CORBA.ORB.Initialize ("ORB");

      if Ada.Command_Line.Argument_Count < 1 then
         Ada.Text_IO.Put_Line
           ("usage : client <IOR_string_from_server> [how_many]");
         return;
      end if;

      IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));
      ORB.String_To_Object (IOR, MyHarness);

      Output ("test not nil reference", not Is_Nil (MyHarness));

      if Ada.Command_Line.Argument_Count = 2 then
         How_Many := Integer'Value (Ada.Command_Line.Argument (2));
      else
         How_Many := 1_000;
      end if;

      T0 := Clock;
      for J in 1 .. How_Many loop
         Ok := Ok and (echoULong (MyHarness, 1234) = 1234);
      end loop;
      T1 := Clock;
      T2 := Clock;

      Output ("Test success", Ok);

      Delta1 := To_Duration (T1 - T0 - (T2 - T1));
      Ada.Text_IO.Put_Line ("Time: " & Duration'Image (Delta1) & "s");

      End_Report;
   end Launch_Client;

end Client_Common;
