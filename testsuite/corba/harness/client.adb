------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;

with CORBA.ORB;

with PolyORB.Utils.Report;

with Harness;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Client is

   use Ada.Real_Time;
   use CORBA;
   use PolyORB.Utils.Report;
   use Harness;

   How_Many : constant Integer := 1000;

   IOR : CORBA.String;
   MyHarness : Harness.Ref;
   Ok : Boolean := True;

   T0, T1 : Time;
   Delta1 : Duration;

begin
   New_Test ("Harness");

   CORBA.ORB.Initialize ("ORB");

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));
   ORB.String_To_Object (IOR, MyHarness);

   Output ("test not nil reference", not Is_Nil (MyHarness));

   T0 := Clock;
   for J in 1 .. How_Many loop
      Ok := Ok and (echoULong (MyHarness, 1234) = 1234);
   end loop;
   T1 := Clock;

   Output ("Test success", Ok);

   Delta1 := To_Duration (T1 - T0);
   Ada.Text_IO.Put_Line ("Time: " & Duration'Image (Delta1) & "s");

   End_Report;
end Client;
