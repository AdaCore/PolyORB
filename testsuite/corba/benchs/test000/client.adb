------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;

with CORBA.ORB;

with Test.Echo;
with Test.Factory;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);
pragma Elaborate_All (PolyORB.Setup.Client);

with PolyORB.Utils.Report;

procedure Client is

   use Ada.Calendar;
   use Ada.Command_Line;
   use Ada.Text_IO;
   use PolyORB.Utils.Report;

   Requests_To_Send : constant Integer := 10_000;
   Preallocated_Objects : CORBA.Long := 100;
   Total_Objects : CORBA.Long := 150;

   procedure Usage;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage:");
      Put_Line ("  client <IOR> [prealloc] [total]");
   end Usage;

begin
   if Argument_Count /= 3
     and then Argument_Count /= 1
   then
      Usage;
      return;
   end if;

   if Argument_Count = 3 then
      Preallocated_Objects := CORBA.Long'Value (Argument (2));
      Total_Objects := CORBA.Long'Value (Argument (3));
   end if;

   New_Test ("Object Activation Benchmarks");

   declare
      Argv : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;
   begin
      CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);
   end;

   declare
      Factory  : Test.Factory.Ref;
      Start, Finish : Time;
      Sequence : Test.Factory.EchoSequence;
      Aux      : CORBA.String;
      pragma Warnings (Off, Aux);

   begin
      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Argument (1)), Factory);

      Start := Clock;
      Test.Factory.Preallocate (Factory, Preallocated_Objects);
      Finish := Clock;
      Put_Line ("Time to initialize"
                & CORBA.Long'Image (Preallocated_Objects)
                & " objects: "
                & Duration'Image (Finish - Start));
      Output ("Object initialization", True);

      Sequence :=
        Test.Factory.Create_References (Factory, Preallocated_Objects);
      Start := Clock;
      for J in 1 .. Test.Factory.Length (Sequence) loop
         Aux :=
           Test.Echo.Echo_String
           (Test.Factory.Element_Of (Sequence, J),
            CORBA.To_CORBA_String ("AAAA"));
      end loop;
      Finish := Clock;
      Put_Line ("Time to process one request/servant, with"
                & CORBA.Long'Image (Preallocated_Objects)
                & " servants:"
                & Duration'Image (Finish - Start));
      Put_Line ("Mean value:" & Duration'Image
                ((Finish - Start) / Test.Factory.Length (Sequence)));
      Output ("Bench #1", True);

      Start := Clock;
      for J in 1 .. Requests_To_Send loop
         Aux :=
           Test.Echo.Echo_String
           (Test.Factory.Element_Of
            (Sequence, 1),
            CORBA.To_CORBA_String ("AAAA"));
      end loop;
      Finish := Clock;
      Put_Line ("Time to process"
                & Integer'Image (Requests_To_Send)
                & Duration'Image (Finish - Start));
      Put_Line ("Mean value:" & Duration'Image
                ((Finish - Start) / Requests_To_Send));
      Output ("Bench #2", True);

      Sequence :=
        Test.Factory.Create_References (Factory, Total_Objects);
      Start := Clock;
      for J in 1 .. Test.Factory.Length (Sequence) loop
         Aux :=
           Test.Echo.Echo_String
           (Test.Factory.Element_Of (Sequence, J),
            CORBA.To_CORBA_String ("AAAA"));
      end loop;
      Finish := Clock;
      Put_Line ("Time to process one request/servant, with"
                & CORBA.Long'Image (Total_Objects)
                & " servants:"
                & Duration'Image (Finish - Start));
      Put_Line ("Mean value:" & Duration'Image
                ((Finish - Start) / Test.Factory.Length (Sequence)));
      Output ("Bench #3", True);
      End_Report;
      CORBA.ORB.Shutdown (False);
   end;
end Client;
