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

      How_Many : constant Integer := 1_000;

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
   end Launch_Client;

end Client_Common;
