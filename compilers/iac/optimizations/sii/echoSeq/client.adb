
with Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Real_Time;

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.ORB;

with echoSeq.Helper; use echoSeq, echoSeq.Helper;
with PolyORB.Utils.Report;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.CORBA_P.Naming_Tools; use PolyORB.CORBA_P.Naming_Tools;

procedure Client is

   use Ada.Text_IO;
   use Ada.Real_Time;
   use PolyORB.Utils.Report;

   MyechoSeq : echoSeq.Ref;
   Ok : Boolean;
   Howmany : Integer := 1;
   Seq_Len : Positive := 10;

   X : U_sequence := U_sequence
     (IDL_SEQUENCE_unsigned_long.Null_Sequence);
   Y : U_sequence := U_sequence
     (IDL_SEQUENCE_unsigned_long.Null_Sequence);
   T0, T1, T2 : Time;
   Delta1 : Duration;

begin
   New_Test ("CORBA Types");

   CORBA.ORB.Initialize ("ORB");
   if Argument_Count < 1 then
      Ada.Text_IO.Put_Line
        ("usage : client <IOR_string_from_server> "
         & "[howmany [Seq_Len]]");
      return;
   end if;

   if Argument_Count >= 2 then
      Howmany := Integer'Value (Argument (2));
   end if;

   if Argument_Count >= 3 then
      Seq_Len := Positive'Value (Argument (3));
   end if;

   MyechoSeq := To_Ref (Locate (Argument (1)));

   if echoSeq.Is_Nil (MyechoSeq) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Output ("test not null", not echoSeq.Is_Nil (MyechoSeq));

   for Index in 1 .. Seq_Len loop
      X := X & CORBA.Unsigned_Long (Index);
   end loop;
   --  X := X & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10;

   T0 := Clock;
   for Index in 1 .. Howmany loop
      Y := echoUsequence (MyechoSeq, X);
   end loop;
   T1 := Clock;
   T2 := Clock;

   Delta1 := To_Duration (T1 - T0 - (T2 - T1));

   Output ("test bounded sequence", Y = X);

   --  Printing result

   Put_Line ("Iterations : " & Integer'Image (Howmany));
   Put_Line ("Length     : " & Positive'Image (Seq_Len));
   Put_Line ("Time       : " & Duration'Image (Delta1) & "s");

   End_Report;
end Client;
