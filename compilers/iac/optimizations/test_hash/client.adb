------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Real_Time;
with GNAT.Calendar.Time_IO;

with CORBA; use CORBA;
with CORBA.ORB;

with IOR_Utils;

with test_hash; use test_hash;

with PolyORB.Utils.Report;
with PolyORB.Utils.Random;
with PolyORB.Types;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Client is

   use PolyORB.Utils.Report;
   use PolyORB.Utils.Random;
   use PolyORB.Types;
   use Ada.Calendar;
   use Ada.Real_Time;
   use GNAT.Calendar.Time_IO;

   type Echo_Long_Access is access function
     (Self : in test_hash.Ref;
      data : in CORBA.Long)
     return CORBA.Long;
   N_Echo_Long : constant PolyORB.Types.Unsigned_Long := 100;

   G       : aliased PolyORB.Utils.Random.Generator;
   --  T     : Ada.Calendar.Time;
   --  L     : PolyORB.Types.Unsigned_Long;
   Total   : Duration := 0.0;
   Test_Id : Natural  := 0;

   Echo_Long_Table : array
     (PolyORB.Types.Unsigned_Long range 0 .. N_Echo_Long - 1)
     of Echo_Long_Access :=
     (echoLong00'Access,
      echoLong01'Access,
      echoLong02'Access,
      echoLong03'Access,
      echoLong04'Access,
      echoLong05'Access,
      echoLong06'Access,
      echoLong07'Access,
      echoLong08'Access,
      echoLong09'Access,
      echoLong10'Access,
      echoLong11'Access,
      echoLong12'Access,
      echoLong13'Access,
      echoLong14'Access,
      echoLong15'Access,
      echoLong16'Access,
      echoLong17'Access,
      echoLong18'Access,
      echoLong19'Access,
      echoLong20'Access,
      echoLong21'Access,
      echoLong22'Access,
      echoLong23'Access,
      echoLong24'Access,
      echoLong25'Access,
      echoLong26'Access,
      echoLong27'Access,
      echoLong28'Access,
      echoLong29'Access,
      echoLong30'Access,
      echoLong31'Access,
      echoLong32'Access,
      echoLong33'Access,
      echoLong34'Access,
      echoLong35'Access,
      echoLong36'Access,
      echoLong37'Access,
      echoLong38'Access,
      echoLong39'Access,
      echoLong40'Access,
      echoLong41'Access,
      echoLong42'Access,
      echoLong43'Access,
      echoLong44'Access,
      echoLong45'Access,
      echoLong46'Access,
      echoLong47'Access,
      echoLong48'Access,
      echoLong49'Access,
      echoLong50'Access,
      echoLong51'Access,
      echoLong52'Access,
      echoLong53'Access,
      echoLong54'Access,
      echoLong55'Access,
      echoLong56'Access,
      echoLong57'Access,
      echoLong58'Access,
      echoLong59'Access,
      echoLong60'Access,
      echoLong61'Access,
      echoLong62'Access,
      echoLong63'Access,
      echoLong64'Access,
      echoLong65'Access,
      echoLong66'Access,
      echoLong67'Access,
      echoLong68'Access,
      echoLong69'Access,
      echoLong70'Access,
      echoLong71'Access,
      echoLong72'Access,
      echoLong73'Access,
      echoLong74'Access,
      echoLong75'Access,
      echoLong76'Access,
      echoLong77'Access,
      echoLong78'Access,
      echoLong79'Access,
      echoLong80'Access,
      echoLong81'Access,
      echoLong82'Access,
      echoLong83'Access,
      echoLong84'Access,
      echoLong85'Access,
      echoLong86'Access,
      echoLong87'Access,
      echoLong88'Access,
      echoLong89'Access,
      echoLong90'Access,
      echoLong91'Access,
      echoLong92'Access,
      echoLong93'Access,
      echoLong94'Access,
      echoLong95'Access,
      echoLong96'Access,
      echoLong97'Access,
      echoLong98'Access,
      echoLong99'Access);

   My_Test_Hash : test_hash.Ref;
   Howmany      : Integer := 1;
   Howmany_Bckp : Integer := Howmany;
   --  Ok           : Boolean := True;
   --  IOR          : CORBA.String;

begin
   New_Test ("Minimal Perfect Hash functions optimization");

   CORBA.ORB.Initialize ("ORB");
   if Argument_Count < 1 then
      Ada.Text_IO.Put_Line
        ("usage : client howmany");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   --  IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
   --  CORBA.ORB.String_To_Object (IOR, My_Test_Hash);

   IOR_Utils.Get_Ref ("ref.ref", My_Test_Hash);

   if test_hash.Is_Nil (My_Test_Hash) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Output ("test not null", not test_hash.Is_Nil (My_Test_Hash));

   Howmany := Integer'Value (Argument (1));
   Howmany_Bckp := Howmany;
   Ada.Text_IO.Put_Line
     ("Total number of iteration for each test : " & Integer'Image (Howmany));

   --  Calling the first operation "Howmany" times

   Total := 0.0;
   Test_Id := Test_Id + 1;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error, "Test " & Natural'Image (Test_Id) & " :");
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error, "Excecuting the first operation only");
   Howmany := Howmany_Bckp;
   for J in 1 ..  Howmany loop
      declare
         L          : CORBA.Long;
         T0, T1     : Ada.Real_Time.Time;
         Delta1     : Duration;
      begin
         T0 := Ada.Real_Time.Clock;
         L := echoLong00 (My_Test_Hash, 123);
         T1 := Ada.Real_Time.Clock;
         Delta1 := Ada.Real_Time.To_Duration (T1 - T0);
         Total := Total + Delta1;
         --  Output ("Calling echoLong" & S (2 .. S'Length), L = 123);
         --  Ada.Text_IO.Put_Line ("Time: " & Duration'Image (Delta1) & "s");
         --  Ada.Text_IO.Put_Line ("0" & "  " & Duration'Image (Delta1));
      end;

      Howmany := Howmany - 1;
   end loop;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Total for the first operation : " & Duration'Image (Total) & "s");

   --  Calling the Last operation "Howmany" times

   Total := 0.0;
   Test_Id := Test_Id + 1;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Test " & Natural'Image (Test_Id) & " :");
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Excecuting the last operation only");
   Howmany := Howmany_Bckp;
   for J in 1 ..  Howmany loop
      declare
         L          : CORBA.Long;
         T0, T1 : Ada.Real_Time.Time;
         Delta1     : Duration;
      begin
         T0 := Ada.Real_Time.Clock;
         L := echoLong99 (My_Test_Hash, 123);
         T1 := Ada.Real_Time.Clock;
         Delta1 := Ada.Real_Time.To_Duration (T1 - T0);
         Total := Total + Delta1;
         --  Output ("Calling echoLong" & S (2 .. S'Length), L = 123);
         --  Ada.Text_IO.Put_Line ("Time: " & Duration'Image (Delta1) & "s");
         --  Ada.Text_IO.Put_Line ("99" & "  " & Duration'Image (Delta1));
      end;

      Howmany := Howmany - 1;
   end loop;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Total for the Last operation : " & Duration'Image (Total) & "s");

   --  Calling all the operations in order "Howmany" times

   Total := 0.0;
   Test_Id := Test_Id + 1;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Test " & Natural'Image (Test_Id) & " :");
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Excecuting all the operations in order");
   Howmany := Howmany_Bckp;
   for J in 1 ..  Howmany loop
      declare
         L          : CORBA.Long;
         Echo_Long  : Echo_Long_Access;
         Id         : PolyORB.Types.Unsigned_Long :=
           PolyORB.Types.Unsigned_Long (J) mod N_Echo_Long;
         S          : String := PolyORB.Types.Unsigned_Long'Image (Id);
         T0, T1     : Ada.Real_Time.Time;
         Delta1     : Duration;
      begin
         Echo_Long := Echo_Long_Table (Id);
         T0 := Ada.Real_Time.Clock;
         L := Echo_Long (My_Test_Hash, 123);
         T1 := Ada.Real_Time.Clock;
         Delta1 := Ada.Real_Time.To_Duration (T1 - T0);
         Total := Total + Delta1;
         --  Output ("Calling echoLong" & S (2 .. S'Length), L = 123);
         --  Ada.Text_IO.Put_Line ("Time: " & Duration'Image (Delta1) & "s");
         --  Ada.Text_IO.Put_Line ("0" & "  " & Duration'Image (Delta1));
      end;

      Howmany := Howmany - 1;
   end loop;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Total for all operations in order : " & Duration'Image (Total) & "s");

   --  Initializing the pseudo random generator
   --  T := Clock;
   --  L := PolyORB.Types.Unsigned_Long'Value
   --  (Image (T, "%s")); --  Seconds since 1-1-1970
   Reset (G'Access, 1121696026);

   Total := 0.0;
   Test_Id := Test_Id + 1;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Test " & Natural'Image (Test_Id) & " :");
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Excecuting random operations");
   Howmany := Howmany_Bckp;
   for J in 1 ..  Howmany loop

      declare
         L          : CORBA.Long;
         Echo_Long  : Echo_Long_Access;
         Id         : PolyORB.Types.Unsigned_Long :=
           Random (G'Access) mod N_Echo_Long;
         S          : String := PolyORB.Types.Unsigned_Long'Image (Id);
         T0, T1     : Ada.Real_Time.Time;
         Delta1     : Duration;
      begin
         Echo_Long := Echo_Long_Table (Id);
         T0 := Ada.Real_Time.Clock;
         L := Echo_Long (My_Test_Hash, 123);
         T1 := Ada.Real_Time.Clock;
         Delta1 := Ada.Real_Time.To_Duration (T1 - T0);
         Total := Total + Delta1;
         --  Output ("Calling echoLong" & S (2 .. S'Length), L = 123);
         --  Ada.Text_IO.Put_Line ("Time: " & Duration'Image (Delta1) & "s");
         --  Ada.Text_IO.Put_Line (S & "  " & Duration'Image (Delta1));
      end;

      Howmany := Howmany - 1;
   end loop;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Total for random operations : " & Duration'Image (Total) & "s");

   End_Report;
end Client;
