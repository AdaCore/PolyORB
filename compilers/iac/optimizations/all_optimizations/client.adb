------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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

with test_all_optims; use test_all_optims;

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

   type Echo_SixteenKb_Access is access function
     (Self : test_all_optims.Ref;
      data : test_all_optims.sixteenKb)
     return test_all_optims.sixteenKb;
   N_Echo_SixteenKb : constant PolyORB.Types.Unsigned_Long := 100;

   G       : aliased PolyORB.Utils.Random.Generator;
   --  T     : Ada.Calendar.Time;
   --  L     : PolyORB.Types.Unsigned_Long;
   Total   : Duration := 0.0;
   Test_Id : Natural  := 0;

   Echo_SixteenKb_Table : array
     (PolyORB.Types.Unsigned_Long range 0 .. N_Echo_SixteenKb - 1)
     of Echo_SixteenKb_Access :=
     (echoSixteenKb00'Access,
      echoSixteenKb01'Access,
      echoSixteenKb02'Access,
      echoSixteenKb03'Access,
      echoSixteenKb04'Access,
      echoSixteenKb05'Access,
      echoSixteenKb06'Access,
      echoSixteenKb07'Access,
      echoSixteenKb08'Access,
      echoSixteenKb09'Access,
      echoSixteenKb10'Access,
      echoSixteenKb11'Access,
      echoSixteenKb12'Access,
      echoSixteenKb13'Access,
      echoSixteenKb14'Access,
      echoSixteenKb15'Access,
      echoSixteenKb16'Access,
      echoSixteenKb17'Access,
      echoSixteenKb18'Access,
      echoSixteenKb19'Access,
      echoSixteenKb20'Access,
      echoSixteenKb21'Access,
      echoSixteenKb22'Access,
      echoSixteenKb23'Access,
      echoSixteenKb24'Access,
      echoSixteenKb25'Access,
      echoSixteenKb26'Access,
      echoSixteenKb27'Access,
      echoSixteenKb28'Access,
      echoSixteenKb29'Access,
      echoSixteenKb30'Access,
      echoSixteenKb31'Access,
      echoSixteenKb32'Access,
      echoSixteenKb33'Access,
      echoSixteenKb34'Access,
      echoSixteenKb35'Access,
      echoSixteenKb36'Access,
      echoSixteenKb37'Access,
      echoSixteenKb38'Access,
      echoSixteenKb39'Access,
      echoSixteenKb40'Access,
      echoSixteenKb41'Access,
      echoSixteenKb42'Access,
      echoSixteenKb43'Access,
      echoSixteenKb44'Access,
      echoSixteenKb45'Access,
      echoSixteenKb46'Access,
      echoSixteenKb47'Access,
      echoSixteenKb48'Access,
      echoSixteenKb49'Access,
      echoSixteenKb50'Access,
      echoSixteenKb51'Access,
      echoSixteenKb52'Access,
      echoSixteenKb53'Access,
      echoSixteenKb54'Access,
      echoSixteenKb55'Access,
      echoSixteenKb56'Access,
      echoSixteenKb57'Access,
      echoSixteenKb58'Access,
      echoSixteenKb59'Access,
      echoSixteenKb60'Access,
      echoSixteenKb61'Access,
      echoSixteenKb62'Access,
      echoSixteenKb63'Access,
      echoSixteenKb64'Access,
      echoSixteenKb65'Access,
      echoSixteenKb66'Access,
      echoSixteenKb67'Access,
      echoSixteenKb68'Access,
      echoSixteenKb69'Access,
      echoSixteenKb70'Access,
      echoSixteenKb71'Access,
      echoSixteenKb72'Access,
      echoSixteenKb73'Access,
      echoSixteenKb74'Access,
      echoSixteenKb75'Access,
      echoSixteenKb76'Access,
      echoSixteenKb77'Access,
      echoSixteenKb78'Access,
      echoSixteenKb79'Access,
      echoSixteenKb80'Access,
      echoSixteenKb81'Access,
      echoSixteenKb82'Access,
      echoSixteenKb83'Access,
      echoSixteenKb84'Access,
      echoSixteenKb85'Access,
      echoSixteenKb86'Access,
      echoSixteenKb87'Access,
      echoSixteenKb88'Access,
      echoSixteenKb89'Access,
      echoSixteenKb90'Access,
      echoSixteenKb91'Access,
      echoSixteenKb92'Access,
      echoSixteenKb93'Access,
      echoSixteenKb94'Access,
      echoSixteenKb95'Access,
      echoSixteenKb96'Access,
      echoSixteenKb97'Access,
      echoSixteenKb98'Access,
      echoSixteenKb99'Access);

   My_Test_All_Optims : test_all_optims.Ref;
   Howmany            : Integer := 1;
   Howmany_Bckp       : Integer := Howmany;
   B                  : test_all_optims.SixteenKb;

begin
   New_Test ("Minimal Perfect Hash functions optimization");

   CORBA.ORB.Initialize ("ORB");
   if Argument_Count < 1 then
      Ada.Text_IO.Put_Line
        ("usage : client howmany");
      return;
   end if;

   --  Getting the reference

   IOR_Utils.Get_Ref ("ref.ref", My_Test_All_Optims);

   if test_all_optims.Is_Nil (My_Test_All_Optims) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Output ("test not null", not test_all_optims.Is_Nil (My_Test_All_Optims));

   Howmany := Integer'Value (Argument (1));
   Howmany_Bckp := Howmany;
   Ada.Text_IO.Put_Line
     ("Total number of iteration for each test : " & Integer'Image (Howmany));

   --  Initialize the big array

   for I in B'Range (1) loop
      for J in B'Range (2) loop
         B (I, J) := CORBA.Long ((I + 1) * (J + 2));
      end loop;
   end loop;

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
         L          : test_all_optims.sixteenKb;
         T0, T1     : Ada.Real_Time.Time;
         Delta1     : Duration;
      begin
         T0 := Ada.Real_Time.Clock;
         L := echoSixteenKb00 (My_Test_All_Optims, B);
         T1 := Ada.Real_Time.Clock;
         Delta1 := Ada.Real_Time.To_Duration (T1 - T0);
         Total := Total + Delta1;
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
         L          : test_all_optims.sixteenKb;
         T0, T1     : Ada.Real_Time.Time;
         Delta1     : Duration;
      begin
         T0 := Ada.Real_Time.Clock;
         L := echoSixteenKb99 (My_Test_All_Optims, B);
         T1 := Ada.Real_Time.Clock;
         Delta1 := Ada.Real_Time.To_Duration (T1 - T0);
         Total := Total + Delta1;
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
         L               : test_all_optims.sixteenKb;
         Echo_SixteenKb  : Echo_SixteenKb_Access;
         Id              : PolyORB.Types.Unsigned_Long :=
           PolyORB.Types.Unsigned_Long (J) mod N_Echo_SixteenKb;
         S               : String := PolyORB.Types.Unsigned_Long'Image (Id);
         T0, T1          : Ada.Real_Time.Time;
         Delta1          : Duration;
      begin
         Echo_SixteenKb := Echo_SixteenKb_Table (Id);
         T0 := Ada.Real_Time.Clock;
         L := Echo_SixteenKb (My_Test_All_Optims, B);
         T1 := Ada.Real_Time.Clock;
         Delta1 := Ada.Real_Time.To_Duration (T1 - T0);
         Total := Total + Delta1;
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
         L               : test_all_optims.sixteenKb;
         Echo_SixteenKb  : Echo_SixteenKb_Access;
         Id              : PolyORB.Types.Unsigned_Long :=
           Random (G'Access) mod N_Echo_SixteenKb;
         S               : String := PolyORB.Types.Unsigned_Long'Image (Id);
         T0, T1          : Ada.Real_Time.Time;
         Delta1          : Duration;
      begin
         Echo_SixteenKb := Echo_SixteenKb_Table (Id);
         T0 := Ada.Real_Time.Clock;
         L := Echo_SixteenKb (My_Test_All_Optims, B);
         T1 := Ada.Real_Time.Clock;
         Delta1 := Ada.Real_Time.To_Duration (T1 - T0);
         Total := Total + Delta1;
      end;

      Howmany := Howmany - 1;
   end loop;
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      "Total for random operations : " & Duration'Image (Total) & "s");

   End_Report;
end Client;
