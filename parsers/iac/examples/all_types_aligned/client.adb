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

--  All_Types client

with Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with CORBA; use CORBA;
--  with CORBA.Object;
with CORBA.ORB;

with all_types.Helper; use all_types, all_types.Helper;
with PolyORB.Utils.Report;

with PolyORB.Setup.Client;

pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.CORBA_P.Naming_Tools; use PolyORB.CORBA_P.Naming_Tools;

procedure Client is

   use PolyORB.Utils.Report;

   Myall_types : all_types.Ref;
   Ok : Boolean;
   Howmany : Integer := 1;

   type Test_Type is (All_Tests, Long_Only, Sequence_Only);
   What : Test_Type := All_Tests;

begin
   New_Test ("CORBA Types");

   CORBA.ORB.Initialize ("ORB");
   if Argument_Count < 1 then
      Ada.Text_IO.Put_Line
        ("usage : client <IOR_string_from_server|name|-i> "
         & "[howmany [what]]");
      return;
   end if;

   if Argument_Count >= 2 then
      Howmany := Integer'Value (Argument (2));
   end if;

   if Argument_Count >= 3 then
      declare
         What_Arg : constant String
           := Ada.Characters.Handling.To_Lower
                (Argument (3));
      begin
         if What_Arg = "true" or else What_Arg = "long" then
            What := Long_Only;
         elsif What_Arg = "sequence" then
            What := Sequence_Only;
         end if;
      end;
   end if;

   if Argument (1) = "-i" then
      Myall_types := To_Ref (Locate ("all_types"));
   else
      Myall_types := To_Ref (Locate (Argument (1)));
   end if;

   if all_types.Is_Nil (Myall_types) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;


   Output ("test not null", not all_types.Is_Nil (Myall_types));

   while Howmany > 0 loop

      if What = All_Tests or else What = Long_Only then
         declare
            L : constant Unsigned_Long := echoULong (Myall_types, 123);
         begin
            if What = Long_Only then
               pragma Assert (L = 123);
               goto End_Of_Loop;
               --  We are only doing an echoULong call, and we are
               --  interested in getting it as fast as possible.
            end if;
            Output ("test unsigned_long", L = 123);
         end;
      end if;

      Output ("test string",
              To_Standard_String
              (echoString
               (Myall_types, To_CORBA_String ("hello distributed world")))
              = "hello distributed world");

      Output ("test boolean", echoBoolean (Myall_types, True));
      Output ("test short", echoShort (Myall_types, 123) = 123);
      Output ("test long",  echoLong (Myall_types, 456) = 456);
      Output ("test unsigned_short", echoUShort (Myall_types, 456) = 456);
      Output ("test unsigned long long",
              echoULLong (Myall_types, 9_192_631_770) = 9_192_631_770);
      Output ("test float", echoFloat (Myall_types, 2.7) = 2.7);
      Output ("test double", echoDouble (Myall_types, 1.5) = 1.5);
      begin
         Output ("test char", echoChar (Myall_types, 'A') = 'A');
      exception
         when E : others =>
            Output ("test char", False);
            Ada.Text_IO.Put_Line ("Got exception:");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      end;
      declare
         Struct    : Simple_Struct;
         Union     : Simple_Union (1);
         Union_Res : Simple_Union;
         U_Seq     : U_Sequence;
      begin
         Struct.a := 123;
         Struct.b := 0.123;
         Struct.c := 'A';
         Output ("test struct", echoStruct (Myall_types, Struct) = Struct);

         Union.Counter := 123.567;
         Union_Res := echounion (Myall_types, Union);
         Output ("test union", Union_Res = Union);

         Output
           ("test union and string ",
            echoUnionAndString (Myall_Types, Union,
                                To_CORBA_String ("Hello World")) = Union);
         Output
           ("test string and union ",
            echoStringAndUnion
            (Myall_Types, Union, To_CORBA_String ("Hello World")) =
            To_CORBA_String ("Hello World"));

         for J in 1 .. 25 loop
            U_Seq := U_Seq & CORBA.Short (J);
         end loop;
         Output ("test unbounded sequence",
                 echousequence (Myall_types, U_Seq) = U_Seq);
      end;

      Output ("test octet", echoOctet (Myall_types, 5) = 5);

      Output ("test boolean and long",
              echoBooleanAndLong (Myall_types, True, 1234) = 1234);
      Output ("test boolean and short",
              echoBooleanAndShort (Myall_types, True, 123) = 123);

      <<End_Of_Loop>>
      Howmany := Howmany - 1;
   end loop;

   begin
      StopServer (Myall_types);
      Ok := True;
   exception
      when others =>
         Ok := False;
         raise;
   end;

   Output ("shut down server", Ok);
   End_Report;
end Client;
