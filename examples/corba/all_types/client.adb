------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  All_Types client.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.ORB;

with all_types.Helper; use all_types, all_types.Helper;
with Report;    use Report;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.CORBA_P.Naming_Tools; use PolyORB.CORBA_P.Naming_Tools;

procedure Client is
   Myall_types : all_types.Ref;
   Ok : Boolean;
   One_Shot : constant Boolean := Ada.Command_Line.Argument_Count /= 2
                 or else Boolean'Value (Ada.Command_Line.Argument (2));
begin
   CORBA.ORB.Initialize ("ORB");
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line
         ("usage : client <IOR_string_from_server|name|-i> [oneshot]");
      return;
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

   loop
      Output ("test string",
              To_Standard_String
              (echoString
               (Myall_types, To_CORBA_String ("hello distributed world")))
              = "hello distributed world");
      Output ("test boolean", echoBoolean (Myall_types, True) = True);
      Output ("test short", echoShort (Myall_types, 123) = 123);
      Output ("test long",  echoLong (Myall_types, 456) = 456);
      Output ("test unsigned_short", echoUShort (Myall_types, 456) = 456);
      Output ("test unsigned_long", echoULong (Myall_types, 123) = 123);
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
      Output ("test octet", echoOctet (Myall_types, 5) = 5);
      begin
         Output ("test enum", echoColor (Myall_types, Blue) = Blue);
      exception
         when E : others =>
            Output ("test enum", False);
            Ada.Text_IO.Put_Line ("Got exception:");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      end;

      --  Unbounded sequences
      declare
         X : U_sequence := U_sequence (IDL_SEQUENCE_short.Null_Sequence);
      begin
         X := X & 1 & 2 & 3 & 4 & 5;
         Output ("test unbounded sequence",
                 echoUsequence (Myall_types, X) = X);
      end;

      --  Bounded sequences
      declare
         X : B_sequence := B_sequence (IDL_SEQUENCE_short_10.Null_Sequence);
      begin
         X := X & 1 & 2 & 3 & 4 & 5;
         Output ("test bounded sequence",  echoBsequence (Myall_types, X) = X);
      end;

      --  Fixed point
--               echoMoney (Myall_types, 6423.50) = 6423.50
--               and then echoMoney (Myall_types, 0.0) = 0.0
--         and then echoMoney (Myall_types, 3.14) = 3.14);
      Output ("test fixed point", False);
      --  Fixed point types are not implemented yet.

      --  Structs
      declare
         Test_Struct : constant simple_struct
           := (123, To_CORBA_String ("Hello world!"));
      begin
         Output ("test struct",
                 echoStruct (Myall_types, Test_Struct) = Test_Struct);
      end;

      declare
         Struct : constant simple_struct
           := (123, To_CORBA_String ("Hello world!"));

         Test_Struct : nested_struct;
      begin
         Test_Struct.ns := Struct;
         Output ("test nested struct",
                 echoNestedStruct (Myall_types, Test_Struct) = Test_Struct);
      end;

      --  Refs
      declare
         X : all_types.Ref;
      begin
         X := echoRef (Myall_types, Myall_types);
         Output ("test self reference", True);

         for I in 1 .. 15 loop
            X := echoRef (X, X);
         end loop;
         Output ("test self reference consistency",
                 echoLong (X, 31337) = 31337);

         X := echoOtherAllTypes (X, X);

         Output ("test self reference typedef", echoLong (X, 31337) = 31337);

         X := all_types.Helper.To_Ref
           (echoObject (X, CORBA.Object.Ref (X)));
         Output ("test object", echoLong (X, 23459) = 23459);

         X := all_types.Helper.To_Ref
           (echoOtherObject (X, CORBA.Object.Ref (X)));
         Output ("test object typedef", echoLong (X, 34563) = 34563);

      end;

      --  Unions
      declare
         Test_Unions : constant array (Integer range <>) of myUnion
           := ((Switch => 0, Unknown => 987),
               (Switch => 1, Counter => 1212),
               (Switch => 2, Flag => True),
               (Switch => 3, Hue => Green));
         Pass : Boolean := True;
      begin
         for I in Test_Unions'Range loop
            Pass := Pass and then echoUnion (Myall_types, Test_Unions (I))
              = Test_Unions (I);
            exit when not Pass;
         end loop;
         Output ("test union", Pass);
      end;

      declare
         Test_Unions : constant array (Integer range <>) of myUnionEnumSwitch
           := ((Switch => Red, Foo => 31337),
               (Switch => Green, Bar => 534),
               (Switch => Blue, Baz => CORBA.To_CORBA_String ("grümpf")));
         Pass : Boolean := True;
      begin
         for I in Test_Unions'Range loop
            Pass := Pass
              and then echoUnionEnumSwitch (Myall_types, Test_Unions (I))
              = Test_Unions (I);
            exit when not Pass;
         end loop;
         Output ("test union with enum switch", Pass);
      end;

      --  Arrays
      declare
         X : constant simple_array := (2, 3, 5, 7, 11);
      begin
         Output ("test simple array", echoArray (Myall_types, X) = X);
      end;
      declare
         M : constant matrix := ((165, 252, 375),
                                 (377, 145, 222),
                                 (202, 477, 147));
      begin
         Output ("test multi-dimensional array",
                 echoMatrix (Myall_types, M) = M);
      end;

      declare
         B : bigmatrix;
      begin
         for I in B'Range (1) loop
            for J in B'Range (2) loop
               B (I, J) := Long ((I + 1) * (J + 2));
            end loop;
         end loop;
         --  Output ("test big multi-dimensional array",
         --      echoBigMatrix (Myall_types, B) = B);
      end;
      Output ("test big multi-dimensional array", False);
      --  XXX idlac generates wrong code for this example, index goes beyond
      --  arrays limits, raising an exception on the server side.

      --  Attributes
      set_myColor (Myall_types, Green);
      Output ("test attribute", get_myColor (Myall_types) = Green);
      declare
         Counter_First_Value : constant CORBA.Long
           := get_Counter (Myall_types);
         Counter_Second_Value : constant CORBA.Long
           := get_Counter (Myall_types);
      begin
         Output ("test read-only attribute",
                 Counter_Second_Value = Counter_First_Value + 1);
      end;

      --  Exceptions
      Ok := False;
      declare
         Member : my_exception_Members;
      begin
         testException (Myall_types, 2485);
      exception
         when E : my_exception =>
            Get_Members (E, Member);
            Ok := (Member.info = 2485);
         when others =>
            null;
      end;
      Output ("test user exception", Ok);

      Ok := False;
      begin
         testUnknownException (Myall_types, 2485);
      exception
         when CORBA.Unknown =>
            Ok := True;
         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      end;
      Output ("test unknown exception", Ok);

      exit when One_Shot;
   end loop;

end Client;
