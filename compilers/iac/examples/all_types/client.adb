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
with CORBA.Object;
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
   Sequence_Length : Integer := 5;

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
            if Argument_Count > 3 then
               Sequence_Length := Integer'Value
                 (Argument (4));
            end if;
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

      if What = All_Tests or else What = Sequence_Only then
         declare
            X : U_sequence := U_sequence (IDL_SEQUENCE_short.Null_Sequence);
         begin
            for J in 1 .. Sequence_Length loop
               X := X & CORBA.Short (J);
            end loop;

            declare
               Res : constant U_sequence := echoUsequence (Myall_types, X);
            begin
               if What = Sequence_Only then
                  pragma Assert (Res = X);
                  goto End_Of_Loop;
               end if;

               Output ("test unbounded sequence", Res = X);
            end;
         end;
      end if;

      Output ("test string",
              To_Standard_String
              (echoString
               (Myall_types, To_CORBA_String ("hello distributed world")))
              = "hello distributed world");

      Output ("test wstring",
              To_Standard_Wide_String
              (echoWString
               (Myall_types, To_CORBA_Wide_String
                ("hello distributed world")))
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

      Output ("test wchar", echoWChar (Myall_types, 'A') = 'A');

      Output ("test octet", echoOctet (Myall_types, 5) = 5);
      begin
         Output ("test enum", echoColor (Myall_types, Blue) = Blue);
      exception
         when E : others =>
            Output ("test enum", False);
            Ada.Text_IO.Put_Line ("Got exception:");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      end;

      declare
         X : Rainbow;
      begin
         for J in X'Range loop
            X (J) := Color'Val (J mod (Color'Pos (Color'Last) + 1));
         end loop;
         Output ("test array of enum", echoRainbow (Myall_types, X) = X);
      end;

      --  Bounded sequences
      declare
         X : B_sequence := B_sequence (IDL_SEQUENCE_10_short.Null_Sequence);
      begin
         X := X & 1 & 2 & 3 & 4 & 5;
         Output ("test bounded sequence", echoBsequence (Myall_types, X) = X);
      end;

      --  Fixed point
      declare
         X : constant array (1 .. 4) of Money :=
           (0.0, 6423.50, 3.14, -27.18);
      begin
         Ok := True;
         for J in X'Range loop
            if X (J) /= echoMoney (Myall_types, X (J)) then
               Ok := False;
            end if;
         end loop;
         Output ("test fixed point", Ok);
      end;

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
         X := all_types.Ref (echoRef (Myall_types, Myall_types));
         Output ("test self reference", True);

         for I in 1 .. 47 loop
            X := all_types.Ref (echoRef (X, X));
         end loop;
         Output ("test self reference consistency",
                 echoLong (X, 31337) = 31337);

         X := all_types.Ref (echoOtherAllTypes (X, X));

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
         Pass : Boolean;
      begin
         for J in Test_Unions'Range loop
            Pass := echoUnion (Myall_types, Test_Unions (J))
              = Test_Unions (J);
            Output ("test union" & Test_Unions (J).Switch'Img, Pass);
         end loop;
      end;

      declare
         Test_Unions : constant array (Integer range <>) of myUnionEnumSwitch
           := ((Switch => Red, Foo => 31337),
               (Switch => Green, Bar => 534),
               (Switch => Blue, Baz => CORBA.To_CORBA_String ("grümpf")));
         Pass : Boolean;
      begin
         for J in Test_Unions'Range loop
            Pass := echoUnionEnumSwitch (Myall_types, Test_Unions (J))
              = Test_Unions (J);
            Output ("test union with enum switch "
                    & Test_Unions (J).Switch'Img, Pass);
         end loop;
      end;

      declare
         X : constant noMemberUnion (True) := (Switch => True);
      begin
         Output ("test union with no member for label",
           echoNoMemberUnion (Myall_types, X) = X);
      exception
         when others =>
            Output ("test union with no member for label", False);
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
         Output ("test big multi-dimensional array",
                 echoBigMatrix (Myall_types, B) = B);
      end;

      declare
         X : constant nested_array := ((2, 3, 5, 7, 11),
                                       (13, 17, 19, 23, 31),
                                       (43, 59, 67, 83, 94));
      begin
         Output ("test nested array", echoNestedArray (Myall_types, X) = X);
      end;

      declare
         B : sixteenKb;
      begin
         for I in B'Range (1) loop
            for J in B'Range (2) loop
               B (I, J) := Long ((I + 1) * (J + 2));
            end loop;
         end loop;
         Output ("test huge (16 Kb) multi-dimensional array",
                 echoSixteenKb (Myall_types, B) = B);
      end;

      --  Attributes
      Set_myColor (Myall_types, Green);
      Output ("test attribute", Get_myColor (Myall_types) = Green);
      declare
         Counter_First_Value : constant CORBA.Long
           := Get_Counter (Myall_types);
         Counter_Second_Value : constant CORBA.Long
           := Get_Counter (Myall_types);
      begin
         Output ("test read-only attribute",
                 Counter_Second_Value = Counter_First_Value + 1);
      end;

      --  Bounded strings
      declare
         X : BoundedStr := BoundedStr (Bounded_String_12.Null_Bounded_String);
      begin
         for Index in 1 .. 12 loop
            X := X & Character'Val (Character'Pos ('a') + Index - 1);
         end loop;
         Output ("test bounded string", echoBoundedStr (Myall_types, X) = X);
      end;

      --  Bounded wide strings
      declare
         X : BoundedWStr := BoundedWStr
           (Bounded_Wide_String_11.Null_Bounded_Wide_String);
      begin
         for Index in 1 .. 8 loop
            X := X & Wide_Character'Val (Wide_Character'Pos ('a') + Index - 1);
         end loop;
         Output ("test bounded wide string",
                 echoBoundedWStr (Myall_types, X) = X);
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
         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
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

      Ok := False;
      begin
         testSystemException (Myall_types, 2485);
      exception
         when CORBA.Bad_Param =>
            Ok := True;

         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      end;

      Output ("test system exception", Ok);

      declare
         Seq        : U_Sequence;
         Nested_Seq : Nested_Sequence;
         Res        : Nested_Sequence;
         Passed     : Boolean := True;
      begin
         for J in 1 .. Sequence_Length loop
            Seq := Seq & CORBA.Short (J);
         end loop;

         for J in 1 .. Sequence_Length loop
            Nested_Seq := Nested_Seq & Seq;
         end loop;

         Res := EchoNestedsequence (Myall_Types, Nested_Seq);

         for J in 1 .. Sequence_Length loop
            Passed := Passed and
              All_Types.IDL_SEQUENCE_All_Types_U_Sequence.Get_Element
              (all_types.IDL_SEQUENCE_All_Types_U_Sequence.Sequence
               (Res),
               Standard.Positive
               (J)) = Seq;
         end loop;
         Output ("test nested sequence", Passed);
      end;
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
