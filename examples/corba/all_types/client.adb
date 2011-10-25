------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2011, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.ORB;

with all_types.Impl;
with all_types.Helper; use all_types, all_types.Helper;
with PolyORB.Utils.Report;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.CORBA_P.Server_Tools; use PolyORB.CORBA_P.Server_Tools;
with PolyORB.CORBA_P.Naming_Tools; use PolyORB.CORBA_P.Naming_Tools;

procedure Client is

   use PolyORB.Utils.Report;

   Myall_types : all_types.Ref;
   Ok : Boolean;
   Howmany : Integer := 1;
   Sequence_Length : Integer := 5;

   Test_Unions : constant array (Integer range <>) of myUnionEnumSwitch :=
                   ((Switch => Red, Foo => 31337),
                    (Switch => Green, Bar => 534),
                    (Switch => Blue, Baz => CORBA.To_CORBA_String ("grümpf")));

   type Test_Type is (All_Tests, Long_Only, Sequence_Only, UnionSequence_Only);
   What : Test_Type := All_Tests;

   Is_Local : Boolean := False;

begin
   New_Test ("CORBA Types");

   CORBA.ORB.Initialize ("ORB");
   if Argument_Count < 1 then
      Ada.Text_IO.Put_Line
        ("Usage: client <IOR_string_from_server|name|-i|""local""> "
         & "[howmany [what [seq-length]]]");
      Ada.Text_IO.Put ("where <what> is one of:");
      for J in Test_Type'Range loop
         Ada.Text_IO.Put (" " & J'Img);
      end loop;
      return;
   end if;

   if Argument_Count >= 2 then
      Howmany := Integer'Value (Argument (2));
   end if;

   if Argument_Count >= 3 then
      begin
         What := Test_Type'Value (Argument (3));
      exception
         when Constraint_Error =>
            What := All_Tests;
      end;

      case What is
         when Sequence_Only | UnionSequence_Only =>
            if Argument_Count > 3 then
               Sequence_Length := Integer'Value (Argument (4));
            end if;

         when others =>
            null;
      end case;
   end if;

   if Argument (1) = "-i" then
      Myall_types := To_Ref (Locate ("all_types"));

   elsif Argument (1) = "local" then
      Initiate_Servant (new all_types.Impl.Object, Myall_types);
      Activate_Server;
      Is_Local := True;

   else
      Myall_types := To_Ref (Locate (Argument (1)));
   end if;

   if all_types.Is_Nil (Myall_types) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Output ("test not null", not all_types.Is_Nil (Myall_types));

   for Iterations in 1 .. Howmany loop

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
            X : U_sequence := To_Sequence (Sequence_Length);
         begin
            for J in 1 .. Sequence_Length loop
               Replace_Element (X, J, CORBA.Short (J));
            end loop;

            declare
               Res : constant U_sequence := echoUsequence (Myall_types, X);
            begin
               if What = Sequence_Only then
                  pragma Assert (Res = X);
                  goto End_Of_Loop;
               end if;

               Output ("test unbounded sequence (length"
                 & Sequence_Length'Img & ")", Res = X);
            end;
         exception
            when others =>
               Output ("test unbounded sequence", False);
         end;
      end if;

      if What = All_Tests or else What = UnionSequence_Only then
         declare
            X : UnionSequence := To_Sequence (Sequence_Length);
         begin
            for J in 1 .. Sequence_Length loop
               Replace_Element (X, J, Test_Unions
                          (Test_Unions'First + J mod Test_Unions'Length));
            end loop;

            declare
               Res : constant UnionSequence := echoUnionSequence
                                                 (Myall_types, X);
            begin
               if What = UnionSequence_Only then
                  pragma Assert (Res = X);
                  goto End_Of_Loop;
               end if;

               Output ("test sequence of unions (length"
                 & Sequence_Length'Img & ")", Res = X);
            end;
         exception
            when others =>
               Output ("test sequence of unions", False);
         end;
      end if;

      begin
         Output ("test string",
                 To_Standard_String
                 (echoString
                  (Myall_types, To_CORBA_String ("hello distributed world")))
                 = "hello distributed world");
      exception
         when others =>
            Output ("test string", False);
      end;

      begin
         Output ("test wstring",
                 To_Standard_Wide_String
                 (echoWString
                  (Myall_types, To_CORBA_Wide_String
                   ("hello distributed world")))
                 = "hello distributed world");
      exception
         when E : CORBA.Marshal =>
            declare
               Member : CORBA.Marshal_Members;
            begin
               CORBA.Get_Members (E, Member);
               Output ("test wstring", Member.Minor = OMGVMCID + 5);
            end;

         when E : CORBA.Inv_Objref =>
            declare
               Member : CORBA.Inv_Objref_Members;
            begin
               CORBA.Get_Members (E, Member);
               Output ("test wstring", Member.Minor = 2);
            end;

         when others =>
            Output ("test wstring", False);
      end;

      begin
         Output ("test boolean", echoBoolean (Myall_types, True));
      exception
         when others =>
            Output ("test boolean", False);
      end;

      begin
         Output ("test short", echoShort (Myall_types, 123) = 123);
      exception
         when others =>
            Output ("test short", False);
      end;

      begin
         Output ("test long",  echoLong (Myall_types, 456) = 456);
      exception
         when others =>
            Output ("test long", False);
      end;

      begin
         Output ("test unsigned_short", echoUShort (Myall_types, 456) = 456);
      exception
         when others =>
            Output ("test unsigned_short", False);
      end;

      Output ("test unsigned long long",
        echoULLong (Myall_types, 9_192_631_770) = 9_192_631_770);
      Output ("test float", echoFloat (Myall_types, 2.7) = 2.7);
      Output ("test double", echoDouble (Myall_types, 1.5) = 1.5);

      begin
         Output ("test char", echoChar (Myall_types, 'A') = 'A');
      exception
         when others =>
            Output ("test char", False);
      end;

      begin
         Output ("test wchar", echoWChar (Myall_types, 'A') = 'A');
      exception
         when E : CORBA.Marshal =>
            declare
               Member : CORBA.Marshal_Members;
            begin
               CORBA.Get_Members (E, Member);
               Output ("test wchar", Member.Minor = OMGVMCID + 5);
            end;

         when E : CORBA.Inv_Objref =>
            declare
               Member : CORBA.Inv_Objref_Members;
            begin
               CORBA.Get_Members (E, Member);
               Output ("test wchar", Member.Minor = 2);
            end;

         when others =>
            Output ("test wchar", False);
      end;

      Output ("test octet", echoOctet (Myall_types, 5) = 5);

      begin
         Output ("test enum", echoColor (Myall_types, Blue) = Blue);
      exception
         when others =>
            Output ("test enum", False);
      end;

      declare
         X_Color : Color;
         pragma Suppress (Validity_Check, X_Color);
         --  Suppress validity checks so that we can test the effect of
         --  the invalid value within the ORB (we don't want to have it
         --  caught here in the client).

         X_Octet : CORBA.Octet;
         for X_Octet'Address use X_Color'Address;
         pragma Import (Ada, X_Octet);

         Success : Boolean := False;
      begin
         X_Color := Color'Last;
         X_Octet := X_Octet + 1;

         --  From this point on, X_Color has an invalid representation

         begin
            Success := echoColor (Myall_types, X_Color) = X_Color;

            --  No exception raised: invalid value was copied verbatim and
            --  was not checked, success.

         exception
            when CORBA.MARSHAL =>
               Success := True;

            when CORBA.UNKNOWN =>
               --  For the local case, we MAY raise CONSTRAINT_ERROR on the
               --  servant side, which is mapped back to CORBA.UNKNOWN on the
               --  caller side.

               Success := Is_Local;

            when E : others =>
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
               Success := False;
         end;
         Output ("test enum invalid rep", Success);
      end;

      declare
         X : Rainbow;
      begin
         for J in X'Range loop
            X (J) := Color'Val (J mod (Color'Pos (Color'Last) + 1));
         end loop;
         Output ("test array of enum", echoRainbow (Myall_types, X) = X);
      exception
         when others =>
            Output ("test array of enum", False);
      end;

      --  Bounded sequences

      declare
         X : constant B_sequence :=
               B_sequence (IDL_SEQUENCE_10_short.To_Sequence
                 (IDL_SEQUENCE_10_short.Element_Array'(1, 2, 3, 4, 5)));
      begin
         Output ("test bounded sequence",  echoBsequence (Myall_types, X) = X);
      exception
         when others =>
            Output ("test bounded sequence", False);
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
      exception
         when others =>
            Output ("test fixed point", False);
      end;

      --  Any

      declare
         X1 : constant CORBA.Unsigned_Long := 1234;
         Y1 : CORBA.Unsigned_Long;

         X2 : BoundedStr := BoundedStr (Bounded_String_12.Null_Bounded_String);
         Y2 : BoundedStr;
      begin
         Y1 := From_Any (echoAny (Myall_types, To_Any (X1)));
         Output ("test any<unsigned long>", Y1 = X1);

         for Index in 1 .. 12 loop
            X2 := X2 & Character'Val (Character'Pos ('A') + Index - 1);
         end loop;
         Y2 := All_Types.Helper.From_Any
                 (echoAny (Myall_types, All_Types.Helper.To_Any (X2)));
         Output ("test any<bounded string>", Y2 = X2);
      exception
         when others =>
            Output ("test any", False);
      end;

      --  Structs
      declare
         Test_Struct : constant simple_struct
           := (123, To_CORBA_String ("Hello world!"));
      begin
         Output ("test struct",
                 echoStruct (Myall_types, Test_Struct) = Test_Struct);
      exception
         when others =>
            Output ("test struct", False);
      end;

      declare
         Struct : constant simple_struct
           := (123, To_CORBA_String ("Hello world!"));

         Test_Struct : nested_struct;
      begin
         Test_Struct.ns := Struct;
         Output ("test nested struct",
                 echoNestedStruct (Myall_types, Test_Struct) = Test_Struct);
      exception
         when others =>
            Output ("test nested struct", False);
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

      exception
         when others =>
            Output ("refs", False);
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
            Pass := echoUnion (Myall_types, Test_Unions (J)) = Test_Unions (J);
            Output ("test union" & Test_Unions (J).Switch'Img, Pass);
         end loop;
      exception
         when others =>
            Output ("test union", False);
      end;

      declare
         Pass : Boolean;
      begin
         for J in Test_Unions'Range loop
            begin
               Pass := echoUnionEnumSwitch (Myall_types, Test_Unions (J))
                 = Test_Unions (J);
               Output ("test union with enum switch "
                       & Test_Unions (J).Switch'Img, Pass);
            exception
               when others =>
                  Output ("test union with enum switch "
                          & Test_Unions (J).Switch'Img, False);
            end;
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
      exception
         when others =>
            Output ("test simple array", False);
      end;

      declare
         M : constant matrix := ((165, 252, 375),
                                 (377, 145, 222),
                                 (202, 477, 147));
      begin
         Output ("test multi-dimensional array",
                 echoMatrix (Myall_types, M) = M);
      exception
         when others =>
            Output ("test multi-dimensional array", False);
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
      exception
         when others =>
            Output ("test big multi-dimensional array", False);
      end;

      declare
         X : constant nested_array := ((2, 3, 5, 7, 11),
                                       (13, 17, 19, 23, 31),
                                       (43, 59, 67, 83, 94));
      begin
         Output ("test nested array", echoNestedArray (Myall_types, X) = X);
      exception
         when others =>
            Output ("test nested array", False);
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
      exception
         when others =>
            Output ("test huge (16 Kb) multi-dimensional array", False);
      end;

      --  Attributes

      begin
         Set_myColor (Myall_types, Green);
         Output ("test attribute", Get_myColor (Myall_types) = Green);
      exception
         when others =>
            Output ("test attribute", False);
      end;

      begin
         declare
            Counter_First_Value : constant CORBA.Long
              := Get_Counter (Myall_types);
            Counter_Second_Value : constant CORBA.Long
              := Get_Counter (Myall_types);
         begin
            Output ("test read-only attribute",
                    Counter_Second_Value = Counter_First_Value + 1);
         end;
      exception
         when others =>
            Output ("test read-only attribute", False);
      end;

      --  Bounded strings

      declare
         X : BoundedStr := BoundedStr (Bounded_String_12.Null_Bounded_String);
      begin
         for Index in 1 .. 12 loop
            X := X & Character'Val (Character'Pos ('a') + Index - 1);
         end loop;
         Output ("test bounded string", echoBoundedStr (Myall_types, X) = X);
      exception
         when others =>
            Output ("test bounded string", False);
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
      exception
         when E : CORBA.Marshal =>
            declare
               Member : CORBA.Marshal_Members;
            begin
               CORBA.Get_Members (E, Member);
               Output ("test bounded wstring", Member.Minor = OMGVMCID + 5);
            end;

         when E : CORBA.Inv_Objref =>
            declare
               Member : CORBA.Inv_Objref_Members;
            begin
               CORBA.Get_Members (E, Member);
               Output ("test bounded wstring", Member.Minor = 2);
            end;

         when others =>
            Output ("test bounded wstring", False);

      end;

      --  Exceptions

      Ok := False;
      declare
         Member : my_exception_Members;
      begin
         testException (Myall_types, 2485, To_CORBA_String ("pouet"));
      exception
         when E : my_exception =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            Get_Members (E, Member);
            Ok := Member.info = 2485
                    and then To_Standard_String (Member.why) = "pouet";

         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      end;
      Output ("test user exception", Ok);

      Ok := False;
      begin
         testUnknownException (Myall_types, 2485);
      exception
         when E : CORBA.Unknown =>
            Ok := True;
            Ada.Text_IO.Put_Line
              (Ada.Exceptions.Exception_Information (E));

         when others =>
            null;
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

      <<End_Of_Loop>>
      null;
   end loop;

   if What /= All_Tests then
      Output ("test " & What'Img & " iterated" & Howmany'Img & " times", True);
   end if;

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
