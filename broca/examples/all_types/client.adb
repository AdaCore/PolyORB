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

with Ada.Command_Line;
with Ada.Text_IO;

with CORBA; use CORBA;
with CORBA.ORB;

with all_types; use all_types;
with Report;    use Report;

procedure Client is
   IOR : CORBA.String;
   Myall_types : all_types.Ref;
   Ok : Boolean;
   One_Shot : Boolean := Ada.Command_Line.Argument_Count /= 2
                 or else Boolean'Value (Ada.Command_Line.Argument (2));
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line
         ("usage : client <IOR_string_from_server> [oneshot]");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1));

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, Myall_types);

   --  checking if it worked
   if all_types.Is_Nil (Myall_types) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Output ("test not null", not all_types.Is_Nil (Myall_types));

   loop
      Output ("test boolean", echoBoolean (Myall_types, True) = True);
      Output ("test short", echoShort (Myall_types, 123) = 123);
      Output ("test long",  echoLong (Myall_types, 456) = 456);
      Output ("test unsigned_short", echoUShort (Myall_types, 456) = 456);
      Output ("test unsigned_long", echoULong (Myall_types, 123) = 123);
      Output ("test float", echoFloat (Myall_types, 2.7) = 2.7);
      Output ("test double", echoDouble (Myall_types, 3.14) = 3.14);
      Output ("test char", echoChar (Myall_types, 'A') = 'A');
      Output ("test octet", echoOctet (Myall_types, 5) = 5);
      Output ("test string",
              To_Standard_String
              (echoString (Myall_types, To_CORBA_String ("hello"))) = "hello");
      Output ("test enum", echoColor (Myall_types, Blue) = Blue);
      Output ("test fixed point", False);
      --  These cause a compiler abort in GNAT 3.12p.
      --  and then echoMoney (Myall_Types, 6423.50) = 6423.50
      --  and then echoMoney (Myall_Types, 0.0) = 0.0
      --  and then echoMoney (Myall_Types, 3.14) = 3.14);

      --  Structs
      declare
         Test_Struct : constant simple_struct
           := (123, To_CORBA_String ("Hello world!"));
      begin
         Output ("test struct",
                 echoStruct (Myall_types, Test_Struct) = Test_Struct);
      end;
      declare
         Test_Struct : constant array_struct
           :=  (A => (0,1,2,3,4,5,6,7,8,9), B => 65533);
      begin
         Output ("test array struct",
                 echoArrayStruct (Myall_types, Test_Struct) = Test_Struct);
      end;

      --  Unions
      declare
         Test_Unions : constant array (0 .. 3) of myUnion
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
      end;
      Output ("test exception", Ok);

      --  Arrays
      declare
         X : simple_array := (2, 3, 5, 7, 11);
      begin
         Output ("test simple array", echoArray (Myall_types, X) = X);
      end;
      declare
         M : matrix := ((165, 252, 375), (377, 145, 222), (202, 477, 147));
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

      --  Attributes
      set_myColor (Myall_types, Green);
      Output ("test attribute", get_myColor (Myall_types) = Green);
      declare
         Counter_First_Value : CORBA.Long
           := get_Counter (Myall_types);
         Counter_Second_Value : CORBA.Long
           := get_Counter (Myall_types);
      begin
         Output ("test read-only attribute",
                 Counter_Second_Value = Counter_First_Value + 1);
      end;

      --  Unbounded sequences
      declare
         X : U_Sequence := U_Sequence (IDL_Sequence_Short.Null_Sequence);
      begin
         X := X & 1 & 2 & 3 & 4 & 5;
         Output ("test unbounded sequence",  echoUsequence (Myall_types, X) = X);
      end;

      --  Bounded sequences
      declare
         X : B_Sequence := B_Sequence (IDL_SEQUENCE_Short_10.Null_Sequence);
      begin
         X := X & 1 & 2 & 3 & 4 & 5;
         Output ("test bounded sequence",  echoBsequence (Myall_types, X) = X);
      end;

      --  Refs
      declare
         X : all_types.Ref;
      begin
         X := echoRef (Myall_types, Myall_types);
         for I in 1 .. 20 loop
            X := echoRef (X, X);
         end loop;
         Output ("test self reference", echoLong (X, 31337) = 31337);
      end;

      exit when One_Shot;
   end loop;

end Client;
