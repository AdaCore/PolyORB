------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.20 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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
   One_Shot : Boolean := True;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
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

      declare
         Test_Struct : constant simple_struct
           := (123, To_CORBA_String ("Hello world!"));
      begin
         Output ("test struct",
                 echoStruct (Myall_types, Test_Struct) = Test_Struct);
      end;

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

      declare
         X : U_sequence := U_sequence (IDL_SEQUENCE_short.Null_Sequence);
      begin
         X := X & 16 & 32 & 64 & 128 & 257;
         Output ("test unbounded sequence",
                 echoUsequence (Myall_types, X) = X);
      end;

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

--   declare
--      X : simple_struct := (A => (0,1,2,3,4,5,6,7,8,9), B => 10);
--   begin
--      Output ("test simple structure", echo2 (Myall_types, X) = X);
--   end;

--   Output ("test enumeration", echo3 (Myall_types, Blue) = Blue);
--
   --  Bounded sequences
--   declare
--      X : B_Sequence := B_Sequence (IDL_SEQUENCE_Long_1.Null_Sequence);
--   begin
--      X := X & 1 & 2 & 3 & 4 & 5;
--      Output ("test bounded sequence",  echo7 (Myall_types, X) = X);
--   end;

--   declare
--      X : all_types.Line
--        := ((Switch => 1, Counter => 19),
--            (Switch => 2, Flags => True),
--            (Switch => 3, Unknown => 25));
--   begin
--      Output ("test arrays (1)", echo8 (Myall_types, X) = X);
--   end;
--
--   declare
--      X : Square
--        := (((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
--             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)),
--            ((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
--             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)));
--   begin
--      Output ("test arrays (2)", echo9 (Myall_types, X) = X);
--   end;
--
--   declare
--      X : Cube
--        := (((To_CORBA_String (Standard.String'("case1")),
--              To_CORBA_String (Standard.String'("case2"))),
--             (To_CORBA_String (Standard.String'("case3")),
--              To_CORBA_String (Standard.String'("case4")))),
--            ((To_CORBA_String (Standard.String'("case5")),
--              To_CORBA_String (Standard.String'("case6"))),
--             (To_CORBA_String (Standard.String'("case7")),
--              To_CORBA_String (Standard.String'("case8")))));
--   begin
--      Output ("test arrays (3)", echo10 (Myall_types, X) = X);
--   end;

--   declare
--      X : all_types.Ref;
--      Y : Example := (Switch => 2, Flags => False);
--   begin
--      Set_N_Attribute (Myall_Types, Y);
--      X := echo11 (Myall_types, Myall_Types);
--      Output ("test reference", Get_N_Attribute (X) = Y);
--   end;

--   declare
--      X : CORBA.Object.Ref := CORBA.Object.Ref (To_Ref (Myall_Types));
--   begin
--      Output ("test CORBA.Object.Ref",
--         Is_Equivalent (echo12 (Myall_types, X), X));
--   end;

--        Set_MyColor (Myall_types, Green);
--        Output ("test attribute", Get_MyColor (Myall_types) = Green);
--        declare
--           Counter_First_Value : CORBA.Long
--             := Get_Counter (Myall_types);
--           Counter_Second_Value : CORBA.Long
--             := Get_Counter (Myall_types);
--        begin
--           Output ("test read-only attribute",
--                   Counter_Second_Value = Counter_First_Value + 1);
--        end;

      exit when One_Shot;
   end loop;

--     declare
--        X : all_types.Ref;
--     begin
--        --  X := all_types.Convert_Forward.From_Forward
--        --   (echoRef (Myall_types, all_types.Convert_Forward.To_Forward
--        --    (Myall_types)));
--        X := echoRef (Myall_types, Myall_types);
--        Output ("test self reference", echoLong (X, 31337) = 31337);
--     end;


end Client;
