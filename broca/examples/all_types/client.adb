------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.19 $
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

-- all_types client
with Ada.Command_Line;
with Ada.Text_IO;

with CORBA; use CORBA;
with CORBA.ORB;

with All_Types; use All_Types;
with Report;    use Report;

procedure Client is
   IOR : CORBA.String;
   MyAll_Types : All_Types.Ref;
   Ok : Boolean;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   --  transforms the Ada string into CORBA.String
   IOR := CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)) ;

   --  getting the CORBA.Object
   CORBA.ORB.String_To_Object (IOR, MyAll_Types);

   --  checking if it worked
   if All_Types.Is_Nil (MyAll_Types) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   Output ("test not null", not All_Types.Is_Nil (MyAll_Types));

   loop
      Output ("test boolean", EchoBoolean (MyAll_Types, True) = True);
      Output ("test short", EchoShort (MyAll_Types, 123) = 123);
      Output ("test long",  EchoLong (MyAll_Types, 456) = 456);
      Output ("test unsigned_short", EchoUShort (MyAll_Types, 456) = 456);
      Output ("test unsigned_long", EchoULong (MyAll_Types, 123) = 123);
      Output ("test float", EchoFloat (MyAll_Types, 2.7) = 2.7);
      Output ("test double", EchoDouble (MyAll_Types, 3.14) = 3.14);
      Output ("test char", EchoChar (MyAll_Types, 'A') = 'A');
      Output ("test octet", EchoOctet (MyAll_Types, 5) = 5);
      Output ("test string",
              To_Standard_String (EchoString (MyAll_Types, To_CORBA_String ("hello"))) = "hello");
      Output ("test enum", EchoColor (MyAll_Types, Blue) = Blue);

      declare
         Test_Struct : constant Simple_Struct
           := ((others => 123), To_CORBA_String ("Hello world!"));
      begin
         Output ("test struct", EchoStruct (MyAll_Types, Test_Struct) = Test_Struct);
      end;

      declare
         Test_Unions : constant array (0 .. 3) of MyUnion
           := ((Switch => 0, Unknown => 987),
               (Switch => 1, Counter => 1212),
               (Switch => 2, Flag => True),
               (Switch => 3, Hue => Green));
         Pass : Boolean := True;
      begin
         for I in Test_Unions'Range loop
            Pass := Pass and then EchoUnion (MyAll_Types, Test_Unions (I))
              = Test_Unions (I);
            exit when not Pass;
         end loop;
         Output ("test union", Pass);
      end;

      declare
         X : U_Sequence := U_Sequence (IDL_SEQUENCE_Short.Null_Sequence);
      begin
         X := X & 16 & 32 & 64 & 128 & 257;
         Output ("test unbounded sequence", EchoUsequence (MyAll_Types, X) = X);
      end;

      declare
         Member : My_Exception_Members;
      begin
         Ok := False;
         TestException (MyAll_Types, 2485);
      exception
         when E : My_Exception =>
            Get_Members (E, Member);
            Ok := (Member.Info = 2485);
      end;
      Output ("test exception", Ok);

      declare
         X : Simple_Array := (2,3,5,7,11);
      begin
         Output ("test simple array", EchoArray (MyAll_Types, X) = X);
      end;

      declare
         M : Matrix := ((165, 252, 375), (377, 145, 222), (202, 477, 147));
      begin
         Output ("test multi-dimensional array", EchoMatrix (MyAll_Types, M) = M);
      end;

--   declare
--      X : Simple_Struct := (A => (0,1,2,3,4,5,6,7,8,9), B => 10);
--   begin
--      Output ("test simple structure", Echo2 (MyAll_Types, X) = X);
--   end;

--   Output ("test enumeration", Echo3 (MyAll_Types, Blue) = Blue);
--
   -- bounded sequences
--   declare
--      X : B_Sequence := B_Sequence (IDL_SEQUENCE_Long_1.Null_Sequence);
--   begin
--      X := X & 1 & 2 & 3 & 4 & 5;
--      Output ("test bounded sequence",  Echo7 (MyAll_Types, X) = X);
--   end;

--   declare
--      X : All_Types.Line
--        := ((Switch => 1, Counter => 19),
--            (Switch => 2, Flags => True),
--            (Switch => 3, Unknown => 25));
--   begin
--      Output ("test arrays (1)", Echo8 (MyAll_Types, X) = X);
--   end;
--
--   declare
--      X : Square
--        := (((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
--             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)),
--            ((A => (0,1,2,3,4,5,6,7,8,9), B=> 23),
--             (A => (9,8,7,6,5,4,3,2,1,0), B=> 17)));
--   begin
--      Output ("test arrays (2)", Echo9 (MyAll_Types, X) = X);
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
--      Output ("test arrays (3)", Echo10 (MyAll_Types, X) = X);
--   end;

--   declare
--      X : All_Types.Ref;
--      Y : Example := (Switch => 2, Flags => False);
--   begin
--      Set_N_Attribute (Myall_Types, Y);
--      X := Echo11 (MyAll_Types, Myall_Types);
--      Output ("test reference", Get_N_Attribute (X) = Y);
--   end;

--   declare
--      X : CORBA.Object.Ref := CORBA.Object.Ref (To_Ref (Myall_Types));
--   begin
--      Output ("test CORBA.Object.Ref",
--         Is_Equivalent (Echo12 (MyAll_Types, X), X));
--   end;

      Set_MyColor (MyAll_Types, Green);
      Output ("test attribute", Get_MyColor (MyAll_Types) = Green);
      declare
         Counter_First_Value : CORBA.Long
           := Get_Counter (MyAll_Types);
         Counter_Second_Value : CORBA.Long
           := Get_Counter (MyAll_Types);
      begin
         Output ("test read-only attribute",
                 Counter_Second_Value = Counter_First_Value + 1);
      end;
 
   end loop;

      declare
         X : All_Types.Ref;
      begin
         --  X := All_Types.Convert_Forward.From_Forward (EchoRef (MyAll_Types, All_Types.Convert_Forward.To_Forward (MyAll_Types)));
         X := EchoRef (MyAll_Types, MyAll_Types);
         Output ("test self reference", EchoLong (X, 31337) = 31337);
      end;


end Client;
