------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2023, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;

with Ada.Text_IO;
with CORBA.ORB;

with benchs;
with Bench_Utils;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Utils.Report;

procedure Client is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use benchs;
   use Bench_Utils;

   use CORBA;

   use PolyORB.Utils.Report;

   B : sixteenKb;
   X : U_sequence := U_sequence (IDL_SEQUENCE_short.Null_Sequence);
   Ref : benchs.Ref;

   ---------------------
   -- Run_NoParameter --
   ---------------------

   procedure Run_NoParameter;

   procedure Run_NoParameter is
      R : CORBA.Short;
      pragma Unreferenced (R);
   begin
      R := benchs.noParameter (Ref);
   end Run_NoParameter;

   procedure Run_Azerty;

   procedure Run_Azerty is
   begin
      benchs.azerty (Ref);
   end Run_Azerty;

   procedure Run_echoBoolean;

   procedure Run_echoBoolean is
      R : Boolean;
      pragma Unreferenced (R);
   begin
      R := benchs.echoBoolean (Ref, True);
   end Run_echoBoolean;

   procedure Run_echoShort;

   procedure Run_echoShort is
      R : CORBA.Short;
      pragma Unreferenced (R);
   begin
      R := benchs.echoShort (Ref, 123);
   end Run_echoShort;

   procedure Run_echoLong;

   procedure Run_echoLong is
      R : CORBA.Long;
      pragma Unreferenced (R);
   begin
      R := benchs.echoLong (Ref, 123);
   end Run_echoLong;

   procedure Run_echoFloat;

   procedure Run_echoFloat is
      R : CORBA.Float;
      pragma Unreferenced (R);
   begin
      R := benchs.echoFloat (Ref, 12.3);
   end Run_echoFloat;

   procedure Run_echoDouble;

   procedure Run_echoDouble is
      R : CORBA.Double;
      pragma Unreferenced (R);
   begin
      R := benchs.echoDouble (Ref, 12.3);
   end Run_echoDouble;

   procedure Run_echoChar;

   procedure Run_echoChar is
      R : CORBA.Char;
      pragma Unreferenced (R);
   begin
      R := benchs.echoChar (Ref, 'A');
   end Run_echoChar;

   procedure Run_echoWchar;

   procedure Run_echoWchar is
      R : CORBA.Wchar;
      pragma Unreferenced (R);
   begin
      R := benchs.echoWChar (Ref, 'A');
   end Run_echoWchar;

   procedure Run_echoString;

   procedure Run_echoString is
      R : CORBA.String;
      pragma Unreferenced (R);
   begin
      R := benchs.echoString (Ref, CORBA.To_CORBA_String ("Hello World !"));
   end Run_echoString;

   procedure Run_echoColor;

   procedure Run_echoColor is
      R : benchs.Color;
      pragma Unreferenced (R);
   begin
      R := benchs.echoColor (Ref, benchs.Blue);
   end Run_echoColor;

   procedure Run_echoRainbow;

   procedure Run_echoRainbow is
      R, X : benchs.Rainbow;
      pragma Unreferenced (R);
   begin
      for J in X'Range loop
         X (J) := benchs.Color'Val
           (J mod (benchs.Color'Pos (benchs.Color'Last) + 1));
      end loop;

      R := benchs.echoRainbow (Ref, X);
   end Run_echoRainbow;

   procedure Run_echoUnion;

   procedure Run_echoUnion is
      Test_Unions : constant array (Integer range <>) of benchs.myUnion
        := ((Switch => 0, Unknown => 987),
            (Switch => 1, Counter => 1212),
            (Switch => 2, Flag => True),
            (Switch => 3, Hue => benchs.Green));
      Pass : Boolean := True;
   begin
      for J in Test_Unions'Range loop
         Pass := (benchs.echoUnion (Ref, Test_Unions (J))
                  = Test_Unions (J)) and Pass;
      end loop;
   end Run_echoUnion;

   procedure Run_echoStruct;

   procedure Run_echoStruct is
      Test_Struct : constant benchs.simple_struct
        := (123, CORBA.To_CORBA_String ("Hello world!"));

      R : benchs.simple_struct;
      pragma Warnings (Off, R);
      --  WAG:7.4 P919-036
      pragma Unreferenced (R);
   begin
      R := benchs.echoStruct (Ref, Test_Struct);
   end Run_echoStruct;

   procedure Run_EchoSixteenKb;

   procedure Run_EchoSixteenKb is
   begin
      B := benchs.echoSixteenKb (Ref, B);
   end Run_EchoSixteenKb;

   procedure Run_EchoUsequence;

   procedure Run_EchoUsequence is
   begin
      X := benchs.echoUsequence (Ref, X);
   end Run_EchoUsequence;

   procedure Test_NoParameter is new Run_Test ("NoParameter", Run_NoParameter);
   procedure Test_Azerty is new Run_Test ("Azerty", Run_Azerty);
   procedure Test_Boolean is new Run_Test ("echoBoolean", Run_echoBoolean);
   procedure Test_Short is new Run_Test ("echoShort", Run_echoShort);
   procedure Test_Long is new Run_Test ("echoLong", Run_echoLong);
   procedure Test_Float is new Run_Test ("echoFloat", Run_echoFloat);
   procedure Test_Double is new Run_Test ("echoDouble", Run_echoDouble);
   procedure Test_Char is new Run_Test ("echoChar", Run_echoChar);
   procedure Test_Wchar is new Run_Test ("echoWchar", Run_echoWchar);
   procedure Test_String is new Run_Test ("echoString", Run_echoString);
   procedure Test_Color is new Run_Test ("echoColor", Run_echoColor);
   procedure Test_Rainbow is new Run_Test ("echoRainbow", Run_echoRainbow);
   procedure Test_Union is new Run_Test ("echoUnion", Run_echoUnion);
   procedure Test_Struct is new Run_Test ("echoStruct", Run_echoStruct);
   procedure Test_SixteenKb is new Run_Test
     ("echoSixteenKb", Run_EchoSixteenKb);
   procedure Test_Usequence is new Run_Test
     ("echoUsequence", Run_EchoUsequence);

   IOR : CORBA.String;

begin
   if Argument_Count > 2 then
      Ada.Text_IO.Put_Line
        ("usage : client <IOR_string_from_server> [bench_duration]");
      return;
   end if;

   IOR := To_CORBA_String (Argument (1));

   if Argument_Count = 2 then
      --  Overwrite default test duration

      Test_Duration := Integer'Value (Argument (2));
   end if;

   CORBA.ORB.Initialize ("ORB");

   CORBA.ORB.String_To_Object (IOR, Ref);

   if benchs.Is_Nil (Ref) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  Precompute a few constants for the tests

   for I in B'Range (1) loop
      for J in B'Range (2) loop
         B (I, J) := Long ((I + 1) * (J + 2));
      end loop;
   end loop;

   for J in 1 .. 2 ** 14 loop
      X := X & CORBA.Short (J);
   end loop;

   --  Launch all tests

   Test_NoParameter;
   Test_Azerty;
   Test_Boolean;
   Test_Short;
   Test_Long;
   Test_Float;
   Test_Double;
   Test_Char;
   Test_Wchar;
   Test_String;
   Test_Struct;
   Test_Color;
   Test_Rainbow;
   Test_Union;
   Test_SixteenKb;
   Test_Usequence;

   --  Stop the server

   StopServer (Ref);
   End_Report;
end Client;
