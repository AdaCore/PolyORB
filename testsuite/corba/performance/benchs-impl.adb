------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          B E N C H S . I M P L                           --
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

with CORBA.ORB;

with benchs.Skel;
pragma Warnings (Off, benchs.Skel);

package body benchs.Impl is

   function NoParameter (Self : access Object) return CORBA.Short is
   begin
      return Self.Data;
   end NoParameter;

   procedure Azerty (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azerty;

   procedure Azertyazerty (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazerty;

   procedure Azertyazertyazerty (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazertyazerty;

   procedure Azertyazertyazertyazerty (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazertyazertyazerty;

   procedure Azertyazertyazertyazertyazerty (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazertyazertyazertyazerty;

   procedure Azertyazertyazertyazertyazertyazerty (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazertyazertyazertyazertyazerty;

   procedure Azertyazertyazertyazertyazertyazertyazerty
     (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazertyazertyazertyazertyazertyazerty;

   procedure Azertyazertyazertyazertyazertyazertyazertyazerty
     (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazertyazertyazertyazertyazertyazertyazerty;

   procedure Azertyazertyazertyazertyazertyazertyazertyazertyazerty
     (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazertyazertyazertyazertyazertyazertyazertyazerty;

   procedure Azertyazertyazertyazertyazertyazertyazertyazertyazertyazerty
     (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      null;
   end Azertyazertyazertyazertyazertyazertyazertyazertyazertyazerty;

   function echoBoolean
     (Self : access Object;
      arg : CORBA.Boolean)
     return CORBA.Boolean
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoBoolean;

   function echoShort
     (Self : access Object;
      arg : CORBA.Short)
     return CORBA.Short
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoShort;

   function echoLong
     (Self : access Object;
      arg : CORBA.Long)
      return CORBA.Long
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoLong;

   function echoUShort
     (Self : access Object;
      arg : CORBA.Unsigned_Short)
     return CORBA.Unsigned_Short
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoUShort;

   function echoULong
     (Self : access Object;
      arg : CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoULong;

   function echoULLong
     (Self : access Object;
      arg : CORBA.Unsigned_Long_Long)
     return CORBA.Unsigned_Long_Long
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoULLong;

   function echoFloat
     (Self : access Object;
      arg : CORBA.Float)
      return CORBA.Float
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoFloat;

   function echoDouble
     (Self : access Object;
      arg : CORBA.Double)
     return CORBA.Double
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoDouble;

   function echoChar
     (Self : access Object;
      arg : CORBA.Char)
     return CORBA.Char
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoChar;

   function echoWChar
     (Self : access Object;
      arg : CORBA.Wchar)
     return CORBA.Wchar
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoWChar;

   function echoOctet
     (Self : access Object;
      arg : CORBA.Octet)
     return CORBA.Octet
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoOctet;

   function echoString
     (Self : access Object;
      arg : CORBA.String)
     return CORBA.String
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoString;

   function echoWString
     (Self : access Object;
      arg : CORBA.Wide_String)
     return CORBA.Wide_String
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoWString;

   function echoColor
     (Self : access Object;
      arg  : Color)
      return Color is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoColor;

   function echoRainbow
     (Self : access Object;
      arg  : Rainbow)
      return Rainbow is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoRainbow;

   function echoStruct
     (Self : access Object;
      arg  : simple_struct)
      return simple_struct is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoStruct;

   function echoArrayStruct
     (Self : access Object;
      arg  : array_struct)
      return array_struct is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoArrayStruct;

   function echoSixteenKb
     (Self : access Object;
      arg : sixteenKb)
     return sixteenKb
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoSixteenKb;

   function echoNestedStruct
     (Self : access Object;
      arg  : nested_struct)
      return nested_struct is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoNestedStruct;

   function echoUnion
     (Self : access Object;
      arg : myUnion)
     return myUnion is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoUnion;

   function echoUnionEnumSwitch
     (Self : access Object;
      arg : myUnionEnumSwitch)
     return myUnionEnumSwitch is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoUnionEnumSwitch;

   function echoUsequence
     (Self : access Object;
      arg : U_sequence)
     return U_sequence
   is
      pragma Unreferenced (Self);
   begin
      return arg;
   end echoUsequence;

   procedure StopServer (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      CORBA.ORB.Shutdown (Wait_For_Completion => False);
   end StopServer;

end benchs.Impl;
