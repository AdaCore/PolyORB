------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A L L _ T Y P E S . I M P L                        --
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

with Ada.Text_IO;
with CORBA.ORB;

with all_types.Skel;
pragma Elaborate (all_types.Skel);
pragma Warnings (Off, all_types.Skel);

with all_types.Helper;

package body all_types.Impl is

   pragma Warnings (Off);
   type IDL_Exception_Members_Ptr is
     access all CORBA.IDL_Exception_Members'Class;

   function echoBoolean
     (Self : access Object;
      arg : in CORBA.Boolean)
      return CORBA.Boolean
   is
   begin
      return arg;
   end echoBoolean;

   function echoShort
     (Self : access Object;
      arg : in CORBA.Short)
      return CORBA.Short
   is
   begin
      return arg;
   end echoShort;

   function echoLong
     (Self : access Object;
      arg : in CORBA.Long)
      return CORBA.Long
   is
   begin
      return arg;
   end echoLong;

   function echoUShort
     (Self : access Object;
      arg : in CORBA.Unsigned_Short)
      return CORBA.Unsigned_Short
   is
   begin
      return arg;
   end echoUShort;

   function echoULong
     (Self : access Object;
      arg : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      return arg;
   end echoULong;

   function echoULLong
     (Self : access Object;
      arg : in CORBA.Unsigned_Long_Long)
      return CORBA.Unsigned_Long_Long
   is
   begin
      return arg;
   end echoULLong;

   function echoFloat
     (Self : access Object;
      arg : in CORBA.Float)
      return CORBA.Float
   is
   begin
      return arg;
   end echoFloat;

   function echoDouble
     (Self : access Object;
      arg : in CORBA.Double)
      return CORBA.Double
   is
   begin
      return arg;
   end echoDouble;

   function echoChar
     (Self : access Object;
      arg : in CORBA.Char)
      return CORBA.Char
   is
   begin
      return arg;
   end echoChar;

   function echoWChar
     (Self : access Object;
      arg : in CORBA.Wchar)
     return CORBA.Wchar
   is
   begin
      return arg;
   end echoWChar;

   function echoOctet
     (Self : access Object;
      arg : in CORBA.Octet)
      return CORBA.Octet
   is
   begin
      return arg;
   end echoOctet;

   function echoString
     (Self : access Object;
      arg : in CORBA.String)
      return CORBA.String
   is
   begin
      Ada.Text_IO.Put_Line
        ("Thus spake my client unto me: � "
         & CORBA.To_Standard_String (arg)
         & " �");
      return arg;
   end echoString;

   function echoWString
     (Self : access Object;
      arg : in CORBA.Wide_String)
      return CORBA.Wide_String
   is
   begin
      return arg;
   end echoWString;

   function echoRef
     (Self : access Object;
      arg : in all_types.Ref)
      return all_types.Ref
   is
   begin
      return arg;
   end echoRef;

   function echoObject
     (Self : access Object;
      arg  : in CORBA.Object.Ref)
     return CORBA.Object.Ref is
   begin
      return arg;
   end echoObject;

   function echoOtherAllTypes
     (Self : access Object;
      arg  : in all_types.otherAllTypes)
     return all_types.otherAllTypes is
   begin
      return arg;
   end echoOtherAllTypes;

   function echoOtherObject
     (Self : access Object;
      arg  : in all_types.otherObject)
     return all_types.otherObject is
   begin
      return arg;
   end echoOtherObject;

   function echoColor
     (Self : access Object;
      arg  : in Color)
      return Color is
   begin
      return arg;
   end echoColor;

   function echoRainbow
     (Self : access Object;
      arg  : in Rainbow)
      return Rainbow is
   begin
      return arg;
   end echoRainbow;

   function echoMoney
     (Self : access Object;
      Arg  : in Money)
      return Money is
   begin
      return Arg;
   end echoMoney;

   function echoArray
     (Self : access Object;
      Arg : in simple_array)
      return simple_array
   is
   begin
      return Arg;
   end echoArray;

   function echoMatrix
     (Self : access Object;
      arg : in matrix)
      return matrix
   is
   begin
      return arg;
   end echoMatrix;

   function echoBigMatrix
     (Self : access Object;
      arg : in bigmatrix)
      return bigmatrix
   is
   begin
      return arg;
   end echoBigMatrix;

   function echoSixteenKb
     (Self : access Object;
      arg : in sixteenKb)
      return sixteenKb
   is
   begin
      return arg;
   end echoSixteenKb;

   procedure testException
     (Self : access Object;
      arg : in CORBA.Long)
   is
   begin
      all_types.Helper.Raise_my_exception
        (my_exception_Members'(Info => arg));
   end testException;

   procedure testUnknownException
     (Self : access Object;
      arg  : in CORBA.Long) is
   begin
      raise Constraint_Error;
   end testUnknownException;

   procedure testSystemException
     (Self : access Object;
      arg : in CORBA.Long) is
   begin
      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end testSystemException;

   function echoStruct
     (Self : access Object;
      arg  : in simple_struct)
      return simple_struct is
   begin
      return arg;
   end echoStruct;

   function echoArrayStruct
     (Self : access Object;
      arg  : in array_struct)
      return array_struct is
   begin
      return arg;
   end echoArrayStruct;

   function echoNestedStruct
     (Self : access Object;
      arg  : in nested_struct)
      return nested_struct is
   begin
      return arg;
   end echoNestedStruct;

   function echoUnion
     (Self : access Object;
      arg : in myUnion)
     return myUnion is
   begin
      return arg;
   end echoUnion;

   function echoUnionEnumSwitch
     (Self : access Object;
      arg : in myUnionEnumSwitch)
     return myUnionEnumSwitch is
   begin
      return arg;
   end echoUnionEnumSwitch;

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence)
     return U_sequence
   is
   begin
      return arg;
   end echoUsequence;

   function echoBsequence
     (Self : access Object;
      arg : in B_sequence)
     return B_sequence
   is
   begin
      return arg;
   end echoBsequence;

   procedure set_MyColor
     (Self : access Object;
      arg : in Color)
   is
   begin
      Self.Attr_My_Color := arg;
   end set_MyColor;

   function get_myColor
     (Self : access Object)
     return Color
   is
   begin
      return Self.Attr_My_Color;
   end get_myColor;

   function get_Counter
     (Self : access Object)
     return CORBA.Long
   is
      use CORBA;
   begin
      Self.Attr_Counter := Self.Attr_Counter + 1;
      return Self.Attr_Counter;
   end get_Counter;

   procedure StopServer (Self : access Object) is
   begin
      CORBA.ORB.Shutdown (Wait_For_Completion => False);
   end StopServer;

end all_types.Impl;
