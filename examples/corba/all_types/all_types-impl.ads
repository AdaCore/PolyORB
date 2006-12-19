------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A L L _ T Y P E S . I M P L                        --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with CORBA.Object;
with PortableServer;

package all_types.Impl is

   --  This is simply used to define the operations.

   type Object is new PortableServer.Servant_Base with record
      Attr_My_Color : Color := Blue;
      Attr_Counter  : CORBA.Long := 0;
   end record;

   function echoBoolean
     (Self : access Object;
      arg : in CORBA.Boolean) return CORBA.Boolean;

   function echoShort
     (Self : access Object;
      arg : in CORBA.Short) return CORBA.Short;

   function echoLong
     (Self : access Object;
      arg : in CORBA.Long) return CORBA.Long;

   function echoUShort
     (Self : access Object;
      arg : in CORBA.Unsigned_Short) return CORBA.Unsigned_Short;

   function echoULong
     (Self : access Object;
      arg : in CORBA.Unsigned_Long) return CORBA.Unsigned_Long;

   function echoULLong
     (Self : access Object;
      arg : in CORBA.Unsigned_Long_Long) return CORBA.Unsigned_Long_Long;

   function echoFloat
     (Self : access Object;
      arg : in CORBA.Float) return CORBA.Float;

   function echoDouble
     (Self : access Object;
      arg : in CORBA.Double) return CORBA.Double;

   function echoChar
     (Self : access Object;
      arg : in CORBA.Char) return CORBA.Char;

   function echoWChar
     (Self : access Object;
      arg : in CORBA.Wchar) return CORBA.Wchar;

   function echoOctet
     (Self : access Object;
      arg : in CORBA.Octet) return CORBA.Octet;

   function echoString
     (Self : access Object;
      arg : in CORBA.String) return CORBA.String;

   function echoWString
     (Self : access Object;
      arg : in CORBA.Wide_String) return CORBA.Wide_String;

   function echoRef
     (Self : access Object;
      arg : in all_types.Ref) return all_types.Ref'Class;

   function echoObject
     (Self : access Object;
      arg  : in CORBA.Object.Ref) return CORBA.Object.Ref;

   function echoOtherAllTypes
     (Self : access Object;
      arg  : in all_types.otherAllTypes) return all_types.otherAllTypes;

   function echoOtherObject
     (Self : access Object;
      arg  : in all_types.otherObject) return all_types.otherObject;

   function echoBoundedStr
     (Self : access Object;
      arg  : in all_types.BoundedStr) return all_types.BoundedStr;

   function echoBoundedWStr
     (Self : access Object;
      arg  : in all_types.BoundedWStr) return all_types.BoundedWStr;

   function echoColor
     (Self : access Object;
      arg  : in Color) return Color;

   function echoRainbow
     (Self : access Object;
      arg  : in Rainbow) return Rainbow;

   function echoArray
     (Self : access Object;
      Arg : in simple_array) return simple_array;

   function echoMatrix
     (Self : access Object;
      arg : in matrix) return matrix;

   function echoBigMatrix
     (Self : access Object;
      arg : in bigmatrix) return bigmatrix;

   function echoNestedArray
     (Self : access Object;
      Arg : in nested_array) return nested_array;

   function echoSixteenKb
     (Self : access Object;
      arg : in sixteenKb) return sixteenKb;

   procedure testException
     (Self : access Object;
      arg : in CORBA.Long);

   procedure testUnknownException
     (Self : access Object;
      arg : in CORBA.Long);

   procedure testSystemException
     (Self : access Object;
      arg : in CORBA.Long);

   function echoStruct
     (Self : access Object;
      arg  : in simple_struct) return simple_struct;

   function echoArrayStruct
     (Self : access Object;
      arg  : in array_struct) return array_struct;

   function echoNestedStruct
     (Self : access Object;
      arg  : in nested_struct) return nested_struct;

   function echoUnion
     (Self : access Object;
      arg : in myUnion) return myUnion;

   function echoUnionEnumSwitch
     (Self : access Object;
      arg : in myUnionEnumSwitch) return myUnionEnumSwitch;

   function echoNoMemberUnion
     (Self : access Object;
      arg : in noMemberUnion) return noMemberUnion;

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence) return U_sequence;

   function echoBsequence
     (Self : access Object;
      arg : in B_sequence) return B_sequence;

   function echoMoney
     (Self : access Object;
      Arg  : in Money) return Money;

   procedure set_MyColor
     (Self : access Object;
      arg : in Color);

   function get_myColor
     (Self : access Object)
     return Color;

   function get_Counter
     (Self : access Object) return CORBA.Long;

   procedure StopServer (Self : access Object);

end all_types.Impl;
