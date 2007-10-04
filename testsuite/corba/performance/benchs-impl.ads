------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          B E N C H S . I M P L                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2007, Free Software Foundation, Inc.             --
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
with PortableServer;

package Benchs.Impl is

   type Object is new PortableServer.Servant_Base with private;

   function NoParameter (Self : access Object) return CORBA.Short;

   procedure Azerty (Self : access Object);
   procedure Azertyazerty (Self : access Object);
   procedure Azertyazertyazerty (Self : access Object);
   procedure Azertyazertyazertyazerty (Self : access Object);
   procedure Azertyazertyazertyazertyazerty (Self : access Object);
   procedure Azertyazertyazertyazertyazertyazerty (Self : access Object);
   procedure Azertyazertyazertyazertyazertyazertyazerty
     (Self : access Object);
   procedure Azertyazertyazertyazertyazertyazertyazertyazerty
     (Self : access Object);
   procedure Azertyazertyazertyazertyazertyazertyazertyazertyazerty
     (Self : access Object);
   procedure Azertyazertyazertyazertyazertyazertyazertyazertyazertyazerty
     (Self : access Object);

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

   function echoColor
     (Self : access Object;
      arg  : in Color) return Color;

   function echoRainbow
     (Self : access Object;
      arg  : in Rainbow) return Rainbow;

   function echoUnion
     (Self : access Object;
      arg : in myUnion) return myUnion;

   function echoUnionEnumSwitch
     (Self : access Object;
      arg : in myUnionEnumSwitch) return myUnionEnumSwitch;

   function echoStruct
     (Self : access Object;
      arg  : in simple_struct) return simple_struct;

   function echoArrayStruct
     (Self : access Object;
      arg  : in array_struct) return array_struct;

   function echoSixteenKb
     (Self : access Object;
      arg : in sixteenKb) return sixteenKb;

   function echoNestedStruct
     (Self : access Object;
      arg  : in nested_struct) return nested_struct;

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence) return U_sequence;

   procedure StopServer (Self : access Object);

private

   type Object is new PortableServer.Servant_Base with record
      Data : CORBA.Short := 123;
   end record;

end Benchs.Impl;
