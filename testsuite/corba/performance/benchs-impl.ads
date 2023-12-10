------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          B E N C H S . I M P L                           --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with PortableServer;

package benchs.Impl is

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
      arg : CORBA.Boolean) return CORBA.Boolean;

   function echoShort
     (Self : access Object;
      arg : CORBA.Short) return CORBA.Short;

   function echoLong
     (Self : access Object;
      arg : CORBA.Long) return CORBA.Long;

   function echoUShort
     (Self : access Object;
      arg : CORBA.Unsigned_Short) return CORBA.Unsigned_Short;

   function echoULong
     (Self : access Object;
      arg : CORBA.Unsigned_Long) return CORBA.Unsigned_Long;

   function echoULLong
     (Self : access Object;
      arg : CORBA.Unsigned_Long_Long) return CORBA.Unsigned_Long_Long;

   function echoFloat
     (Self : access Object;
      arg : CORBA.Float) return CORBA.Float;

   function echoDouble
     (Self : access Object;
      arg : CORBA.Double) return CORBA.Double;

   function echoChar
     (Self : access Object;
      arg : CORBA.Char) return CORBA.Char;

   function echoWChar
     (Self : access Object;
      arg : CORBA.Wchar) return CORBA.Wchar;

   function echoOctet
     (Self : access Object;
      arg : CORBA.Octet) return CORBA.Octet;

   function echoString
     (Self : access Object;
      arg : CORBA.String) return CORBA.String;

   function echoWString
     (Self : access Object;
      arg : CORBA.Wide_String) return CORBA.Wide_String;

   function echoColor
     (Self : access Object;
      arg  : Color) return Color;

   function echoRainbow
     (Self : access Object;
      arg  : Rainbow) return Rainbow;

   function echoUnion
     (Self : access Object;
      arg : myUnion) return myUnion;

   function echoUnionEnumSwitch
     (Self : access Object;
      arg : myUnionEnumSwitch) return myUnionEnumSwitch;

   function echoStruct
     (Self : access Object;
      arg  : simple_struct) return simple_struct;

   function echoArrayStruct
     (Self : access Object;
      arg  : array_struct) return array_struct;

   function echoSixteenKb
     (Self : access Object;
      arg : sixteenKb) return sixteenKb;

   function echoNestedStruct
     (Self : access Object;
      arg  : nested_struct) return nested_struct;

   function echoUsequence
     (Self : access Object;
      arg : U_sequence) return U_sequence;

   procedure StopServer (Self : access Object);

private

   type Object is new PortableServer.Servant_Base with record
      Data : CORBA.Short := 123;
   end record;

end benchs.Impl;
