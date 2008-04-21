------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A L L _ T Y P E S . I M P L                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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
with Ada.Wide_Text_IO;
with CORBA.ORB;

with all_types.Skel;
pragma Warnings (Off, all_types.Skel);

with all_types.Helper;
pragma Warnings (Off, all_types.Helper);

package body all_types.Impl is

   pragma Warnings (Off);
   type IDL_Exception_Members_Ptr is
     access all CORBA.IDL_Exception_Members'Class;

   function echoBoolean
     (Self : access Object;
      arg : CORBA.Boolean)
      return CORBA.Boolean
   is
   begin
      return arg;
   end echoBoolean;

   function echoShort
     (Self : access Object;
      arg : CORBA.Short)
      return CORBA.Short
   is
   begin
      return arg;
   end echoShort;

   function echoLong
     (Self : access Object;
      arg : CORBA.Long)
      return CORBA.Long
   is
   begin
      return arg;
   end echoLong;

   function echoUShort
     (Self : access Object;
      arg : CORBA.Unsigned_Short)
      return CORBA.Unsigned_Short
   is
   begin
      return arg;
   end echoUShort;

   function echoULong
     (Self : access Object;
      arg : CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long
   is
   begin
      return arg;
   end echoULong;

   function echoULLong
     (Self : access Object;
      arg : CORBA.Unsigned_Long_Long)
      return CORBA.Unsigned_Long_Long
   is
   begin
      return arg;
   end echoULLong;

   function echoFloat
     (Self : access Object;
      arg : CORBA.Float)
      return CORBA.Float
   is
   begin
      return arg;
   end echoFloat;

   function echoDouble
     (Self : access Object;
      arg : CORBA.Double)
      return CORBA.Double
   is
   begin
      return arg;
   end echoDouble;

   function echoChar
     (Self : access Object;
      arg : CORBA.Char)
      return CORBA.Char
   is
   begin
      return arg;
   end echoChar;

   function echoWChar
     (Self : access Object;
      arg : CORBA.Wchar)
     return CORBA.Wchar
   is
   begin
      return arg;
   end echoWChar;

   function echoOctet
     (Self : access Object;
      arg : CORBA.Octet)
      return CORBA.Octet
   is
   begin
      return arg;
   end echoOctet;

   function echoString
     (Self : access Object;
      arg : CORBA.String)
      return CORBA.String
   is
   begin
      Ada.Text_IO.Put_Line
        ("Unbounded standard string: « "
         & CORBA.To_Standard_String (arg)
         & " »");
      return arg;
   end echoString;

   function echoWString
     (Self : access Object;
      arg : CORBA.Wide_String)
      return CORBA.Wide_String
   is
   begin
      Ada.Wide_Text_IO.Put_Line
       ("Unbounded wide string: « "
        & CORBA.To_Standard_Wide_String (arg)
        & " »");
      return arg;
   end echoWString;

   function echoBooleanAndLong
     (Self : access Object;
      arg1 : CORBA.Boolean;
      arg2 : CORBA.Long) return CORBA.Long
   is
   begin
      return Arg2;
   end echoBooleanAndLong;

   function echoBooleanAndShort
     (Self : access Object;
      arg1 : CORBA.Boolean;
      arg2 : CORBA.Short) return CORBA.Short
   is
   begin
      return Arg2;
   end echoBooleanAndShort;

   function echoUnion
     (Self : access Object;
      arg : Simple_Union) return Simple_Union
   is
   begin
      return Arg;
   end echoUnion;

   function echoStringAndUnion
     (Self : access Object;
      arg1 : Simple_Union;
      arg2 : CORBA.string) return CORBA.String
   is
   begin
      return Arg2;
   end echoStringAndUnion;

   function echoUnionAndString
     (Self : access Object;
      arg1 : Simple_Union;
      arg2 : CORBA.string) return Simple_Union
   is
   begin
      return Arg1;
   end echoUnionAndString;

   function echoStruct
     (Self : access Object;
      arg : Simple_Struct) return Simple_Struct
   is
   begin
      return Arg;
   end echoStruct;

   function echoUsequence
     (Self : access Object;
      arg  : U_sequence) return U_Sequence
   is
   begin
      return Arg;
   end echoUsequence;

   function echoBsequence
     (Self : access Object;
      arg  : B_sequence) return B_Sequence
   is
   begin
      return Arg;
   end echoBsequence;

   procedure StopServer (Self : access Object) is
   begin
      CORBA.ORB.Shutdown (Wait_For_Completion => False);
   end StopServer;

end all_types.Impl;
