------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       A L L _ T Y P E S . I M P L                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.17 $
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

with all_types.Skel;
with Broca.Exceptions; use Broca.Exceptions;
pragma Elaborate (all_types.Skel);
pragma Warnings (Off, all_types.Skel);

package body all_types.Impl is

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
      return arg;
   end echoString;

   function echoRef
     (Self : access Object;
      arg : in all_types.Ref)
      return all_types.Ref
   is
   begin
      return arg;
   end echoRef;

   function echoColor
     (Self : access Object;
      arg  : in Color)
      return Color is
   begin
      return arg;
   end echoColor;

   function echoMoney
     (Self : access Object;
      arg  : in Money)
      return Money is
   begin
      return arg;
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

   procedure testException
     (Self : access Object;
      arg : in CORBA.Long)
   is
      Members : IDL_Exception_Members_Ptr
         := new My_Exception_Members'(info => arg);
      --  FIXME: introducing potential memory leak in server.
   begin
      Broca.Exceptions.User_Raise_Exception
        (My_Exception'Identity, Members.all);
   end testException;

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

   function echoUnion
     (Self : access Object;
      arg : in myUnion)
     return myUnion is
   begin
      return arg;
   end echoUnion;

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence)
     return U_Sequence
   is
   begin
      return arg;
   end echoUsequence;

   function echoBsequence
     (Self : access Object;
      arg : in B_sequence)
     return B_Sequence
   is
   begin
      return arg;
   end echoBsequence;

   procedure set_myColor
     (Self : access Object;
      arg : in Color)
   is
   begin
      Self.Attr_My_Color := arg;
   end set_myColor;

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

end all_types.Impl;
