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

--  with Broca.Exceptions; use Broca.Exceptions;
with all_types.Skel;
pragma Elaborate (all_types.Skel);
pragma Warnings (Off, all_types.Skel);

package body all_types.Impl is

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

--     procedure testException
--       (Self : access Object;
--        arg : in CORBA.Long)
--     is
--        Members : IDL_Exception_Members_Ptr
--           := new My_Exception_Members'(Info => arg);
--        --  FIXME: introducing potential memory leak in server.
--     begin
--        Broca.Exceptions.User_Raise_Exception
--          (My_Exception'Identity, Members);
--     end testException;
   procedure testException
     (Self : access Object;
      arg : in CORBA.Long)
   is
   begin
      return;
   end testException;

   function echoStruct
     (Self : access Object;
      arg  : in simple_struct)
      return simple_struct is
   begin
      return arg;
   end echoStruct;

   function echoUnion
     (Self : access Object;
      arg : in myUnion)
     return myUnion is
   begin
      return arg;
   end echoUnion;

--     function echoUsequence
--       (Self : access Object;
--        arg : in U_sequence)
--       return U_Sequence
--     is
--     begin
--        return arg;
--     end echoUsequence;

   procedure Set_MyColor
     (Self : access Object;
      arg : in Color)
   is
   begin
      Self.Attr_My_Color := arg;
   end Set_MyColor;

   function Get_MyColor
     (Self : access Object)
     return Color
   is
   begin
      return Self.Attr_My_Color;
   end Get_MyColor;

   function Get_Counter
     (Self : access Object)
     return CORBA.Long
   is
      use CORBA;
   begin
      Self.Attr_Counter := Self.Attr_Counter + 1;
      return Self.Attr_Counter;
   end Get_Counter;

--   procedure simple_exception_test
--     (Self : access Object)
--   is
--   begin
--      raise simple_exception;
--   end simple_exception_test;
--
--   procedure complexe_exception_test
--     (Self : access Object)
--   is
--      Member : Complexe_Exception_Members;
--   begin
--      Member.Excep := 21;
--      AdaBroker.Exceptions.Raise_Corba_Exception
--        (Complexe_Exception'Identity, Member);
--   end complexe_exception_test;
--
--   function echo1
--     (Self : access Object;
--      arg : in example)
--      return example
--   is
--   begin
--      return arg;
--   end echo1;
--
--   function echo2
--     (Self : access Object;
--      arg : in simple_struct)
--      return simple_struct
--   is
--   begin
--      return arg;
--   end echo2;
--
--   function InverseStruct
--     (Self : access Object;
--      arg : in Manu_Struct)
--      return Manu_Struct
--   is
--      Res : Manu_Struct;
--   begin
--      Res.A := not arg.A;
--      Res.B := - arg.B;
--      return Res;
--   end InverseStruct;
--
--   function echo3
--     (Self : access Object;
--      arg : in Color)
--      return Color
--   is
--   begin
--      return arg;
--   end echo3;
--
--   function echo4
--     (Self : access Object;
--      arg : in U_string)
--      return U_string
--   is
--   begin
--      return arg;
--   end echo4;
--
--   function echo6
--     (Self : access Object;
--      arg : in U_sequence)
--      return U_sequence
--   is
--   begin
--      return arg;
--   end echo6;
--
--   function echo7
--     (Self : access Object;
--      arg : in B_sequence)
--      return B_sequence
--   is
--   begin
--      return arg;
--   end echo7;
--
--   function Get_R_attribute
--     (Self : access Object)
--      return Color
--   is
--   begin
--      return Self.all.Pd_Col;
--   end Get_R_attribute;
--
--   function Get_N_attribute
--     (Self : access Object)
--      return example
--   is
--   begin
--      return Self.all.Pd_Ex;
--   end Get_N_attribute;
--
--   procedure Set_N_attribute
--     (Self : access Object;
--      To   : in example)
--   is
--   begin
--      Self.all.Pd_Ex := To;
--   end Set_N_attribute;
--
--   function echo8
--     (Self : access Object;
--      arg : in line)
--      return line
--   is
--   begin
--      return arg;
--   end echo8;
--
--   function echo9
--     (Self : access Object;
--      arg : in square)
--      return square
--   is
--   begin
--      return arg;
--   end echo9;
--
--   function echo10
--     (Self : access Object;
--      arg : in cube)
--      return cube
--   is
--   begin
--      return arg;
--   end echo10;
--
--   function echo11
--     (Self : access Object;
--      arg : in Ref)
--      return Ref
--   is
--   begin
--      return arg;
--   end echo11;
--
--   function echo12
--     (Self : access Object;
--      arg : in CORBA.Object.Ref)
--      return CORBA.Object.Ref
--   is
--   begin
--      return arg;
--   end echo12;
--
--   function get_myself
--     (Self : access Object)
--      return Ref
--   is
--      Result : Ref;
--   begin
--      CORBA.Object.Ref (Result) :=
--         CORBA.Object.OmniORB.To_Ref (Self.all, Repository_Id);
--      return Result;
--   end get_myself;
--
end all_types.Impl;
