------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       A L L _ T Y P E S . I M P L                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.15 $
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

with All_Types.Skel;
--  with All_Types_IDL_FILE;
with CORBA;

package all_types.Impl is
   --  My own implementation of echo object.
   --  This is simply used to define the operations.

   type Object is new All_Types.Skel.Object with private;

   type Object_Acc is access all Object;

private

   type Object is new All_Types.Skel.Object with record
      Attr_My_Color : Color := Blue;
      Attr_Counter  : CORBA.Long := 0;
   end record;

   function echoBoolean
     (Self : access Object;
      arg : in CORBA.Boolean)
      return CORBA.Boolean;

   function echoShort
     (Self : access Object;
      arg : in CORBA.Short)
      return CORBA.Short;

   function echoLong
     (Self : access Object;
      arg : in CORBA.Long)
      return CORBA.Long;

   function echoUShort
     (Self : access Object;
      arg : in CORBA.Unsigned_Short)
      return CORBA.Unsigned_Short;

   function echoULong
     (Self : access Object;
      arg : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;

   function echoFloat
     (Self : access Object;
      arg : in CORBA.Float)
      return CORBA.Float;

   function echoDouble
     (Self : access Object;
      arg : in CORBA.Double)
      return CORBA.Double;

   function echoChar
     (Self : access Object;
      arg : in CORBA.Char)
      return CORBA.Char;

   function echoOctet
     (Self : access Object;
      arg : in CORBA.Octet)
      return CORBA.Octet;

   function echoString
     (Self : access Object;
      arg : in CORBA.String)
      return CORBA.String;

   function echoRef
     (Self : access Object;
      arg : in All_Types.Ref)
      return All_Types.Ref;

   function echoColor
     (Self : access Object;
      arg  : in Color)
      return Color;

   function echoArray
     (Self : access Object;
      Arg : in Simple_Array)
      return Simple_Array;

   function echoMatrix
     (Self : access Object;
      arg : in matrix)
      return matrix;

   procedure testException
     (Self : access Object;
      arg : in CORBA.Long);

   function echoStruct
     (Self : access Object;
      arg  : in Simple_Struct)
      return Simple_Struct;


   function echoUnion
     (Self : access Object;
      arg : in myUnion)
     return myUnion;

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence)
      return U_sequence;

   procedure Set_MyColor
     (Self : access Object;
      arg : in Color);

   function Get_MyColor
     (Self : access Object)
     return Color;

   function Get_Counter
     (Self : access Object)
     return CORBA.Long;

--   procedure simple_exception_test
--     (Self : access Object);
--
--   procedure complexe_exception_test
--     (Self : access Object);
--
--   function echo1
--     (Self : access Object;
--      arg : in example)
--      return example;
--
--   function echo2
--     (Self : access Object;
--      arg : in simple_struct)
--      return simple_struct;
--
--   function InverseStruct
--     (Self : access Object;
--      Arg : in Manu_Struct)
--      return Manu_Struct;
--
--
--   function echo3
--     (Self : access Object;
--      arg : in Color)
--      return Color;
--
--   function echo4
--     (Self : access Object;
--      arg : in U_string)
--      return U_string;
--
--   function echo6
--     (Self : access Object;
--      arg : in U_sequence)
--      return U_sequence;
--
--   function echo7
--     (Self : access Object;
--      arg : in B_sequence)
--      return B_sequence;
--
--   function Get_R_attribute
--     (Self : access Object)
--      return Color;
--
--   function Get_N_attribute
--     (Self : access Object)
--      return example;
--
--   procedure Set_N_attribute
--     (Self : access Object;
--      To   : in example);
--
--   function echo8
--     (Self : access Object;
--      arg : in line)
--      return line;
--
--   function echo9
--     (Self : access Object;
--      arg : in square)
--      return square;
--
--   function echo10
--     (Self : access Object;
--      arg : in cube)
--      return cube;
--
--   function echo11
--     (Self : access Object;
--      arg : in Ref)
--      return Ref;
--
--   function echo12
--     (Self : access Object;
--      arg : in CORBA.Object.Ref)
--      return CORBA.Object.Ref;
--
--   function get_myself
--     (Self : access Object)
--      return Ref;
--

end all_types.Impl;
