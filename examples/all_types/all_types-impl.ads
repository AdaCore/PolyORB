------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       A L L _ T Y P E S . I M P L                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.16 $
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

with CORBA;
with PortableServer;

package all_types.Impl is
   --  My own implementation of echo object.
   --  This is simply used to define the operations.

   type Object is new PortableServer.Servant_Base with record
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
      arg : in all_types.Ref)
      return all_types.Ref;

   function echoColor
     (Self : access Object;
      arg  : in Color)
      return Color;

   function echoArray
     (Self : access Object;
      Arg : in simple_array)
      return simple_array;

   function echoMatrix
     (Self : access Object;
      arg : in matrix)
      return matrix;

   function echoBigMatrix
     (Self : access Object;
      arg : in bigmatrix)
      return bigmatrix;

   procedure testException
     (Self : access Object;
      arg : in CORBA.Long);

   procedure testUnknownException
     (Self : access Object;
      arg : in CORBA.Long);

   function echoStruct
     (Self : access Object;
      arg  : in simple_struct)
      return simple_struct;

   function echoArrayStruct
     (Self : access Object;
      arg  : in array_struct)
      return array_struct;

   function echoUnion
     (Self : access Object;
      arg : in myUnion)
     return myUnion;

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence)
     return U_sequence;

   function echoBsequence
     (Self : access Object;
      arg : in B_sequence)
     return B_sequence;

   function echoMoney
     (Self : access Object;
      Arg  : in Money)
     return Money;

   procedure set_MyColor
     (Self : access Object;
      arg : in Color);

   function get_myColor
     (Self : access Object)
     return Color;

   function get_Counter
     (Self : access Object)
     return CORBA.Long;

end all_types.Impl;
