------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--          C O R B A . O R B . T Y P E C O D E _ C R E A T I O N           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package CORBA.ORB.Typecode_Creation is

   function Create_Struct_Tc
     (Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.StructMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Union_Tc
     (Id                 : in CORBA.RepositoryId;
      Name               : in CORBA.Identifier;
      Discriminator_Type : in CORBA.TypeCode.Object;
      Members            : in CORBA.UnionMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Enum_Tc
     (Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.EnumMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Alias_Tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object;

   function Create_Exception_Tc
     (Id      : in CORBA.RepositoryId;
      Name    : in CORBA.Identifier;
      Members : in CORBA.StructMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Interface_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object;

   function Create_String_Tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object;

   function Create_Wstring_Tc
     (Bound : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object;

   function Create_Fixed_Tc
     (IDL_Digits : in CORBA.Unsigned_Short;
      Scale  : in CORBA.Short)
      return CORBA.TypeCode.Object;

   function Create_Sequence_Tc
     (Bound       : in CORBA.Unsigned_Long;
      Elementtype : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object;

   function Create_Recursive_Sequence_Tc
     (Bound  : in CORBA.Unsigned_Long;
      Offset : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object;

   function Create_Array_Tc
     (Length       : in CORBA.Unsigned_Long;
      Element_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object;

   function Create_Value_Tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Type_Modifier : in CORBA.ValueModifier;
      Concrete_Base : in CORBA.TypeCode.Object;
      Members       : in CORBA.ValueMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Value_Box_Tc
     (Id         : in CORBA.RepositoryId;
      Name       : in CORBA.Identifier;
      Boxed_Type : in CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object;

   function Create_Native_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object;

   function Create_Recursive_Tc
     (Id   : in CORBA.RepositoryId)
      return CORBA.TypeCode.Object;

   function Create_Abstract_Interface_Tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
      return CORBA.TypeCode.Object;

end CORBA.ORB.Typecode_Creation;
