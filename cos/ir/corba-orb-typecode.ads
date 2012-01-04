------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   C O R B A . O R B . T Y P E C O D E                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with CORBA.Repository_Root;

package CORBA.ORB.Typecode is

   function Create_Struct_Tc
     (Id      : CORBA.RepositoryId;
      Name    : CORBA.Identifier;
      Members : CORBA.Repository_Root.StructMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Union_Tc
     (Id                 : CORBA.RepositoryId;
      Name               : CORBA.Identifier;
      Discriminator_Type : CORBA.TypeCode.Object;
      Members            : CORBA.Repository_Root.UnionMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Enum_Tc
     (Id      : CORBA.RepositoryId;
      Name    : CORBA.Identifier;
      Members : CORBA.Repository_Root.EnumMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Alias_Tc
     (Id            : CORBA.RepositoryId;
      Name          : CORBA.Identifier;
      Original_Type : CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object;

   function Create_Exception_Tc
     (Id      : CORBA.RepositoryId;
      Name    : CORBA.Identifier;
      Members : CORBA.Repository_Root.StructMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Interface_Tc
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier)
      return CORBA.TypeCode.Object;

   function Create_String_Tc
     (Bound : CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object;

   function Create_Wstring_Tc
     (Bound : CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object;

   function Create_Fixed_Tc
     (IDL_Digits : CORBA.Unsigned_Short;
      Scale  : CORBA.Short)
      return CORBA.TypeCode.Object;

   function Create_Sequence_Tc
     (Bound       : CORBA.Unsigned_Long;
      Elementtype : CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object;

   function Create_Recursive_Sequence_Tc
     (Bound  : CORBA.Unsigned_Long;
      Offset : CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object;

   function Create_Array_Tc
     (Length       : CORBA.Unsigned_Long;
      Element_Type : CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object;

   function Create_Value_Tc
     (Id            : CORBA.RepositoryId;
      Name          : CORBA.Identifier;
      Type_Modifier : CORBA.ValueModifier;
      Concrete_Base : CORBA.TypeCode.Object;
      Members       : CORBA.Repository_Root.ValueMemberSeq)
      return CORBA.TypeCode.Object;

   function Create_Value_Box_Tc
     (Id         : CORBA.RepositoryId;
      Name       : CORBA.Identifier;
      Boxed_Type : CORBA.TypeCode.Object)
      return CORBA.TypeCode.Object;

   function Create_Native_Tc
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier)
      return CORBA.TypeCode.Object;

   function Create_Recursive_Tc
     (Id   : CORBA.RepositoryId)
      return CORBA.TypeCode.Object;

   function Create_Abstract_Interface_Tc
     (Id   : CORBA.RepositoryId;
      Name : CORBA.Identifier)
      return CORBA.TypeCode.Object;

end CORBA.ORB.Typecode;
