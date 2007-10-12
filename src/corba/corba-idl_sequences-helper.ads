------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C O R B A . I D L _ S E Q U E N C E S . H E L P E R            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

with PolyORB.Any;

with CORBA;
pragma Elaborate_All (CORBA);

package CORBA.IDL_SEQUENCES.Helper is

   --  AnySeq sequence

   TC_IDL_SEQUENCE_Any : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Any.Sequence;

   function To_Any (Item : IDL_SEQUENCE_Any.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Any.Sequence)
     return PolyORB.Any.Content'Class;

   TC_AnySeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return AnySeq;
   function To_Any (Item : AnySeq) return CORBA.Any;

   --  BooleanSeq sequence

   TC_IDL_SEQUENCE_Boolean : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Boolean.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Boolean.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Boolean.Sequence)
     return PolyORB.Any.Content'Class;

   TC_BooleanSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return BooleanSeq;
   function To_Any (Item : BooleanSeq) return CORBA.Any;

   --  CharSeq sequence

   TC_IDL_SEQUENCE_Char : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Char.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Char.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Char.Sequence)
     return PolyORB.Any.Content'Class;

   TC_CharSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return CharSeq;
   function To_Any (Item : CharSeq) return CORBA.Any;

   --  WCharSeq sequence

   TC_IDL_SEQUENCE_Wide_Char : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Wide_Char.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Wide_Char.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Wide_Char.Sequence)
     return PolyORB.Any.Content'Class;

   TC_WCharSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return WCharSeq;
   function To_Any (Item : WCharSeq) return CORBA.Any;

   --  Octet sequence

   TC_IDL_SEQUENCE_Octet : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Octet.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Octet.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Octet.Sequence)
     return PolyORB.Any.Content'Class;

   TC_OctetSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return OctetSeq;
   function To_Any (Item : OctetSeq) return CORBA.Any;

   --  ShortSeq sequence

   TC_IDL_SEQUENCE_Short : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Short.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Short.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Short.Sequence)
     return PolyORB.Any.Content'Class;

   TC_ShortSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return ShortSeq;
   function To_Any (Item : ShortSeq) return CORBA.Any;

   --  UShortSeq sequence

   TC_IDL_SEQUENCE_Unsigned_Short : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Unsigned_Short.Sequence;
   function To_Any
     (Item : IDL_SEQUENCE_Unsigned_Short.Sequence)
     return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Unsigned_Short.Sequence)
     return PolyORB.Any.Content'Class;

   TC_UShortSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return UShortSeq;
   function To_Any (Item : UShortSeq) return CORBA.Any;

   --  LongSeq sequence

   TC_IDL_SEQUENCE_Long : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Long.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Long.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Long.Sequence)
     return PolyORB.Any.Content'Class;

   TC_LongSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return LongSeq;
   function To_Any (Item : LongSeq) return CORBA.Any;

   --  ULongSeq sequence

   TC_IDL_SEQUENCE_Unsigned_Long : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Unsigned_Long.Sequence;
   function To_Any
     (Item : IDL_SEQUENCE_Unsigned_Long.Sequence)
     return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Unsigned_Long.Sequence)
     return PolyORB.Any.Content'Class;

   TC_ULongSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return ULongSeq;
   function To_Any (Item : ULongSeq) return CORBA.Any;

   --  LongLongSeq sequence

   TC_IDL_SEQUENCE_Long_Long : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Long_Long.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Long_Long.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Long_Long.Sequence)
     return PolyORB.Any.Content'Class;

   TC_LongLongSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return LongLongSeq;
   function To_Any (Item : LongLongSeq) return CORBA.Any;

   --  UnsignedLongLongSeq sequence

   TC_IDL_SEQUENCE_Unsigned_Long_Long : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Unsigned_Long_Long.Sequence;
   function To_Any
     (Item : IDL_SEQUENCE_Unsigned_Long_Long.Sequence)
     return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Unsigned_Long_Long.Sequence)
     return PolyORB.Any.Content'Class;

   TC_ULongLongSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return ULongLongSeq;
   function To_Any (Item : ULongLongSeq) return CORBA.Any;

   --  FloatSeq sequence

   TC_IDL_SEQUENCE_Float : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Float.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Float.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Float.Sequence)
     return PolyORB.Any.Content'Class;

   TC_FloatSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return FloatSeq;
   function To_Any (Item : FloatSeq) return CORBA.Any;

   --  DoubleSeq sequence

   TC_IDL_SEQUENCE_Double : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Double.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Double.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Double.Sequence)
     return PolyORB.Any.Content'Class;

   TC_DoubleSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return DoubleSeq;
   function To_Any (Item : DoubleSeq) return CORBA.Any;

   --  LongDoubleSeq sequence

   TC_IDL_SEQUENCE_Long_Double : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Long_Double.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Long_Double.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Long_Double.Sequence)
     return PolyORB.Any.Content'Class;

   TC_LongDoubleSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return LongDoubleSeq;
   function To_Any (Item : LongDoubleSeq) return CORBA.Any;

   --  StringSeq sequence

   TC_IDL_SEQUENCE_String : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_String.Sequence;
   function To_Any (Item : IDL_SEQUENCE_String.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_String.Sequence)
     return PolyORB.Any.Content'Class;

   TC_StringSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return StringSeq;
   function To_Any (Item : StringSeq) return CORBA.Any;

   --  WStringSeq sequence

   TC_IDL_SEQUENCE_Wide_String : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
      return IDL_SEQUENCE_Wide_String.Sequence;
   function To_Any (Item : IDL_SEQUENCE_Wide_String.Sequence) return CORBA.Any;

   function Wrap
     (X : access IDL_SEQUENCE_Wide_String.Sequence)
     return PolyORB.Any.Content'Class;

   TC_WStringSeq : CORBA.TypeCode.Object;

   function From_Any (Item : CORBA.Any) return WStringSeq;
   function To_Any (Item : WStringSeq) return CORBA.Any;

end CORBA.IDL_SEQUENCES.Helper;
