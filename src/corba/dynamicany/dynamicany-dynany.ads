------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    D Y N A M I C A N Y . D Y N A N Y                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with Ada.Exceptions;

with CORBA.AbstractBase;
with CORBA.IDL_Sequences;
with CORBA.Object;

package DynamicAny.DynAny is

   type Local_Ref is new CORBA.Object.Ref with null record;

   --  InvalidValue exception

   InvalidValue : exception;

   type InvalidValue_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidValue_Members);

   --  TypeMismatch exception

   TypeMismatch : exception;

   type TypeMismatch_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : in     Ada.Exceptions.Exception_Occurrence;
      To   :    out TypeMismatch_Members);

   function IDL_Type (Self : in Local_Ref) return CORBA.TypeCode.Object;

   procedure Assign
     (Self    : in Local_Ref;
      Dyn_Any : in Local_Ref'Class);

   procedure From_Any
     (Self  : in Local_Ref;
      Value : in CORBA.Any);

   function To_Any (Self : in Local_Ref) return CORBA.Any;

   function Equal
     (Self    : in Local_Ref;
      Dyn_Any : in Local_Ref'Class)
      return CORBA.Boolean;

   procedure Destroy (Self : in Local_Ref);

   function Copy (Self : in Local_Ref) return Local_Ref'Class;

   procedure Insert_Boolean
     (Self  : in Local_Ref;
      Value : in CORBA.Boolean);

   procedure Insert_Octet
     (Self  : in Local_Ref;
      Value : in CORBA.Octet);

   procedure Insert_Char
     (Self  : in Local_Ref;
      Value : in CORBA.Char);

   procedure Insert_Short
     (Self  : in Local_Ref;
      Value : in CORBA.Short);

   procedure Insert_UShort
     (Self  : in Local_Ref;
      Value : in CORBA.Unsigned_Short);

   procedure Insert_Long
     (Self  : in Local_Ref;
      Value : in CORBA.Long);

   procedure Insert_ULong
     (Self  : in Local_Ref;
      Value : in CORBA.Unsigned_Long);

   procedure Insert_Float
     (Self  : in Local_Ref;
      Value : in CORBA.Float);

   procedure Insert_Double
     (Self  : in Local_Ref;
      Value : in CORBA.Double);

   procedure Insert_String
     (Self  : in Local_Ref;
      Value : in CORBA.String);

   procedure Insert_Reference
     (Self  : in Local_Ref;
      Value : in CORBA.Object.Ref);

   procedure Insert_TypeCode
     (Self  : in Local_Ref;
      Value : in CORBA.TypeCode.Object);

   procedure Insert_LongLong
     (Self  : in Local_Ref;
      Value : in CORBA.Long_Long);

   procedure Insert_ULongLong
     (Self  : in Local_Ref;
      Value : in CORBA.Unsigned_Long_Long);

   procedure Insert_LongDouble
     (Self  : in Local_Ref;
      Value : in CORBA.Long_Double);

   procedure Insert_WChar
     (Self  : in Local_Ref;
      Value : in CORBA.Wchar);

   procedure Insert_WString
     (Self  : in Local_Ref;
      Value : in CORBA.Wide_String);

   procedure Insert_Any
     (Self  : in Local_Ref;
      Value : in CORBA.Any);

   procedure Insert_Dyn_Any
     (Self  : in Local_Ref;
      Value : in DynAny.Local_Ref'Class);

   function Get_Boolean (Self : in Local_Ref) return CORBA.Boolean;

   function Get_Octet (Self : in Local_Ref) return CORBA.Octet;

   function Get_Char (Self : in Local_Ref) return CORBA.Char;

   function Get_Short (Self : in Local_Ref) return CORBA.Short;

   function Get_UShort (Self : in Local_Ref) return CORBA.Unsigned_Short;

   function Get_Long (Self : in Local_Ref) return CORBA.Long;

   function Get_ULong (Self : in Local_Ref) return CORBA.Unsigned_Long;

   function Get_Float (Self : in Local_Ref) return CORBA.Float;

   function Get_Double (Self : in Local_Ref) return CORBA.Double;

   function Get_String (Self : in Local_Ref) return CORBA.String;

   function Get_Reference (Self : in Local_Ref) return CORBA.Object.Ref;

   function Get_TypeCode (Self : in Local_Ref) return CORBA.TypeCode.Object;

   function Get_LongLong (Self : in Local_Ref) return CORBA.Long_Long;

   function Get_ULongLong (Self : in Local_Ref)
     return CORBA.Unsigned_Long_Long;

   function Get_LongDouble (Self : in Local_Ref) return CORBA.Long_Double;

   function Get_WChar (Self : in Local_Ref) return CORBA.Wchar;

   function Get_WString (Self : in Local_Ref) return CORBA.Wide_String;

   function Get_Any (Self : in Local_Ref) return CORBA.Any;

   function Get_Dyn_Any (Self : in Local_Ref) return Local_Ref'Class;

   function Seek
     (Self  : in Local_Ref;
      Index : in CORBA.Long)
      return CORBA.Boolean;

   procedure Rewind (Self : in Local_Ref);

   function Next (Self : in Local_Ref) return CORBA.Boolean;

   function Component_Count (Self : in Local_Ref) return CORBA.Unsigned_Long;

   function Current_Component (Self : in Local_Ref) return Local_Ref'Class;

   procedure Insert_Abstract
     (Self  : in Local_Ref;
      Value : in CORBA.AbstractBase.Ref);

   function Get_Abstract (Self : in Local_Ref) return CORBA.AbstractBase.Ref;

   procedure Insert_Boolean_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.BooleanSeq);

   procedure Insert_Octet_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.OctetSeq);

   procedure Insert_Char_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.CharSeq);

   procedure Insert_Short_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.ShortSeq);

   procedure Insert_UShort_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.UShortSeq);

   procedure Insert_Long_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.LongSeq);

   procedure Insert_ULong_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.ULongSeq);

   procedure Insert_Float_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.FloatSeq);

   procedure Insert_Double_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.DoubleSeq);

   procedure Insert_LongLong_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.LongLongSeq);

   procedure Insert_ULongLong_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.ULongLongSeq);

   procedure Insert_LongDouble_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.LongDoubleSeq);

   procedure Insert_WChar_Seq
     (Self  : in Local_Ref;
      Value : in CORBA.IDL_Sequences.WCharSeq);

   function Get_Boolean_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.BooleanSeq;

   function Get_Octet_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.OctetSeq;

   function Get_Char_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.CharSeq;

   function Get_Short_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.ShortSeq;

   function Get_UShort_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.UShortSeq;

   function Get_Long_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.LongSeq;

   function Get_ULong_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.ULongSeq;

   function Get_Float_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.FloatSeq;

   function Get_Double_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.DoubleSeq;

   function Get_LongLong_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.LongLongSeq;

   function Get_ULongLong_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.ULongLongSeq;

   function Get_LongDouble_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.LongDoubleSeq;

   function Get_WChar_Seq
     (Self : in Local_Ref)
      return CORBA.IDL_Sequences.WCharSeq;

   --  Convert_Forward type

   package Convert_Forward is new DynAny_Forward.Convert (Local_Ref);

   --  Repository Ids

   Repository_Id                       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny:1.0";

   Assign_Repository_Id                : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/assign:1.0";

   Component_Count_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/component_count:1.0";

   Copy_Repository_Id                  : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/copy:1.0";

   Current_Component_Repository_Id     : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/current_component:1.0";

   Destroy_Repository_Id               : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/destroy:1.0";

   Equal_Repository_Id                 : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/equal:1.0";

   From_Any_Repository_Id              : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/from_any:1.0";

   Get_Abstract_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_abstract:1.0";

   Get_Any_Repository_Id               : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_any:1.0";

   Get_Boolean_Repository_Id           : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_boolean:1.0";

   Get_Boolean_Seq_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_boolean_seq:1.0";

   Get_Char_Repository_Id              : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_char:1.0";

   Get_Char_Seq_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_char_seq:1.0";

   Get_Double_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_double:1.0";

   Get_Double_Seq_Repository_Id        : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_double_seq:1.0";

   Get_Dyn_Any_Repository_Id           : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_dyn_any:1.0";

   Get_Float_Repository_Id             : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_float:1.0";

   Get_Float_Seq_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_float_seq:1.0";

   Get_Long_Repository_Id              : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_long:1.0";

   Get_Long_Seq_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_long_seq:1.0";

   Get_LongDouble_Repository_Id        : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_longdouble:1.0";

   Get_LongDouble_Seq_Repository_Id    : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_longdouble_seq:1.0";

   Get_LongLong_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_longlong:1.0";

   Get_Longlong_Seq_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_longlong_seq:1.0";

   Get_Octet_Repository_Id             : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_octet:1.0";

   Get_Octet_Seq_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_octet_seq:1.0";

   Get_Reference_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_reference:1.0";

   Get_Short_Repository_Id             : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_short:1.0";

   Get_Short_Seq_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_short_seq:1.0";

   Get_String_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_string:1.0";

   Get_TypeCode_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_typecode:1.0";

   Get_ULong_Repository_Id             : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_ulong:1.0";

   Get_ULong_Seq_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_ulong_seq:1.0";

   Get_ULongLong_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_ulonglong:1.0";

   Get_ULongLong_Seq_Repository_Id     : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_ulonglong_seq:1.0";

   Get_UShort_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_ushort:1.0";

   Get_UShort_Seq_Repository_Id        : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_ushort_seq:1.0";

   Get_WChar_Repository_Id             : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_wchar:1.0";

   Get_WChar_Seq_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_wchar_seq:1.0";

   Get_WString_Repository_Id           : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/get_wstring:1.0";

   IDL_Type_Repository_Id              : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/type:1.0";

   Insert_Abstract_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_abstract:1.0";

   Insert_Any_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_any:1.0";

   Insert_Boolean_Repository_Id        : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_boolean:1.0";

   Insert_Boolean_Seq_Repository_Id    : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_boolean_seq:1.0";

   Insert_Char_Repository_Id           : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_char:1.0";

   Insert_Char_Seq_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_char_seq:1.0";

   Insert_Double_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_double:1.0";

   Insert_Double_Seq_Repository_Id     : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_double_seq:1.0";

   Insert_Dyn_Any_Repository_Id        : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_dyn_any:1.0";

   Insert_Float_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_float:1.0";

   Insert_Float_Seq_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_float_seq:1.0";

   Insert_Long_Repository_Id           : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_long:1.0";

   Insert_Long_Seq_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_long_seq:1.0";

   Insert_LongDouble_Repository_Id     : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_longdouble:1.0";

   Insert_LongDouble_Seq_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_longdouble_seq:1.0";

   Insert_LongLong_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_longlong:1.0";

   Insert_LongLong_Seq_Repository_Id   : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_longlong_seq:1.0";

   Insert_Octet_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_octet:1.0";

   Insert_Octet_Seq_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_octet_seq:1.0";

   Insert_Reference_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_reference:1.0";

   Insert_Short_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_short:1.0";

   Insert_Short_Seq_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_short_seq:1.0";

   Insert_String_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_string:1.0";

   Insert_TypeCode_Repository_Id       : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_typecode:1.0";

   Insert_ULong_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_ulong:1.0";

   Insert_ULong_Seq_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_ulong_seq:1.0";

   Insert_ULongLong_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_ulonglong:1.0";

   Insert_ULongLong_Seq_Repository_Id  : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_ulonglong_seq:1.0";

   Insert_UShort_Repository_Id         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_ushort:1.0";

   Insert_UShort_Seq_Repository_Id     : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_ushort_seq:1.0";

   Insert_WChar_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_wchar:1.0";

   Insert_WChar_Seq_Repository_Id      : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_wchar_seq:1.0";

   Insert_WString_Repository_Id        : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/insert_wstring:1.0";

   InvalidValue_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/InvalidValue:1.0";

   Next_Repository_Id                  : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/next:1.0";

   Rewind_Repository_Id                : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/rewind:1.0";

   Seek_Repository_Id                  : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/seek:1.0";

   To_Any_Repository_Id                : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/to_any:1.0";

   TypeMismatch_Repository_Id          : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAny/TypeMismatch:1.0";

end DynamicAny.DynAny;
