------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    D Y N A M I C A N Y . D Y N A N Y                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with CORBA.AbstractBase;
with CORBA.IDL_SEQUENCES;
with CORBA.Object;

package DynamicAny.DynAny is

   type Local_Ref is new CORBA.Object.Ref with null record;

   --  InvalidValue exception

   InvalidValue : exception;

   type InvalidValue_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidValue_Members);

   --  TypeMismatch exception

   TypeMismatch : exception;

   type TypeMismatch_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out TypeMismatch_Members);

   function IDL_Type (Self : Local_Ref) return CORBA.TypeCode.Object;

   procedure Assign
     (Self    : Local_Ref;
      Dyn_Any : Local_Ref'Class);

   procedure From_Any
     (Self  : Local_Ref;
      Value : CORBA.Any);

   function To_Any (Self : Local_Ref) return CORBA.Any;

   function Equal
     (Self    : Local_Ref;
      Dyn_Any : Local_Ref'Class)
      return CORBA.Boolean;

   procedure Destroy (Self : Local_Ref);

   function Copy (Self : Local_Ref) return Local_Ref'Class;

   procedure Insert_Boolean
     (Self  : Local_Ref;
      Value : CORBA.Boolean);

   procedure Insert_Octet
     (Self  : Local_Ref;
      Value : CORBA.Octet);

   procedure Insert_Char
     (Self  : Local_Ref;
      Value : CORBA.Char);

   procedure Insert_Short
     (Self  : Local_Ref;
      Value : CORBA.Short);

   procedure Insert_UShort
     (Self  : Local_Ref;
      Value : CORBA.Unsigned_Short);

   procedure Insert_Long
     (Self  : Local_Ref;
      Value : CORBA.Long);

   procedure Insert_ULong
     (Self  : Local_Ref;
      Value : CORBA.Unsigned_Long);

   procedure Insert_Float
     (Self  : Local_Ref;
      Value : CORBA.Float);

   procedure Insert_Double
     (Self  : Local_Ref;
      Value : CORBA.Double);

   procedure Insert_String
     (Self  : Local_Ref;
      Value : CORBA.String);

   procedure Insert_Reference
     (Self  : Local_Ref;
      Value : CORBA.Object.Ref);

   procedure Insert_TypeCode
     (Self  : Local_Ref;
      Value : CORBA.TypeCode.Object);

   procedure Insert_LongLong
     (Self  : Local_Ref;
      Value : CORBA.Long_Long);

   procedure Insert_ULongLong
     (Self  : Local_Ref;
      Value : CORBA.Unsigned_Long_Long);

   procedure Insert_LongDouble
     (Self  : Local_Ref;
      Value : CORBA.Long_Double);

   procedure Insert_WChar
     (Self  : Local_Ref;
      Value : CORBA.Wchar);

   procedure Insert_WString
     (Self  : Local_Ref;
      Value : CORBA.Wide_String);

   procedure Insert_Any
     (Self  : Local_Ref;
      Value : CORBA.Any);

   procedure Insert_Dyn_Any
     (Self  : Local_Ref;
      Value : DynAny.Local_Ref'Class);

   function Get_Boolean (Self : Local_Ref) return CORBA.Boolean;

   function Get_Octet (Self : Local_Ref) return CORBA.Octet;

   function Get_Char (Self : Local_Ref) return CORBA.Char;

   function Get_Short (Self : Local_Ref) return CORBA.Short;

   function Get_UShort (Self : Local_Ref) return CORBA.Unsigned_Short;

   function Get_Long (Self : Local_Ref) return CORBA.Long;

   function Get_ULong (Self : Local_Ref) return CORBA.Unsigned_Long;

   function Get_Float (Self : Local_Ref) return CORBA.Float;

   function Get_Double (Self : Local_Ref) return CORBA.Double;

   function Get_String (Self : Local_Ref) return CORBA.String;

   function Get_Reference (Self : Local_Ref) return CORBA.Object.Ref;

   function Get_TypeCode (Self : Local_Ref) return CORBA.TypeCode.Object;

   function Get_LongLong (Self : Local_Ref) return CORBA.Long_Long;

   function Get_ULongLong (Self : Local_Ref)
     return CORBA.Unsigned_Long_Long;

   function Get_LongDouble (Self : Local_Ref) return CORBA.Long_Double;

   function Get_WChar (Self : Local_Ref) return CORBA.Wchar;

   function Get_WString (Self : Local_Ref) return CORBA.Wide_String;

   function Get_Any (Self : Local_Ref) return CORBA.Any;

   function Get_Dyn_Any (Self : Local_Ref) return Local_Ref'Class;

   function Seek
     (Self  : Local_Ref;
      Index : CORBA.Long)
      return CORBA.Boolean;

   procedure Rewind (Self : Local_Ref);

   function Next (Self : Local_Ref) return CORBA.Boolean;

   function Component_Count (Self : Local_Ref) return CORBA.Unsigned_Long;

   function Current_Component (Self : Local_Ref) return Local_Ref'Class;

   procedure Insert_Abstract
     (Self  : Local_Ref;
      Value : CORBA.AbstractBase.Ref);

   function Get_Abstract (Self : Local_Ref) return CORBA.AbstractBase.Ref;

   procedure Insert_Boolean_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.BooleanSeq);

   procedure Insert_Octet_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.OctetSeq);

   procedure Insert_Char_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.CharSeq);

   procedure Insert_Short_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.ShortSeq);

   procedure Insert_UShort_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.UShortSeq);

   procedure Insert_Long_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.LongSeq);

   procedure Insert_ULong_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.ULongSeq);

   procedure Insert_Float_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.FloatSeq);

   procedure Insert_Double_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.DoubleSeq);

   procedure Insert_LongLong_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.LongLongSeq);

   procedure Insert_ULongLong_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.ULongLongSeq);

   procedure Insert_LongDouble_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.LongDoubleSeq);

   procedure Insert_WChar_Seq
     (Self  : Local_Ref;
      Value : CORBA.IDL_SEQUENCES.WCharSeq);

   function Get_Boolean_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.BooleanSeq;

   function Get_Octet_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.OctetSeq;

   function Get_Char_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.CharSeq;

   function Get_Short_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.ShortSeq;

   function Get_UShort_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.UShortSeq;

   function Get_Long_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.LongSeq;

   function Get_ULong_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.ULongSeq;

   function Get_Float_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.FloatSeq;

   function Get_Double_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.DoubleSeq;

   function Get_LongLong_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.LongLongSeq;

   function Get_ULongLong_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.ULongLongSeq;

   function Get_LongDouble_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.LongDoubleSeq;

   function Get_WChar_Seq
     (Self : Local_Ref)
      return CORBA.IDL_SEQUENCES.WCharSeq;

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
