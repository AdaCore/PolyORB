------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               D Y N A M I C A N Y . D Y N A N Y . I M P L                --
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

with CORBA.Local;
with CORBA.Object;
with PolyORB.Any;
with PolyORB.Utils.Chained_Lists;

package DynamicAny.DynAny.Impl is

   type Object is new CORBA.Local.Object with private;

   type Object_Ptr is access all Object'Class;

   function IDL_Type (Self : access Object) return CORBA.TypeCode.Object;

   procedure Assign
     (Self    : access Object;
      Dyn_Any : in     Local_Ref'Class);

   procedure From_Any
     (Self  : access Object;
      Value : in     CORBA.Any);

   function To_Any (Self : access Object) return CORBA.Any;

   function Equal
     (Self    : access Object;
      Dyn_Any : in     Local_Ref'Class)
      return CORBA.Boolean;

   procedure Destroy (Self : access Object);

   function Copy (Self : access Object) return Local_Ref'Class;

   procedure Insert_Boolean
     (Self  : access Object;
      Value : in     CORBA.Boolean);

   procedure Insert_Octet
     (Self  : access Object;
      Value : in     CORBA.Octet);

   procedure Insert_Char
     (Self  : access Object;
      Value : in     CORBA.Char);

   procedure Insert_Short
     (Self  : access Object;
      Value : in     CORBA.Short);

   procedure Insert_UShort
     (Self  : access Object;
      Value : in     CORBA.Unsigned_Short);

   procedure Insert_Long
     (Self  : access Object;
      Value : in     CORBA.Long);

   procedure Insert_ULong
     (Self  : access Object;
      Value : in     CORBA.Unsigned_Long);

   procedure Insert_Float
     (Self  : access Object;
      Value : in     CORBA.Float);

   procedure Insert_Double
     (Self  : access Object;
      Value : in     CORBA.Double);

   procedure Insert_String
     (Self  : access Object;
      Value : in     CORBA.String);

   procedure Insert_Reference
     (Self  : access Object;
      Value : in     CORBA.Object.Ref);

   procedure Insert_TypeCode
     (Self  : access Object;
      Value : in     CORBA.TypeCode.Object);

   procedure Insert_LongLong
     (Self  : access Object;
      Value : in     CORBA.Long_Long);

   procedure Insert_ULongLong
     (Self  : access Object;
      Value : in     CORBA.Unsigned_Long_Long);

   procedure Insert_LongDouble
     (Self  : access Object;
      Value : in     CORBA.Long_Double);

   procedure Insert_WChar
     (Self  : access Object;
      Value : in     CORBA.Wchar);

   procedure Insert_WString
     (Self  : access Object;
      Value : in     CORBA.Wide_String);

   procedure Insert_Any
     (Self  : access Object;
      Value : in     CORBA.Any);

   procedure Insert_Dyn_Any
     (Self  : access Object;
      Value : in     Local_Ref'Class);

   function Get_Boolean (Self : access Object) return CORBA.Boolean;

   function Get_Octet (Self : access Object) return CORBA.Octet;

   function Get_Char (Self : access Object) return CORBA.Char;

   function Get_Short (Self : access Object) return CORBA.Short;

   function Get_UShort (Self : access Object) return CORBA.Unsigned_Short;

   function Get_Long (Self : access Object) return CORBA.Long;

   function Get_ULong (Self : access Object) return CORBA.Unsigned_Long;

   function Get_Float (Self : access Object) return CORBA.Float;

   function Get_Double (Self : access Object) return CORBA.Double;

   function Get_String (Self : access Object) return CORBA.String;

   function Get_Reference (Self : access Object) return CORBA.Object.Ref;

   function Get_TypeCode (Self : access Object) return CORBA.TypeCode.Object;

   function Get_LongLong (Self : access Object) return CORBA.Long_Long;

   function Get_ULongLong (Self : access Object)
     return CORBA.Unsigned_Long_Long;

   function Get_LongDouble (Self : access Object) return CORBA.Long_Double;

   function Get_WChar (Self : access Object) return CORBA.Wchar;

   function Get_WString (Self : access Object) return CORBA.Wide_String;

   function Get_Any (Self : access Object) return CORBA.Any;

   function Get_Dyn_Any (Self : access Object) return Local_Ref'Class;

   function Seek
     (Self  : access Object;
      Index : in     CORBA.Long)
      return CORBA.Boolean;

   procedure Rewind (Self : access Object);

   function Next (Self : access Object) return CORBA.Boolean;

   function Component_Count (Self : access Object) return CORBA.Unsigned_Long;

   function Current_Component (Self : access Object) return Local_Ref'Class;

   procedure Insert_Abstract
     (Self  : access Object;
      Value : in     CORBA.AbstractBase.Ref);

   function Get_Abstract (Self : access Object) return CORBA.AbstractBase.Ref;

   procedure Insert_Boolean_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.BooleanSeq);

   procedure Insert_Octet_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.OctetSeq);

   procedure Insert_Char_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.CharSeq);

   procedure Insert_Short_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.ShortSeq);

   procedure Insert_UShort_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.UShortSeq);

   procedure Insert_Long_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.LongSeq);

   procedure Insert_ULong_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.ULongSeq);

   procedure Insert_Float_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.FloatSeq);

   procedure Insert_Double_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.DoubleSeq);

   procedure Insert_LongLong_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.LongLongSeq);

   procedure Insert_ULongLong_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.ULongLongSeq);

   procedure Insert_LongDouble_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.LongDoubleSeq);

   procedure Insert_WChar_Seq
     (Self  : access Object;
      Value : in     CORBA.IDL_SEQUENCES.WCharSeq);

   function Get_Boolean_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.BooleanSeq;

   function Get_Octet_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.OctetSeq;

   function Get_Char_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.CharSeq;

   function Get_Short_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.ShortSeq;

   function Get_UShort_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.UShortSeq;

   function Get_Long_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.LongSeq;

   function Get_ULong_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.ULongSeq;

   function Get_Float_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.FloatSeq;

   function Get_Double_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.DoubleSeq;

   function Get_LongLong_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.LongLongSeq;

   function Get_ULongLong_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.ULongLongSeq;

   function Get_LongDouble_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.LongDoubleSeq;

   function Get_WChar_Seq
     (Self : access Object)
      return CORBA.IDL_SEQUENCES.WCharSeq;

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : in     Standard.String)
      return Boolean;

   --  Implementation specific subprograms

   package Internals is

      function Is_Destroyed (Self : access Object'Class) return Boolean;
      --  Return True iff Self marked to be destroyed as part of top level
      --  dynamic any destruction.

      function Get_Value (Self : access Object'Class) return PolyORB.Any.Any;
      --  Return internal Value

      --  Constructors

      procedure Initialize
        (Self   : access Object'Class;
         Value  : in     PolyORB.Any.Any;
         Parent : in     Object_Ptr);
      --  Initialize DynAny object. If Parent is null (initialized object is
      --  a top level object) then independent copy of Value is created;
      --  otherwise value stored inside object.

      procedure Initialize
        (Self     : access Object'Class;
         IDL_Type : in     PolyORB.Any.TypeCode.Object);
      --  Initialize DynAny object and setup default value

      function Create
        (Value  : in PolyORB.Any.Any;
         Parent : in Object_Ptr)
         return Local_Ref;

      function Create
        (Value : in PolyORB.Any.TypeCode.Object)
         return DynAny.Local_Ref;

   end Internals;

private

   package Local_Ref_Lists is
     new PolyORB.Utils.Chained_Lists (Local_Ref);

   type Object is new CORBA.Local.Object with record
      Value        : PolyORB.Any.Any;
      Current      : CORBA.Long;
      Parent       : Object_Ptr;
      Children     : Local_Ref_Lists.List;
      Is_Destroyed : Boolean := False;
   end record;

   procedure Mark_Destroyed (Self : access Object);
   --  Mark DynAny and all it children are destroyed

   procedure Finalize (Self : in out Object);
   --  Destructor. Called automatically from Smart Pointers.

end DynamicAny.DynAny.Impl;
