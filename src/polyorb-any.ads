------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . A N Y                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/polyorb-any.ads#23 $

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with PolyORB.Locks;
with PolyORB.Types;

package PolyORB.Any is

   pragma Elaborate_Body;

   -----------
   --  Any  --
   -----------

   type Any is private;
   type Any_Ptr is access all Any;
   --  The end of this part is after the typecode part;

   type Content is abstract tagged null record;
   type Any_Content_Ptr is access all Content'Class;

   function Duplicate
     (Object : access Content)
     return Any_Content_Ptr is abstract;
   --  Duplicate the data pointed by Object, making a deep copy.

   procedure Deallocate (Object : access Content);
   --  Deallocate an Any_Content_Ptr.
   --  Overridden for aggregates, since those have to
   --  deallocate all the list of their elements.

   function Image (A : Any) return Standard.String;
   --  For debugging purposes.

   ---------------
   -- TypeCodes --
   ---------------

   --  See spec CORBA V2.3, Ada Langage Mapping 1.33

   type TCKind is
      (Tk_Null,
       Tk_Void,
       Tk_Short,
       Tk_Long,
       Tk_Ushort,
       Tk_Ulong,
       Tk_Float,
       Tk_Double,
       Tk_Boolean,
       Tk_Char,
       Tk_Octet,
       Tk_Any,
       Tk_TypeCode,
       Tk_Principal,
       Tk_Objref,
       Tk_Struct,
       Tk_Union,
       Tk_Enum,
       Tk_String,
       Tk_Sequence,
       Tk_Array,
       Tk_Alias,
       Tk_Except,
       Tk_Longlong,
       Tk_Ulonglong,
       Tk_Longdouble,
       Tk_Widechar,
       Tk_Wstring,
       Tk_Fixed,
       Tk_Value,
       Tk_Valuebox,
       Tk_Native,
       Tk_Abstract_Interface);

   type ValueModifier is new Types.Short;
   VTM_NONE : constant ValueModifier;
   VTM_CUSTOM : constant ValueModifier;
   VTM_ABSTRACT : constant ValueModifier;
   VTM_TRUNCATABLE : constant ValueModifier;

   type Visibility is new Types.Short;
   PRIVATE_MEMBER : constant Visibility;
   PUBLIC_MEMBER : constant Visibility;

   package TypeCode is

      ----------
      -- Spec --
      ----------

      type Object is private;
      type Object_Ptr is access all Object;

      Bounds : exception;
      BadKind : exception;
      Bad_TypeCode : exception;
      --  Note: this unit raises 'pure' Ada exceptions.
      --  A CORBA personality built upon these subprograms
      --  must wrap them to raise proper CORBA expcetions
      --  (with members).

      function "=" (Left, Right : in Object) return Boolean;
      --  TypeCode equality.

      function Equal (Left, Right : in Object) return Boolean
        renames "=";

      --  equivalence between two typecodes
      --  the equivalence is defined in section 10.7.1 of the
      --  CORBA V2.3 spec : the Typecode interface
      function Equivalent (Left, Right : in Object)
                           return Boolean;

      --  FIXME : to be defined
      function Get_Compact_TypeCode (Self : in Object)
                                     return Object;

      --  returns the kind of a typecode
      function Kind (Self : in Object) return TCKind;

      --  returns the Id associated with a typecode in case its kind is
      --  objref, struct, union, enum, alias, value, valueBox, native,
      --  abstract_interface or except. Raises badKind else.
      function Id (Self : in Object)
        return Types.RepositoryId;

      --  returns the name associated with a typecode in case its kind is
      --  objref, struct, union, enum, alias, value, valueBox, native,
      --  abstract_interface or except. Raises badKind else.
      function Name (Self : in Object)
        return Types.Identifier;

      --  returns the number of members associated with a typecode in
      --  case its kind is struct, union, enum, value or except.
      --  Raises badKind else.
      function Member_Count (Self : in Object)
                             return Types.Unsigned_Long;

      --  returns the name of a given member associated with a typecode
      --  in case its kind is struct, union, enum, value or except.
      --  Raises badKind else.
      --  If there is not enough members, raises bounds.
      function Member_Name (Self  : in Object;
                            Index : in Types.Unsigned_Long)
                            return Types.Identifier;

      --  returns the type of a given member associated with a typecode
      --  in case its kind is struct, union, value or except.
      --  Raises badKind else.
      --  If there is not enough members, raises bounds.
      function Member_Type
        (Self  : in Object;
         Index : in Types.Unsigned_Long) return Object;

      --  returns the label of a given member associated with a typecode
      --  in case its kind is union.
      --  Raises badKind else.
      --  If there is not enough members, raises bounds.
      function Member_Label
          (Self  : in Object;
           Index : in Types.Unsigned_Long) return Any;

      --  returns the discriminator type associated with a typecode
      --  in case its kind is union.
      --  Raises badKind else.
      function Discriminator_Type (Self : in Object)
                                   return Object;

      --  returns the position of the default index in the parameters
      --  of a typecode in case its kind is union.
      --  Raises badKind else.
      --  If there is no default index, return -1
      function Default_Index (Self : in Object)
                              return Types.Long;

      --  returns the length associated with a typecode
      --  in case its kind is string, wide_string, sequence or array.
      --  Raises badKind else.
      function Length (Self : in Object)
                       return Types.Unsigned_Long;

      --  returns the content type associated with a typecode
      --  in case its kind is sequence, array, valueBox or alias.
      --  Raises badKind else.
      function Content_Type (Self : in Object) return Object;

      --  returns the number of digits associated with a typecode
      --  in case its kind is fixed.
      --  Raises badKind else.
      function Fixed_Digits (Self : in Object)
                             return Types.Unsigned_Short;

      --  returns the scale associated with a typecode
      --  in case its kind is fixed.
      --  Raises badKind else.
      function Fixed_Scale (Self : in Object)
                            return Types.Short;

      --  returns the visibility associated with a member of a typecode
      --  in case its kind is value.
      --  Raises badKind else.
      --  If there is not enough members, raises bounds.
      function Member_Visibility
        (Self  : in Object;
         Index : in Types.Unsigned_Long) return Visibility;

      --  returns the type modifier associated with a typecode
      --  in case its kind is value.
      --  Raises badKind else.
      function Type_Modifier (Self : in Object)
                              return ValueModifier;

      --  returns the concrete base type associated with a typecode
      --  in case its kind is value.
      --  Raises badKind else.
      function Concrete_Base_Type (Self : in Object)
                                   return Object;

      -----------------
      -- Not in spec --
      -----------------

      --  returns the type of a given member associated with an
      --  union typecode for a given label. The index is the index
      --  of the member among the members associated with Label. The
      --  other members are not taken into account
      --  Raises badKind if Self is not an union typecode.
      --  If there is not enough members, raises bounds.
      function Member_Type_With_Label
        (Self  : in Object;
         Label : in Any;
         Index : in Types.Unsigned_Long) return Object;

      --  returns the number of members associated with a typecode of
      --  kind union for a given label.
      --  Raises badKind if Self is not an union typecode.
      function Member_Count_With_Label
        (Self : in Object;
         Label : in Any)
         return Types.Unsigned_Long;

      --  returns the parameter nb index in the list of Self's
      --  parameters. Raises Out_Of_Bounds_Index exception if
      --  this parameter does not exist
      function Get_Parameter (Self : in Object;
                              Index : in Types.Unsigned_Long)
                              return Any;

      --  adds the parameter Param in the list of Self's
      --  parameters.
      procedure Add_Parameter (Self  : in out Object;
                               Param : in Any);

      --  Sets the kind of a typecode
      --  By the way, erases all parameters
      procedure Set_Kind (Self : out Object;
                          Kind : in TCKind);

      --  Simple typecodes
      function TC_Null               return TypeCode.Object;
      function TC_Void               return TypeCode.Object;
      function TC_Short              return TypeCode.Object;
      function TC_Long               return TypeCode.Object;
      function TC_Long_Long          return TypeCode.Object;
      function TC_Unsigned_Short     return TypeCode.Object;
      function TC_Unsigned_Long      return TypeCode.Object;
      function TC_Unsigned_Long_Long return TypeCode.Object;
      function TC_Float              return TypeCode.Object;
      function TC_Double             return TypeCode.Object;
      function TC_Long_Double        return TypeCode.Object;
      function TC_Boolean            return TypeCode.Object;
      function TC_Char               return TypeCode.Object;
      function TC_Wchar              return TypeCode.Object;
      function TC_Octet              return TypeCode.Object;
      function TC_Any                return TypeCode.Object;
      function TC_TypeCode           return TypeCode.Object;
      function TC_String             return TypeCode.Object;
      function TC_Wide_String        return TypeCode.Object;

      --  Complex typecodes. These functions create "empty" typecodes;
      --  it is the caller's responsibility to add the proper parameters.
      function TC_Principal          return TypeCode.Object;
      function TC_Struct             return TypeCode.Object;
      function TC_Union              return TypeCode.Object;
      function TC_Enum               return TypeCode.Object;
      function TC_Alias              return TypeCode.Object;
      function TC_Except             return TypeCode.Object;
      function TC_Object             return TypeCode.Object;
      function TC_Fixed              return TypeCode.Object;
      function TC_Sequence           return TypeCode.Object;
      function TC_Array              return TypeCode.Object;
      function TC_Value              return TypeCode.Object;
      function TC_Valuebox           return TypeCode.Object;
      function TC_Native             return TypeCode.Object;
      function TC_Abstract_Interface return TypeCode.Object;

      function Parameter_Count
        (Self : in Object) return Types.Unsigned_Long;
      --  Returns the number of parameters in typecode Self.

   private

      -----------------------------------------------------
      -- A list of typecode parameters (which are Any's) --
      -----------------------------------------------------

      --  NOTE: Cannot be easily converted to an instance of
      --  PolyORB.Utils.Lists, because at this point, Any is
      --  still the public view of a private type.

      type Cell;
      type Cell_Ptr is access all Cell;
      type Cell is record
         Parameter : Any;
         Next : Cell_Ptr;
      end record;

      type Object is record
         Kind : TCKind := Tk_Void;
         Parameters : Cell_Ptr := null;
      end record;

      ---------------------------
      -- Encoding of TypeCodes --
      ---------------------------

      --  1. For null, void, short, long, long_long, unsigned_short,
      --     unsigned_long, unsigned_long_long, float, double,
      --     long_double, boolean, char, Wchar, octet, any,
      --     TypeCode, Principal: parameters = null
      --
      --  2. For Objref, struct, union, enum, alias, value, valueBox,
      --     native, abstract_interface and except, the first parameter
      --     will contain the name and the second the repository id.
      --
      --     objref, native and abstract_interface don't have
      --     any further parameters.
      --
      --  3. For struct and except, the next parameters will
      --     be alternatively a type and a name. So the number of
      --     parameters will be 2 * number_of_members + 2
      --
      --  4. For union, the third parameter will be the
      --     discriminator type. The fourth will be the index of the
      --     default case as a long. If there's no default case, then
      --     you'll find -1. Then we'll have alternatively a
      --     member label, a member type and a member name. At least,
      --     for the default label, the member label will contain a
      --     valid label but without any semantic significance.
      --     So the number of parameters will be 3 * number_of_members + 4
      --
      --  5. For enum, the next parameters will be names of the
      --     different enumerators. So the number of parameters will be
      --     number_of_enumerators + 2
      --
      --  6. For alias, the third parameter is its content type
      --
      --  7. For value, the third parameter will be a type
      --     modifier and the fourth one a concrete base type. The next
      --     parameters will be alternatively a visibility, a type and
      --     a name. So the number of parameters will be
      --     3 * number_of_members + 4.
      --
      --  8. For valueBox, the third parameter is the content type
      --
      --  9. For string and wide_string, the only parameter will
      --     be the length of the string. Its value will be 0 for
      --     unbounded strings or wide strings.
      --
      --  10. For sequence and array, the first parameter will
      --      be the length of the sequence or the array and the second
      --      the content type. As for strings, an unbounded sequence will
      --      have a length of 0.
      --
      --  11. For fixed, the first parameter will be the digits
      --      number and the second the scale.

      --  The most current typecodes
      PTC_Null               : constant Object := (Tk_Null, null);
      PTC_Void               : constant Object := (Tk_Void, null);
      PTC_Short              : constant Object := (Tk_Short, null);
      PTC_Long               : constant Object := (Tk_Long, null);
      PTC_Long_Long          : constant Object := (Tk_Longlong, null);
      PTC_Unsigned_Short     : constant Object := (Tk_Ushort, null);
      PTC_Unsigned_Long      : constant Object := (Tk_Ulong, null);
      PTC_Unsigned_Long_Long : constant Object := (Tk_Ulonglong, null);
      PTC_Float              : constant Object := (Tk_Float, null);
      PTC_Double             : constant Object := (Tk_Double, null);
      PTC_Long_Double        : constant Object := (Tk_Longdouble, null);
      PTC_Boolean            : constant Object := (Tk_Boolean, null);
      PTC_Char               : constant Object := (Tk_Char, null);
      PTC_Wchar              : constant Object := (Tk_Widechar, null);
      PTC_Octet              : constant Object := (Tk_Octet, null);
      PTC_Any                : constant Object := (Tk_Any, null);
      PTC_TypeCode           : constant Object := (Tk_TypeCode, null);

      PTC_String             : constant Object := (Tk_String, null);
      PTC_Wide_String        : constant Object := (Tk_Wstring, null);

      PTC_Principal          : constant Object := (Tk_Principal, null);
      PTC_Struct             : constant Object := (Tk_Struct, null);
      PTC_Union              : constant Object := (Tk_Union, null);
      PTC_Enum               : constant Object := (Tk_Enum, null);
      PTC_Alias              : constant Object := (Tk_Alias, null);
      PTC_Except             : constant Object := (Tk_Except, null);
      PTC_Object             : constant Object := (Tk_Objref, null);
      PTC_Fixed              : constant Object := (Tk_Fixed, null);
      PTC_Sequence           : constant Object := (Tk_Sequence, null);
      PTC_Array              : constant Object := (Tk_Array, null);
      PTC_Value              : constant Object := (Tk_Value, null);
      PTC_Valuebox           : constant Object := (Tk_Valuebox, null);
      PTC_Native             : constant Object := (Tk_Native, null);
      PTC_Abstract_Interface : constant Object
        := (Tk_Abstract_Interface, null);

   end TypeCode;

   --  pre-defined TypeCode "constants"
   function TC_Null               return TypeCode.Object
     renames TypeCode.TC_Null;
   function TC_Void               return TypeCode.Object
     renames TypeCode.TC_Void;
   function TC_Short              return TypeCode.Object
     renames TypeCode.TC_Short;
   function TC_Long               return TypeCode.Object
     renames TypeCode.TC_Long;
   function TC_Long_Long          return TypeCode.Object
     renames TypeCode.TC_Long_Long;
   function TC_Unsigned_Short     return TypeCode.Object
     renames TypeCode.TC_Unsigned_Short;
   function TC_Unsigned_Long      return TypeCode.Object
     renames TypeCode.TC_Unsigned_Long;
   function TC_Unsigned_Long_Long return TypeCode.Object
     renames TypeCode.TC_Unsigned_Long_Long;
   function TC_Float              return TypeCode.Object
     renames TypeCode.TC_Float;
   function TC_Double             return TypeCode.Object
     renames TypeCode.TC_Double;
   function TC_Long_Double        return TypeCode.Object
     renames TypeCode.TC_Long_Double;
   function TC_Boolean            return TypeCode.Object
     renames TypeCode.TC_Boolean;
   function TC_Char               return TypeCode.Object
     renames TypeCode.TC_Char;
   function TC_Wchar              return TypeCode.Object
     renames TypeCode.TC_Wchar;
   function TC_Octet              return TypeCode.Object
     renames TypeCode.TC_Octet;
   function TC_Any                return TypeCode.Object
     renames TypeCode.TC_Any;
   function TC_TypeCode           return TypeCode.Object
     renames TypeCode.TC_TypeCode;
   function TC_String             return TypeCode.Object
     renames TypeCode.TC_String;
   function TC_Wide_String        return TypeCode.Object
     renames TypeCode.TC_Wide_String;
   function TC_Object             return TypeCode.Object
     renames TypeCode.TC_Object;

   -----------
   --  Any  --
   -----------

   function "=" (Left, Right : in Any) return Boolean;

   function Equal (Left, Right : in Any) return Boolean
     renames "=";

   function Compare_Any_Contents (Left : in Any; Right : in Any)
     return Boolean;
   --  Check if two Anys are pointing to the same content object.

   function To_Any (Item : in Types.Short)              return Any;
   function To_Any (Item : in Types.Long)               return Any;
   function To_Any (Item : in Types.Long_Long)          return Any;
   function To_Any (Item : in Types.Unsigned_Short)     return Any;
   function To_Any (Item : in Types.Unsigned_Long)      return Any;
   function To_Any (Item : in Types.Unsigned_Long_Long) return Any;
   function To_Any (Item : in Types.Float)        return Any;
   function To_Any (Item : in Types.Double)             return Any;
   function To_Any (Item : in Types.Long_Double)        return Any;
   function To_Any (Item : in Types.Boolean)            return Any;
   function To_Any (Item : in Types.Char)               return Any;
   function To_Any (Item : in Types.Wchar)              return Any;
   function To_Any (Item : in Types.Octet)              return Any;
   function To_Any (Item : in Any)                return Any;
   function To_Any (Item : in TypeCode.Object)    return Any;
   function To_Any (Item : in Types.String)       return Any;
   function To_Any (Item : in Types.Wide_String)  return Any;

   function From_Any (Item : in Any) return Types.Short;
   function From_Any (Item : in Any) return Types.Long;
   function From_Any (Item : in Any) return Types.Long_Long;
   function From_Any (Item : in Any) return Types.Unsigned_Short;
   function From_Any (Item : in Any) return Types.Unsigned_Long;
   function From_Any (Item : in Any) return Types.Unsigned_Long_Long;
   function From_Any (Item : in Any) return Types.Float;
   function From_Any (Item : in Any) return Types.Double;
   function From_Any (Item : in Any) return Types.Long_Double;
   function From_Any (Item : in Any) return Types.Boolean;
   function From_Any (Item : in Any) return Types.Char;
   function From_Any (Item : in Any) return Types.Wchar;
   function From_Any (Item : in Any) return Types.Octet;
   function From_Any (Item : in Any) return Any;
   function From_Any (Item : in Any) return TypeCode.Object;
   function From_Any (Item : in Any) return Types.String;
   function From_Any (Item : in Any) return Types.Wide_String;

   function Get_Type (The_Any : in Any) return TypeCode.Object;

   function Unwind_Typedefs
     (TC : in TypeCode.Object)
     return TypeCode.Object;
   --  Unwind any typedef (alias) from TC.

   function Get_Unwound_Type (The_Any : in Any) return TypeCode.Object;
   --  Return the actual type of The_Any, after resolution of
   --  all typedef levels.

   procedure Set_Type
     (The_Any : in out Any;
      The_Type : in TypeCode.Object);
   --  not in spec : change the type of an any without changing its
   --  value : to be used carefully

   generic
      with procedure Process (The_Any : in Any;
                              Continue : out Boolean);
   procedure Iterate_Over_Any_Elements (In_Any : in Any);

   --  returns  an empty Any (with no value but a type)
   function Get_Empty_Any (Tc : TypeCode.Object) return Any;

   --  Not in spec : return true if the Any has a value, false
   --  if it is an empty one
   function Is_Empty (Any_Value : in Any) return Boolean;

   --  These functions allows the user to set the value of an any
   --  directly if he knows its kind. It a function is called on a
   --  bad kind of any, a BAD_TYPECODE exception will be raised
   --  Note that the Any can be empty. In this case, the value
   --  will be created
   --  Should never be called outside the broca.cdr package
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Octet);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Short);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Long);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Long_Long);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Unsigned_Short);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Unsigned_Long);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Unsigned_Long_Long);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Boolean);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Char);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Wchar);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.String);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Wide_String);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Float);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Double);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Types.Long_Double);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in TypeCode.Object);
   procedure Set_Any_Value (Any_Value : in out Any;
                            Value : in Any);

   procedure Set_Any_Aggregate_Value (Any_Value : in out Any);
   --  This one is a bit special : it doesn't put any value but
   --  create the aggregate value if it does not exist.

   --  Not in spec : some methods to deal with any aggregates.
   --  What is called any aggregate is an any, made of an aggregate
   --  of values, instead of one unique. It is used for structs,
   --  unions, enums, arrays, sequences, objref, values...

   function Get_Aggregate_Count (Value : Any) return Types.Unsigned_Long;
   --  Returns the number of elements in an any aggregate

   procedure Add_Aggregate_Element
     (Value   : in out Any;
      Element : in     Any);
   --  Adds an element to an any aggregate
   --  This element is given as a typecode but only its value is
   --  added to the aggregate

   function Get_Aggregate_Element
     (Value : Any;
      Tc    : TypeCode.Object;
      Index : Types.Unsigned_Long)
      return Any;
   --  Gets an element in an any agregate
   --  returns an any made of the typecode Tc and the value read in
   --  the aggregate.

   --  returns an empty any aggregate
   --  puts its type to Tc
   function Get_Empty_Any_Aggregate
     (Tc : TypeCode.Object)
     return Any;

   procedure Copy_Any_Value (Dest : Any; Src : Any);
   --  Set the value of Dest from the value of Src (as
   --  Set_Any_Value would do, but without the need to
   --  know the precise type of Src). Dest and Src must be Any's
   --  with identical typecodes. Dest may be empty.
   --  This is not the same as Set_Any_Value (Dest, Src), which
   --  sets the value of Dest (an Any which a Tk_Any type code)
   --  to be Src (not the /value/ of Src).

   -----------------
   --  NamedValue --
   -----------------

   type Flags is new Types.Unsigned_Long;

   ARG_IN :        constant Flags;
   ARG_OUT :       constant Flags;
   ARG_INOUT :     constant Flags;
   IN_COPY_VALUE : constant Flags;

   type NamedValue is record
      Name      : Types.Identifier;
      Argument  : Any;
      Arg_Modes : Flags;
   end record;

   function Image (NV : NamedValue) return Standard.String;
   --  For debugging purposes.

private

   --  Null_String : constant CORBA.String :=
   --  CORBA.String (Ada.Strings.Unbounded.Null_Unbounded_String);

   VTM_NONE : constant ValueModifier := 0;
   VTM_CUSTOM : constant ValueModifier := 1;
   VTM_ABSTRACT : constant ValueModifier := 2;
   VTM_TRUNCATABLE : constant ValueModifier := 3;

   PRIVATE_MEMBER : constant Visibility := 0;
   PUBLIC_MEMBER : constant Visibility := 1;

   -----------
   --  Any  --
   -----------

   --  any is implemented this way :
   --   one field for the typecode (TypeCode.Object)
   --   one field for the value
   --
   --  To be able to carry values of different types, the second
   --  field is an Any_Content_Ptr_Ptr which is an access to an access to any
   --  type deriving from Content. Every basic types Foo that can be carried
   --  into an Any should be associated to a child of Content (Content_Foo)
   --  which contains a field of the Foo type.
   --  For complex types (with several values, like structures, arrays...),
   --  we use a special child of Content, Content_Aggregate, which has a field
   --  pointing on a list of childs of Content; various methods are provided
   --  to manipulate this list.


   type Any_Content_Ptr_Ptr is access all Any_Content_Ptr;
   Null_Content_Ptr_Ptr : constant Any_Content_Ptr_Ptr := null;

   --  Frees an Any_Content_Ptr
   procedure Deallocate_Any_Content is new Ada.Unchecked_Deallocation
     (Content'Class, Any_Content_Ptr);
   --  Frees an Any_Content_Ptr_Ptr
   procedure Deallocate_Any_Content_Ptr is new Ada.Unchecked_Deallocation
     (Any_Content_Ptr, Any_Content_Ptr_Ptr);

   --  the content_TypeCode type is defined inside the TypeCode package
   --  However, the corresponding deallocate function is here
   --  This is due to the fact that the TypeCode.Object type is private
   --  in package TypeCode which implies that Deallocate sould be private
   --  too if it were declared there.
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (TypeCode.Object, TypeCode.Object_Ptr);

   type Natural_Ptr is access Natural;
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Natural, Natural_Ptr);

   ------------------
   -- The Any type --
   ------------------

   type Any is new Ada.Finalization.Controlled with record
      The_Type     : TypeCode.Object;
      --  TypeCode describing the data.

      The_Value    : Any_Content_Ptr_Ptr;
      --  Pointer to the actual value contained.

      Ref_Counter  : Natural_Ptr;
      --  Reference counter associated with the
      --  designated container.

      Any_Lock     : PolyORB.Locks.Rw_Lock_Access;
      --  Lock to guarantee consistent concurrent access
      --  to Ref_Counter.
   end record;

   --  Some methods to deal with the Any fields.

   --  These are the only way to deal with the fields if you want to
   --  stay thread safe
   --  Apart from the management of locks, these methods do not
   --  make any test. So use them carefully

   procedure Set_Value (Obj : in out Any; The_Value : in Any_Content_Ptr);
   function Get_Value (Obj : Any) return Any_Content_Ptr;
   function Get_Value_Ptr (Obj : Any) return Any_Content_Ptr_Ptr;
   function Get_Counter (Obj : Any) return Natural;

   --  The control procedures to the Any type
   procedure Initialize (Object : in out Any);
   procedure Adjust (Object : in out Any);
   procedure Finalize (Object : in out Any);

   --  And the management of the counter
   procedure Inc_Usage (Obj : in Any);
   procedure Dec_Usage (Obj : in out Any);

   --  deallocation of Any pointers
   procedure Deallocate is new Ada.Unchecked_Deallocation (Any, Any_Ptr);

   ------------------
   --  Named_Value --
   ------------------

   ARG_IN :        constant Flags := 0;
   ARG_OUT :       constant Flags := 1;
   ARG_INOUT :     constant Flags := 2;
   IN_COPY_VALUE : constant Flags := 3;

end PolyORB.Any;
