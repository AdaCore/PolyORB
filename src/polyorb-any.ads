------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . A N Y                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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

--  Definition of the container type 'Any'

with Ada.Unchecked_Deallocation;

with PolyORB.Smart_Pointers;
with PolyORB.Types;

package PolyORB.Any is

   pragma Preelaborate;

   ---------
   -- Any --
   ---------

   type Any is new PolyORB.Smart_Pointers.Ref with private;

   procedure Initialize (Self : in out Any);

   type Any_Ptr is access all Any;
   --  The end of this part is after the typecode part;

   function Image
     (A : Any)
     return Standard.String;
   --  For debugging purposes.

   ---------------
   -- TypeCodes --
   ---------------

   --  See spec CORBA V3.0, Ada Langage Mapping 1.33

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

      --  Aggregates
       Tk_Struct,
       Tk_Union,
       Tk_Enum,
       Tk_Sequence,
       Tk_Array,
       Tk_Except,
       Tk_Fixed,
      --  End aggregates

       Tk_String,
       Tk_Alias,
       Tk_Longlong,
       Tk_Ulonglong,
       Tk_Longdouble,
       Tk_Widechar,
       Tk_Wstring,
       Tk_Value,
       Tk_Valuebox,
       Tk_Native,
       Tk_Abstract_Interface,
       Tk_Local_Interface,
       Tk_Component,
       Tk_Home,
       Tk_Event);
   subtype Aggregate_TCKind is TCKind range Tk_Struct .. Tk_Fixed;

   type ValueModifier is new Types.Short;

   VTM_NONE        : constant ValueModifier;
   VTM_CUSTOM      : constant ValueModifier;
   VTM_ABSTRACT    : constant ValueModifier;
   VTM_TRUNCATABLE : constant ValueModifier;

   type Visibility is new Types.Short;

   PRIVATE_MEMBER : constant Visibility;
   PUBLIC_MEMBER  : constant Visibility;

   package TypeCode is

      ----------
      -- Spec --
      ----------

      type Object is private;

      type Object_Ptr is access all Object;

      Bounds       : exception;
      BadKind      : exception;
      Bad_TypeCode : exception;
      --  Note: this unit raises 'pure' Ada exceptions.  An application or
      --  protocol personality built upon these subprograms must wrap them
      --  to raise proper exceptions or messages.

      function "="
        (Left, Right : in Object)
        return Boolean;
      --  TypeCode equality.

      function Equal
        (Left, Right : in Object)
        return Boolean
        renames "=";

      function Equivalent
        (Left, Right : in Object)
        return Boolean;
      --  Equivalence between two typecodes as defined in
      --  section 10.7.1 of the CORBA V2.3.

      function Get_Compact_TypeCode
        (Self : in Object)
        return Object;
      --  XXX not implemented, to be defined.

      function Kind
        (Self : in Object)
        return TCKind;
      --  Return the kind of a typecode.

      function Id
        (Self : in Object)
        return Types.RepositoryId;
      --  Return the Id associated with a typecode in case its kind is
      --  objref, struct, union, enum, alias, value, valueBox, native,
      --  abstract_interface or except. Raise BadKind else.

      function Name
        (Self : in Object)
        return Types.Identifier;
      --  Return the name associated with a typecode in case its kind is
      --  objref, struct, union, enum, alias, value, valueBox, native,
      --  abstract_interface or except. Raise BadKind else.

      function Member_Count
        (Self : in Object)
        return Types.Unsigned_Long;
      --  Return the number of members associated with a typecode in
      --  case its kind is struct, union, enum, value or except.
      --  Raise BadKind else.

      function Member_Name
        (Self  : in Object;
         Index : in Types.Unsigned_Long)
        return Types.Identifier;
      --  Return the name of a given member associated with a typecode
      --  in case its kind is struct, union, enum, value or except.
      --  Raise BadKind else.
      --  If there is not enough members, Raise Bounds.

      function Member_Type
        (Self  : in Object;
         Index : in Types.Unsigned_Long)
        return Object;
      --  Return the type of a given member associated with a typecode
      --  in case its kind is struct, union, value or except.
      --  Raise BadKind else.
      --  If there is not enough members, Raise Bounds.

      function Member_Label
        (Self  : in Object;
         Index : in Types.Unsigned_Long)
        return Any;
      --  Return the label of a given member associated with a typecode
      --  in case its kind is union.
      --  Raise BadKind else.
      --  If there is not enough members, Raise Bounds.

      function Enumerator_Name
        (Self  : in Object;
         Index : in Types.Unsigned_Long)
        return Types.Identifier;
      --  Return the name of the Index'th enumerator in an enumeration.
      --  If there is not enough members, Raise Bounds.

      function Discriminator_Type
        (Self : in Object)
        return Object;
      --  Return the discriminator type associated with a typecode
      --  in case its kind is union.
      --  Raise BadKind else.

      function Default_Index
        (Self : in Object)
        return Types.Long;
      --  Return the position of the default index in the parameters
      --  of a typecode in case its kind is union.
      --  Raise BadKind else.
      --  If there is no default index, return -1

      function Length
        (Self : in Object)
        return Types.Unsigned_Long;
      --  Return the length associated with a typecode
      --  in case its kind is string, wide_string, sequence or array.
      --  Raise BadKind else.

      function Content_Type
        (Self : in Object)
        return Object;
      --  Return the content type associated with a typecode
      --  in case its kind is sequence, array, valueBox or alias.
      --  Raise BadKind else.

      function Fixed_Digits
        (Self : in Object)
        return Types.Unsigned_Short;
      --  Return the number of digits associated with a typecode
      --  in case its kind is fixed.
      --  Raise BadKind else.

      function Fixed_Scale
        (Self : in Object)
        return Types.Short;
      --  Return the scale associated with a typecode
      --  in case its kind is fixed.
      --  Raise BadKind else.

      function Member_Visibility
        (Self  : in Object;
         Index : in Types.Unsigned_Long)
        return Visibility;
      --  Return the visibility associated with a member of a typecode
      --  in case its kind is value.
      --  Raise BadKind else.
      --  If there is not enough members, Raise Bounds.

      function Type_Modifier
        (Self : in Object)
        return ValueModifier;
      --  Return the type modifier associated with a typecode
      --  in case its kind is value.
      --  Raise BadKind else.

      function Concrete_Base_Type
        (Self : in Object)
        return Object;
      --  Return the concrete base type associated with a typecode
      --  in case its kind is value.
      --  Raise BadKind else.

      -----------------
      -- Not in spec --
      -----------------

      function Member_Type_With_Label
        (Self  : in Object;
         Label : in Any)
        return Object;
      --  Return the type of a given member associated with an
      --  union typecode for a given label. The index is the index
      --  of the member among the members associated with Label. The
      --  other members are not taken into account
      --  Raise BadKind if Self is not an union typecode.
      --  If there is not enough members, Raise Bounds.

      function Member_Count_With_Label
        (Self  : in Object;
         Label : in Any)
         return Types.Unsigned_Long;
      pragma Unreferenced (Member_Count_With_Label);
      --  Return the number of members associated with a typecode of
      --  kind union for a given label.
      --  Raise BadKind if Self is not an union typecode.

      function Get_Parameter
        (Self  : in Object;
         Index : in Types.Unsigned_Long)
        return Any;
      --  Return the parameter nb index in the list of Self's
      --  parameters. Raise Out_Of_Bounds_Index exception if
      --  this parameter does not exist

      procedure Add_Parameter
        (Self  : in out Object;
         Param : in     Any);
      --  Add the parameter Param in the list of Self's
      --  parameters.

      procedure Set_Volatile
        (Self        : in out Object;
         Is_Volatile : in     Boolean);
      pragma Unreferenced (Set_Volatile);
      --  Set to True if TypeCode is volatile, i.e. can be destroyed,
      --  False otherwise. Currently unused.

      procedure Destroy_TypeCode
        (Self : in out Object);
      --  Free all elements contained in Self iff Self has been marked
      --  volatile.

      procedure Set_Kind
        (Self : out Object;
         Kind : in  TCKind);
      --  Return a typecode of kind Kind, with an empty parameter list.

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

      --  Complex typecodes. These functions create non-empty typecodes;
      --  they are initialized for unbounded strings.
      --  XXX to define
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
      function TC_Local_Interface    return TypeCode.Object;
      function TC_Component          return TypeCode.Object;
      function TC_Home               return TypeCode.Object;
      function TC_Event              return TypeCode.Object;

      function Parameter_Count
        (Self : in Object)
        return Types.Unsigned_Long;
      --  Return the number of parameters in typecode Self.

      procedure Initialize;

   private

      pragma Inline (Kind);

      -----------------------------------------------------
      -- A list of typecode parameters (which are Any's) --
      -----------------------------------------------------

      --  NOTE: TypeCode internal chained list cannot be easily converted to
      --  an instance of PolyORB.Utils.Chained_Lists : at this point, Any is
      --  still the public view of a private type.

      type Cell;
      type Cell_Ptr is access all Cell;
      type Cell is record
         Parameter : Any;
         Next      : Cell_Ptr;
      end record;

      type Object is record
         Kind         : TCKind   := Tk_Void;
         Parameters   : Cell_Ptr := null;
         Is_Volatile  : Boolean  := False;
         Is_Destroyed : Boolean  := False;
      end record;

      ---------------------------
      -- Encoding of TypeCodes --
      ---------------------------

      --  1. For null, void, short, long, long_long, unsigned_short,
      --     unsigned_long, unsigned_long_long, float, double,
      --     long_double, boolean, char, Wchar, octet, any,
      --     TypeCode, Principal: parameters = null
      --
      --  2. For Objref, struct, union, enum, alias, except, value, valueBox,
      --     native, abstract_interface, local_interface, component, home
      --     and event, the first parameter will contain the name and the
      --     second the repository id.
      --
      --     objref, native, abstract_interface, local_interface, component
      --     and home don't have any further parameters.
      --
      --  3. For struct and except, the next parameters will
      --     be alternatively a type and a name. So the number of
      --     parameters will be 2 * number_of_members + 2
      --
      --  4. For union, the third parameter will be the
      --     discriminator type. The fourth will be the index of the
      --     default case as a long. If there's no default case, then
      --     you'll find -1. Then we'll have alternatively a
      --     member label, a member type and a member name.
      --     For the default label, the member label will contain a
      --     valid label but without any semantic significance.
      --     So the number of parameters will be 3 * number_of_members + 4
      --
      --  5. For enum, the next parameters will be names of the
      --     different enumerators. So the number of parameters will be
      --     number_of_enumerators + 2
      --
      --  6. For alias, the third parameter is its content type
      --
      --  7. For value and event, the third parameter will be a type
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
      PTC_Null               : constant Object
        := (Tk_Null, null, False, False);
      PTC_Void               : constant Object
        := (Tk_Void, null, False, False);
      PTC_Short              : constant Object
        := (Tk_Short, null, False, False);
      PTC_Long               : constant Object
        := (Tk_Long, null, False, False);
      PTC_Long_Long          : constant Object
        := (Tk_Longlong, null, False, False);
      PTC_Unsigned_Short     : constant Object
        := (Tk_Ushort, null, False, False);
      PTC_Unsigned_Long      : constant Object
        := (Tk_Ulong, null, False, False);
      PTC_Unsigned_Long_Long : constant Object
        := (Tk_Ulonglong, null, False, False);
      PTC_Float              : constant Object
        := (Tk_Float, null, False, False);
      PTC_Double             : constant Object
        := (Tk_Double, null, False, False);
      PTC_Long_Double        : constant Object
        := (Tk_Longdouble, null, False, False);
      PTC_Boolean            : constant Object
        := (Tk_Boolean, null, False, False);
      PTC_Char               : constant Object
        := (Tk_Char, null, False, False);
      PTC_Wchar              : constant Object
        := (Tk_Widechar, null, False, False);
      PTC_Octet              : constant Object
        := (Tk_Octet, null, False, False);
      PTC_Any                : constant Object
        := (Tk_Any, null, False, False);
      PTC_TypeCode           : constant Object
        := (Tk_TypeCode, null, False, False);

      PTC_String             : constant Object
        := (Tk_String, null, False, False);
      PTC_Wide_String        : constant Object
        := (Tk_Wstring, null, False, False);

      PTC_Principal          : constant Object
        := (Tk_Principal, null, False, False);
      PTC_Struct             : constant Object
        := (Tk_Struct, null, False, False);
      PTC_Union              : constant Object
        := (Tk_Union, null, False, False);
      PTC_Enum               : constant Object
        := (Tk_Enum, null, False, False);
      PTC_Alias              : constant Object
        := (Tk_Alias, null, False, False);
      PTC_Except             : constant Object
        := (Tk_Except, null, False, False);
      PTC_Object             : constant Object
        := (Tk_Objref, null, False, False);
      PTC_Fixed              : constant Object
        := (Tk_Fixed, null, False, False);
      PTC_Sequence           : constant Object
        := (Tk_Sequence, null, False, False);
      PTC_Array              : constant Object
        := (Tk_Array, null, False, False);
      PTC_Value              : constant Object
        := (Tk_Value, null, False, False);
      PTC_Valuebox           : constant Object
        := (Tk_Valuebox, null, False, False);
      PTC_Native             : constant Object
        := (Tk_Native, null, False, False);
      PTC_Abstract_Interface : constant Object
        := (Tk_Abstract_Interface, null, False, False);
      PTC_Local_Interface    : constant Object
        := (Tk_Local_Interface, null, False, False);
      PTC_Component          : constant Object
        := (Tk_Component, null, False, False);
      PTC_Home               : constant Object
        := (Tk_Home, null, False, False);
      PTC_Event              : constant Object
        := (Tk_Event, null, False, False);

   end TypeCode;

   --  Pre-defined TypeCode "constants".
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

   ---------
   -- Any --
   ---------

   function "="
     (Left, Right : in Any)
     return Boolean;

   function Equal
     (Left, Right : in Any)
     return Boolean
     renames "=";

   function Compare_Any_Contents
     (Left  : in Any;
      Right : in Any)
     return Boolean;
   --  Check if two Anys are pointing to the same content object.

   function To_Any (Item : in Types.Short)              return Any;
   function To_Any (Item : in Types.Long)               return Any;
   function To_Any (Item : in Types.Long_Long)          return Any;
   function To_Any (Item : in Types.Unsigned_Short)     return Any;
   function To_Any (Item : in Types.Unsigned_Long)      return Any;
   function To_Any (Item : in Types.Unsigned_Long_Long) return Any;
   function To_Any (Item : in Types.Float)              return Any;
   function To_Any (Item : in Types.Double)             return Any;
   function To_Any (Item : in Types.Long_Double)        return Any;
   function To_Any (Item : in Types.Boolean)            return Any;
   function To_Any (Item : in Types.Char)               return Any;
   function To_Any (Item : in Types.Wchar)              return Any;
   function To_Any (Item : in Types.Octet)              return Any;
   function To_Any (Item : in Any)                      return Any;
   function To_Any (Item : in TypeCode.Object)          return Any;
   function To_Any (Item : in Standard.String)          return Any;
   function To_Any (Item : in Types.String)             return Any;
   function To_Any (Item : in Types.Wide_String)        return Any;

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
   function From_Any (Item : in Any) return Standard.String;
   function From_Any (Item : in Any) return Types.String;
   function From_Any (Item : in Any) return Types.Wide_String;

   function Get_Type
     (The_Any : in Any)
     return TypeCode.Object;

   function Unwind_Typedefs
     (TC : in TypeCode.Object)
     return TypeCode.Object;
   --  Unwind any typedef (alias) from TC.

   function Get_Unwound_Type
     (The_Any : in Any)
     return TypeCode.Object;
   --  Return the actual type of The_Any, after resolution of
   --  all typedef levels.

   procedure Set_Type
     (The_Any  : in out Any;
      The_Type : in     TypeCode.Object);
   --  Not in spec : change the type of an any without changing its
   --  value : to be used carefully

   generic
      with procedure Process
        (The_Any  : in  Any;
         Continue : out Boolean);
   procedure Iterate_Over_Any_Elements (In_Any : in Any);
   --  XXX Not implemented.

   function Get_Empty_Any
     (Tc : TypeCode.Object)
     return Any;
   --  Return an empty Any (with no value but a type).

   function Get_Empty_Any_Aggregate
     (Tc : TypeCode.Object)
     return Any;
   --  Return an empty any aggregate
   --  puts its type to Tc
   --  If the underlying type for TC (with typedefs unwound)
   --  does not have an aggregate TCKind, this is equivalent
   --  to Get_Empty_Any.

   function Is_Empty
     (Any_Value : in Any)
     return Boolean;
   --  Not in spec : return true if the Any is empty, false
   --  if it has a value.

   --  These functions allows the user to set the value of an any
   --  directly if he knows its kind. If a function is called on a bad
   --  kind of any, a BAD_TYPECODE exception will be raised Note that
   --  the Any can be empty. In this case, the value will be
   --  created.

   --  These functions should never be called outside a package
   --  achieving the PolyORB's 'Representation' service.

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Octet);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Short);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in    Types.Long);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Long_Long);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Unsigned_Short);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Unsigned_Long);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Unsigned_Long_Long);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Boolean);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Char);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Wchar);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.String);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Wide_String);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Float);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Double);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Types.Long_Double);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     TypeCode.Object);

   procedure Set_Any_Value
     (Any_Value : in out Any;
      Value     : in     Any);

   procedure Set_Any_Aggregate_Value
     (Any_Value : in out Any);
   --  This one is a bit special : it doesn't put any value but
   --  create the aggregate value if it does not exist.

   --  Not in spec : some methods to deal with any aggregates.
   --  What is called any aggregate is an any, made of an aggregate
   --  of values, instead of one unique. It is used for structs,
   --  unions, enums, arrays, sequences, objref, values...

   function Get_Aggregate_Count
     (Value : Any)
     return Types.Unsigned_Long;
   --  Return the number of elements in an any aggregate

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
   --  Return an any made of the typecode Tc and the value read in
   --  the aggregate. The first element has index 0.

   procedure Copy_Any_Value (Dest : Any; Src : Any);
   --  Set the value of Dest from a copy of the value of Src (as
   --  Set_Any_Value would do, but without the need to
   --  know the precise type of Src). Dest and Src must be Any's
   --  with identical typecodes. Dest may be empty.
   --  This is not the same as Set_Any_Value (Dest, Src), which
   --  sets the value of Dest (an Any which a Tk_Any type code)
   --  to be Src (not the /value/ of Src).

   procedure Move_Any_Value (Dest : Any; Src : Any);
   --  Set the value of Dest to the value of Src, and make Src empty.
   --  Dest and Src must be Any's with identical typecodes. Dest may be empty.

   ----------------
   -- NamedValue --
   ----------------

   type Flags is new Types.Unsigned_Long;

   ARG_IN        : constant Flags;
   ARG_OUT       : constant Flags;
   ARG_INOUT     : constant Flags;
   IN_COPY_VALUE : constant Flags;

   type NamedValue is record
      Name      : Types.Identifier;
      Argument  : Any;
      Arg_Modes : Flags;
   end record;

   function Image
     (TC : TypeCode.Object)
     return Standard.String;

   function Image
     (NV : NamedValue)
     return Standard.String;
   --  For debugging purposes.

private

   VTM_NONE        : constant ValueModifier := 0;
   VTM_CUSTOM      : constant ValueModifier := 1;
   VTM_ABSTRACT    : constant ValueModifier := 2;
   VTM_TRUNCATABLE : constant ValueModifier := 3;

   PRIVATE_MEMBER : constant Visibility := 0;
   PUBLIC_MEMBER  : constant Visibility := 1;

   ---------
   -- Any --
   ---------

   --  An Any is a smart reference-counted pointer to a container that holds:
   --   - one field for the typecode (TypeCode.Object)
   --   - one field for the value
   --
   --  To be able to carry values of different types, the second
   --  field is an Any_Content_Ptr which is  an access to any
   --  type deriving from Content. Every basic types Foo that can be carried
   --  into an Any should be associated to a child of Content (Content_Foo)
   --  which contains a field of the Foo type.
   --  For complex types (with several values, like structures, arrays...),
   --  we use a special child of Content, Content_Aggregate, which has a field
   --  pointing on a list of childs of Content; various methods are provided
   --  to manipulate this list.
   --  The contents of an Any must be referenced by at most one container
   --  at any given time. Distinct Any's that are supposed to correspond to
   --  the same stored object must be references to the same container.
   --  This allows all the memory management of Content objects to be done
   --  in containers. In particular, when changing the Value field of a
   --  container, the previous contents (if non-null) is deallocated.

   type Content is abstract tagged null record;
   type Any_Content_Ptr is access all Content'Class;

   procedure Deallocate
     (Object : access Content);
   --  Deallocate an Any_Content_Ptr.
   --  Overridden for aggregates, since those have to
   --  deallocate all the list of their elements.

   function Duplicate
     (Object : access Content)
     return Any_Content_Ptr
      is abstract;
   --  Duplicate the data pointed by Object, making a deep copy

   --  Frees an Any_Content_Ptr
   procedure Deallocate_Any_Content is new Ada.Unchecked_Deallocation
     (Content'Class, Any_Content_Ptr);

   --  The content_TypeCode type is defined inside the TypeCode package
   --  However, the corresponding deallocate function is here
   --  This is due to the fact that the TypeCode.Object type is private
   --  in package TypeCode which implies that Deallocate sould be private
   --  too if it were declared there.
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (TypeCode.Object, TypeCode.Object_Ptr);

   ------------------
   -- The Any type --
   ------------------

   type Any is new PolyORB.Smart_Pointers.Ref with null record;

   type Any_Container is new PolyORB.Smart_Pointers.Non_Controlled_Entity with
      record
         The_Type     : TypeCode.Object;
         --  TypeCode describing the data

         The_Value    : Any_Content_Ptr;
         --  Pointer to the actual value contained

         Is_Finalized : Boolean := False;
         --  Set to True in Finalize, used to detect double
         --  finalization.

      end record;

   type Any_Container_Ptr is access all Any_Container;

   procedure Finalize   (Self : in out Any_Container);

   --  Some methods to deal with the Any fields.

   procedure Set_Value
     (Obj       : in Any;
      The_Value : in Any_Content_Ptr);
   pragma Inline (Set_Value);

   function Get_Value
     (Obj : Any)
     return Any_Content_Ptr;
   pragma Inline (Get_Value);

   --  Deallocation of Any pointers.
   procedure Deallocate is new Ada.Unchecked_Deallocation (Any, Any_Ptr);

   ------------------
   -- Named_Value --
   ------------------

   ARG_IN        : constant Flags := 0;
   ARG_OUT       : constant Flags := 1;
   ARG_INOUT     : constant Flags := 2;
   IN_COPY_VALUE : constant Flags := 3;

   ---------------------
   -- Debugging hooks --
   ---------------------

   --  For debugging purposes, the body of this unit needs to call
   --  Ada.Tags.External_Tag for any contents. However,
   --  we do not want any dependence on Ada.Tags, because that would
   --  prevent this unit from being preelaborate. Consequently, we
   --  declare hooks to be initialized during elaboration.

   type Content_External_Tag_Hook is access
     function (X : Content'Class)
     return String;

   Content_External_Tag : Content_External_Tag_Hook := null;
   --  Hooks to be set up by a child unit during PolyORB
   --  initialization.

end PolyORB.Any;
