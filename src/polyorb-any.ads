------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . A N Y                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

--  Definition of the universal container/wrapper type 'Any'

with Ada.Unchecked_Deallocation;
with Ada.Strings.Superbounded;
with Ada.Strings.Wide_Superbounded;

with PolyORB.Smart_Pointers;
with PolyORB.Types;

package PolyORB.Any is

   pragma Preelaborate;

   ---------
   -- Any --
   ---------

   type Any is private;

   procedure Initialize (Self : in out Any);

   type Any_Ptr is access all Any;
   --  The end of this part is after the typecode part;

   function Image (A : Any) return Standard.String;
   --  For debugging purposes.

   type Any_Container is tagged limited private;
   type Any_Container_Ptr is access all Any_Container'Class;
   --  The entity designated by an Any

   function Image (C : Any_Container'Class) return Standard.String;
   --  For debugging purposes.

   -------------
   -- Content --
   -------------

   --  Wrapper for an access to the stored value of an Any

   type Content is abstract tagged private;
   type Content_Ptr is access all Content'Class;

   function Clone
     (CC   : Content;
      Into : Content_Ptr := null) return Content_Ptr is abstract;
   --  Value copy primitive. If Into is null, storage is first allocated to
   --  hold a new copy of the value designated by Content. Then, if the value
   --  can be copied directly (i.e. either when Into was null and an adapted
   --  new Content has been allocated, or when Into designates a Content of
   --  the proper type), the value is assigned, and an access to the new copy
   --  is returned (may be Into itself).
   --  When Into is not null but does not support direct in-place assignment
   --  of the value, no copy is performed, and null is returned.

   procedure Finalize_Value (CC : in out Content) is abstract;
   --  Deallocate the stored value

   type No_Content is new Content with private;
   --  Placeholder for a missing content

   generic
      type T is private;
   function No_Wrap (X : access T) return Content'Class;
   --  Dummy Wrap function for types that do not implement proper wrapping
   --  (should never be called).

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

      type Local_Ref is private;

      type Object (Kind : TCKind) is
        new Smart_Pointers.Non_Controlled_Entity
      with record
         Parameters : Content_Ptr;
      end record;

      type Object_Ptr is access all Object;
      --  A typecode is a locality constrained pseudo object with reference
      --  counting.

      function Object_Of (Self : Local_Ref) return Object_Ptr;
      pragma Inline (Object_Of);
      --  Return a pointer to underlying object (care must be taken to not
      --  keep this pointer around beyond the lifetime of said object).

      function To_Ref (Self : Object_Ptr) return Local_Ref;
      --  Build a new reference to an existing TypeCode object

      function Is_Nil (Self : Local_Ref) return Boolean;
      --  True if Self has not been set to designate any specific TypeCode
      --  object.

      procedure Disable_Reference_Counting (Self : in out Object);
      --  Mark Self as not to be subjected to reference counting

      Bounds       : exception;
      BadKind      : exception;
      Bad_TypeCode : exception;
      --  Note: this unit raises 'pure' Ada exceptions.  An application or
      --  protocol personality built upon these subprograms must wrap them
      --  to raise proper exceptions or messages.

      function Equal (Left, Right : Object_Ptr) return Boolean;
      function Equal (Left, Right : Local_Ref) return Boolean;
      --  TypeCode equality

      function "=" (Left, Right : Local_Ref) return Boolean renames Equal;

      function Equivalent (Left, Right : Object_Ptr) return Boolean;
      function Equivalent (Left, Right : Local_Ref) return Boolean;
      --  Equivalence between two typecodes as defined in section 10.7.1 of
      --  the CORBA V2.3 specifications.

      --------------------------------
      -- Accessors from CORBA specs --
      --------------------------------

      function Kind (Self : Object_Ptr) return TCKind;
      function Kind (Self : Local_Ref) return TCKind;
      --  Return the kind of a typecode. Note: as a small optimization, calling
      --  this function on an unset typecode reference will return Tk_Void.

      function Id (Self : Object_Ptr) return Types.RepositoryId;
      function Id (Self : Local_Ref) return Types.RepositoryId;
      --  Return the repository id associated with a complex typecode of one
      --  of the following kinds: objref, struct, union, enum, alias, value,
      --  valuebox, native, abstract_interface or except.
      --  Otherwise raises BadKind.
      --  ??? should return Standard.String

      function Name (Self : Object_Ptr) return Types.Identifier;
      function Name (Self : Local_Ref) return Types.Identifier;
      --  Return the name associated with a complex typecode of one of the
      --  following kinds: objref, struct, union, enum, alias, value, valuebox,
      --  native, abstract_interface or except.
      --  Otherwise raises BadKind.
      --  ??? should return Standard.String

      function Member_Count (Self : Object_Ptr) return Types.Unsigned_Long;
      function Member_Count (Self : Local_Ref) return Types.Unsigned_Long;
      --  Return the number of members associated with a struct, union, enum,
      --  value or except typecode.
      --  Otherwise raises BadKind.

      function Member_Name
        (Self  : Object_Ptr;
         Index : Types.Unsigned_Long) return Types.Identifier;
      function Member_Name
        (Self  : Local_Ref;
         Index : Types.Unsigned_Long) return Types.Identifier;
      --  Return the name of the indicated member of a struct, union, enum,
      --  value or except typecode. Raises Bounds if Index is too big.
      --  Raises BadKind for other typecode kinds.
      --  ???? should return Standard.String

      function Member_Type
        (Self : Object_Ptr; Index : Types.Unsigned_Long) return Object_Ptr;
      function Member_Type
        (Self : Local_Ref; Index : Types.Unsigned_Long) return Local_Ref;
      --  Return the type of the indicated member of a struct, union, enum,
      --  value or except typecode. Raises Bounds if Index is too big.
      --  Raises BadKind for other typecode kinds.

      function Member_Label
        (Self  : Object_Ptr;
         Index : Types.Unsigned_Long) return Any_Container_Ptr;
      function Member_Label
        (Self  : Object_Ptr;
         Index : Types.Unsigned_Long) return Any;
      function Member_Label
        (Self  : Local_Ref;
         Index : Types.Unsigned_Long) return Any_Container_Ptr;
      function Member_Label
        (Self  : Local_Ref;
         Index : Types.Unsigned_Long) return Any;
      --  Return the label of the indicated member of a union typecode.
      --  Raises Bounds if Index is too big.
      --  Raises BadKind for other typecode kinds.

      function Enumerator_Name
        (Self  : Object_Ptr;
         Index : Types.Unsigned_Long) return Types.Identifier;
      function Enumerator_Name
        (Self  : Local_Ref;
         Index : Types.Unsigned_Long) return Types.Identifier;
      --  Return the name of the Index'th enumerator in an enumeration.
      --  Raises Bounds if Index is too big.
      --  Raises BadKind for other typecode kinds.
      --  ??? should return Standard.String

      function Discriminator_Type (Self : Object_Ptr) return Object_Ptr;
      function Discriminator_Type (Self : Local_Ref) return Local_Ref;
      --  Return the discriminator type of a union typecode.
      --  Raises BadKind for other typecode kinds.

      function Default_Index (Self : Object_Ptr) return Types.Long;
      function Default_Index (Self : Local_Ref) return Types.Long;
      --  Return the position of the default member of a union typecode, or
      --  -1 if there is no default member.
      --  Raises BadKind for other typecode kinds.

      function Length (Self : Object_Ptr) return Types.Unsigned_Long;
      function Length (Self : Local_Ref) return Types.Unsigned_Long;
      --  Return the length associated with a string, wide_string, sequence
      --  or array typecode.
      --  Raise BadKind for other typecode kinds.

      function Content_Type (Self : Object_Ptr) return Object_Ptr;
      function Content_Type (Self : Local_Ref) return Local_Ref;
      --  Return the element type associated with a string, wide_string,
      --  sequence or array typecode.
      --  Raise BadKind for other typecode kinds.

      function Fixed_Digits (Self : Object_Ptr) return Types.Unsigned_Short;
      function Fixed_Digits (Self : Local_Ref) return Types.Unsigned_Short;
      --  Return the number of digits of a fixed typecode.
      --  Raise BadKind for other typecode kinds.

      function Fixed_Scale (Self : Object_Ptr) return Types.Short;
      function Fixed_Scale (Self : Local_Ref) return Types.Short;
      --  Return the scale of digits of a fixed typecode.
      --  Raise BadKind for other typecode kinds.

      function Member_Visibility
        (Self : Object_Ptr; Index : Types.Unsigned_Long) return Visibility;
      function Member_Visibility
        (Self : Local_Ref; Index : Types.Unsigned_Long) return Visibility;
      --  Return the visibility of the indicated member of a value typecode.
      --  Raises Bounds if Index is too big.
      --  Raises BadKind for other typecode kinds.

      function Type_Modifier (Self : Object_Ptr) return ValueModifier;
      function Type_Modifier (Self : Local_Ref) return ValueModifier;
      --  Return the type modifier of a value typecode.
      --  Raises BadKind for other typecode kinds.

      function Concrete_Base_Type (Self : Object_Ptr) return Object_Ptr;
      function Concrete_Base_Type (Self : Local_Ref) return Local_Ref;
      --  Return the concrete base type of a value typecode.
      --  Raises BadKind for other typecode kinds.

      -------------------------------------------------
      -- Supplementary accessors provided by PolyORB --
      -------------------------------------------------

      function Parameter_Count (Self : Object_Ptr) return Types.Unsigned_Long;
      function Parameter_Count (Self : Local_Ref) return Types.Unsigned_Long;
      --  Return the number of parameters in typecode Self

      function Member_Type_With_Label
        (Self : Object_Ptr; Label : Any_Container'Class) return Object_Ptr;
      function Member_Type_With_Label
        (Self : Object_Ptr; Label : Any) return Object_Ptr;
      function Member_Type_With_Label
        (Self : Local_Ref; Label : Any_Container'Class) return Local_Ref;
      function Member_Type_With_Label
        (Self : Local_Ref; Label : Any) return Local_Ref;
      --  Return the type of the member of a union typecode for the given
      --  label value.
      --  Raises BadKind for other typecode kinds.

      -------------------------------------------------------------------
      -- Low-level accessors for construction/destruction of typecodes --
      -------------------------------------------------------------------

      procedure Add_Parameter (Self : Local_Ref; Param : Any);
      --  Append Param to Self's parameter list

      procedure Finalize (Self : in out Object);
      --  Reclaim all storage associated with Self's parameters

      --  Standard typecode constants

      function TC_Null               return Local_Ref;
      function TC_Void               return Local_Ref;
      function TC_Short              return Local_Ref;
      function TC_Long               return Local_Ref;
      function TC_Long_Long          return Local_Ref;
      function TC_Unsigned_Short     return Local_Ref;
      function TC_Unsigned_Long      return Local_Ref;
      function TC_Unsigned_Long_Long return Local_Ref;
      function TC_Float              return Local_Ref;
      function TC_Double             return Local_Ref;
      function TC_Long_Double        return Local_Ref;
      function TC_Boolean            return Local_Ref;
      function TC_Char               return Local_Ref;
      function TC_Wchar              return Local_Ref;
      function TC_Octet              return Local_Ref;
      function TC_Any                return Local_Ref;
      function TC_TypeCode           return Local_Ref;

      --  Unbounded string typecodes

      function TC_String             return Local_Ref;
      function TC_Wide_String        return Local_Ref;

      --  Factories for complex typecodes

      function TC_Principal          return Local_Ref;
      function TC_Struct             return Local_Ref;
      function TC_Union              return Local_Ref;
      function TC_Enum               return Local_Ref;
      function TC_Alias              return Local_Ref;
      function TC_Except             return Local_Ref;
      function TC_Object             return Local_Ref;
      function TC_Fixed              return Local_Ref;
      function TC_Sequence           return Local_Ref;
      function TC_Array              return Local_Ref;
      function TC_Value              return Local_Ref;
      function TC_Valuebox           return Local_Ref;
      function TC_Native             return Local_Ref;
      function TC_Abstract_Interface return Local_Ref;
      function TC_Local_Interface    return Local_Ref;
      function TC_Component          return Local_Ref;
      function TC_Home               return Local_Ref;
      function TC_Event              return Local_Ref;

      --  Typecode objects for root types

      PTC_Null               : aliased Object (Tk_Null);
      PTC_Void               : aliased Object (Tk_Void);
      PTC_Short              : aliased Object (Tk_Short);
      PTC_Long               : aliased Object (Tk_Long);
      PTC_Long_Long          : aliased Object (Tk_Longlong);
      PTC_Unsigned_Short     : aliased Object (Tk_Ushort);
      PTC_Unsigned_Long      : aliased Object (Tk_Ulong);
      PTC_Unsigned_Long_Long : aliased Object (Tk_Ulonglong);
      PTC_Float              : aliased Object (Tk_Float);
      PTC_Double             : aliased Object (Tk_Double);
      PTC_Long_Double        : aliased Object (Tk_Longdouble);
      PTC_Boolean            : aliased Object (Tk_Boolean);
      PTC_Char               : aliased Object (Tk_Char);
      PTC_Wchar              : aliased Object (Tk_Widechar);
      PTC_Octet              : aliased Object (Tk_Octet);
      PTC_Any                : aliased Object (Tk_Any);
      PTC_TypeCode           : aliased Object (Tk_TypeCode);

      type Any_Array is array (Natural range <>) of Any;

      function Build_Complex_TC
        (Kind       : TCKind;
         Parameters : Any_Array) return Local_Ref;
      --  Fill Base, a typecode with an empty parameter list as created by one
      --  of the above factories, with the given Parameters.

      function Build_String_TC (Max : Types.Unsigned_Long) return Local_Ref;
      --  Build typcode for [bounded] strings

      function Build_Wstring_TC (Max : Types.Unsigned_Long) return Local_Ref;
      --  Build typcode for [bounded] wide strings

      function Build_Sequence_TC
        (Element_TC : TypeCode.Local_Ref; Max : Natural) return Local_Ref;
      --  Build typecode for bounded sequence (if Max > 0), for unbounded
      --  sequence (if Max = 0).

      procedure Initialize;

   private

      pragma Inline (Kind);

      --  Internally, the parameters of a typecode are stored using a
      --  Default_Aggregate_Content, i.e. a dynamic table of Any_Containers.

      type Local_Ref is new Smart_Pointers.Ref with null record;

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

   end TypeCode;

   --  Pre-defined TypeCode "constants".

   function TC_Null               return TypeCode.Local_Ref
     renames TypeCode.TC_Null;
   function TC_Void               return TypeCode.Local_Ref
     renames TypeCode.TC_Void;
   function TC_Short              return TypeCode.Local_Ref
     renames TypeCode.TC_Short;
   function TC_Long               return TypeCode.Local_Ref
     renames TypeCode.TC_Long;
   function TC_Long_Long          return TypeCode.Local_Ref
     renames TypeCode.TC_Long_Long;
   function TC_Unsigned_Short     return TypeCode.Local_Ref
     renames TypeCode.TC_Unsigned_Short;
   function TC_Unsigned_Long      return TypeCode.Local_Ref
     renames TypeCode.TC_Unsigned_Long;
   function TC_Unsigned_Long_Long return TypeCode.Local_Ref
     renames TypeCode.TC_Unsigned_Long_Long;
   function TC_Float              return TypeCode.Local_Ref
     renames TypeCode.TC_Float;
   function TC_Double             return TypeCode.Local_Ref
     renames TypeCode.TC_Double;
   function TC_Long_Double        return TypeCode.Local_Ref
     renames TypeCode.TC_Long_Double;
   function TC_Boolean            return TypeCode.Local_Ref
     renames TypeCode.TC_Boolean;
   function TC_Char               return TypeCode.Local_Ref
     renames TypeCode.TC_Char;
   function TC_Wchar              return TypeCode.Local_Ref
     renames TypeCode.TC_Wchar;
   function TC_Octet              return TypeCode.Local_Ref
     renames TypeCode.TC_Octet;
   function TC_Any                return TypeCode.Local_Ref
     renames TypeCode.TC_Any;
   function TC_TypeCode           return TypeCode.Local_Ref
     renames TypeCode.TC_TypeCode;
   function TC_String             return TypeCode.Local_Ref
     renames TypeCode.TC_String;
   function TC_Wide_String        return TypeCode.Local_Ref
     renames TypeCode.TC_Wide_String;

   ---------
   -- Any --
   ---------

   function "=" (Left, Right : Any_Container'Class) return Boolean;
   function "=" (Left, Right : Any) return Boolean;
   --  Equality on stored value

   function Get_Container (A : Any) return Any_Container_Ptr;
   --  Get the container designated by A

   function Get_Value (C : Any_Container'Class) return Content_Ptr;
   --  Retrieve a pointer to C's contents wrapper. This pointer shall not be
   --  permanently saved.

   procedure Set_Type
     (C  : in out Any_Container'Class;
      TC : TypeCode.Object_Ptr);
   procedure Set_Type
     (C  : in out Any_Container'Class;
      TC : TypeCode.Local_Ref);
   --  Set the type of C to TC

   procedure Set_Value (C : in out Any_Container'Class; CC : Content_Ptr);
   --  Set the contents of C to CC. CC, and any associated storage, are
   --  assumed to be externally managed and won't be deallocated by the Any
   --  management subsystem.

   procedure Finalize_Value (C : in out Any_Container'Class);
   --  Destroy the stored content wrapper for C, if non-null and non-foreign

   -----------------------
   -- Aggregate_Content --
   -----------------------

   --  Abstract interface implemented by all aggregate contents wrappers

   type Aggregate_Content is abstract new Content with private;

   function Get_Aggregate_Count
     (ACC : Aggregate_Content) return Types.Unsigned_Long is abstract;
   --  Return elements count

   procedure Set_Aggregate_Count
     (ACC    : in out Aggregate_Content;
      Length : Types.Unsigned_Long) is abstract;
   --  Ensure that ACC has appropriate storage allocated for the given element
   --  count. For the case of a fixed-size aggregate container,
   --  Constraint_Error is raised if Count does not match the proper aggregate
   --  element count.

   type Mechanism is (By_Reference, By_Value);

   function Get_Aggregate_Element
     (ACC   : not null access Aggregate_Content'Class;
      TC    : TypeCode.Local_Ref;
      Index : Types.Unsigned_Long;
      Mech  : not null access Mechanism) return Content'Class;

   function Get_Aggregate_Element
     (ACC   : not null access Aggregate_Content;
      TC    : TypeCode.Object_Ptr;
      Index : Types.Unsigned_Long;
      Mech  : not null access Mechanism) return Content'Class is abstract;
   --  Return contents wrapper for one stored element.
   --  Upon entry, if Mech is By_Reference, the caller requests access to
   --  the stored element in order to update it; if it is By_Value, the caller
   --  needs only the value of the stored element.
   --
   --  Upon exit, Mech is set to By_Value if the designated storage space is
   --  provided by the ACC content wrapper (as opposed to the actual user data
   --  space), in which case updates to the designated Content must be followed
   --  by a call to Set_Aggregate_Element to reflect the update to the original
   --  user data.
   --
   --  If Mech is By_Reference upon entry, No_Content may be returned, in which
   --  case Mech must be By_Value upon exit.

   procedure Set_Aggregate_Element
     (ACC    : in out Aggregate_Content'Class;
      TC     : TypeCode.Local_Ref;
      Index  : Types.Unsigned_Long;
      From_C : in out Any_Container'Class);
   procedure Set_Aggregate_Element
     (ACC    : in out Aggregate_Content;
      TC     : TypeCode.Object_Ptr;
      Index  : Types.Unsigned_Long;
      From_C : in out Any_Container'Class);
   --  Update contents wrapper for one stored element using value provided by
   --  From_C. This may be called only in the case of an aggregate element that
   --  is accessed by value (i.e. for which a previous Get_Aggregate_Element
   --  returned with Mech set to By_Value upon exit).
   --  A derived type of Aggregate_Content that may return elements by value
   --  this primitive.
   --
   --  This operation may leave From_C unchanged (in which case the caller is
   --  still responsible for deallocation of its contents) or make it empty
   --  (in which case this responsibility is transferred to the owner of the
   --  ACC aggregate). The latter case may only occur when From_C is not
   --  foreign.

   procedure Add_Aggregate_Element
     (ACC : in out Aggregate_Content;
      El  : Any_Container_Ptr);
   --  Add an element to ACC. This is not supported by default but may be
   --  overridden by derived types.

   -------------------
   -- Set_Any_Value --
   -------------------

   procedure Set_Any_Value (X : Types.Short;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Long;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Long_Long;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Unsigned_Short;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Unsigned_Long;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Unsigned_Long_Long;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Float;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Double;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Long_Double;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Boolean;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Char;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Wchar;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Octet;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Any;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : TypeCode.Local_Ref;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Standard.String;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.String;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Types.Wide_String;
                            C : in out Any_Container'Class);

   procedure Set_Any_Value (X : String; Bound : Positive;
                            C : in out Any_Container'Class);
   procedure Set_Any_Value (X : Wide_String; Bound : Positive;
                            C : in out Any_Container'Class);
   --  Special variants for bounded string types

   function To_Any (X : Types.Short)              return Any;
   function To_Any (X : Types.Long)               return Any;
   function To_Any (X : Types.Long_Long)          return Any;
   function To_Any (X : Types.Unsigned_Short)     return Any;
   function To_Any (X : Types.Unsigned_Long)      return Any;
   function To_Any (X : Types.Unsigned_Long_Long) return Any;
   function To_Any (X : Types.Float)              return Any;
   function To_Any (X : Types.Double)             return Any;
   function To_Any (X : Types.Long_Double)        return Any;
   function To_Any (X : Types.Boolean)            return Any;
   function To_Any (X : Types.Char)               return Any;
   function To_Any (X : Types.Wchar)              return Any;
   function To_Any (X : Types.Octet)              return Any;
   function To_Any (X : Any)                      return Any;
   function To_Any (X : TypeCode.Local_Ref)       return Any;
   function To_Any (X : Standard.String)          return Any;
   function To_Any (X : Types.String)             return Any;
   function To_Any (X : Types.Wide_String)        return Any;

   function Wrap (X : access Types.Short)              return Content'Class;
   function Wrap (X : access Types.Long)               return Content'Class;
   function Wrap (X : access Types.Long_Long)          return Content'Class;
   function Wrap (X : access Types.Unsigned_Short)     return Content'Class;
   function Wrap (X : access Types.Unsigned_Long)      return Content'Class;
   function Wrap (X : access Types.Unsigned_Long_Long) return Content'Class;
   function Wrap (X : access Types.Float)              return Content'Class;
   function Wrap (X : access Types.Double)             return Content'Class;
   function Wrap (X : access Types.Long_Double)        return Content'Class;
   function Wrap (X : access Types.Boolean)            return Content'Class;
   function Wrap (X : access Types.Char)               return Content'Class;
   function Wrap (X : access Types.Wchar)              return Content'Class;
   function Wrap (X : access Types.Octet)              return Content'Class;
   function Wrap (X : access Any)                      return Content'Class;
   function Wrap (X : access TypeCode.Local_Ref)       return Content'Class;
   function Wrap (X : access Types.String)             return Content'Class;
   function Wrap (X : access Types.Wide_String)        return Content'Class;
   function Wrap (X : access Ada.Strings.Superbounded.Super_String)
                                                       return Content'Class;
   function Wrap (X : access Ada.Strings.Wide_Superbounded.Super_String)
                                                       return Content'Class;

   function From_Any (C : Any_Container'Class) return Types.Short;
   function From_Any (C : Any_Container'Class) return Types.Long;
   function From_Any (C : Any_Container'Class) return Types.Long_Long;
   function From_Any (C : Any_Container'Class) return Types.Unsigned_Short;
   function From_Any (C : Any_Container'Class) return Types.Unsigned_Long;
   function From_Any (C : Any_Container'Class) return Types.Unsigned_Long_Long;
   function From_Any (C : Any_Container'Class) return Types.Float;
   function From_Any (C : Any_Container'Class) return Types.Double;
   function From_Any (C : Any_Container'Class) return Types.Long_Double;
   function From_Any (C : Any_Container'Class) return Types.Boolean;
   function From_Any (C : Any_Container'Class) return Types.Char;
   function From_Any (C : Any_Container'Class) return Types.Wchar;
   function From_Any (C : Any_Container'Class) return Types.Octet;
   function From_Any (C : Any_Container'Class) return Any;
   function From_Any (C : Any_Container'Class) return TypeCode.Local_Ref;
   function From_Any (C : Any_Container'Class) return Types.String;
   function From_Any (C : Any_Container'Class) return Types.Wide_String;

   function From_Any (C : Any_Container'Class) return Standard.String;
   function From_Any (C : Any_Container'Class) return Standard.Wide_String;
   --  Special variant operating on both bounded and unbounded string anys

   function From_Any (A : Any) return Types.Short;
   function From_Any (A : Any) return Types.Long;
   function From_Any (A : Any) return Types.Long_Long;
   function From_Any (A : Any) return Types.Unsigned_Short;
   function From_Any (A : Any) return Types.Unsigned_Long;
   function From_Any (A : Any) return Types.Unsigned_Long_Long;
   function From_Any (A : Any) return Types.Float;
   function From_Any (A : Any) return Types.Double;
   function From_Any (A : Any) return Types.Long_Double;
   function From_Any (A : Any) return Types.Boolean;
   function From_Any (A : Any) return Types.Char;
   function From_Any (A : Any) return Types.Wchar;
   function From_Any (A : Any) return Types.Octet;
   function From_Any (A : Any) return Any;
   function From_Any (A : Any) return TypeCode.Local_Ref;
   function From_Any (A : Any) return Types.String;
   function From_Any (A : Any) return Types.Wide_String;

   function From_Any (A : Any) return String;
   function From_Any (A : Any) return Wide_String;

   function Get_Type_Obj (A : Any) return TypeCode.Object_Ptr;
   function Get_Type_Obj (C : Any_Container'Class) return TypeCode.Object_Ptr;
   function Get_Type (A : Any) return TypeCode.Local_Ref;
   function Get_Type (C : Any_Container'Class) return TypeCode.Local_Ref;
   --  Accessors for the typecode of an Any

   function Unwind_Typedefs
     (TC : TypeCode.Object_Ptr) return TypeCode.Object_Ptr;
   function Unwind_Typedefs
     (TC : TypeCode.Local_Ref) return TypeCode.Local_Ref;
   --  Unwind any typedef (alias) from TC

   function Get_Unwound_Type (The_Any : Any) return TypeCode.Object_Ptr;
   --  Return the actual type of The_Any, after resolution of all alias levels

   procedure Set_Type (A : in out Any; TC : TypeCode.Object_Ptr);
   procedure Set_Type (A : in out Any; TC : TypeCode.Local_Ref);
   --  Not in spec : change the type of an any without changing its
   --  value : to be used carefully

   function Get_Empty_Any (Tc : TypeCode.Local_Ref) return Any;
   --  Return an empty Any (with no value but a type)

   function Get_Empty_Any_Aggregate (TC : TypeCode.Local_Ref) return Any;
   --  Return an empty Any for the given type code. The contents of TC are
   --  initialized to a default aggregate contents if TC is an aggregate.
   --  Otherwise, this is equivalent to Get_Empty_Any.

   function Is_Empty (A : Any) return Boolean;
   function Is_Empty (C : Any_Container'Class) return Boolean;
   --  True when A/C has null contents

   procedure Set_Any_Aggregate_Value (Agg_C : in out Any_Container'Class);
   --  This one is a bit special : it doesn't put any value but
   --  create the aggregate value if it does not exist.

   --  Not in spec : some methods to deal with any aggregates.
   --  What is called any aggregate is an any, made of an aggregate
   --  of values, instead of one unique. It is used for structs,
   --  unions, enums, arrays, sequences, objref, values...

   function Get_Aggregate_Count (Value : Any) return Types.Unsigned_Long;
   --  Return the number of elements in an any aggregate

   procedure Add_Aggregate_Element
     (Value   : in out Any;
      Element : Any);
   --  Adds an element to an any aggregate
   --  This element is given as a typecode but only its value is
   --  added to the aggregate

   function Get_Aggregate_Element
     (Value : Any;
      TC    : TypeCode.Local_Ref;
      Index : Types.Unsigned_Long) return Any;
   --  Gets an element in an any aggregate
   --  Return an any made of the typecode Tc and the value read in
   --  the aggregate. The first element has index 0.

   function Get_Aggregate_Element
     (Value : Any_Container'Class;
      Index : Types.Unsigned_Long) return Types.Unsigned_Long;
   function Get_Aggregate_Element
     (Value : Any;
      Index : Types.Unsigned_Long) return Types.Unsigned_Long;
   function Get_Aggregate_Element
     (Value : Any_Container'Class;
      Index : Types.Unsigned_Long) return Types.Octet;
   function Get_Aggregate_Element
     (Value : Any;
      Index : Types.Unsigned_Long) return Types.Octet;
   --  Specialized efficient versions for often-used base data types

   procedure Copy_Any_Value (Dst : Any; Src : Any);
   --  Set the value of Dest from a copy of the value of Src (as Set_Any_Value
   --  would do, but without the need to know the precise type of Src). Dest
   --  and Src must be Any's with identical typecodes.
   --  If Dest is empty, new storage is allocated for it.
   --  Note: This is not the same as Set_Any_Value (Dest, Src), which sets the
   --  value of Dest (an Any which a TC_Any type code) to be Src (not just the
   --  /value/ of Src).

   procedure Move_Any_Value (Dst : Any; Src : Any);
   --  Set the value of Dest to the value of Src, and make Src empty.
   --  Dest and Src must be Any's with identical typecodes. Dst may be empty.

   function Copy_Any (Src : Any) return Any;
   --  Create a new Any with the same typecode as Src, and set its value to
   --  a copy of Src's.

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

   function Image (TC : TypeCode.Object_Ptr) return Standard.String;
   function Image (TC : TypeCode.Local_Ref) return Standard.String;
   function Image (NV : NamedValue) return Standard.String;
   --  For debugging purposes

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
   --  To be able to carry values of different types, the second field is a
   --  pointer to an Content wrapper, which encapsulates a pointer to the
   --  actual stored data. For every elementary type that can be stored in an
   --  Any, there exsists derived type of Any_Container with appropriate
   --  accessors.
   --
   --  For complex types (with several values, like structures, arrays...),
   --  we use a special wrapper, Content_Aggregate, which has a field
   --  pointing on a list of stored objects; various methods are provided
   --  to manipulate this list.

   type Content is abstract tagged null record;
   type No_Content is new Content with null record;

   function Clone
     (CC   : No_Content;
      Into : Content_Ptr := null) return Content_Ptr;
   procedure Finalize_Value (CC : in out No_Content);
   --  These operations should never be called on a No_Content value

   ------------------
   -- The Any type --
   ------------------

   type Any is new PolyORB.Smart_Pointers.Ref with null record;

   type Any_Container is new PolyORB.Smart_Pointers.Non_Controlled_Entity with
      record
         The_Type     : TypeCode.Local_Ref;
         --  TypeCode describing the data

         The_Value    : Content_Ptr;
         --  Pointer to the stored value, wrapper in a Content.
         --  Null for an empty Any.

         Is_Finalized : Boolean := False;
         --  Set to True in Finalize, used to detect double finalization

         Foreign      : Boolean := True;
         --  If True, storage for The_Value and for the designated actual
         --  stored value was provided by the client of PolyORB.Any, and must
         --  not be deallocated upon finalization of the container. If False,
         --  the storage was provided by the Any management routines, and is
         --  deallocated when the container is destroyed.

      end record;

   procedure Finalize (Self : in out Any_Container);
   --  Finalize Container, deallocating associated resources if necessary
   --  (this is not Ada finalization, but the Finalize primitive of the
   --  Non_Controlled_Entity type).

   --  Some methods to deal with the Any fields.

   --  Deallocation of Any pointers.
   procedure Deallocate is new Ada.Unchecked_Deallocation (Any, Any_Ptr);

   -----------------------
   -- Aggregate_Content --
   -----------------------

   type Aggregate_Content is abstract new Content with null record;

   ------------------
   -- Named_Value --
   ------------------

   ARG_IN        : constant Flags := 0;
   ARG_OUT       : constant Flags := 1;
   ARG_INOUT     : constant Flags := 2;
   IN_COPY_VALUE : constant Flags := 3;

   --------------------------------------------------------------------
   -- Facilities for construction of generic elementary Any handlers --
   --------------------------------------------------------------------

   generic
      type T (<>) is private;
      with function From_Any (C : Any_Container'Class) return T;
   function From_Any_G (A : Any) return T;
   --  Default From_Any

   generic
      type T (<>) is private;
      with function TC return TypeCode.Local_Ref;
      with procedure Set_Any_Value (X : T; C : in out Any_Container'Class);
   function To_Any_G (X : T) return Any;
   --  Default To_Any

   --  Generic Any container for elementary types

   generic
      type T (<>) is private;
      Kind : TCKind;
   package Elementary_Any is

      type T_Ptr is access all T;
      type T_Content is new Content with private;

      function Clone
        (CC   : T_Content;
         Into : Content_Ptr := null) return Content_Ptr;
      procedure Finalize_Value (CC : in out T_Content);

      function From_Any (C : Any_Container'Class) return T;
      function From_Any is new From_Any_G (T, From_Any);
      pragma Inline (From_Any);

      procedure Set_Any_Value (X : T; C : in out Any_Container'Class);
      --  Note: this assumes that C has the proper typecode

      function Wrap (X : access T) return Content'Class;

      function Unchecked_Get_V (X : access T_Content) return T_Ptr;
      pragma Inline (Unchecked_Get_V);
      --  Unchecked access to the wrapper value

      function Get_Aggregate_Element
        (Value : Any_Container'Class;
         Index : Types.Unsigned_Long) return T;
      function Get_Aggregate_Element
        (Value : Any;
         Index : Types.Unsigned_Long) return T;
      --  Shortcut accessors for aggregate elements of type T

   private
      type T_Content is new Content with record
         V : T_Ptr;
      end record;
   end Elementary_Any;

end PolyORB.Any;
