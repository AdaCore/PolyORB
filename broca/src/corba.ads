------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                C O R B A                                 --
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

with Ada.Exceptions;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Deallocation;
with Interfaces;
with Broca.Locks;

package CORBA is

   --  CORBA Module: In order to prevent names defined with the CORBA
   --  specification from clashing with names in programming languages and
   --  other software systems, all names defined by CORBA are treated as if
   --  they were defined with a module named CORBA.

   --  Each IDL data type is mapped to a native data type via the
   --  appropriate language mapping. The following definitions may
   --  differ. See the mapping specification for more information.

   type    Short              is new Interfaces.Integer_16;
   type    Long               is new Interfaces.Integer_32;
   type    Long_Long          is new Interfaces.Integer_64;
   type    Unsigned_Short     is new Interfaces.Unsigned_16;
   type    Unsigned_Long      is new Interfaces.Unsigned_32;
   type    Unsigned_Long_Long is new Interfaces.Unsigned_64;
   type    Float              is new Interfaces.IEEE_Float_32;
   type    Double             is new Interfaces.IEEE_Float_64;
   type    Long_Double        is new Interfaces.IEEE_Extended_Float;
   subtype Char               is Standard.Character;
   subtype Wchar              is Standard.Wide_Character;
   type    Octet              is new Interfaces.Unsigned_8;
   subtype Boolean            is Standard.Boolean;
   type    String             is new Ada.Strings.Unbounded.Unbounded_String;
   type    Wide_String        is
     new Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   --  here are the type definitions for pointers on the previous types

   type    Short_Ptr              is access all Short;
   type    Long_Ptr               is access all Long;
   type    Long_Long_Ptr          is access all Long_Long;
   type    Unsigned_Short_Ptr     is access all Unsigned_Short;
   type    Unsigned_Long_Ptr      is access all Unsigned_Long;
   type    Unsigned_Long_Long_Ptr is access all Unsigned_Long_Long;
   type    Float_Ptr              is access all Float;
   type    Double_Ptr             is access all Double;
   type    Long_Double_Ptr        is access all Long_Double;
   type    Char_Ptr               is access all Char;
   type    Wchar_Ptr              is access all Wchar;
   type    Octet_Ptr              is access all Octet;
   type    Boolean_Ptr            is access all Boolean;
   type    String_Ptr             is access all String;
   type    Wide_String_Ptr        is access all Wide_String;

   --  and the deallocation method for each pointer type

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Short, Short_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Long, Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Long_Long, Long_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unsigned_Short, Unsigned_Short_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unsigned_Long, Unsigned_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Unsigned_Long_Long, Unsigned_Long_Long_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Float, Float_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Double, Double_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Long_Double, Long_Double_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Char, Char_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Wchar, Wchar_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Octet, Octet_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Boolean, Boolean_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (String, String_Ptr);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Wide_String, Wide_String_Ptr);

   ---------------------------------
   -- String conversion functions --
   ---------------------------------

   function To_CORBA_String (Source : Standard.String)
                             return CORBA.String;

   function To_Standard_String (Source : CORBA.String)
                                return Standard.String;

   Null_String : constant CORBA.String := CORBA.String
     (Ada.Strings.Unbounded.To_Unbounded_String (""));

   function To_CORBA_Wide_String (Source : Standard.Wide_String)
                                  return CORBA.Wide_String;

   function To_Standard_Wide_String (Source : CORBA.Wide_String)
                                     return Standard.Wide_String;

   Null_Wide_String : constant CORBA.Wide_String := CORBA.Wide_String
     (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (""));


   -------------
   --  Types  --
   -------------

   type Identifier is new CORBA.String;
   Null_Identifier : constant Identifier := Identifier (Null_String);

   type RepositoryId is new CORBA.String;
   Null_RepositoryId : constant RepositoryId := RepositoryId (Null_String);

   type ScopedName is new CORBA.String;
   Null_ScopedName : constant ScopedName := ScopedName (Null_String);



   ----------------
   -- Exceptions --
   ----------------

   type IDL_Exception_Members is abstract tagged null record;
   --  Base type for all corba exception members. A member is a record
   --  attached to an exception that allows the programmer to pass
   --  arguments when an exception is raised. The default Member record is
   --  abstract and empty but all other records will inherit from it.

   --  inconsistent spec, this line is not defined in package CORBA
   --  but it is used in other packages.
   subtype Exception_Occurrence is Ada.Exceptions.Exception_Occurrence;


   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out IDL_Exception_Members) is abstract;
   --  This method return the member corresponding to an exception
   --  occurence This methos must be redefined for each new member
   --  type. That's why it is declared abstract.

   --  Free method associated to the type Idl_Exception_Members_Ptr

   type Completion_Status is (Completed_Yes, Completed_No, Completed_Maybe);
   --  Type used for characterize the state of an exception It is defined
   --  by the CORBA specification.

   type Exception_Type is (No_Exception, System_Exception, User_Exception);
   --  Type used for characterize exceptions.  It is defined by the CORBA
   --  specification.

   type System_Exception_Members is new CORBA.IDL_Exception_Members with
      record
         Minor     : CORBA.Unsigned_Long;
         Completed : Completion_Status;
      end record;
   --  Member type for System exceptions.  It is defined by the CORBA
   --  specification

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members);
   --  This method return the member corresponding to a system exception
   --  occurence.

   Unknown       : exception;          --  the unknown exception
   Bad_Param     : exception;          --  an invalid parameter was passed
   No_Memory     : exception;          --  dynamic memory allocation failure
   Imp_Limit     : exception;          --  violated implementation limit
   Comm_Failure  : exception;          --  communication failure
   Inv_Objref    : exception;          --  invalid object reference
   No_Permission : exception;          --  no permission for attempted op.
   Internal      : exception;          --  ORB internal error
   Marshal       : exception;          --  error marshalling param/result
   Initialization_Failure : exception; --  ORB initialization failure
   No_Implement  : exception;          --  operation implementation unavailable
   Bad_TypeCode  : exception;          --  bad typecode
   Bad_Operation : exception;          --  invalid operation
   No_Resources  : exception;          --  insufficient resources for req.
   No_Response   : exception;          --  response to request not available
   Persist_Store : exception;          --  persistent storage failure
   Bad_Inv_Order : exception;          --  routine invocations out of order
   Transient     : exception;          --  transient failure - reissue request
   Free_Mem      : exception;          --  cannot free memory
   Inv_Ident     : exception;          --  invalid identifier syntax
   Inv_Flag      : exception;          --  invalid flag was specified
   Intf_Repos    : exception;          --  error accessing interface repository
   Bad_Context   : exception;          --  error processing context object
   Obj_Adapter   : exception;          --  failure detected by object adapter
   Data_Conversion : exception;        --  data conversion error
   Object_Not_Exist       : exception;
   Transaction_Required   : exception;
   Transaction_Rolledback : exception;
   Invalid_Transaction    : exception;

   type Unknown_Members         is new System_Exception_Members
     with null record;

   type Bad_Param_Members       is new System_Exception_Members
     with null record;
   type No_Memory_Members       is new System_Exception_Members
     with null record;
   type Imp_Limit_Members       is new System_Exception_Members
     with null record;
   type Comm_Failure_Members    is new System_Exception_Members
     with null record;
   type Inv_Objref_Members      is new System_Exception_Members
     with null record;
   type No_Permission_Members   is new System_Exception_Members
     with null record;
   type Internal_Members        is new System_Exception_Members
     with null record;
   type Marshal_Members         is new System_Exception_Members
     with null record;
   type Initialization_Failure_Members is new System_Exception_Members
     with null record;
   type No_Implement_Members    is new System_Exception_Members
     with null record;
   type Bad_Typecode_Members    is new System_Exception_Members
     with null record;
   type Bad_Operation_Members   is new System_Exception_Members
     with null record;
   type No_Resources_Members    is new System_Exception_Members
     with null record;
   type No_Response_Members     is new System_Exception_Members
     with null record;
   type Persist_Store_Members   is new System_Exception_Members
     with null record;
   type Bad_Inv_Order_Members   is new System_Exception_Members
     with null record;
   type Transient_Members       is new System_Exception_Members
     with null record;
   type Free_Mem_Members        is new System_Exception_Members
     with null record;
   type Inv_Ident_Members       is new System_Exception_Members
     with null record;
   type Inv_Flag_Members        is new System_Exception_Members
     with null record;
   type Intf_Repos_Members      is new System_Exception_Members
     with null record;
   type Bad_Context_Members     is new System_Exception_Members
     with null record;
   type Obj_Adapter_Members     is new System_Exception_Members
     with null record;
   type Data_Conversion_Members is new System_Exception_Members
     with null record;
   type Object_Not_Exist_Members       is new System_Exception_Members
     with null record;
   type Transaction_Required_Members   is new System_Exception_Members
     with null record;
   type Transaction_Rolledback_Members is new System_Exception_Members
     with null record;
   type Invalid_Transaction_Members    is new System_Exception_Members
     with null record;


   -----------------------------
   -- exceptions for the ORB  --
   -----------------------------

   --  Defined in 4.7
   type PolicyType is new CORBA.Unsigned_Long;

   --  excpetion PolicyError
   PolicyError : exception;

   type PolicyErrorCode is new Short;

   type PolicyError_Members is new CORBA.IDL_Exception_Members
     with record
        Reason : PolicyErrorCode;
     end record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PolicyError_Members);

   --  exception InvalidName
   InvalidName : exception;
   type InvalidName_Members is new CORBA.IDL_Exception_Members
     with null record;
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InvalidName_Members);

   --  exception InconsistentTypeCode
   InconsistentTypeCode : exception;
   type InconsistentTypeCode_Members is new CORBA.IDL_Exception_Members
     with null record;
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InconsistentTypeCode_Members);


   -------------------------
   -- types and constants --
   -------------------------
   type ServiceType is new Unsigned_Short;
   type ServiceOption is new Unsigned_Long;
   type ServiceDetailType is new Unsigned_Long;

   Security : constant ServiceType := 1;


   -----------
   --  Any  --
   -----------

   type Any is private;
   type Any_Ptr is access all Any;
   --  The end of this part is after the typecode part;


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

   type ValueModifier is new Short;
   VTM_NONE : constant ValueModifier;
   VTM_CUSTOM : constant ValueModifier;
   VTM_ABSTRACT : constant ValueModifier;
   VTM_TRUNCATABLE : constant ValueModifier;

   type Visibility is new Short;
   PRIVATE_MEMBER : constant Visibility;
   PUBLIC_MEMBER : constant Visibility;


   package TypeCode is

      --  Spec  --
      ------------
      type Object is private;
      type Object_Ptr is access all Object;

      Bounds : exception;
      type Bounds_Members is new CORBA.IDL_Exception_Members with null record;

      --  gives the member associated with a bounds exception
      procedure Get_Members
        (From : in Ada.Exceptions.Exception_Occurrence;
         To    : out Bounds_Members);

      BadKind : exception;
      type BadKind_Members is new CORBA.IDL_Exception_Members with null record;

      --  gives the member associated with a badKind exception
      procedure Get_Members
        (From : in Ada.Exceptions.Exception_Occurrence;
         To    : out BadKind_Members);

      --  equality between two typecodes
      function "=" (Left, Right : in Object) return Boolean;

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
                   return CORBA.RepositoryId;

      --  returns the name associated with a typecode in case its kind is
      --  objref, struct, union, enum, alias, value, valueBox, native,
      --  abstract_interface or except. Raises badKind else.
      function Name (Self : in Object)
                     return CORBA.Identifier;

      --  returns the number of members associated with a typecode in
      --  case its kind is struct, union, enum, value or except.
      --  Raises badKind else.
      function Member_Count (Self : in Object)
                             return CORBA.Unsigned_Long;

      --  returns the name of a given member associated with a typecode
      --  in case its kind is struct, union, enum, value or except.
      --  Raises badKind else.
      --  If there is not enough members, raises bounds.
      function Member_Name (Self  : in Object;
                            Index : in CORBA.Unsigned_Long)
                            return CORBA.Identifier;

      --  returns the type of a given member associated with a typecode
      --  in case its kind is struct, union, value or except.
      --  Raises badKind else.
      --  If there is not enough members, raises bounds.
      function Member_Type
        (Self  : in Object;
         Index : in CORBA.Unsigned_Long) return Object;

      --  returns the label of a given member associated with a typecode
      --  in case its kind is union.
      --  Raises badKind else.
      --  If there is not enough members, raises bounds.
      function Member_Label
          (Self  : in Object;
           Index : in CORBA.Unsigned_Long) return CORBA.Any;

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
                              return CORBA.Long;

      --  returns the length associated with a typecode
      --  in case its kind is string, wide_string, sequence or array.
      --  Raises badKind else.
      function Length (Self : in Object)
                       return CORBA.Unsigned_Long;

      --  returns the content type associated with a typecode
      --  in case its kind is sequence, array, valueBox or alias.
      --  Raises badKind else.
      function Content_Type (Self : in Object) return Object;

      --  returns the number of digits associated with a typecode
      --  in case its kind is fixed.
      --  Raises badKind else.
      function Fixed_Digits (Self : in Object)
                             return CORBA.Unsigned_Short;

      --  returns the scale associated with a typecode
      --  in case its kind is fixed.
      --  Raises badKind else.
      function Fixed_Scale (Self : in Object)
                            return CORBA.Short;

      --  returns the visibility associated with a member of a typecode
      --  in case its kind is value.
      --  Raises badKind else.
      --  If there is not enough members, raises bounds.
      function Member_Visibility
        (Self  : in Object;
         Index : in CORBA.Unsigned_Long) return Visibility;

      --  returns the type modifier associated with a typecode
      --  in case its kind is value.
      --  Raises badKind else.
      function Type_Modifier (Self : in Object)
                              return CORBA.ValueModifier;

      --  returns the concrete base type associated with a typecode
      --  in case its kind is value.
      --  Raises badKind else.
      function Concrete_Base_Type (Self : in Object)
                                   return Object;

      --  Not in spec  --
      -------------------
      --  returns the type of a given member associated with an
      --  union typecode for a given label. The index is the index
      --  of the member among the members associated with Label. The
      --  other members are not taken into account
      --  Raises badKind if Self is not an union typecode.
      --  If there is not enough members, raises bounds.
      function Member_Type_With_Label
        (Self  : in Object;
         Label : in Any;
         Index : in CORBA.Unsigned_Long) return Object;

      --  returns the number of members associated with a typecode of
      --  kind union for a given label.
      --  Raises badKind if Self is not an union typecode.
      function Member_Count_With_Label
        (Self : in Object;
         Label : in Any)
         return CORBA.Unsigned_Long;

      --  returns the parameter nb index in the list of Self's
      --  parameters. Raises Out_Of_Bounds_Index exception if
      --  this parameter does not exist
      function Get_Parameter (Self : in Object;
                              Index : in CORBA.Unsigned_Long)
                              return Any;

      --  adds the parameter Param in the list of Self's
      --  parameters.
      procedure Add_Parameter (Self  : in out Object;
                               Param : in CORBA.Any);

      --  Sets the kind of a typecode
      --  By the way, erases all parameters
      procedure Set_Kind (Self : out Object;
                          Kind : in CORBA.TCKind);

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

      --  more complex ones. Creates "empty" typecodes
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

      --  returns the number of parameters of Self
      function Parameter_Count (Self : in Object)
                                return Unsigned_Long;

   private
      --       --  implementation defined
      --       Out_Of_Bounds_Index : exception;  --  FIXME : remove it ?

      --  list of parameters (which are some any)
      type Cell;
      type Cell_Ptr is access all Cell;
      type Cell is record
         Parameter : CORBA.Any;
         Next : Cell_Ptr;
      end record;

      --  type code implementation
      type Object is
         record
            Kind : CORBA.TCKind := Tk_Void;
            Parameters : Cell_Ptr := null;
         end record;

      --  here is the way the typecodes are coded :
      --    for null, void, short, long, long_long,
      --  unsigned_short, unsigned_long, unsigned_long_long, float,
      --  double, long_double, boolean, char, Wchar, octet, any,
      --  typeCode, Principal parameters = null
      --    for Objref, Struct, union, enum, alias, value, valueBox,
      --  native, abstract_interface and except, the first parameter
      --  will contain the name and the second the repository Id
      --    The objref, native and abstract_interface don't have
      --  any other parameter.
      --    for the struct and the except, the next parameters will
      --  be alternatively a type and a name. So the number of
      --  parameters will be 2 * number_of_members + 2
      --    for the union, the third parameter will be the
      --  discriminator type. The fourth will be the index of the
      --  default case as a long. If there's no default case, then
      --  you'll find -1. Then we'll have alternatively a
      --  member label, a member type and a member name. At least,
      --  for the default label, the member label will contain a
      --  valid label but without any semantic significance.
      --  So the number of parameters will be 3 * number_of_members + 4
      --    for the enum, the next parameters will be names of the
      --  different members. So the number of parameters will be
      --  number_of_members + 2
      --    for the alias, the third parameter is its content type
      --    for the value, the third parameter will be a type
      --  modifier and the fourth one a concrete base type. The next
      --  parameters will be alternatively a visibility, a type and
      --  a name. So the number of parameters will be
      --  3 * number_of_members + 4.
      --    for the valueBox, the third parameter is the content type
      --    for the string and wide_string, the only parameter will
      --  be the length of the string. Its value will be 0 in case of
      --  unbounded strings or wide strings.
      --    for the sequence and the array, the first parameter will
      --  be the length of the sequence or the array and the second
      --  the content type. As for strings, an unbounded sequence will
      --  have a length of 0.
      --    for the fixed, the first parameter will be the digits
      --  number and the second the scale.

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
   --  function TC_Object is in CORBA.Object.

   -----------
   --  Any  --
   -----------

   --  This is the returned exception in case of dynamic invocation
   UnknownUserException : exception;
   type UnknownUserException_Members is
     new CORBA.IDL_Exception_Members with record
        IDL_Exception : Any;
     end record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out UnknownUserException_Members);

   function "=" (Left, Right : in Any) return Boolean;

   function Equal (Left, Right : in Any) return Boolean
     renames "=";

   function To_Any (Item : in Short)              return Any;
   function To_Any (Item : in Long)               return Any;
   function To_Any (Item : in Long_Long)          return Any;
   function To_Any (Item : in Unsigned_Short)     return Any;
   function To_Any (Item : in Unsigned_Long)      return Any;
   function To_Any (Item : in Unsigned_Long_Long) return Any;
   function To_Any (Item : in Float)              return Any;
   function To_Any (Item : in Double)             return Any;
   function To_Any (Item : in Long_Double)        return Any;
   function To_Any (Item : in Boolean)            return Any;
   function To_Any (Item : in Char)               return Any;
   function To_Any (Item : in Wchar)              return Any;
   function To_Any (Item : in Octet)              return Any;
   function To_Any (Item : in Any)                return Any;
   function To_Any (Item : in TypeCode.Object)    return Any;
   function To_Any (Item : in CORBA.String)       return Any;
   function To_Any (Item : in CORBA.Wide_String)  return Any;

   function From_Any (Item : in Any) return Short;
   function From_Any (Item : in Any) return Long;
   function From_Any (Item : in Any) return Long_Long;
   function From_Any (Item : in Any) return Unsigned_Short;
   function From_Any (Item : in Any) return Unsigned_Long;
   function From_Any (Item : in Any) return Unsigned_Long_Long;
   function From_Any (Item : in Any) return Float;
   function From_Any (Item : in Any) return Double;
   function From_Any (Item : in Any) return Long_Double;
   function From_Any (Item : in Any) return Boolean;
   function From_Any (Item : in Any) return Char;
   function From_Any (Item : in Any) return Wchar;
   function From_Any (Item : in Any) return Octet;
   function From_Any (Item : in Any) return Any;
   function From_Any (Item : in Any) return TypeCode.Object;
   function From_Any (Item : in Any) return CORBA.String;
   function From_Any (Item : in Any) return CORBA.Wide_String;

   function Get_Type (The_Any : in Any) return TypeCode.Object;

   --  not in spec : returns the most precise type of an Any. It
   --  means that it removes any alias level
   function Get_Precise_Type (The_Any : in Any) return TypeCode.Object;

   --  not in spec : change the type of an any without changing its
   --  value : to be used carefully
   procedure Set_Type (The_Any : in out Any;
                       The_Type : in TypeCode.Object);

   generic
      with procedure Process (The_Any : in Any;
                              Continue : out Boolean);
   procedure Iterate_Over_Any_Elements (In_Any : in Any);

   --  returns  an empty Any (with no value but a type)
   function Get_Empty_Any (Tc : TypeCode.Object) return Any;

   --  Not in spec : return true if the Any has a value, false
   --  if it is an empty one
   function Is_Empty (Any_Value : in CORBA.Any) return Boolean;

   --  These functions allows the user to set the value of an any
   --  directly if he knows its kind. It a function is called on a
   --  bad kind of any, a BAD_TYPECODE exception will be raised
   --  Note that the Any can be empty. In this case, the value
   --  will be created
   --  Should never be called outside the broca.cdr package
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Octet);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Short);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Long);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Long_Long);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Unsigned_Short);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Unsigned_Long);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Unsigned_Long_Long);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Boolean);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Char);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Wchar);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.String);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Wide_String);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Float);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Double);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Long_Double);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.TypeCode.Object);
   procedure Set_Any_Value (Any_Value : in out CORBA.Any;
                            Value : in CORBA.Any);

   --  This one is a bit special : it doesn't put any value but
   --  create the aggregate value if it does not exist.
   procedure Set_Any_Aggregate_Value (Any_Value : in out CORBA.Any);

   --  Not in spec : some methods to deal with any aggregates.
   --  What is called any aggregate is an any, made of an aggregate
   --  of values, instead of one unique. It is used for structs,
   --  unions, enums, arrays, sequences, objref, values...

   --  returns the number of elements in an any aggregate
   function Get_Aggregate_Count (Value : Any) return CORBA.Unsigned_Long;

   --  Adds an element to an any aggregate
   --  This element is given as a typecode but only its value is
   --  added to the aggregate
   procedure Add_Aggregate_Element (Value : in out Any;
                                    Element : in Any);

   --  Gets an element in an any agregate
   --  returns an any made of the typecode Tc and the value read in
   --  the aggregate
   function Get_Aggregate_Element (Value : Any;
                                   Tc : CORBA.TypeCode.Object;
                                   Index : CORBA.Unsigned_Long)
                                   return Any;

   --  returns an empty any aggregate
   --  puts its type to Tc
   function Get_Empty_Any_Aggregate (Tc : CORBA.TypeCode.Object)
                                     return Any;

   -----------------
   --  NamedValue --
   -----------------

   type Flags is new CORBA.Unsigned_Long;

   ARG_IN :        constant Flags;
   ARG_OUT :       constant Flags;
   ARG_INOUT :     constant Flags;
   IN_COPY_VALUE : constant Flags;

   type NamedValue is record
      Name :      CORBA.Identifier;
      Argument :  CORBA.Any;
      Arg_Modes : CORBA.Flags;
   end record;

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


   type Content is abstract tagged null record;
   type Any_Content_Ptr is access all Content'Class;
   Null_Content_Ptr : constant Any_Content_Ptr := null;
   type Any_Content_Ptr_Ptr is access all Any_Content_Ptr;
   Null_Content_Ptr_Ptr : constant Any_Content_Ptr_Ptr := null;

   --  This function duplicates its argument and give back
   --  a deep copy of it.
   --  It is actually overridden for every subtype of Content
   function Duplicate (Object : access Content)
                       return Any_Content_Ptr;

   --  Frees a Content_Ptr
   --  It is overridden for aggregates since those have to
   --  deallocate all the list of their elements
   procedure Deallocate (Object : access Content);

   --  Frees an Any_Content_Ptr
   procedure Deallocate_Any_Content is new Ada.Unchecked_Deallocation
     (Content'Class, Any_Content_Ptr);
   --  Frees an Any_Content_Ptr_Ptr
   procedure Deallocate_Any_Content_Ptr is new Ada.Unchecked_Deallocation
     (Any_Content_Ptr, Any_Content_Ptr_Ptr);

   type Content_Octet is new Content with
      record
         Value : CORBA.Octet_Ptr;
      end record;
   type Content_Octet_Ptr is access all Content_Octet;
   procedure Deallocate (Object : access Content_Octet);
   function Duplicate (Object : access Content_Octet)
                       return Any_Content_Ptr;

   type Content_Short is new Content with
      record
         Value : CORBA.Short_Ptr;
      end record;
   type Content_Short_Ptr is access all Content_Short;
   procedure Deallocate (Object : access Content_Short);
   function Duplicate (Object : access Content_Short)
                       return Any_Content_Ptr;

   type Content_Long is new Content with
      record
         Value : CORBA.Long_Ptr;
      end record;
   type Content_Long_Ptr is access all Content_Long;
   procedure Deallocate (Object : access Content_Long);
   function Duplicate (Object : access Content_Long)
                       return Any_Content_Ptr;

   type Content_Long_Long is new Content with
      record
         Value : CORBA.Long_Long_Ptr;
      end record;
   type Content_Long_Long_Ptr is access all Content_Long_Long;
   procedure Deallocate (Object : access Content_Long_Long);
   function Duplicate (Object : access Content_Long_Long)
                       return Any_Content_Ptr;

   type Content_UShort is new Content with
      record
         Value : CORBA.Unsigned_Short_Ptr;
      end record;
   type Content_UShort_Ptr is access all Content_UShort;
   procedure Deallocate (Object : access Content_UShort);
   function Duplicate (Object : access Content_UShort)
                       return Any_Content_Ptr;

   type Content_ULong is new Content with
      record
         Value : CORBA.Unsigned_Long_Ptr;
      end record;
   type Content_ULong_Ptr is access all Content_ULong;
   procedure Deallocate (Object : access Content_ULong);
   function Duplicate (Object : access Content_ULong)
                       return Any_Content_Ptr;

   type Content_ULong_Long is new Content with
      record
         Value : CORBA.Unsigned_Long_Long_Ptr;
      end record;
   type Content_ULong_Long_Ptr is access all Content_ULong_Long;
   procedure Deallocate (Object : access Content_ULong_Long);
   function Duplicate (Object : access Content_ULong_Long)
                       return Any_Content_Ptr;

   type Content_Boolean is new Content with
      record
         Value : CORBA.Boolean_Ptr;
      end record;
   type Content_Boolean_Ptr is access all Content_Boolean;
   procedure Deallocate (Object : access Content_Boolean);
   function Duplicate (Object : access Content_Boolean)
                       return Any_Content_Ptr;

   type Content_Char is new Content with
      record
         Value : CORBA.Char_Ptr;
      end record;
   type Content_Char_Ptr is access all Content_Char;
   procedure Deallocate (Object : access Content_Char);
   function Duplicate (Object : access Content_Char)
                       return Any_Content_Ptr;

   type Content_Wchar is new Content with
      record
         Value : CORBA.Wchar_Ptr;
      end record;
   type Content_Wchar_Ptr is access all Content_Wchar;
   procedure Deallocate (Object : access Content_Wchar);
   function Duplicate (Object : access Content_Wchar)
                       return Any_Content_Ptr;

   type Content_String is new Content with
      record
         Value : CORBA.String_Ptr;
      end record;
   type Content_String_Ptr is access all Content_String;
   procedure Deallocate (Object : access Content_String);
   function Duplicate (Object : access Content_String)
                       return Any_Content_Ptr;

   type Content_Wide_String is new Content with
      record
         Value : CORBA.Wide_String_Ptr;
      end record;
   type Content_Wide_String_Ptr is access all Content_Wide_String;
   procedure Deallocate (Object : access Content_Wide_String);
   function Duplicate (Object : access Content_Wide_String)
                       return Any_Content_Ptr;

   type Content_Float is new Content with
      record
         Value : CORBA.Float_Ptr;
      end record;
   type Content_Float_Ptr is access all Content_Float;
   procedure Deallocate (Object : access Content_Float);
   function Duplicate (Object : access Content_Float)
                       return Any_Content_Ptr;

   type Content_Double is new Content with
      record
         Value : CORBA.Double_Ptr;
      end record;
   type Content_Double_Ptr is access all Content_Double;
   procedure Deallocate (Object : access Content_Double);
   function Duplicate (Object : access Content_Double)
                       return Any_Content_Ptr;

   type Content_Long_Double is new Content with
      record
         Value : CORBA.Long_Double_Ptr;
      end record;
   type Content_Long_Double_Ptr is access all Content_Long_Double;
   procedure Deallocate (Object : access Content_Long_Double);
   function Duplicate (Object : access Content_Long_Double)
                       return Any_Content_Ptr;

   type Content_TypeCode is new Content with
      record
         Value : CORBA.TypeCode.Object_Ptr;
      end record;
   type Content_TypeCode_Ptr is access all Content_TypeCode;
   procedure Deallocate (Object : access Content_TypeCode);
   function Duplicate (Object : access Content_TypeCode)
                       return Any_Content_Ptr;

   type Content_Any is new Content with
      record
         Value : CORBA.Any_Ptr;
      end record;
   type Content_Any_Ptr is access all Content_Any;
   procedure Deallocate (Object : access Content_Any);
   function Duplicate (Object : access Content_Any)
                       return Any_Content_Ptr;

   --  the content_TypeCode type is defined inside the TypeCode package
   --  However, the corresponding deallocate function is here
   --  This is due to the fact that the TypeCode.Object type is private
   --  in package TypeCode which implies that Deallocate sould be private
   --  too if it were declared there.
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (TypeCode.Object, TypeCode.Object_Ptr);

   --  a list of any
   type Content_Cell;
   type Content_List is access all Content_Cell;
   type Content_Cell is record
      The_Value : Any_Content_Ptr := null;
      Next : Content_List := null;
   end record;
   Null_Content_List : constant Content_List := null;
   function Duplicate (List : in Content_List) return Content_List;
   procedure Deep_Deallocate (List : in out Content_List);
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Content_Cell, Content_List);

   --  for complex types that could be defined in Idl
   --  content_aggregate will be used.
   --  complex types include Struct, Union, Enum, Sequence,
   --  Array, Except, Fixed, Value, Valuebox, Abstract_Interface.
   --  Here is the way the content_list is used in each case
   --  (See CORBA V2.3 - 15.3) :
   --     - for Struct, Except : the elements are the values of each
   --  field in the order of the declaration
   --     - for Union : the value of the switch element comes
   --  first. Then come all the values of the corresponding fields
   --     - for Enum : an unsigned_long corresponding to the position
   --  of the value in the declaration is the only element
   --     - for Array : all the elements of the array, one by one.
   --     - for Sequence : the length first and then all the elements
   --  of the sequence, one by one.
   --     - for Fixed : FIXME
   --     - for Value : FIXME
   --     - for Valuebox : FIXME
   --     - for Abstract_Interface : FIXME
   type Content_Aggregate is new Content with record
      Value : Content_List := null;
   end record;
   type Content_Aggregate_Ptr is access all Content_Aggregate;
   function Duplicate (Object : access Content_Aggregate)
                       return Any_Content_Ptr;
   procedure Deallocate (Object : access Content_Aggregate);

   type Natural_Ptr is access Natural;
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Natural, Natural_Ptr);

   --  a lock for the Any
   type Rw_Lock_Type_Ptr is access all Broca.Locks.Rw_Lock_Type;
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Broca.Locks.Rw_Lock_Type, Rw_Lock_Type_Ptr);

   --  The actual Any type
   --  The first two fields are clear, the third one tells whether
   --  the Any has a semantic of reference or of value, the fourth
   --  one counts the number of references on the field The_Value
   --  and the last one is a lock to manage thread safe features.
   type Any is new Ada.Finalization.Controlled with record
      The_Value : Any_Content_Ptr_Ptr;
      The_Type  : CORBA.TypeCode.Object;
      As_Reference : Boolean := False;
      Ref_Counter : Natural_Ptr;
      Any_Lock : Rw_Lock_Type_Ptr;
   end record;

   --  Some methods to deal with the Any fields.
   --  These are the only way to deal with the fields if you want to
   --  stay thread safe
   --  Apart from the management of locks, these methods do not
   --  make any test. So use them carefully
   procedure Set_Value (Obj : in out Any; The_Value : in Any_Content_Ptr);
   procedure Set_Counter (Obj : in out Any; The_Counter : in Natural_Ptr);
   function Get_Value (Obj : Any) return Any_Content_Ptr;
   function Get_Value_Ptr (Obj : Any) return Any_Content_Ptr_Ptr;
   function Get_Counter (Obj : Any) return Natural_Ptr;

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

end CORBA;
