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
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;
with Interfaces;
with Sequences.Unbounded;
pragma Elaborate_All (Sequences.Unbounded);

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
   Bad_Typecode  : exception;          --  bad typecode
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
                             return Unsigned_Long;

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

      function TC_Principal          return TypeCode.Object;
      function TC_Struct             return TypeCode.Object;
      function TC_Union              return TypeCode.Object;
      function TC_Enum               return TypeCode.Object;
      function TC_Alias              return TypeCode.Object;
      function TC_Except             return TypeCode.Object;
      function TC_ObjRef             return TypeCode.Object;
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
      --       Out_Of_Bounds_Index : exception;

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
      --  discriminator type. Then we'll have alternatively a
      --  member label, a member type and a member name. At least,
      --  we could have a default label. In this case, the member
      --  label would be an any containing the zero octet. So the number of
      --  parameters will be 3 * number_of_members + 3
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
      PTC_Objref             : constant Object := (Tk_Objref, null);
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
   function TC_ObjRef             return TypeCode.Object
     renames TypeCode.TC_ObjRef;
   function TC_String             return TypeCode.Object
     renames TypeCode.TC_String;
   function TC_Wide_String        return TypeCode.Object
     renames TypeCode.TC_Wide_String;


   -----------
   --  Any  --
   -----------

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

   generic
      with procedure Process (The_Any : in Any;
                              Continue : out Boolean);
   procedure Iterate_Over_Any_Elements (In_Any : in Any);


   ------------------
   --  Named_Value --
   ------------------

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

   ----------------------------
   --  Interface Repository  --
   ----------------------------

   type VersionSpec is new CORBA.String;

   --  Structs
   type StructMember is record
      Name : CORBA.Identifier;
      IDL_Type : CORBA.TypeCode.Object;
      --  Type_Def : IDLType;
   end record;

   package IDL_SEQUENCE_StructMember is
      new Sequences.Unbounded (StructMember);
   type StructMemberSeq is new IDL_SEQUENCE_StructMember.Sequence;

   --  Unions
   type UnionMember is record
      Name : CORBA.Identifier;
--      Label : CORBA.Any;
      IDL_Type : CORBA.TypeCode.Object;
      --  Type_Def : IDLType ;
   end record;

   package IDL_SEQUENCE_UnionMember is
      new Sequences.Unbounded (UnionMember);
   type UnionMemberSeq is new IDL_SEQUENCE_UnionMember.Sequence;

   --  Enums
   package IDL_SEQUENCE_Identifier is
      new Sequences.Unbounded (CORBA.Identifier);
   type EnumMemberSeq is new IDL_SEQUENCE_Identifier.Sequence;

   --  values
   type ValueMember is record
      Name :       CORBA.Identifier;
      Id :         CORBA.RepositoryId;
      Defined_In : CORBA.RepositoryId;
      Version :    CORBA.VersionSpec;
      IDL_Type :   CORBA.TypeCode.Object;
      --  Type_Def :   CORBA.IDLtype;
      IDL_Access : CORBA.Visibility;
   end record;

   package IDL_SEQUENCE_ValueMember is
      new Sequences.Unbounded (ValueMember);
   type ValueMemberSeq is new IDL_SEQUENCE_ValueMember.Sequence;

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
   --  field is an Any_Content_Ptr which is an access to any type
   --  deriving from Content. Every basic types XXX that can be carried
   --  into an Any should be associated to a child of Content (Content_XXX)
   --  which contains a field of the XXX type.
   --  For complex types (with several values, like structures, arrays...),
   --  we use a special child of Content, Content_Agregat, which has a field
   --  pointing on a list of childs of Content; various methods are provided
   --  to manipulate this list.


   type Content is abstract tagged null record;
   type Any_Content_Ptr is access Content'Class;

   type Content_Octet is new Content with
      record
         Value : CORBA.Octet;
      end record;
   type Content_Octet_Ptr is access all Content_Octet;

   type Content_Short is new Content with
      record
         Value : CORBA.Short;
      end record;
   type Content_Short_Ptr is access all Content_Short;

   type Content_Long is new Content with
      record
         Value : CORBA.Long;
      end record;
   type Content_Long_Ptr is access all Content_Long;

   type Content_Long_Long is new Content with
      record
         Value : CORBA.Long_Long;
      end record;
   type Content_Long_Long_Ptr is access all Content_Long_Long;

   type Content_UShort is new Content with
      record
         Value : CORBA.Unsigned_Short;
      end record;
   type Content_UShort_Ptr is access all Content_UShort;

   type Content_ULong is new Content with
      record
         Value : CORBA.Unsigned_Long;
      end record;
   type Content_ULong_Ptr is access all Content_ULong;

   type Content_ULong_Long is new Content with
      record
         Value : CORBA.Unsigned_Long_Long;
      end record;
   type Content_ULong_Long_Ptr is access all Content_ULong_Long;

   type Content_Boolean is new Content with
      record
         Value : CORBA.Boolean;
      end record;
   type Content_Boolean_Ptr is access all Content_Boolean;

   type Content_Char is new Content with
      record
         Value : CORBA.Char;
      end record;
   type Content_Char_Ptr is access all Content_Char;

   type Content_Wchar is new Content with
      record
         Value : CORBA.Wchar;
      end record;
   type Content_Wchar_Ptr is access all Content_Wchar;

   type Content_String is new Content with
      record
         Value : CORBA.String;
      end record;
   type Content_String_Ptr is access all Content_String;

   type Content_Wide_String is new Content with
      record
         Value : CORBA.Wide_String;
      end record;
   type Content_Wide_String_Ptr is access all Content_Wide_String;

   type Content_Float is new Content with
      record
         Value : CORBA.Float;
      end record;
   type Content_Float_Ptr is access all Content_Float;

   type Content_Double is new Content with
      record
         Value : CORBA.Double;
      end record;
   type Content_Double_Ptr is access all Content_Double;

   type Content_Long_Double is new Content with
      record
         Value : CORBA.Long_Double;
      end record;
   type Content_Long_Double_Ptr is access all Content_Long_Double;

   type Content_TypeCode is new Content with
      record
         Value : CORBA.TypeCode.Object;
      end record;
   type Content_TypeCode_Ptr is access all Content_TypeCode;

   type Content_Any is new Content with
      record
         Value : CORBA.Any;
      end record;
   type Content_Any_Ptr is access all Content_Any;

   --  a list of any
   type Content_Cell;
   type Content_List is access all Content_Cell;
   type Content_Cell is record
      The_Value : Any_Content_Ptr := null;
      Next : Content_List := null;
   end record;

   --  for complex types that could be defined in Idl, descendants of
   --  content_agregat will be used.
   --  complex types include Struct, Union, Enum, Sequence,
   --  Array, Except, Fixed, Value, Valuebox, Abstract_Interface.
   --  Here is the way the content_list is used in each case :
   --     - for Struct, Except : the elements are the values of each
   --  field in the order of the declaration
   --     - for Union : the value of the switch element comes
   --  first. Then come all the values of the corresponding fields
   --     - for Enum : an unsigned_long corresponding to the position
   --  of the value in the declaration is the only element
   --     - for Sequence, Array : all the elements of the sequence
   --  or array, one by one
   --     - for Fixed : FIXME
   --     - for Value : FIXME
   --     - for Valuebox : FIXME
   --     - for Abstract_Interface : FIXME
   type Content_Agregat is new Content with
      record
         Value : Content_List := null;
      end record;
   type Content_Agregat_Ptr is access all Content_Agregat;

   type Content_Struct is new Content_Agregat with null record;
   type Content_Struct_Ptr is access all Content_Struct;

   type Content_Except is new Content_Agregat with null record;
   type Content_Except_Ptr is access all Content_Except;

   type Content_Union is new Content_Agregat with null record;
   type Content_Union_Ptr is access all Content_Union;

   type Content_Enum is new Content_Agregat with null record;
   type Content_Enum_Ptr is access all Content_Enum;

   type Content_Sequence is new Content_Agregat with null record;
   type Content_Sequence_Ptr is access all Content_Sequence;

   type Content_Array is new Content_Agregat with null record;
   type Content_Array_Ptr is access all Content_Array;

   type Content_Fixed is new Content_Agregat with null record;
   type Content_Fixed_Ptr is access all Content_Fixed;

   type Content_Value is new Content_Agregat with null record;
   type Content_Value_Ptr is access all Content_Value;

   type Content_Valuebox is new Content_Agregat with null record;
   type Content_Valuebox_Ptr is access all Content_Valuebox;

   type Content_Abstract_Interface is new Content_Agregat with null record;
   type Content_Abstract_Interface_Ptr is
      access all Content_Abstract_Interface;

   function Agregate_Count
     (Cl : in Content_List)
      return CORBA.Long;
   --  returns the number of elements of the Content_List list

   type Any is
     record
        The_Value : Any_Content_Ptr;
        The_Type  : CORBA.TypeCode.Object;
     end record;


   ------------------
   --  Named_Value --
   ------------------

   ARG_IN :        constant Flags := 0;
   ARG_OUT :       constant Flags := 1;
   ARG_INOUT :     constant Flags := 2;
   IN_COPY_VALUE : constant Flags := 3;

end CORBA;
