------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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
with Ada.Unchecked_Deallocation;

with Interfaces;

with PolyORB.Any;
with PolyORB.Types;

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
   --  type    String             is
   --    new Ada.Strings.Unbounded.Unbounded_String;
   subtype String is PolyORB.Types.String;
   type    Wide_String        is
     new Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   --  Pointers on the previous types

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

   --  ... and deallocation method for each pointer type

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

   function To_CORBA_String
     (Source : Standard.String)
     return CORBA.String;

   function To_Standard_String
     (Source : CORBA.String)
     return Standard.String;

   Null_String : constant CORBA.String := CORBA.String
     (Ada.Strings.Unbounded.To_Unbounded_String (""));

   function To_CORBA_Wide_String
     (Source : Standard.Wide_String)
     return CORBA.Wide_String;

   function To_Standard_Wide_String
     (Source : CORBA.Wide_String)
     return Standard.Wide_String;

   Null_Wide_String : constant CORBA.Wide_String := CORBA.Wide_String
     (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (""));


   -------------
   --  Types  --
   -------------

   --  type Identifier is new CORBA.String;
   subtype Identifier is PolyORB.Types.Identifier;
   Null_Identifier : constant Identifier := Identifier (Null_String);

   function "=" (X, Y : Identifier) return Boolean
     renames PolyORB.Types."=";

   function To_Standard_String (S : Identifier) return Standard.String
     renames PolyORB.Types.To_Standard_String;
   function To_CORBA_String (S : Standard.String) return Identifier
     renames PolyORB.Types.To_PolyORB_String;

   subtype RepositoryId is PolyORB.Types.RepositoryId;
   Null_RepositoryId : constant RepositoryId := RepositoryId (Null_String);

   function "=" (X, Y : RepositoryId) return Boolean
     renames PolyORB.Types."=";

   function To_Standard_String
     (S : RepositoryId)
     return Standard.String;
   function To_CORBA_String
     (S : Standard.String)
     return RepositoryId;

   type ScopedName is new CORBA.String;
   Null_ScopedName : constant ScopedName := ScopedName (Null_String);

   function To_Standard_String
     (S : ScopedName)
     return Standard.String;
   function To_CORBA_String
     (S : Standard.String)
     return ScopedName;

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

   Adapter_Already_Exists : exception;
   Invalid_Policy         : exception;
   Wrong_Policy           : exception;
   Servant_Already_Active : exception;
   Object_Already_Active  : exception;
   Servant_Not_Active     : exception;
   Object_Not_Active      : exception;
   Adapter_Inactive       : exception;

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

   type Adapter_Already_Exists_Members is new System_Exception_Members
     with null record;
   type Invalid_Policy_Members         is new System_Exception_Members
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

   subtype Any is PolyORB.Any.Any;
   subtype Any_Ptr is PolyORB.Any.Any;
   function Image (A : Any) return Standard.String
     renames PolyORB.Any.Image;

   ---------------
   -- TypeCodes --
   ---------------

   --  See spec CORBA V2.3, Ada Langage Mapping 1.33

   subtype TCKind is PolyORB.Any.TCKind;

   subtype ValueModifier is PolyORB.Any.ValueModifier;
   VTM_NONE        : constant ValueModifier;
   VTM_CUSTOM      : constant ValueModifier;
   VTM_ABSTRACT    : constant ValueModifier;
   VTM_TRUNCATABLE : constant ValueModifier;

   subtype Visibility is PolyORB.Any.Visibility;

   PRIVATE_MEMBER : constant Visibility;
   PUBLIC_MEMBER  : constant Visibility;

   package TypeCode renames PolyORB.Any.TypeCode;
   --  XXX This is not actually correct, because D.A.TypeCode
   --  uses 'pure' (member-less) Ada exceptions, while CORBA.TypeCode
   --  is expected to use CORBA exceptions (with members).

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
   function To_Any (Item : in CORBA.Float)        return Any;
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
   function From_Any (Item : in Any) return CORBA.Float;
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

   --  type Flags is new CORBA.Unsigned_Long;
   subtype Flags is PolyORB.Any.Flags;

   ARG_IN :        constant Flags;
   ARG_OUT :       constant Flags;
   ARG_INOUT :     constant Flags;
   IN_COPY_VALUE : constant Flags;

   subtype NamedValue is PolyORB.Any.NamedValue;

   function Image (NV : NamedValue) return Standard.String;
   --  For debugging purposes.

private

   VTM_NONE        : constant ValueModifier := PolyORB.Any.VTM_NONE;
   VTM_CUSTOM      : constant ValueModifier := PolyORB.Any.VTM_CUSTOM;
   VTM_ABSTRACT    : constant ValueModifier := PolyORB.Any.VTM_ABSTRACT;
   VTM_TRUNCATABLE : constant ValueModifier := PolyORB.Any.VTM_TRUNCATABLE;

   PRIVATE_MEMBER : constant Visibility := PolyORB.Any.PRIVATE_MEMBER;
   PUBLIC_MEMBER  : constant Visibility := PolyORB.Any.PRIVATE_MEMBER;

   pragma Inline (To_Any);
   pragma Inline (From_Any);
   pragma Inline (To_Standard_String);
   pragma Inline (To_CORBA_String);
   pragma Inline (Set_Any_Value);
   pragma Inline (Get_Aggregate_Count);
   pragma Inline (Get_Aggregate_Element);

   ------------------
   --  Named_Value --
   ------------------

   ARG_IN :        constant Flags := PolyORB.Any.ARG_IN;
   ARG_OUT :       constant Flags := PolyORB.Any.ARG_OUT;
   ARG_INOUT :     constant Flags := PolyORB.Any.ARG_INOUT;
   IN_COPY_VALUE : constant Flags := PolyORB.Any.IN_COPY_VALUE;

end CORBA;
