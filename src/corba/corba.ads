------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/corba/corba.ads#32 $

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces;

with PolyORB.Any;
with PolyORB.Types;
with PolyORB.Exceptions;

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
   --  subtype String is PolyORB.Types.String;
   type String is new PolyORB.Types.String;

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

   -----------
   -- Types --
   -----------

   --  type Identifier is new CORBA.String;
   type Identifier is new PolyORB.Types.Identifier;
   function To_CORBA_String
     (Source : Standard.String)
     return Identifier;

   Null_Identifier : constant Identifier := Identifier (Null_String);

--    function "=" (X, Y : Identifier) return Boolean
--      renames PolyORB.Types."=";

--    function To_Standard_String (S : Identifier) return Standard.String
--      renames PolyORB.Types.To_Standard_String;
--    function To_CORBA_String (S : Standard.String) return Identifier
--      renames PolyORB.Types.To_PolyORB_String;

   type RepositoryId is new CORBA.String;
   Null_RepositoryId : constant RepositoryId := RepositoryId (Null_String);

--    function "=" (X, Y : RepositoryId) return Boolean
--      renames PolyORB.Types."=";

--    function To_Standard_String
--      (S : RepositoryId)
--      return Standard.String;
--    function To_CORBA_String
--      (S : Standard.String)
--      return RepositoryId;

   type ScopedName is new CORBA.String;
   Null_ScopedName : constant ScopedName := ScopedName (Null_String);

--    function To_Standard_String
--      (S : ScopedName)
--      return Standard.String;
--    function To_CORBA_String
--      (S : Standard.String)
--      return ScopedName;

   ----------------
   -- Exceptions --
   ----------------

   subtype IDL_Exception_Members is PolyORB.Exceptions.Exception_Members;
   --  Base type for all CORBA exception members. A member is a record
   --  attached to an exception that allows the programmer to pass
   --  arguments when an exception is raised. The default Member record is
   --  abstract and empty but all other records will inherit from it.

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out IDL_Exception_Members)
      is abstract;
   --  This method return the member corresponding to an exception
   --  occurence This method must be redefined for each new member
   --  type. That's why it is declared abstract.

   type Completion_Status is new PolyORB.Exceptions.Completion_Status;
   --     (Completed_Yes,
   --      Completed_No,
   --      Completed_Maybe);
   --  Type used for characterize the state of an exception.

   function From_Any
     (Item : PolyORB.Any.Any)
     return CORBA.Completion_Status;

   function To_Any
     (Item : CORBA.Completion_Status)
     return PolyORB.Any.Any;

   type Exception_Type is (No_Exception, System_Exception, User_Exception);
   --  Type used for characterize exceptions.

   -----------------------
   -- System Exceptions --
   -----------------------

   Unknown                 : exception; --  unknown exception
   Bad_Param               : exception; --  an invalid parameter was passed
   No_Memory               : exception; --  dynamic memory allocation failure
   Imp_Limit               : exception; --  violated implementation limit
   Comm_Failure            : exception; --  communication failure
   Inv_Objref              : exception; --  invalid object reference
   No_Permission           : exception; --  no permission for attempted op.
   Internal                : exception; --  ORB internal error
   Marshal                 : exception; --  error marshalling param/result
   Initialize              : exception; --  ORB initialization failure
   No_Implement            : exception; --  operation impleme. unavailable
   Bad_TypeCode            : exception; --  bad typecode
   Bad_Operation           : exception; --  invalid operation
   No_Resources            : exception; --  insufficient resources for req.
   No_Response             : exception; --  response to request not available
   Persist_Store           : exception; --  persistent storage failure
   Bad_Inv_Order           : exception; --  routine invocations out of order
   Transient               : exception; --  transient failure - reissue request
   Free_Mem                : exception; --  cannot free memory
   Inv_Ident               : exception; --  invalid identifier syntax
   Inv_Flag                : exception; --  invalid flag was specified
   Intf_Repos              : exception; --  error accessing intf. repository
   Bad_Context             : exception; --  error processing context object
   Obj_Adapter             : exception; --  failure detected by object adapter
   Data_Conversion         : exception; --  data conversion error
   Object_Not_Exist        : exception; --  non-existent object, delete ref.
   Transaction_Required    : exception; --  transaction required
   Transaction_Rolledback  : exception; --  transaction rolled back
   Invalid_Transaction     : exception; --  invalid transaction
   Inv_Policy              : exception; --  invalid policy
   Codeset_Incompatible    : exception; --  incompatible code set
   Rebind                  : exception; --  rebind needed
   Timeout                 : exception; --  operation timed out
   Transaction_Unavailable : exception; --  no transaction
   Transaction_Mode        : exception; --  invalid transaction mode
   Bad_Qos                 : exception; --  bad quality of service

   Initialization_Failure  : exception renames Initialize;
   --  Note: this exception is defined in Ada mapping specification,
   --  not in CORBA specification.

   type System_Exception_Members is new PolyORB.Exceptions.Exception_Members
     with record
        Minor     : CORBA.Unsigned_Long;
        Completed : CORBA.Completion_Status;
     end record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members);
   --  Return the member corresponding to a system exception occurence.

   procedure Raise_From_Error
     (Error : in out PolyORB.Exceptions.Error_Container);

   procedure Raise_System_Exception
     (Excp      : in Ada.Exceptions.Exception_Id;
      Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_System_Exception);

   procedure Raise_Unknown
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Unknown);

   procedure Raise_Bad_Param
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Bad_Param);

   procedure Raise_No_Memory
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_No_Memory);

   procedure Raise_Imp_Limit
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Imp_Limit);

   procedure Raise_Comm_Failure
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Comm_Failure);

   procedure Raise_Inv_Objref
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Inv_Objref);

   procedure Raise_No_Permission
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_No_Permission);

   procedure Raise_Internal
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Internal);

   procedure Raise_Marshal
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Marshal);

   procedure Raise_Initialize
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Initialize);

   procedure Raise_No_Implement
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_No_Implement);

   procedure Raise_Bad_TypeCode
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Bad_Operation
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Bad_Operation);

   procedure Raise_No_Resources
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_No_Resources);

   procedure Raise_No_Response
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_No_Response);

   procedure Raise_Persist_Store
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Persist_Store);

   procedure Raise_Bad_Inv_Order
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Bad_Inv_Order);

   procedure Raise_Transient
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Transient);

   procedure Raise_Free_Mem
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Free_Mem);

   procedure Raise_Inv_Ident
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Inv_Ident);

   procedure Raise_Inv_Flag
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Inv_Flag);

   procedure Raise_Intf_Repos
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Intf_Repos);

   procedure Raise_Bad_Context
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Bad_Context);

   procedure Raise_Obj_Adapter
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Obj_Adapter);

   procedure Raise_Data_Conversion
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Data_Conversion);

   procedure Raise_Object_Not_Exist
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Object_Not_Exist);

   procedure Raise_Transaction_Required
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Transaction_Required);

   procedure Raise_Transaction_Rolledback
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Transaction_Rolledback);

   procedure Raise_Invalid_Transaction
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Invalid_Transaction);

   procedure Raise_Inv_Policy
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Inv_Policy);

   procedure Raise_Codeset_Incompatible
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Codeset_Incompatible);

   procedure Raise_Rebind
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Rebind);

   procedure Raise_Timeout
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Timeout);

   procedure Raise_Transaction_Unavailable
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Transaction_Unavailable);

   procedure Raise_Transaction_Mode
     (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Transaction_Mode);

   procedure Raise_Bad_Qos
        (Excp_Memb : in System_Exception_Members);
   pragma No_Return (Raise_Bad_Qos);

   procedure Raise_Initialization_Failure
     (Excp_Memb : in System_Exception_Members)
     renames Raise_Initialize;

   Default_Sys_Member : constant System_Exception_Members
     := System_Exception_Members'(Minor => 0,
                                  Completed => CORBA.Completed_No);

   type Unknown_Members                 is new System_Exception_Members
     with null record;
   type Bad_Param_Members               is new System_Exception_Members
     with null record;
   type No_Memory_Members               is new System_Exception_Members
     with null record;
   type Imp_Limit_Members               is new System_Exception_Members
     with null record;
   type Comm_Failure_Members            is new System_Exception_Members
     with null record;
   type Inv_Objref_Members              is new System_Exception_Members
     with null record;
   type No_Permission_Members           is new System_Exception_Members
     with null record;
   type Internal_Members                is new System_Exception_Members
     with null record;
   type Marshal_Members                 is new System_Exception_Members
     with null record;
   type Initialize_Members              is new System_Exception_Members
     with null record;
   type No_Implement_Members            is new System_Exception_Members
     with null record;
   type Bad_Typecode_Members            is new System_Exception_Members
     with null record;
   type Bad_Operation_Members           is new System_Exception_Members
     with null record;
   type No_Resources_Members            is new System_Exception_Members
     with null record;
   type No_Response_Members             is new System_Exception_Members
     with null record;
   type Persist_Store_Members           is new System_Exception_Members
     with null record;
   type Bad_Inv_Order_Members           is new System_Exception_Members
     with null record;
   type Transient_Members               is new System_Exception_Members
     with null record;
   type Free_Mem_Members                is new System_Exception_Members
     with null record;
   type Inv_Ident_Members               is new System_Exception_Members
     with null record;
   type Inv_Flag_Members                is new System_Exception_Members
     with null record;
   type Intf_Repos_Members              is new System_Exception_Members
     with null record;
   type Bad_Context_Members             is new System_Exception_Members
     with null record;
   type Obj_Adapter_Members             is new System_Exception_Members
     with null record;
   type Data_Conversion_Members         is new System_Exception_Members
     with null record;
   type Object_Not_Exist_Members        is new System_Exception_Members
     with null record;
   type Transaction_Required_Members    is new System_Exception_Members
     with null record;
   type Transaction_Rolledback_Members  is new System_Exception_Members
     with null record;
   type Invalid_Transaction_Members     is new System_Exception_Members
     with null record;
   type Inv_Policy_Members              is new System_Exception_Members
     with null record;
   type Codeset_Incompatible_Members    is new System_Exception_Members
     with null record;
   type Rebind_Members                  is new System_Exception_Members
     with null record;
   type Timeout_Members                 is new System_Exception_Members
     with null record;
   type Transaction_Unavailable_Members is new System_Exception_Members
     with null record;
   type Transaction_Mode_Members        is new System_Exception_Members
     with null record;
   type Bad_Qos_Members                 is new System_Exception_Members
     with null record;

   subtype Initialization_Failure_Members is Initialize_Members;

   ------------
   -- Policy --
   ------------

   --  Defined in 4.7

   type PolicyType is new CORBA.Unsigned_Long;

   type PolicyErrorCode is new CORBA.Short;

   BAD_POLICY               : constant PolicyErrorCode := PolicyErrorCode'(0);
   UNSUPPORTED_POLICY       : constant PolicyErrorCode := PolicyErrorCode'(1);
   BAD_POLICY_TYPE          : constant PolicyErrorCode := PolicyErrorCode'(2);
   BAD_POLICY_VALUE         : constant PolicyErrorCode := PolicyErrorCode'(3);
   UNSUPPORTED_POLICY_VALUE : constant PolicyErrorCode := PolicyErrorCode'(4);

   --------------------
   -- ORB Exceptions --
   --------------------

   --  exception PolicyError
   PolicyError : exception;

   type PolicyError_Members is new CORBA.IDL_Exception_Members with record
      Reason : PolicyErrorCode;
   end record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out PolicyError_Members);

   --  exception InvalidName
   InvalidName : exception;

   type InvalidName_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members);

   --  exception InconsistentTypeCode
   InconsistentTypeCode : exception;

   type InconsistentTypeCode_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out InconsistentTypeCode_Members);

   -------------------------
   -- Types and constants --
   -------------------------

   type ServiceType       is new CORBA.Unsigned_Short;
   type ServiceOption     is new CORBA.Unsigned_Long;
   type ServiceDetailType is new CORBA.Unsigned_Long;

   Security : constant ServiceType := 1;

   ---------
   -- Any --
   ---------

   subtype Any is PolyORB.Any.Any;
   subtype Any_Ptr is PolyORB.Any.Any;
   function Image (A : Any) return Standard.String
     renames PolyORB.Any.Image;

   ---------------
   -- TypeCodes --
   ---------------

   --  See spec CORBA V2.3, Ada Langage Mapping 1.33

   subtype TCKind is PolyORB.Any.TCKind;

   --  Accessors functions on TCKind values

   function Tk_Null return TCKind renames PolyORB.Any.Tk_Null;
   function Tk_Void return TCKind renames PolyORB.Any.Tk_Void;
   function Tk_Short return TCKind renames PolyORB.Any.Tk_Short;
   function Tk_Long return TCKind renames PolyORB.Any.Tk_Long;
   function Tk_Ushort return TCKind renames PolyORB.Any.Tk_Ushort;
   function Tk_Ulong return TCKind renames PolyORB.Any.Tk_Ulong;
   function Tk_Float return TCKind renames PolyORB.Any.Tk_Float;
   function Tk_Double return TCKind renames PolyORB.Any.Tk_Double;
   function Tk_Boolean return TCKind renames PolyORB.Any.Tk_Boolean;
   function Tk_Char return TCKind renames PolyORB.Any.Tk_Char;
   function Tk_Octet return TCKind renames PolyORB.Any.Tk_Octet;
   function Tk_Any return TCKind renames PolyORB.Any.Tk_Any;
   function Tk_TypeCode return TCKind renames PolyORB.Any.Tk_TypeCode;
   function Tk_Principal return TCKind renames PolyORB.Any.Tk_Principal;
   function Tk_Objref return TCKind renames PolyORB.Any.Tk_Objref;
   function Tk_Struct return TCKind renames PolyORB.Any.Tk_Struct;
   function Tk_Union return TCKind renames PolyORB.Any.Tk_Union;
   function Tk_Enum return TCKind renames PolyORB.Any.Tk_Enum;
   function Tk_String return TCKind renames PolyORB.Any.Tk_String;
   function Tk_Sequence return TCKind renames PolyORB.Any.Tk_Sequence;
   function Tk_Array return TCKind renames PolyORB.Any.Tk_Array;
   function Tk_Alias return TCKind renames PolyORB.Any.Tk_Alias;
   function Tk_Except return TCKind renames PolyORB.Any.Tk_Except;
   function Tk_Longlong return TCKind renames PolyORB.Any.Tk_Longlong;
   function Tk_Ulonglong return TCKind renames PolyORB.Any.Tk_Ulonglong;
   function Tk_Longdouble return TCKind renames PolyORB.Any.Tk_Longdouble;
   function Tk_Widechar return TCKind renames PolyORB.Any.Tk_Widechar;
   function Tk_Wstring return TCKind renames PolyORB.Any.Tk_Wstring;
   function Tk_Fixed return TCKind renames PolyORB.Any.Tk_Fixed;
   function Tk_Value return TCKind renames PolyORB.Any.Tk_Value;
   function Tk_Valuebox return TCKind renames PolyORB.Any.Tk_Valuebox;
   function Tk_Native return TCKind renames PolyORB.Any.Tk_Native;
   function Tk_Abstract_Interface return TCKind
     renames PolyORB.Any.Tk_Abstract_Interface;

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
   function Get_Unwound_Type (The_Any : in Any) return TypeCode.Object;

   --  not in spec : change the type of an any without changing its
   --  value : to be used carefully
   procedure Set_Type
     (The_Any  : in out Any;
      The_Type : in     TypeCode.Object);

   generic
      with procedure Process
        (The_Any  : in  Any;
         Continue : out Boolean);
   procedure Iterate_Over_Any_Elements (In_Any : in Any);

   --  returns an empty Any (with no value but a type)
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

   --  XXX investigate the last comment, does it means these functions
   --  are useless for the CORBA personality ???

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
   procedure Set_Any_Aggregate_Value
     (Any_Value : in out CORBA.Any);

   --  Not in spec : some methods to deal with any aggregates.
   --  What is called any aggregate is an any, made of an aggregate
   --  of values, instead of one unique. It is used for structs,
   --  unions, enums, arrays, sequences, objref, values...

   --  returns the number of elements in an any aggregate
   function Get_Aggregate_Count
     (Value : Any)
     return CORBA.Unsigned_Long;

   --  Adds an element to an any aggregate
   --  This element is given as a typecode but only its value is
   --  added to the aggregate
   procedure Add_Aggregate_Element
     (Value : in out Any;
      Element : in Any);

   --  Gets an element in an any agregate
   --  returns an any made of the typecode Tc and the value read in
   --  the aggregate
   function Get_Aggregate_Element
     (Value : Any;
      Tc    : CORBA.TypeCode.Object;
      Index : CORBA.Unsigned_Long)
      return Any;

   --  returns an empty any aggregate
   --  puts its type to Tc
   function Get_Empty_Any_Aggregate
     (Tc : CORBA.TypeCode.Object)
      return Any;

   ----------------
   -- NamedValue --
   ----------------

   type Flags is new CORBA.Unsigned_Long;
   --  subtype Flags is PolyORB.Any.Flags;

   ARG_IN :        constant Flags;
   ARG_OUT :       constant Flags;
   ARG_INOUT :     constant Flags;
   IN_COPY_VALUE : constant Flags;

   --  subtype NamedValue is PolyORB.Any.NamedValue;
   type NamedValue is record
      Name      : Identifier;
      Argument  : Any;
      Arg_Modes : Flags;
   end record;

--    function To_PolyORB_NV (CNV : NamedValue)
--      return PolyORB.Any.NamedValue;

--    function To_CORBA_NV (PNV : PolyORB.Any.NamedValue)
--      return NamedValue;

   function To_PolyORB_NV is new Ada.Unchecked_Conversion
     (NamedValue, PolyORB.Any.NamedValue);
   function To_CORBA_NV is new Ada.Unchecked_Conversion
     (PolyORB.Any.NamedValue, NamedValue);

   function Image (NV : NamedValue) return Standard.String;
   --  For debugging purposes.

   ------------------
   -- RepositoryId --
   ------------------

   function Is_Equivalent (RI1, RI2 : RepositoryId)
     return Boolean;

   function Is_Equivalent (RI1, RI2 : Standard.String)
     return Boolean;
   --  Return True if, and only if, RI1 and RI2 denote the same
   --  repository entity (a case-insensitive string match).

private

   VTM_NONE        : constant ValueModifier := PolyORB.Any.VTM_NONE;
   VTM_CUSTOM      : constant ValueModifier := PolyORB.Any.VTM_CUSTOM;
   VTM_ABSTRACT    : constant ValueModifier := PolyORB.Any.VTM_ABSTRACT;
   VTM_TRUNCATABLE : constant ValueModifier := PolyORB.Any.VTM_TRUNCATABLE;

   PRIVATE_MEMBER : constant Visibility := PolyORB.Any.PRIVATE_MEMBER;
   PUBLIC_MEMBER  : constant Visibility := PolyORB.Any.PUBLIC_MEMBER;

   pragma Inline (To_Any);
   pragma Inline (From_Any);
   pragma Inline (To_CORBA_String);
   pragma Inline (Set_Any_Value);
   pragma Inline (Get_Aggregate_Count);
   pragma Inline (Get_Aggregate_Element);
   pragma Inline (To_PolyORB_NV);
   pragma Inline (To_CORBA_NV);

   -----------------
   -- Named_Value --
   -----------------

   ARG_IN :        constant Flags := Flags (PolyORB.Any.ARG_IN);
   ARG_OUT :       constant Flags := Flags (PolyORB.Any.ARG_OUT);
   ARG_INOUT :     constant Flags := Flags (PolyORB.Any.ARG_INOUT);
   IN_COPY_VALUE : constant Flags := Flags (PolyORB.Any.IN_COPY_VALUE);

end CORBA;
