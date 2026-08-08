------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;
with PolyORB.Utils.Unchecked_Deallocation;

with Interfaces;

with PolyORB.Any;
with PolyORB.Errors;
with PolyORB.Types;

package CORBA is

   --  CORBA Module: In order to prevent names defined with the CORBA
   --  specification from clashing with names in programming languages and
   --  other software systems, all names defined by CORBA are treated as if
   --  they were defined with a module named CORBA.

   --  Each IDL data type is mapped to a native data type via the
   --  appropriate language mapping. The following definitions may
   --  differ. See the mapping specification for more information.

   --  Note that some of the names required by the CORBA standard duplicate
   --  names in Standard, and GNAT warns about that, so we suppress the
   --  warnings below.

   type    Short              is new Interfaces.Integer_16;
   type    Long               is new Interfaces.Integer_32;
   type    Long_Long          is new Interfaces.Integer_64;
   type    Unsigned_Short     is new Interfaces.Unsigned_16;
   type    Unsigned_Long      is new Interfaces.Unsigned_32;
   type    Unsigned_Long_Long is new Interfaces.Unsigned_64;
   pragma Warnings (Off); -- redefinition of entity in Standard
   type    Float              is new Interfaces.IEEE_Float_32;
   pragma Warnings (On);
   type    Double             is new Interfaces.IEEE_Float_64;
   type    Long_Double        is new Interfaces.IEEE_Extended_Float;
   subtype Char               is Standard.Character;
   subtype Wchar              is Standard.Wide_Character;
   type    Octet              is new Interfaces.Unsigned_8;
   pragma Warnings (Off); -- redefinition of entity in Standard
   subtype Boolean            is Standard.Boolean;

   type String is new PolyORB.Types.String;

   type    Wide_String        is
     new Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   pragma Warnings (On);

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

   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Short,


      Name   => Short_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Long,

      Name   => Long_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Long_Long,

      Name   => Long_Long_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Unsigned_Short,

      Name   => Unsigned_Short_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Unsigned_Long,

      Name   => Unsigned_Long_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Unsigned_Long_Long,

      Name   => Unsigned_Long_Long_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Float,

      Name   => Float_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Double,

      Name   => Double_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Long_Double,

      Name   => Long_Double_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Char,

      Name   => Char_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Wchar,

      Name   => Wchar_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Octet,

      Name   => Octet_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Boolean,

      Name   => Boolean_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => String,

      Name   => String_Ptr);
   procedure Deallocate is new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Wide_String,

      Name   => Wide_String_Ptr);

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

   type Identifier is new PolyORB.Types.Identifier;
   function To_CORBA_String (Source : Standard.String) return Identifier;

   Null_Identifier : constant Identifier := Identifier (Null_String);

   type RepositoryId is new CORBA.String;
   Null_RepositoryId : constant RepositoryId := RepositoryId (Null_String);

   type ScopedName is new CORBA.String;
   Null_ScopedName : constant ScopedName := ScopedName (Null_String);

   ----------------
   -- Exceptions --
   ----------------

   subtype IDL_Exception_Members is PolyORB.Errors.Exception_Members;
   --  Base type for all CORBA exception members. A member is a record attached
   --  to an exception that allows the programmer to pass arguments when an
   --  exception is raised. The default Member record is abstract and empty but
   --  all other records will inherit from it.

   --  procedure Get_Members
   --    (From : Ada.Exceptions.Exception_Occurrence;
   --     To   : out IDL_Exception_Members) is abstract;
   --  Return the member corresponding to an exception occurence.
   --  There is no abstract dispatching operation; this is defined only
   --  for derived types of Exception_Members.

   type Completion_Status is new PolyORB.Errors.Completion_Status;
   --  Characterization the state of execution when an exception occurs

   type Exception_Type is (No_Exception, System_Exception, User_Exception);
   --  Type used for characterize exceptions.

   -----------------------
   -- System Exceptions --
   -----------------------

   OMGVMCID : constant CORBA.Unsigned_Long := 16#4f4d0000#;
   --  The CORBA speficiations mandate that the actual value for the
   --  minor field of system exceptions is obtained by or-ing the
   --  value with this constant, for all values defined in CORBA A.5.

   Unknown                 : exception; --  unknown exception
   Bad_Param               : exception; --  an invalid parameter was passed
   No_Memory               : exception; --  dynamic memory allocation failure
   Imp_Limit               : exception; --  violated implementation limit
   Comm_Failure            : exception; --  communication failure
   Inv_Objref              : exception; --  invalid object reference
   No_Permission           : exception; --  no permission for attempted op
   Internal                : exception; --  ORB internal error
   Marshal                 : exception; --  error marshalling param/result
   Initialize              : exception; --  ORB initialization failure
   No_Implement            : exception; --  operation impleme. unavailable
   Bad_TypeCode            : exception; --  bad typecode
   Bad_Operation           : exception; --  invalid operation
   No_Resources            : exception; --  insufficient resources for req
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
   Object_Not_Exist        : exception; --  non-existent object or deleted ref
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
   --  Implementation Note: this exception is defined in the Ada mapping
   --  specification, not in the CORBA specification itself.

   type System_Exception_Members is new PolyORB.Errors.Exception_Members
     with record
        Minor     : CORBA.Unsigned_Long;
        Completed : CORBA.Completion_Status;
     end record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out System_Exception_Members);
   --  Return the member corresponding to a system exception occurence.

   --  The following procedures are used to raise specific system exceptions

   procedure Raise_Unknown
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Unknown);

   procedure Raise_Bad_Param
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Bad_Param);

   procedure Raise_No_Memory
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_No_Memory);

   procedure Raise_Imp_Limit
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Imp_Limit);

   procedure Raise_Comm_Failure
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Comm_Failure);

   procedure Raise_Inv_Objref
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Inv_Objref);

   procedure Raise_No_Permission
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_No_Permission);

   procedure Raise_Internal
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Internal);

   procedure Raise_Marshal
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Marshal);

   procedure Raise_Initialize
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Initialize);

   procedure Raise_No_Implement
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_No_Implement);

   procedure Raise_Bad_TypeCode
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Bad_TypeCode);

   procedure Raise_Bad_Operation
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Bad_Operation);

   procedure Raise_No_Resources
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_No_Resources);

   procedure Raise_No_Response
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_No_Response);

   procedure Raise_Persist_Store
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Persist_Store);

   procedure Raise_Bad_Inv_Order
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Bad_Inv_Order);

   procedure Raise_Transient
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Transient);

   procedure Raise_Free_Mem
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Free_Mem);

   procedure Raise_Inv_Ident
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Inv_Ident);

   procedure Raise_Inv_Flag
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Inv_Flag);

   procedure Raise_Intf_Repos
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Intf_Repos);

   procedure Raise_Bad_Context
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Bad_Context);

   procedure Raise_Obj_Adapter
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Obj_Adapter);

   procedure Raise_Data_Conversion
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Data_Conversion);

   procedure Raise_Object_Not_Exist
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Object_Not_Exist);

   procedure Raise_Transaction_Required
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Transaction_Required);

   procedure Raise_Transaction_Rolledback
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Transaction_Rolledback);

   procedure Raise_Invalid_Transaction
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Invalid_Transaction);

   procedure Raise_Inv_Policy
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Inv_Policy);

   procedure Raise_Codeset_Incompatible
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Codeset_Incompatible);

   procedure Raise_Rebind
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Rebind);

   procedure Raise_Timeout
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Timeout);

   procedure Raise_Transaction_Unavailable
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Transaction_Unavailable);

   procedure Raise_Transaction_Mode
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Transaction_Mode);

   procedure Raise_Bad_Qos
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "");
   pragma No_Return (Raise_Bad_Qos);

   procedure Raise_Initialization_Failure
     (Excp_Memb : System_Exception_Members;
      Message   : Standard.String := "")
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

   type SetOverrideType is (SET_OVERRIDE, ADD_OVERRIDE);

   --------------------
   -- ORB Exceptions --
   --------------------

   --  exception PolicyError

   PolicyError : exception;

   type PolicyError_Members is new CORBA.IDL_Exception_Members with record
      Reason : PolicyErrorCode;
   end record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out PolicyError_Members);

   --  exception InvalidName

   InvalidName : exception;

   type InvalidName_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members);

   --  exception InconsistentTypeCode

   InconsistentTypeCode : exception;

   type InconsistentTypeCode_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
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

   type Any is new PolyORB.Any.Any;

   ---------------
   -- TypeCodes --
   ---------------

   --  See spec CORBA V2.3, Ada Langage Mapping 1.33

   subtype TCKind is PolyORB.Any.TCKind;

   --  Accessors functions on TCKind values
   --
   --  Implementation Note: these function are defined to allow
   --  visibility on the different TCKind values.

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

   --  Implementation Note: see CORBA.Repository_Root for a discussion
   --  of the issue with the declaration of the Visibility type.

   subtype Visibility is PolyORB.Any.Visibility;

   PRIVATE_MEMBER : constant Visibility;
   PUBLIC_MEMBER  : constant Visibility;

   package TypeCode is

      type Object is private;
      --  Mandated by standard mapping. Note that this pseudo-object cannot
      --  be made limited, and we cannot force the usage of a proper ref
      --  type on the CORBA side.

      --  exception Bounds

      Bounds : exception;

      type Bounds_Members is new CORBA.IDL_Exception_Members with null record;

      procedure Get_Members
        (From : Ada.Exceptions.Exception_Occurrence;
         To   : out Bounds_Members);

      --  exception BadKind

      BadKind : exception;

      type BadKind_Members is new CORBA.IDL_Exception_Members with null record;

      procedure Get_Members
        (From : Ada.Exceptions.Exception_Occurrence;
         To   : out BadKind_Members);

      overriding function "=" (Left, Right : Object) return Boolean;
      function Equal (Left, Right : Object) return Boolean
        renames "=";

      function Equivalent (Left, Right : Object) return Boolean;

      function Get_Compact_TypeCode (Self : Object) return Object;
      --  XXX not implemented

      function Kind (Self : Object) return TCKind;

      function Id (Self : Object) return RepositoryId;

      function Name (Self : Object) return Identifier;

      function Member_Count (Self : Object) return Unsigned_Long;

      function Member_Name
        (Self  : Object;
         Index : Unsigned_Long)
        return Identifier;

      function Member_Type
        (Self  : Object;
         Index : Unsigned_Long)
        return Object;

      function Member_Label
        (Self  : Object;
         Index : Unsigned_Long)
        return Any;

      function Discriminator_Type (Self : Object) return Object;

      function Default_Index (Self : Object) return Long;

      function Length (Self : Object) return Unsigned_Long;

      function Content_Type (Self : Object) return Object;

      function Fixed_Digits (Self : Object) return Unsigned_Short;

      function Fixed_Scale (Self : Object) return Short;

      function Member_Visibility
        (Self  : Object;
         Index : Unsigned_Long)
        return Visibility;

      function Type_Modifier (Self : Object) return ValueModifier;

      function Concrete_Base_Type (Self : Object) return Object;

      package Internals is

         --  Internal implementation subprograms. These shall not be
         --  used outside of PolyORB.

         procedure Set_Kind (Self : out Object; Kind : TCKind);
         --  Return a typecode of kind Kind, with an empty parameter list

         procedure Add_Parameter (Self : in out Object; Param : Any);
         --  Add the parameter Param in the list of Self's parameters

         function To_PolyORB_Object
           (Self : CORBA.TypeCode.Object)
           return PolyORB.Any.TypeCode.Local_Ref;

         function To_CORBA_Object
           (Self : PolyORB.Any.TypeCode.Local_Ref)
           return CORBA.TypeCode.Object;

         function Is_Nil (Self : CORBA.TypeCode.Object) return Boolean;
         --  True when Self has not been initialized to contain any typecode
         --  information.

         procedure Freeze (Self : CORBA.TypeCode.Object);
         --  Indicate that no further parameters will be added to Self and that
         --  optimizations can now be computed.

         procedure Disable_Ref_Counting (Self : CORBA.TypeCode.Object);
         --  Disable reference counting on the underlying storage of Self
         --  (meant to be used for library-level typecode objects).

         function Build_Alias_TC
           (Name, Id : CORBA.String;
            Parent   : Object) return Object;
         function Build_Sequence_TC (Element_TC : Object; Max : Natural)
           return Object;
         function Build_String_TC (Max : CORBA.Unsigned_Long) return Object;
         function Build_Wstring_TC (Max : CORBA.Unsigned_Long) return Object;
         --  ??? Should use CORBA.ORB.Create_*_Tc instead of these

         function Wrap
           (X : not null access Object) return PolyORB.Any.Content'Class;

      private
         pragma Inline (To_PolyORB_Object);
         pragma Inline (To_CORBA_Object);
      end Internals;

   private

      type Object is new PolyORB.Any.TypeCode.Local_Ref;
      --  In the neutral layer, TypeCode.Object is a limited type with
      --  reference counting.

   end TypeCode;

   --  Pre-defined TypeCode "constants"

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

   --  Implementation Note: function TC_Object is defined in
   --  CORBA.Object.

   --  This is the returned exception in case of dynamic invocation

   UnknownUserException : exception;
   type UnknownUserException_Members is
     new CORBA.IDL_Exception_Members with record
        IDL_Exception : Any;
     end record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out UnknownUserException_Members);

   function To_Any (Item : Short)              return Any;
   function To_Any (Item : Long)               return Any;
   function To_Any (Item : Long_Long)          return Any;
   function To_Any (Item : Unsigned_Short)     return Any;
   function To_Any (Item : Unsigned_Long)      return Any;
   function To_Any (Item : Unsigned_Long_Long) return Any;
   function To_Any (Item : CORBA.Float)        return Any;
   function To_Any (Item : Double)             return Any;
   function To_Any (Item : Long_Double)        return Any;
   --  function To_Any (Item : Boolean)            return Any;
   --  function To_Any (Item : Char)               return Any;
   --  function To_Any (Item : Wchar)              return Any;
   --  function To_Any (Item : Any)                return Any;
   --  Implicitly inherited
   function To_Any (Item : TypeCode.Object)    return Any;
   function To_Any (Item : Octet)              return Any;
   function To_Any (Item : CORBA.String)       return Any;
   function To_Any (Item : CORBA.Wide_String)  return Any;

   function From_Any (Item : Any) return Short;
   function From_Any (Item : Any) return Long;
   function From_Any (Item : Any) return Long_Long;
   function From_Any (Item : Any) return Unsigned_Short;
   function From_Any (Item : Any) return Unsigned_Long;
   function From_Any (Item : Any) return Unsigned_Long_Long;
   function From_Any (Item : Any) return CORBA.Float;
   function From_Any (Item : Any) return Double;
   function From_Any (Item : Any) return Long_Double;
   --  function From_Any (Item : Any) return Boolean;
   --  function From_Any (Item : Any) return Char;
   --  function From_Any (Item : Any) return Wchar;
   --  function From_Any (Item : Any) return Any;
   --  Implicitly inherited
   function From_Any (Item : Any) return TypeCode.Object;
   function From_Any (Item : Any) return Octet;
   function From_Any (Item : Any) return CORBA.String;
   function From_Any (Item : Any) return CORBA.Wide_String;

   subtype Any_Container is PolyORB.Any.Any_Container;

   function From_Any (Item : Any_Container'Class) return Short;
   function From_Any (Item : Any_Container'Class) return Long;
   function From_Any (Item : Any_Container'Class) return Long_Long;
   function From_Any (Item : Any_Container'Class) return Unsigned_Short;
   function From_Any (Item : Any_Container'Class) return Unsigned_Long;
   function From_Any (Item : Any_Container'Class) return Unsigned_Long_Long;
   function From_Any (Item : Any_Container'Class) return CORBA.Float;
   function From_Any (Item : Any_Container'Class) return Double;
   function From_Any (Item : Any_Container'Class) return Long_Double;
   function From_Any (Item : Any_Container'Class) return Boolean;
   function From_Any (Item : Any_Container'Class) return Char;
   function From_Any (Item : Any_Container'Class) return Wchar;
   function From_Any (Item : Any_Container'Class) return Octet;
   --  function From_Any (Item : Any_Container'Class) return Any;
   --  Implicitly inherited
   function From_Any (Item : Any_Container'Class) return TypeCode.Object;
   function From_Any (Item : Any_Container'Class) return CORBA.String;
   function From_Any (Item : Any_Container'Class) return CORBA.Wide_String;

   subtype Content is PolyORB.Any.Content;

   function Wrap (X : not null access Short)              return Content'Class;
   function Wrap (X : not null access Long)               return Content'Class;
   function Wrap (X : not null access Long_Long)          return Content'Class;
   function Wrap (X : not null access Unsigned_Short)     return Content'Class;
   function Wrap (X : not null access Unsigned_Long)      return Content'Class;
   function Wrap (X : not null access Unsigned_Long_Long) return Content'Class;
   function Wrap (X : not null access CORBA.Float)        return Content'Class;
   function Wrap (X : not null access Double)             return Content'Class;
   function Wrap (X : not null access Long_Double)        return Content'Class;
   function Wrap (X : not null access Boolean)            return Content'Class;
   function Wrap (X : not null access Char)               return Content'Class;
   function Wrap (X : not null access Wchar)              return Content'Class;
   function Wrap (X : not null access Octet)              return Content'Class;
   --  function Wrap (X : not null access Any)            return Content'Class;
   --  Implicitly inherited
   function Wrap (X : not null access TypeCode.Object)    return Content'Class;
   function Wrap (X : not null access CORBA.String)       return Content'Class;
   function Wrap (X : not null access CORBA.Wide_String)  return Content'Class;
   pragma Inline (Wrap);

   ----------------
   -- NamedValue --
   ----------------

   type Flags is new CORBA.Unsigned_Long;

   ARG_IN :        constant Flags;
   ARG_OUT :       constant Flags;
   ARG_INOUT :     constant Flags;
   IN_COPY_VALUE : constant Flags;

   type NamedValue is record
      Name      : Identifier;
      Argument  : Any;
      Arg_Modes : Flags;
   end record;

   function Image (NV : NamedValue) return Standard.String;
   --  Return a human-readable image of NV for debugging purposes

   ------------------
   -- RepositoryId --
   ------------------

   function Is_Equivalent (RI1, RI2 : RepositoryId) return Boolean;

   function Is_Equivalent (RI1, RI2 : Standard.String) return Boolean;
   --  Return True if, and only if, RI1 and RI2 denote the same
   --  repository entity (a case-insensitive string match).

   --  Helper function for CORBA.Completion_Status

   function From_Any (Item : CORBA.Any) return CORBA.Completion_Status;
   function To_Any (Item : CORBA.Completion_Status) return CORBA.Any;

   function Get_Type (The_Any : Any) return CORBA.TypeCode.Object;
   --  Return the typecode of The_Any

   package Internals is

      --  Implementation Note: This package defines internal subprograms
      --  specific to PolyORB which should not be used directly from
      --  application code.

      function Get_Unwound_Type
        (The_Any : Any) return PolyORB.Any.TypeCode.Object_Ptr;
      --  Returns the type of The_Any after unwinding all typedefs

      procedure Set_Type
        (The_Any  : in out Any;
         The_Type : TypeCode.Object);
      --  Change the type of an any without changing its value (to be used
      --  carefully)

      function Get_Wrapper_Any
        (TC : TypeCode.Object;
         CC : access PolyORB.Any.Content'Class) return Any;
      --  Return an Any with the specified typecode and contents wrapper

      function Get_Empty_Any (TC : TypeCode.Object) return Any;
      function Get_Empty_Any (TC : TypeCode.Object) return PolyORB.Any.Any;
      --  Return an empty any with the given Typecode but not value

      function Is_Empty (Any_Value : CORBA.Any) return Boolean;
      --  True iff Any_Value does not have a value

      --  Not in spec : some methods to deal with any aggregates.
      --  What is called any aggregate is an any, made of an aggregate
      --  of values, instead of one unique. It is used for structs,
      --  unions, enums, arrays, sequences, objref, values...

      function Get_Empty_Any_Aggregate (TC : CORBA.TypeCode.Object) return Any;
      --  Return an Any with an aggregate value containing zero elements and
      --  having the specified typecode

      function Get_Aggregate_Count (Value : Any) return CORBA.Unsigned_Long;
      --  Return the number of elements in an any aggregate

      procedure Add_Aggregate_Element
        (Value   : in out CORBA.Any;
         Element : CORBA.Any);
      --  Append the value of Element to aggregate Value (note that the
      --  TypeCode of Element is discarded: it is assumed that the necessary
      --  type information is contained within the typecode of Value.

      function Get_Aggregate_Element
        (Value : Any;
         TC    : CORBA.TypeCode.Object;
         Index : CORBA.Unsigned_Long) return Any;
      --  Return an any constructed with typecode Tc and the value extracted
      --  from position Index in aggregate Value

      procedure Move_Any_Value (Dest : Any; Src : Any);

      procedure Add_Parameter (TC : TypeCode.Object; Param : Any);

   private

      pragma Inline (Get_Aggregate_Count);
      pragma Inline (Get_Aggregate_Element);

   end Internals;

private

   VTM_NONE        : constant ValueModifier := PolyORB.Any.VTM_NONE;
   VTM_CUSTOM      : constant ValueModifier := PolyORB.Any.VTM_CUSTOM;
   VTM_ABSTRACT    : constant ValueModifier := PolyORB.Any.VTM_ABSTRACT;
   VTM_TRUNCATABLE : constant ValueModifier := PolyORB.Any.VTM_TRUNCATABLE;

   PRIVATE_MEMBER : constant Visibility := PolyORB.Any.PRIVATE_MEMBER;
   PUBLIC_MEMBER  : constant Visibility := PolyORB.Any.PUBLIC_MEMBER;

   function To_CORBA_NV (NV : PolyORB.Any.NamedValue) return NamedValue;

   pragma Inline (From_Any);
   pragma Inline (To_Any);
   pragma Inline (To_CORBA_String);

   ----------------
   -- NamedValue --
   ----------------

   ARG_IN :        constant Flags := Flags (PolyORB.Any.ARG_IN);
   ARG_OUT :       constant Flags := Flags (PolyORB.Any.ARG_OUT);
   ARG_INOUT :     constant Flags := Flags (PolyORB.Any.ARG_INOUT);
   IN_COPY_VALUE : constant Flags := Flags (PolyORB.Any.IN_COPY_VALUE);

end CORBA;
