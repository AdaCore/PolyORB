------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                                C O R B A                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.45 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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

with Interfaces;

with System;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;

package CORBA is

   --  CORBA Module: In order to prevent names defined with the CORBA
   --  specification from clashing with names in programming languages and
   --  other software systems, all names defined by CORBA are treated as if
   --  they were defined with a module named CORBA.

   --  Each IDL data type is mapped to a native data type via the
   --  appropriate language mapping. The following definitions may
   --  differ. See the mapping specification for more information.

   subtype Boolean        is Standard.Boolean;
   type    Short          is new Interfaces.Integer_16;
   type    Long           is new Interfaces.Integer_32;
   type    Unsigned_Short is new Interfaces.Unsigned_16;
   type    Unsigned_Long  is new Interfaces.Unsigned_32;
   type    Float          is new Interfaces.IEEE_Float_32;
   type    Double         is new Interfaces.IEEE_Float_64;
   subtype Char           is Standard.Character;
   type    Octet          is new Interfaces.Unsigned_8;
   type    String         is new Ada.Strings.Unbounded.Unbounded_String;


   ----------------
   -- Exceptions --
   ----------------

   type IDL_Exception_Members is abstract tagged null record;
   --  Base type for all corba exception members. A member is a record
   --  attached to an exception that allows the programmer to pass
   --  arguments when an exception is raised. The default Member record is
   --  abstract and empty but all other records will inherit from it.

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

   type Ex_Body is new CORBA.IDL_Exception_Members with
      record
         Minor     : CORBA.Unsigned_Long;
         Completed : Completion_Status;
      end record;
   --  Member type for System exceptions.  It is defined by the CORBA
   --  specification

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out Ex_Body);
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

   function To_CORBA_String
     (S : in Standard.String)
      return CORBA.String;
   --  Transforms a standard string into the correponding corba string

   function To_Standard_String
     (S : in CORBA.String)
      return Standard.String;
   --  Transforms a CORBA string into the correponding standard string

   ---------------
   -- TypeCodes --
   ---------------

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
       Tk_Array);

   type Any is private;
   --  Any Type: the any type permits the specification of
   --  values that can express an IDL type.
   --  implementation defined


   package TypeCode is
      --  TypeCodes are values that represent invocation argument types,
      --  attribute types, and Object types.
      --  see spec 23-28

      type Object is private;

      procedure Set (O : out Object;
                     K : in CORBA.TCKind);

      Bounds : exception;

      type Bounds_Members is new CORBA.IDL_Exception_Members with null record;

      procedure Get_Members
        (From : in Ada.Exceptions.Exception_Occurrence;
         To   : out Bounds_Members);
      --  must be there to override abstract declaration

      function Get_Members
        (X : Ada.Exceptions.Exception_Occurrence)
         return Bounds_Members;
      --  does the same as the same-named procedure but must be there
      --  since specified

      function Equal
        (Self : in Object;
         TC   : in Object)
         return CORBA.Boolean;

      function "="
        (Left, Right : in Object)
         return Boolean
        renames Equal;

      function Kind
        (Self : in Object)
         return TCKind;
      --  return the kind of Object

      function Param_Count
        (Self : in Object)
         return CORBA.Long;
      --  the number of parameters for this TypeCode

      function Parameter
        (Self  : in Object;
         Index : in CORBA.Long) -- note origin is 0
         return Any;
      --  the index'th parameter. Parameters are indexed
      --  from 0 to (Param_Count - 1)




   private
      --  implementation defined
      Out_Of_Bounds_Index : exception;
      type Cell;
      type Cell_Ptr is access all Cell;
      type Cell is record
         Parameter : CORBA.Any;
         Next : Cell_Ptr;
      end record;
      --  choice of a list implementation may be temporary
      type Object is
         record
            Kind : CORBA.TCKind;
            Parameters : Cell_Ptr := null;
         end record;

   end TypeCode;


   function Get_Type (The_Any : in CORBA.Any) return CORBA.TypeCode.Object;

   procedure SetAny
     (A : out Any;
      V : in System.Address;
      T : in CORBA.TypeCode.Object);

   function To_Any (From : in CORBA.Octet)          return CORBA.Any;
   function To_Any (From : in CORBA.Short)          return CORBA.Any;
   function To_Any (From : in CORBA.Long)           return CORBA.Any;
   function To_Any (From : in CORBA.Unsigned_Short) return CORBA.Any;
   function To_Any (From : in CORBA.Unsigned_Long)  return CORBA.Any;
   function To_Any (From : in CORBA.Boolean)        return CORBA.Any;
   function To_Any (From : in CORBA.Char)           return CORBA.Any;
   function To_Any (From : in CORBA.String)         return CORBA.Any;

   function From_Any (From : in CORBA.Any) return CORBA.Octet;
   function From_Any (From : in CORBA.Any) return CORBA.Short;
   function From_Any (From : in CORBA.Any) return CORBA.Unsigned_Short;
   function From_Any (From : in CORBA.Any) return CORBA.Unsigned_Long;
   function From_Any (From : in CORBA.Any) return CORBA.Boolean;
   function From_Any (From : in CORBA.Any) return CORBA.Char;
   function From_Any (From : in CORBA.Any) return CORBA.String;


   type Identifier is new CORBA.String;

   ----------------------------------
   -- Dynamic Invocation Interface --
   --    Common Data Structures    --
   ----------------------------------

   type Flags is new CORBA.Unsigned_Long;
   ARG_IN    : constant Flags := 1;
   ARG_OUT   : constant Flags := 2;
   ARG_INOUT : constant Flags := 3;

   type NamedValue is record
      Name      : Identifier; -- arg name
      Argument  : CORBA.Any;  -- value
      Len       : Long;       -- length/count of arg value
      Arg_Modes : Flags;      -- arg mode flags
   end record;

   OUT_LIST_MEMORY    : constant Flags := 4; -- CORBA 6.2.1
   IN_COPY_VALUE      : constant Flags := 5; -- CORBA 6.2.2
   INV_NO_RESPONSE    : constant Flags := 6; -- CORBA 6.3.1
   INV_TERM_ON_ERR    : constant Flags := 7; -- CORBA 6.3.2
   RESP_NO_WAIT       : constant Flags := 8; -- CORBA 6.3.3
   DEPENDENT_LIST     : constant Flags := 9; -- CORBA 6.4.2
   CTX_RESTRICT_SCOPE : constant Flags := 10; -- CORBA 6.6.4

   --  Container and Contained Objects
   --  moved to child package CORBA.Repository_Root

   type Status is new CORBA.Unsigned_Long;


   ---------------
   -- AdaBroker --
   ---------------

   --  This should be moved somewhere else.

   type Unknown_Members         is new Ex_Body with null record;
   type Bad_Param_Members       is new Ex_Body with null record;
   type No_Memory_Members       is new Ex_Body with null record;
   type Imp_Limit_Members       is new Ex_Body with null record;
   type Comm_Failure_Members    is new Ex_Body with null record;
   type Inv_Objref_Members      is new Ex_Body with null record;
   type No_Permission_Members   is new Ex_Body with null record;
   type Internal_Members        is new Ex_Body with null record;
   type Marshal_Members         is new Ex_Body with null record;
   type Initialization_Failure_Members is new Ex_Body with null record;
   type No_Implement_Members    is new Ex_Body with null record;
   type Bad_Typecode_Members    is new Ex_Body with null record;
   type Bad_Operation_Members   is new Ex_Body with null record;
   type No_Resources_Members    is new Ex_Body with null record;
   type No_Response_Members     is new Ex_Body with null record;
   type Persist_Store_Members   is new Ex_Body with null record;
   type Bad_Inv_Order_Members   is new Ex_Body with null record;
   type Transient_Members       is new Ex_Body with null record;
   type Free_Mem_Members        is new Ex_Body with null record;
   type Inv_Ident_Members       is new Ex_Body with null record;
   type Inv_Flag_Members        is new Ex_Body with null record;
   type Intf_Repos_Members      is new Ex_Body with null record;
   type Bad_Context_Members     is new Ex_Body with null record;
   type Obj_Adapter_Members     is new Ex_Body with null record;
   type Data_Conversion_Members is new Ex_Body with null record;


   procedure Raise_CORBA_Exception
     (Excp      : in Ada.Exceptions.Exception_Id;
      Excp_Memb : in IDL_Exception_Members'Class);
   --  Raises the corresponding exception corba exception and stores its
   --  member so that it can be retrieved with Get_Members


   AdaBroker_Fatal_Error         : exception;
   --  Error in the AdaBroker runtime

   AdaBroker_Not_Implemented_Yet : exception;
   --  Function was not implemented yet

   No_Initialisation_Error       : exception;
   --  A C object was used before being initialised via an Init function

   C_Out_Of_Range                : exception;
   --  A C Value was to be converted into an Ada Value but was out of range

   Dummy_User                    : exception;
   --  The user tried to call an unchecked case in an union

   -----------------------
   -- omniORB2 specific --
   -----------------------

   OmniORB_Fatal_Error    : exception;
   Object_Not_Exist       : exception;
   Transaction_Required   : exception;
   Transaction_Rolledback : exception;
   Invalid_Transaction    : exception;
   Wrong_Transaction      : exception;

   type Object_Not_Exist_Members       is new Ex_Body with null record;
   type Transaction_Required_Members   is new Ex_Body with null record;
   type Transaction_Rolledback_Members is new Ex_Body with null record;
   type Invalid_Transaction_Members    is new Ex_Body with null record;
   type Wrong_Transaction_Members      is new Ex_Body with null record;

private
   --  implementation defined
   type Any is
     record
        The_Value : System.Address;
        The_Type  : CORBA.TypeCode.Object;
     end record;

   type CORBA_Octet_Ptr is access all CORBA.Octet;
   package Address_To_CORBA_Octet is
      new System.Address_To_Access_Conversions (CORBA.Octet);
   function To_CORBA_Octet is
     new Ada.Unchecked_Conversion
     (Address_To_CORBA_Octet.Object_Pointer, CORBA_Octet_Ptr);

   type CORBA_Short_Ptr is access all CORBA.Short;
   package Address_To_CORBA_Short is
      new System.Address_To_Access_Conversions (CORBA.Short);
   function To_CORBA_Short is
     new Ada.Unchecked_Conversion
     (Address_To_CORBA_Short.Object_Pointer, CORBA_Short_Ptr);

   type CORBA_Long_Ptr is access all CORBA.Long;
   package Address_To_CORBA_Long is
      new System.Address_To_Access_Conversions (CORBA.Long);
   function To_CORBA_Long is
     new Ada.Unchecked_Conversion
     (Address_To_CORBA_Long.Object_Pointer, CORBA_Long_Ptr);

   type CORBA_U_Short_Ptr is access all CORBA.Unsigned_Short;
   package Address_To_CORBA_U_Short is
      new System.Address_To_Access_Conversions (CORBA.Unsigned_Short);
   function To_CORBA_U_Short is
     new Ada.Unchecked_Conversion
     (Address_To_CORBA_U_Short.Object_Pointer, CORBA_U_Short_Ptr);

   type CORBA_U_Long_Ptr is access all CORBA.Unsigned_Long;
   package Address_To_CORBA_U_Long is
      new System.Address_To_Access_Conversions (CORBA.Unsigned_Long);
   function To_CORBA_U_Long is
     new Ada.Unchecked_Conversion
     (Address_To_CORBA_U_Long.Object_Pointer, CORBA_U_Long_Ptr);

   type CORBA_Boolean_Ptr is access all CORBA.Boolean;
   package Address_To_CORBA_Boolean is
      new System.Address_To_Access_Conversions (CORBA.Boolean);
   function To_CORBA_Boolean is
     new Ada.Unchecked_Conversion
     (Address_To_CORBA_Boolean.Object_Pointer, CORBA_Boolean_Ptr);

   type CORBA_Char_Ptr is access all CORBA.Char;
   package Address_To_CORBA_Char is
      new System.Address_To_Access_Conversions (CORBA.Char);
   function To_CORBA_Char is
     new Ada.Unchecked_Conversion
     (Address_To_CORBA_Char.Object_Pointer, CORBA_Char_Ptr);

   type CORBA_String_Ptr is access all CORBA.String;
   package Address_To_CORBA_String is
      new System.Address_To_Access_Conversions (CORBA.String);
   function To_CORBA_String is
     new Ada.Unchecked_Conversion
     (Address_To_CORBA_String.Object_Pointer, CORBA_String_Ptr);


end CORBA;
