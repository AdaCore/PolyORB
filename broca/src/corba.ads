with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Interfaces;

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

   function To_CORBA_String
     (S : in Standard.String)
      return CORBA.String;
   --  Transforms a standard string into the correponding corba string

   function To_Standard_String
     (S : in CORBA.String)
      return Standard.String;
   --  Transforms a corba string into the correponding standard string

   Null_String : constant CORBA.String;

--    function To_CORBA_String
--      (S : in Constants.Exception_Id)
--       return CORBA.String;
--    --  Transforms a standard string into the correponding corba string

--    function To_Exception_Id
--      (S : in CORBA.String)
--       return Constants.Exception_Id;
--    --  Transforms a corba string into the correponding standard string

--    function Length
--      (S : in CORBA.String)
--       return CORBA.Unsigned_Long;
   --  Returns the length of a corba string

   --  Defined in 4.7
   type PolicyType is new CORBA.Unsigned_Long;

   --  Defined in 21.34

   type PolicyErrorCode is new Short;

   InvalidName : exception;
   type InvalidName_Members is new CORBA.IDL_Exception_Members
     with null record;
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InvalidName_Members);

   InconsistentTypeCode : exception;
   type InconsistentTypeCode_Members is new CORBA.IDL_Exception_Members
     with null record;
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out InconsistentTypeCode_Members);

   PolicyError : exception;
   type PolicyError_Members is new CORBA.IDL_Exception_Members
     with null record;
   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To : out PolicyError_Members);

   type RepositoryId is new CORBA.String;
   type Identifier is new CORBA.String;
   type ServiceType is new Unsigned_Short;
   type ServiceOption is new Unsigned_Long;
   type ServiceDetailType is new Unsigned_Long;

   Security : constant ServiceType := 1;


   -----------------------
   -- omniORB2 specific --
   -----------------------

   Wrong_Transaction      : exception;

   type Wrong_Transaction_Members is new System_Exception_Members
     with null record;

private

   Null_String : constant CORBA.String :=
     CORBA.String (Ada.Strings.Unbounded.Null_Unbounded_String);

end CORBA;
