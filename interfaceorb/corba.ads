--  This package deffines all general types and associated functions used
--  in AdaBroker. The first part is the definition of corba types out of
--  Ada ones. Pointers on these types are also defined as well as the
--  associated free functions. Then, the corba exception type is defined
--  and all corba system exceptions. At last, some OmniOrb or AdaBroker

with Ada.Exceptions;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Interfaces;

with Constants;

package CORBA is

   --  CORBA Module: In order to prevent names defined with the
   --  CORBA specification from clashing with names in programming languages
   --  and other software systems, all names defined by CORBA are treated as
   --  if they were defined with a module named CORBA.

   --  Each IDL data type is mapped to a native data
   --  type via the appropriate language mapping.
   --  The following definitions may differ. See the mapping
   --  specification for more information.

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

   type IDL_Exception_Members_Ptr is access all IDL_Exception_Members'Class;
   --  Type pointer on the IDL_Exception_Members type

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out IDL_Exception_Members) is abstract;
   --  This method return the member corresponding to an exception
   --  occurence This methos must be redefined for each new member
   --  type. That's why it is declared abstract.

   procedure Free is
     new Ada.Unchecked_Deallocation
     (IDL_Exception_Members'Class,
      IDL_Exception_Members_Ptr);
   --  Free method associated to the type Idl_Exception_Members_Ptr

   type Exception_Type is (No_Exception, System_Exception, User_Exception);
   --  Type used for characterize exceptions.  It is defined by the CORBA
   --  specification.

   type Completion_Status is (Completed_Yes, Completed_No, Completed_Maybe);
   --  Type used for characterize the state of an exception It is defined
   --  by the CORBA specification.

   type Ex_Body is new CORBA.IDL_Exception_Members with
      record
         Minor     : CORBA.Unsigned_Long;
         Completed : Completion_Status;
      end record;
   --  Member type for System exceptions.  It is defined by the CORBA
   --  specification

   type Ex_Body_Ptr is access all Ex_Body;
   --  Type pointer on the Ex_Body type

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


   ------------------------
   -- AdaBroker specific --
   ------------------------

   type Boolean_Ptr        is access all Boolean;
   type Short_Ptr          is access all Short;
   type Long_Ptr           is access all Long;
   type Unsigned_Short_Ptr is access all Unsigned_Short;
   type Unsigned_Long_Ptr  is access all Unsigned_Long;
   type Float_Ptr          is access all Float;
   type Double_Ptr         is access all Double;
   type Char_Ptr           is access all Char;
   type Octet_Ptr          is access all Octet;
   type String_Ptr         is access all String;

   procedure Free is
     new Ada.Unchecked_Deallocation (Boolean, Boolean_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Short, Short_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Long, Long_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Unsigned_Short, Unsigned_Short_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Unsigned_Long, Unsigned_Long_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Float, Float_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Double, Double_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Char, Char_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Octet, Octet_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (String, String_Ptr);

   procedure Raise_CORBA_Exception
     (Excp      : in Ada.Exceptions.Exception_Id;
      Excp_Memb : in IDL_Exception_Members'Class);
   --  Raises the corresponding exception corba exception and stores its
   --  member so that it can be retrieved with Get_Members


   CRLF : constant Standard.String
     := (Ada.Characters.Latin_1.LF, Ada.Characters.Latin_1.CR);
   --  Definition of the cariage return-line feed string used when one
   --  wants to split a string into sevral lines : just write "first line"
   --  & CORBA.CRLF & "second line"

   AdaBroker_Fatal_Error : exception;
   --  Error in the AdaBroker runtime

   AdaBroker_Not_Implemented_Yet : exception;
   --  Function was not implemented yet

   No_Initialisation_Error : exception;
   --  A C object was used before being initialised via an Init function

   C_Out_Of_Range : exception;
   --  A C Value was to be converted into an Ada Value but was out of range

   Dummy_User : exception;
   --  The user tried to call an unchecked case in an union

   function To_CORBA_String
     (S : in Standard.String)
      return CORBA.String;
   --  Transforms a standard string into the correponding corba string

   function To_Standard_String
     (S : in CORBA.String)
      return Standard.String;
   --  Transforms a corba string into the correponding standard string

   function To_CORBA_String
     (S : in Constants.Exception_Id)
      return CORBA.String;
   --  Transforms a standard string into the correponding corba string

   function To_Exception_Id
     (S : in CORBA.String)
      return Constants.Exception_Id;
   --  Transforms a corba string into the correponding standard string

   function Length
     (S : in CORBA.String)
      return CORBA.Unsigned_Long;
   --  Returns the length of a corba string


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

end CORBA;
