-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package CORBA                                ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



with Ada.Exceptions ;
with Ada.Strings.Unbounded ;
with Implementation_Defined ;
with Interfaces ;

--I module CORBA {
package Corba is

   -----------------------------------------------------------
   ----           base types                               ---
   -----------------------------------------------------------


   -- CORBA Module: In order to prevent names defined with the
   -- CORBA specification from clashing with names in programming languages
   -- and other software systems, all names defined by CORBA are treated as
   -- if they were defined with a module named CORBA.

   -- Each IDL data type is mapped to a native data
   -- type via the appropriate language mapping.
   -- The following definitions may differ. See the mapping
   -- specification for more information.

   subtype Boolean is Standard.Boolean;
   type Short is new Interfaces.Integer_16;
   type Long is new Interfaces.Integer_32;
   type Unsigned_Short is new Interfaces.Unsigned_16;
   type Unsigned_Long is new Interfaces.Unsigned_32;
   type Float is new Interfaces.Ieee_Float_32;
   type Double is new Interfaces.Ieee_Float_64;
   subtype Char is Standard.Character;
   type Octet is new Interfaces.Unsigned_8;
   type String is new Ada.Strings.Unbounded.Unbounded_String;





   -----------------------------------------------------------
   ----           Exceptions                               ---
   -----------------------------------------------------------

    type Idl_Exception_Members is abstract tagged null record;

    procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence;
                           To : out Idl_Exception_Members) is abstract;

    -- Standard Exceptions:
    --I #define ex_body{ unsigned long minor, completion_status completed;}
    --I enum completion_status{COMPLETED_YES, COMPLETED_NO, COMPLETED_MAYBE};
    type Completion_Status is (Completed_Yes, Completed_No, Completed_Maybe);


    --I enum exception_type{ NO_EXCEPTION, USER_EXCEPTION, SYSTEM_EXCEPTION};
    type Exception_Type is (No_Exception, System_Exception, User_Exception);


    type System_Exception_Members is new Corba.Idl_Exception_Members with
        record
            Minor : Corba.Unsigned_Long;
            Completed : Completion_Status;
        end record;


    procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence;
                           To : out System_Exception_Members);

    Unknown : exception;  -- the unknown exception
    Bad_Param : exception;  -- an invalid parameter was passed
    No_Memory : exception;  -- dynamic memory allocation failure
    Imp_Limit : exception;  -- violated implementation limit
    Comm_Failure : exception;  -- communication failure
    Inv_Objref : exception;  -- invalid object reference
    No_Permission : exception;  -- no permission for attempted op.
    Internal : exception;  -- ORB internal error
    Marshal : exception;  -- error marshalling param/result
    Initialization_Failure : exception;  -- ORB initialization failure
    No_Implement : exception;  -- operation implementation unavailable
    Bad_Typecode : exception;  -- bad typecode
    Bad_Operation : exception;  -- inavlid operation
    No_Resources : exception;  -- insufficient resources for req.
    No_Response : exception;  -- response to request not yet available
    Persist_Store : exception;  -- persistent storage failure
    Bad_Inv_Order : exception;  -- routine invocations out of order
    Transient : exception;  -- transient failure - reissue request
    Free_Mem : exception;  -- cannot free memory
    Inv_Ident : exception;  -- invalid identifier syntax
    Inv_Flag : exception;  -- invalid flag was specified
    Intf_Repos : exception;  -- error accessing interface repository
    Bad_Context : exception;  -- error processing context object
    Obj_Adapter : exception;  -- failure detected by object adapter
    Data_Conversion : exception;  -- data conversion error


    type Unknown_Members is new System_Exception_Members with null record;
    type Bad_Param_Members is new System_Exception_Members with null record;
    type No_Memory_Members is new System_Exception_Members with null record;
    type Imp_Limit_Members is new System_Exception_Members with null record;
    type Comm_Failure_Members is new System_Exception_Members with null record;
    type Inv_Objref_Members is new System_Exception_Members with null record;
    type No_Permission_Members is new System_Exception_Members with null record;
    type Internal_Members is new System_Exception_Members with null record;
    type Marshal_Members is new System_Exception_Members with null record;
    type Initialization_Failure_Members is new System_Exception_Members with null record;
    type No_Implement_Members is new System_Exception_Members with null record;
    type Bad_Typecode_Members is new System_Exception_Members with null record;
    type Bad_Operation_Members is new System_Exception_Members with null record;
    type No_Resources_Members is new System_Exception_Members with null record;
    type No_Response_Members is new System_Exception_Members with null record;
    type Persist_Store_Members is new System_Exception_Members with null record;
    type Bad_Inv_Order_Members is new System_Exception_Members with null record;
    type Transient_Members is new System_Exception_Members with null record;
    type Free_Mem_Members is new System_Exception_Members with null record;
    type Inv_Ident_Members is new System_Exception_Members with null record;
    type Inv_Flag_Members is new System_Exception_Members with null record;
    type Intf_Repos_Members is new System_Exception_Members with null record;
    type Bad_Context_Members is new System_Exception_Members with null record;
    type Obj_Adapter_Members is new System_Exception_Members with null record;
    type Data_Conversion_Members is new System_Exception_Members with null record;




   -----------------------------------------------------------
   ----           functions, procedures                    ---
   -----------------------------------------------------------






   -----------------------------------------------------------
   ----           not in spec                              ---
   -----------------------------------------------------------




private

   procedure MarshalObjRef() ;
   -- wrapper around void CORBA::MarshalObjRef(CORBA::Object_ptr obj,
   --        const char* repoId,
   --        size_t repoIdSize,
   --        NetBufferedStream &s)
   -- defined in objectRef.cc L721

   procedure MarshalObjRef() ;
   -- wrapper around void CORBA::MarshalObjRef(CORBA::Object_ptr obj,
   --        const char* repoId,
   --        size_t repoIdSize,
   --        MemBufferedStream &s)
   -- defined in objectRef.cc L850

   function UnMarshalObjRef() return CORBA.Object.Ref ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           NetBufferedStream& s)
   -- in objectRef.cc L637

   function UnMarshalObjRef() return CORBA.Object.Ref ;
   -- return ???
   -- wrapper around CORBA::Object_ptr
   --                CORBA::UnMarshalObjRef(
   --                           const char* repoId,
   --                           MemBufferedStream& s)
   -- in objectRef.cc L765

   function AlignedObjRef() return ;
   -- wrapper around size_t
   --                CORBA::AlignedObjRef(CORBA::Object_ptr obj,
   --                const char* repoId,
   --                size_t repoIdSize,
   --                size_t initialoffset)
   -- in objectRef.cc L744






end Corba;




