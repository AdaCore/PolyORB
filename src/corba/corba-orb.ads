with Sequences.Unbounded;
with CORBA.Object;
with CORBA.NVList;
with CORBA.Context;
with CORBA.Policy;

package CORBA.ORB is

   package Octet_Sequence is new Sequences.Unbounded (Octet);
   type ServiceDetail is record
      Service_Detail_Type : ServiceDetailType;
      Service_Detail      : Octet_Sequence.Sequence;
   end record;

   package IDL_Sequence_ServiceOption is new
     Sequences.Unbounded (ServiceOption);

   package IDL_Sequence_ServiceDetail is new
     Sequences.Unbounded (ServiceDetail);

   type ServiceInformation is record
      service_options : IDL_Sequence_ServiceOption.Sequence;
      service_details : IDL_Sequence_ServiceDetail.Sequence;
   end record;

   type ObjectId is new CORBA.String;

   package IDL_Sequence_ObjectId is new
     Sequences.Unbounded (ObjectId);
   type ObjectIdList is new IDL_Sequence_ObjectId.Sequence;

   function  Object_To_String
     (Obj : in CORBA.Object.Ref'Class)
     return CORBA.String;

   procedure String_To_Object
     (From : in     CORBA.String;
      To   : in out CORBA.Object.Ref'Class);

   --  Dynamic Invocation related operations

   procedure Create_List
     (Count    : in     CORBA.Long;
      New_List :    out CORBA.NVList.Ref);

   --  ??? Requires CORBA.OperationDef

   --    procedure Create_Operation_List
   --      (Oper     : in     CORBA.OperationDef.Ref'Class;
   --       New_List :    out CORBA.NVList.Object);

   function Get_Default_Context
     return CORBA.Context.Ref;

   --  Service information operations

   procedure Get_Service_Information
     (Service_Type        : in     CORBA.ServiceType;
      Service_Information :    out ServiceInformation;
      Returns             :    out CORBA.Boolean);

   --  initial reference operations

   function List_Initial_Services return ObjectIdList;

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref;

   --  ??? Requires CORBA.StructMemberSeq

   --    function  create_struct_tc
   --      (Id      : in CORBA.RepositoryId;
   --       Name    : in CORBA.Identifier;
   --       Members : in CORBA.StructMemberSeq)
   --      return CORBA.TypeCode.Object;

   --  ??? Requires CORBA.EnumMemberSeq

   --    function  create_enum_tc
   --      (Id      : in CORBA.RepositoryId;
   --       Name    : in CORBA.Identifier;
   --       Members : in CORBA.EnumMemberSeq)
   --      return CORBA.TypeCode.Object;

   function  create_alias_tc
     (Id            : in CORBA.RepositoryId;
      Name          : in CORBA.Identifier;
      Original_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   --  ??? Requires CORBA.StructMemberSeq

   --    function  create_exception_tc
   --      (Id      : in CORBA.RepositoryId;
   --       Name    : in CORBA.Identifier;
   --       Members : in CORBA.StructMemberSeq)
   --      return CORBA.TypeCode.Object;

   function  create_interface_tc
     (Id   : in CORBA.RepositoryId;
      Name : in CORBA.Identifier)
     return CORBA.TypeCode.Object;

   function  create_string_tc
     (Bound : in CORBA.Unsigned_long)
     return CORBA.TypeCode.Object;

   function  create_wstring_tc
     (Bound : in CORBA.Unsigned_long)
     return CORBA.TypeCode.Object;

   function  create_fixed_tc
     (IDL_Digits : in CORBA.Unsigned_short;
      scale      : in CORBA.Short)
     return CORBA.TypeCode.Object;

   function  create_sequence_tc
     (Bound        : in CORBA.Unsigned_long;
      Element_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function  create_recursive_sequence_tc
     (Bound  : in CORBA.Unsigned_long;
      Offset : in CORBA.Unsigned_long)
     return CORBA.TypeCode.Object;

   function create_array_tc
     (Length       : in CORBA.Unsigned_long;
      Element_Type : in CORBA.TypeCode.Object)
     return CORBA.TypeCode.Object;

   function create_native_tc
     (Id   : in RepositoryId;
      Name : in Identifier)
     return CORBA.TypeCode.Object;

   --  Thread related operations

   function Work_Pending return Boolean;
   procedure Perform_Work;
   procedure Shutdown (Wait_For_Completion : in Boolean);
   procedure Run;

   --  Policy related operations

   --  ??? in the spec this is declared a function.

   procedure Create_Policy
     (The_Type : in PolicyType;
      Val      : Any);

private

   -- type ORB_Policy is new CORBA.Policy.Ref with null record;



end CORBA.ORB;




