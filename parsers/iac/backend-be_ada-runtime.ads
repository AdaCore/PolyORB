with Types; use Types;

package Backend.BE_Ada.Runtime is

   --  Runtime Units

   type RU_Id is
     (RU_Null,
      RU_Ada,
      RU_Ada_Exceptions,
      RU_CORBA,
      RU_CORBA_AbstractBase,
      RU_CORBA_ExceptionList,
      RU_CORBA_ExceptionList_Internals,
      RU_CORBA_Fixed_Point,
      RU_CORBA_Internals,
      RU_CORBA_NVList,
      RU_CORBA_Object,
      RU_CORBA_Object_Internals,
      RU_CORBA_Object_Helper,
      RU_CORBA_ORB,
      RU_CORBA_ServerRequest,
      RU_CORBA_TypeCode,
      RU_CORBA_TypeCode_Internals,
      RU_PolyORB,
      RU_PolyORB_Any,
      RU_PolyORB_Any_NVList,
      RU_PolyORB_Any_TypeCode,
      RU_PolyORB_Any_TypeCode_Internals,
      RU_PolyORB_Exceptions,
      RU_PolyORB_Initialization,
      RU_PolyORB_CORBA_P,
      RU_PolyORB_CORBA_P_Interceptors_Hooks,
      RU_PolyORB_CORBA_P_Exceptions,
      RU_PolyORB_Requests,
      RU_PolyORB_Types,
      RU_PolyORB_Utils,
      RU_PolyORB_Utils_Strings,
      RU_PolyORB_Utils_Strings_Lists,
      RU_PortableServer,
      RU_PortableServer_Internals,
      RU_Standard);

   --  Runtime Entities

   type RE_Id is
     (RE_Ref_0,                     --  Ref
      RE_To_Any_1,                  --  To_Any
      RE_Boolean_0,                 --  Boolean
      RE_False,                     --  False
      RE_True,                      --  True
      RE_On,                        --  On
      RE_Off,                       --  Off
      RE_Exception_Occurrence,      --  Ada.Exceptions.Exception_Occurrence
      RE_ARG_IN_0,                  --  CORBA.ARG_IN
      RE_ARG_OUT_0,                 --  CORBA.ARG_OUT
      RE_ARG_INOUT_0,               --  CORBA.ARG_INOUT
      RE_Default_Sys_Member,        --  CORBA.Default_Sys_Member
      RE_TC_Void,                   --  CORBA.TC_Void
      RE_Any,                       --  CORBA.Any
      RE_To_Any_0,                  --  CORBA.To_Any
      RE_From_Any_0,                --  CORBA.From_Any
      RE_Create_List_1,             --  CORBA.ExceptionList.Create_List
      RE_Add_1,                     --  CORBA.ExceptionList.Add
      RE_Ref_5,                     --  CORBA.ExceptionList.Ref
      RE_To_PolyORB_Ref_1,          --  CORBA.ExceptionList.Internals
      RE_Get_Aggregate_Element,     --  CORBA.Internals.Get_Aggregate_Element
      RE_Get_Empty_Any,             --  CORBA.Internals.Get_Empty_Any
      RE_Identifier_0,              --  CORBA.Identifier
      RE_Is_Equivalent,             --  CORBA.Is_Equivalent
      RE_Float,                     --  CORBA.Float
      RE_Double,                    --  CORBA.Double
      RE_Long_Double,               --  CORBA.Long_Double
      RE_Short,                     --  CORBA.Short
      RE_Long,                      --  CORBA.Long
      RE_Long_Long,                 --  CORBA.Long_Long
      RE_Unsigned_Short,            --  CORBA.Unsigned_Short
      RE_Unsigned_Long,             --  CORBA.Unsigned_Long
      RE_Unsigned_Long_Long,        --  CORBA.Unsigned_Long_Long
      RE_Char,                      --  CORBA.Char
      RE_WChar,                     --  CORBA.WChar
      RE_String_0,                  --  CORBA.String
      RE_Wide_String,               --  CORBA.Wide_String
      RE_Boolean,                   --  CORBA.Boolean
      RE_Octet,                     --  CORBA.Octet
      RE_TC_Float,                  --  CORBA.TC_Float
      RE_TC_Double,                 --  CORBA.TC_Double
      RE_TC_Long_Double,            --  CORBA.TC_Long_Double
      RE_TC_Short,                  --  CORBA.TC_Short
      RE_TC_Long,                   --  CORBA.TC_Long
      RE_TC_Long_Long,              --  CORBA.TC_Long_Long
      RE_TC_Unsigned_Short,         --  CORBA.TC_Unsigned_Short
      RE_TC_Unsigned_Long,          --  CORBA.TC_Unsigned_Long
      RE_TC_Unsigned_Long_Long,     --  CORBA.TC_Unsigned_Long_Long
      RE_TC_Char,                   --  CORBA.TC_Char
      RE_TC_WChar,                  --  CORBA.TC_WChar
      RE_TC_String,                 --  CORBA.TC_String
      RE_TC_Wide_String,            --  CORBA.TC_Wide_String
      RE_TC_Boolean,                --  CORBA.TC_Boolean
      RE_TC_Octet,                  --  CORBA.TC_Octet
      RE_To_Standard_String,        --  CORBA.To_Standard_String
      RE_IDL_Exception_Members,     --  CORBA.IDL_Exception_Members
      RE_Object_Is_Nil,             --  CORBA.Object_Is_Nil
      RE_Raise_Inv_Objref,          --  CORBA.Raise_Inv_Objref
      RE_Raise_Bad_Operation,       --  CORBA.Raise_Bad_Operation
      RE_Raise_Bad_Param,           --  CORBA.Raise_Bad_Param
      RE_To_CORBA_String,           --  CORBA.To_CORBA_String
      RE_Ref_1,                     --  CORBA.AbstractBase.Ref
      RE_Set_Type,                  --  CORBA.Internals.Set_Type
      RE_Get_Empty_Any_Aggregate,   --  CORBA.Internals.Get_Empty_Any_Agregate
      RE_Add_Aggregate_Element,     --  CORBA.Internals.Add_Aggregate_Element
      RE_To_CORBA_Any,              --  CORBA.Internals.To_CORBA_Any
      RE_To_PolyORB_Any,            --  CORBA.Internals.To_PolyORB_Any
      RE_Move_Any_Value,            --  CORBA.Internals.Move_Any_Value
      RE_Add_Item_0,                --  CORBA.NVList.Add_Item
      RE_Ref_4,                     --  CORBA.NVList.Ref
      RE_Is_Nil,                    --  CORBA.Object.Is_Nil
      RE_Ref_2,                     --  CORBA.Object.Ref
      RE_Object_Of,                 --  CORBA.Object.Object_Of
      RE_Is_A,                      --  CORBA.Object.Is_A
      RE_From_Any_1,                --  CORBA.Object.Helper.From_Any
      RE_TC_Object_0,               --  CORBA.Object.Helper.TC_Object
      RE_To_Any_3,                  --  CORBA.Object.Helper.To_Any
      RE_To_PolyORB_Ref,            --  CORBA.Object.Internals.To_PolyORB_Ref
      RE_Create_List,               --  CORBA.ORB.Create_List,
      RE_Object,                    --  CORBA.TypeCode.Object
      RE_Add_Parameter,             --  CORBA.TypeCode.Internals.Add_Parameter
      RE_To_CORBA_Object,           --  CORBA.TypeCode.
      --                            --    Internals.To_CORBA_Object
      RE_To_PolyORB_Object,         --  CORBA.TypeCode.
      --                            --    Internals.To_PolyORB_Object
      RE_Arguments,                 --  CORBA.ServerRequest.Arguments
      RE_Object_Ptr,                --  CORBA.ServerRequest.Object_ptr
      RE_Operation,                 --  CORBA.ServerRequest.Operation
      RE_Set_Exception,             --  CORBA.ServerRequest.Set_Exception
      RE_Set_Result,                --  CORBA.ServerRequest.Set_Result
      RE_Any_1,                     --  PolyORB.Any.Any
      RE_NamedValue,                --  PolyORB.Any.NamedValue
      RE_Is_Empty,                  --  PolyORB.Any.Is_Empty
      RE_ARG_IN_1,                  --  PolyORB.Any.ARG_IN
      RE_ARG_OUT_1,                 --  PolyORB.Any.ARG_OUT
      RE_ARG_INOUT_1,               --  PolyORB.Any.ARG_INOUT
      RE_Ref_3,                     --  PolyORB.Any.NVList.Ref
      RE_Create,                    --  PolyORB.Any.NVList.Create
      RE_Add_Item_1,                --  PolyORB.Any.NVList.Add_Item
      RE_TC_Object_1,                 --  PolyORB.Any.TypeCode.TC_Object
      RE_TC_Alias,                  --  PolyORB.Any.TypeCode.TC_Alias
      RE_TC_Enum,                   --  PolyORB.Any.TypeCode.TC_Enum
      RE_TC_Except,                 --  PolyORB.Any.TypeCode.TC_Except
      RE_TC_Struct,                 --  PolyORB.Any.TypeCode.TC_Struct
      RE_TC_Array,                  --  PolyORB.Any.TypeCode.TC_Array
      RE_TC_Union,                  --  PolyORB.Any.TypeCode.TC_Union
      RE_TC_Fixed,                  --  PolyORB.Any.TypeCode.TC_Fixed
      RE_Register_Exception,        --  PolyORB.Any.TypeCode.Register_Exception
      RE_User_Get_Members,          --  PolyORB.Exceptions.User_get_Members
      RE_User_Raise_Exception,      --  PolyORB.Exceptions.User_Raise_Exception
      RE_Module_Info,               --  PolyORB.Initialization.Module_Info
      RE_Register_Module,           --  PolyORB.Initialization.Register_Module
      RE_Request_Access,            --  PolyORB.Requests.Request_Access
      RE_Create_Request,            --  PolyORB.Requests.Create_Request
      RE_Flags,                     --  PolyORB.Requests.Flags
      RE_Sync_None,                 --  PolyORB.Requests.Sync_None
      RE_Sync_With_Transport,       --  PolyORB.Requests.Sync_With_Transport
      RE_Sync_With_Server,          --  PolyORB.Requests.Sync_With_Server
      RE_Sync_With_Target,          --  PolyORB.Requests.Sync_With_Target
      RE_Sync_Call_Back,            --  PolyORB.Requests.Sync_Call_Back
      RE_Destroy_Request,           --  PolyORB.Requests.Destroy_Request
      RE_Identifier,                --  PolyORB.Types.Identifier
      RE_To_PolyORB_String,         --  PolyORB.Types.To_PolyORB_String
      RE_Client_Invoke,             --  PolyORB.CORBA_P.
                                    --     Interceptors_Hooks.Client_Invoke
      RE_System_Exception_To_Any,   --  PolyORB.CORBA_P.
                                    --     Exceptions.System_Exception_To_Any
      RE_Raise_From_Any,            --  PolyORB.CORBA_P.
                                    --     Exceptions.Raise_From_Any
      RE_Add,                       --  PolyORB.Utils.Strings."+"
      RE_And,                       --  PolyORB.Utils.Strings.Lists."&"
      RE_Empty,                     --  PolyORB.Utils.Strings.Lists.Empty
      RE_Register_Skeleton,         --  PortableServer.Internals.
                                    --    Register_Skeleton
      RE_Servant,                   --  PortableServer.Servant
      RE_Servant_Base,              --  PortableServer.Servant_Base
      RE_String_2);                  --  Standard.String

   RE_Unit_Table : constant array (RE_Id) of RU_Id
     := (RE_Ref_0                   => RU_Null,
         RE_To_Any_1                => RU_Null,
         RE_Boolean_0               => RU_Null,
         RE_False                   => RU_Null,
         RE_True                    => RU_Null,
         RE_On                      => RU_Null,
         RE_Off                     => RU_Null,
         RE_Exception_Occurrence    => RU_Ada_Exceptions,
         RE_ARG_IN_0                => RU_CORBA,
         RE_ARG_OUT_0               => RU_CORBA,
         RE_ARG_INOUT_0             => RU_CORBA,
         RE_Default_Sys_Member      => RU_CORBA,
         RE_TC_Void                 => RU_CORBA,
         RE_Any                     => RU_CORBA,
         RE_To_Any_0                => RU_CORBA,
         RE_From_Any_0              => RU_CORBA,
         RE_Get_Aggregate_Element   => RU_CORBA_Internals,
         RE_Get_Empty_Any           => RU_CORBA_Internals,
         RE_Identifier_0            => RU_CORBA,
         RE_Is_Equivalent           => RU_CORBA,
         RE_Float                   => RU_CORBA,
         RE_Double                  => RU_CORBA,
         RE_Long_Double             => RU_CORBA,
         RE_Short                   => RU_CORBA,
         RE_Long                    => RU_CORBA,
         RE_Long_Long               => RU_CORBA,
         RE_Unsigned_Short          => RU_CORBA,
         RE_Unsigned_Long           => RU_CORBA,
         RE_Unsigned_Long_Long      => RU_CORBA,
         RE_Char                    => RU_CORBA,
         RE_WChar                   => RU_CORBA,
         RE_String_0                => RU_CORBA,
         RE_Wide_String             => RU_CORBA,
         RE_Boolean                 => RU_CORBA,
         RE_Octet                   => RU_CORBA,
         RE_TC_Float                => RU_CORBA,
         RE_TC_Double               => RU_CORBA,
         RE_TC_Long_Double          => RU_CORBA,
         RE_TC_Short                => RU_CORBA,
         RE_TC_Long                 => RU_CORBA,
         RE_TC_Long_Long            => RU_CORBA,
         RE_TC_Unsigned_Short       => RU_CORBA,
         RE_TC_Unsigned_Long        => RU_CORBA,
         RE_TC_Unsigned_Long_Long   => RU_CORBA,
         RE_TC_Char                 => RU_CORBA,
         RE_TC_WChar                => RU_CORBA,
         RE_TC_String               => RU_CORBA,
         RE_TC_Wide_String          => RU_CORBA,
         RE_TC_Boolean              => RU_CORBA,
         RE_TC_Octet                => RU_CORBA,
         RE_To_Standard_String      => RU_CORBA,
         RE_IDL_Exception_Members   => RU_CORBA,
         RE_Object_Is_Nil           => RU_CORBA,
         RE_To_CORBA_String         => RU_CORBA,
         RE_Raise_Bad_Operation     => RU_CORBA,
         RE_Raise_Inv_Objref        => RU_CORBA,
         RE_Raise_Bad_Param         => RU_CORBA,
         RE_Create_List_1           => RU_CORBA_ExceptionList,
         RE_Add_1                   => RU_CORBA_ExceptionList,
         RE_Ref_5                   => RU_CORBA_ExceptionList,
         RE_To_PolyORB_Ref_1        => RU_CORBA_ExceptionList_Internals,
         RE_Set_Type                => RU_CORBA_Internals,
         RE_Get_Empty_Any_Aggregate => RU_CORBA_Internals,
         RE_Add_Aggregate_Element   => RU_CORBA_Internals,
         RE_Ref_1                   => RU_CORBA_AbstractBase,
         RE_To_CORBA_Any            => RU_CORBA_Internals,
         RE_To_PolyORB_Any          => RU_CORBA_Internals,
         RE_Move_Any_Value          => RU_CORBA_Internals,
         RE_Add_Item_0              => RU_CORBA_NVList,
         RE_Ref_4                   => RU_CORBA_NVList,
         RE_Ref_2                   => RU_CORBA_Object,
         RE_Object_Of               => RU_CORBA_Object,
         RE_Is_A                    => RU_CORBA_Object,
         RE_Is_Nil                  => RU_CORBA_Object,
         RE_To_PolyORB_Ref          => RU_CORBA_Object_Internals,
         RE_From_Any_1              => RU_CORBA_Object_Helper,
         RE_TC_Object_0             => RU_CORBA_Object_Helper,
         RE_To_Any_3                => RU_CORBA_Object_Helper,
         RE_Create_List             => RU_CORBA_ORB,
         RE_Arguments               => RU_CORBA_ServerRequest,
         RE_Object_Ptr              => RU_CORBA_ServerRequest,
         RE_Operation               => RU_CORBA_ServerRequest,
         RE_Set_Exception           => RU_CORBA_ServerRequest,
         RE_Set_Result              => RU_CORBA_ServerRequest,
         RE_Object                  => RU_CORBA_TypeCode,
         RE_Add_Parameter           => RU_CORBA_TypeCode_Internals,
         RE_To_CORBA_Object         => RU_CORBA_TypeCode_Internals,
         RE_To_PolyORB_Object       => RU_CORBA_TypeCode_Internals,
         RE_Any_1                   => RU_PolyORB_Any,
         RE_Is_Empty                => RU_PolyORB_Any,
         RE_NamedValue              => RU_PolyORB_Any,
         RE_ARG_IN_1                => RU_PolyORB_Any,
         RE_ARG_OUT_1               => RU_PolyORB_Any,
         RE_ARG_INOUT_1             => RU_PolyORB_Any,
         RE_Ref_3                   => RU_PolyORB_Any_NVList,
         RE_Create                  => RU_PolyORB_Any_NVList,
         RE_Add_Item_1              => RU_PolyORB_Any_NVList,
         RE_TC_Alias                => RU_PolyORB_Any_TypeCode,
         RE_TC_Array                => RU_PolyORB_Any_TypeCode,
         RE_TC_Enum                 => RU_PolyORB_Any_TypeCode,
         RE_TC_Except               => RU_PolyORB_Any_TypeCode,
         RE_TC_Object_1             => RU_PolyORB_Any_TypeCode,
         RE_TC_Struct               => RU_PolyORB_Any_TypeCode,
         RE_TC_Union                => RU_PolyORB_Any_TypeCode,
         RE_TC_Fixed                => RU_PolyORB_Any_TypeCode,
         RE_Register_Exception      => RU_PolyORB_Exceptions,
         RE_User_Get_Members        => RU_PolyORB_Exceptions,
         RE_User_Raise_Exception    => RU_PolyORB_Exceptions,
         RE_Raise_From_Any          => RU_PolyORB_CORBA_P_Exceptions,
         RE_System_Exception_To_Any => RU_PolyORB_CORBA_P_Exceptions,
         RE_Client_Invoke           => RU_PolyORB_CORBA_P_Interceptors_Hooks,
         RE_Module_Info             => RU_PolyORB_Initialization,
         RE_Register_Module         => RU_PolyORB_Initialization,
         RE_Request_Access          => RU_PolyORB_Requests,
         RE_Create_Request          => RU_PolyORB_Requests,
         RE_Destroy_Request         => RU_PolyORB_Requests,
         RE_Flags                   => RU_PolyORB_Requests,
         RE_Sync_None               => RU_PolyORB_Requests,
         RE_Sync_With_Transport     => RU_PolyORB_Requests,
         RE_Sync_With_Server        => RU_PolyORB_Requests,
         RE_Sync_With_Target        => RU_PolyORB_Requests,
         RE_Sync_Call_Back          => RU_PolyORB_Requests,
         RE_Identifier              => RU_PolyORB_Types,
         RE_To_PolyORB_String       => RU_PolyORB_Types,
         RE_Add                     => RU_PolyORB_Utils_Strings,
         RE_And                     => RU_PolyORB_Utils_Strings_Lists,
         RE_Empty                   => RU_PolyORB_Utils_Strings_Lists,
         RE_Register_Skeleton       => RU_PortableServer_Internals,
         RE_Servant                 => RU_PortableServer,
         RE_Servant_Base            => RU_PortableServer,
         RE_String_2                => RU_Standard);

   procedure Initialize;

   function RE (Id : RE_Id) return Node_Id;
   --  Return a designator for entity Id

   function RU (Id : RU_Id) return Node_Id;
   --  Return a node for Unit id.

end Backend.BE_Ada.Runtime;
