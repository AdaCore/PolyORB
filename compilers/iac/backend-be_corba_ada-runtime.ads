------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--         B A C K E N D . B E _ C O R B A _ A D A . R U N T I M E          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

package Backend.BE_CORBA_Ada.Runtime is

   --  Runtime Units

   type RU_Id is
     (RU_Null,
      RU_Ada,
      RU_Ada_Exceptions,
      RU_Ada_Streams,
      RU_Ada_Unchecked_Deallocation,
      RU_CORBA,
      RU_CORBA_AbstractBase,
      RU_CORBA_Bounded_Strings,
      RU_CORBA_Bounded_Wide_Strings,
      RU_CORBA_Context,
      RU_CORBA_ExceptionList,
      RU_CORBA_ExceptionList_Internals,
      RU_CORBA_Forward,
      RU_CORBA_Fixed_Point,
      RU_CORBA_Helper,
      RU_CORBA_IDL_Sequences,
      RU_CORBA_IDL_Sequences_Helper,
      RU_CORBA_Internals,
      RU_CORBA_Local,
      RU_CORBA_Sequences,
      RU_CORBA_Sequences_Bounded,
      RU_CORBA_Sequences_Unbounded,
      RU_CORBA_NVList,
      RU_CORBA_NVList_Internals,
      RU_CORBA_ORB,
      RU_CORBA_ServerRequest,
      RU_CORBA_Object,   --  Begin: CORBA predefined entities
      RU_CORBA_OObject,  --  Workaround in orb.idl
      RU_CORBA_Current,
      RU_CORBA_Policy,
      RU_CORBA_DomainManager,
      RU_CORBA_TypeCode, --  End: CORBA predefined entities
      RU_CORBA_Current_Impl,
      RU_CORBA_Policy_Impl,
      RU_CORBA_Object_Impl,
      RU_CORBA_TypeCode_Impl,
      RU_CORBA_DomainManager_Impl,
      RU_CORBA_DomainManager_Helper,
      RU_CORBA_Object_Internals,
      RU_CORBA_Object_Helper,
      RU_CORBA_TypeCode_Internals,
      RU_PolyORB,
      RU_PolyORB_Annotations,
      RU_PolyORB_Any,
      RU_PolyORB_Any_NVList,
      RU_PolyORB_Any_TypeCode,
      RU_PolyORB_Any_TypeCode_Internals,
      RU_PolyORB_Buffers,
      RU_PolyORB_Buffers_Optimization,
      RU_PolyORB_Buffers_Optimization_Fixed_Point,
      RU_PolyORB_Exceptions,
      RU_PolyORB_Errors,
      RU_PolyORB_Initialization,
      RU_PolyORB_CORBA_P,
      RU_PolyORB_CORBA_P_Domain_Management,
      RU_PolyORB_CORBA_P_Interceptors_Hooks,
      RU_PolyORB_CORBA_P_IR_Hooks,
      RU_PolyORB_CORBA_P_Exceptions,
      RU_PolyORB_References,
      RU_PolyORB_Representations,
      RU_PolyORB_References_Binding,
      RU_PolyORB_Setup,
      RU_PolyORB_Smart_Pointers,
      RU_PolyORB_Reprsentations,
      RU_PolyORB_Representations_CDR,
      RU_PolyORB_Representations_CDR_GIOP_1_0,
      RU_PolyORB_Representations_CDR_GIOP_1_1,
      RU_PolyORB_Representations_CDR_GIOP_1_2,
      RU_PolyORB_Components,
      RU_PolyORB_Binding_Data,
      RU_PolyORB_Binding_Objects,
      RU_PolyORB_Binding_Data_GIOP,
      RU_PolyORB_Representations_CDR_Common,
      RU_PolyORB_Representations_CDR_Common_Fixed_Point,
      RU_PolyORB_Protocols,
      RU_PolyORB_Protocols_GIOP,
      RU_PolyORB_Protocols_GIOP_GIOP_1_0,
      RU_PolyORB_Protocols_GIOP_GIOP_1_1,
      RU_PolyORB_Protocols_GIOP_GIOP_1_2,
      RU_PolyORB_Requests,
      RU_PolyORB_Request_QoS,
      RU_PolyORB_Sequences,
      RU_PolyORB_Sequences_Bounded,
      RU_PolyORB_Sequences_Bounded_CORBA_Helper,
      RU_PolyORB_Sequences_Unbounded,
      RU_PolyORB_Sequences_Unbounded_CORBA_Helper,
      RU_PolyORB_Types,
      RU_PolyORB_Aligned_Types,
      RU_PolyORB_Aligned_Types_Bounded_Strings,
      RU_PolyORB_Aligned_Types_Bounded_Wide_Strings,
      RU_PolyORB_Aligned_Types_Sequences,
      RU_PolyORB_Aligned_Types_Sequences_Bounded,
      RU_PolyORB_Aligned_Types_Sequences_Unbounded,
      RU_PolyORB_Opaque,
      RU_PolyORB_Utils,
      RU_PolyORB_Utils_Strings,
      RU_PolyORB_Utils_Strings_Lists,
      RU_PortableServer,
      RU_PortableServer_Internals,
      RU_Standard,
      RU_Standard_ASCII);

   --  Runtime Entities

   type RE_Id is
     (RE_Null,                      --  work around to denote a null RE
      RE_Ref_0,                     --  Ref
      RE_To_Any_1,                  --  To_Any
      RE_Boolean_0,                 --  Boolean
      RE_False,                     --  False
      RE_True,                      --  True
      RE_On,                        --  On
      RE_Off,                       --  Off
      RE_Discriminant_Check,        --  Discriminant_Check
      RE_Range_Check,               --  Range_Check
      RE_Convert,                   --  Convert
      RE_Convert_Forward,           --  Convert_Forward
      RE_Element_TC,                --  Element_TC
      RE_Marshaller,                --  Marshaller
      RE_Sequence_TC,               --  Sequence_TC
      RE_LocalObject,               --  LocalObject
      RE_TC_Bounded_String,         --  TC_Bounded_String
      RE_TC_Bounded_Wide_String,    --  TC_Bounded_Wide_String
      RE_Unmarshaller,              --  Unmarshaller
      RE_Payload_Args,              --  Payload_Args
      RE_Length_2,                  --  Length
      RE_Get_Element,               --  Get_Element
      RE_Exception_Occurrence,      --  Ada.Exceptions.Exception_Occurrence
      RE_Exception_Information,     --  Ada.Exceptions.Exception_Information
      RE_Stream_Element_Count,      --  Ada.Streams.Stream_Element_Count
      RE_ARG_IN_0,                  --  CORBA.ARG_IN
      RE_ARG_OUT_0,                 --  CORBA.ARG_OUT
      RE_ARG_INOUT_0,               --  CORBA.ARG_INOUT
      RE_Default_Sys_Member,        --  CORBA.Default_Sys_Member
      RE_TC_Void,                   --  CORBA.TC_Void
      RE_To_Any_0,                  --  CORBA.To_Any
      RE_From_Any_0,                --  CORBA.From_Any
      RE_Wrap_2,                    --  CORBA.Wrap
      RE_Ref_11,                    --  CORBA.DomainManager.Ref
      RE_Object_8,                  --  CORBA.DomainManager.Impl.Object
      RE_From_Any_5,                --  CORBA.DomainManager.Helper.From_Any
      RE_To_Any_5,                  --  CORBA.DomainManager.Helper.To_Any
      RE_TC_DomainManager,          --  CORBA.DomainManager.Helper.
      --                                   TC_DomainManager
      RE_Create_List_1,             --  CORBA.ExceptionList.Create_List
      RE_Add_1,                     --  CORBA.ExceptionList.Add
      RE_Ref_5,                     --  CORBA.ExceptionList.Ref
      RE_To_PolyORB_Ref_1,          --  CORBA.ExceptionList.Internals
      RE_Get_Aggregate_Element,     --  CORBA.Internals.Get_Aggregate_Element
      RE_Get_Empty_Any,             --  CORBA.Internals.Get_Empty_Any
      --  Begin of the CORBA entities declared in orb.idl that may be invoked
      --  in user idl files
      RE_Any,                       --  CORBA.Any
      RE_Identifier_0,              --  CORBA.Identifier
      RE_RepositoryId,              --  CORBA.RepositoryId
      RE_ScopedName,                --  CORBA.ScopedName
      RE_Visibility,                --  CORBA.Visibility
      RE_PolicyType,                --  CORBA.PolicyType
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
      --  Original Sequence types
      RE_AnySeq_1,                  --  CORBA.AnySeq
      RE_FloatSeq_1,                --  CORBA.FloatSeq
      RE_DoubleSeq_1,               --  CORBA.DoubleSeq
      RE_LongDoubleSeq_1,           --  CORBA.LongDoubleSeq
      RE_ShortSeq_1,                --  CORBA.ShortSeq
      RE_LongSeq_1,                 --  CORBA.LongSeq
      RE_LongLongSeq_1,             --  CORBA.LongLongSeq
      RE_UShortSeq_1,               --  CORBA.UShort
      RE_ULongSeq_1,                --  CORBA.ULongSeq
      RE_ULongLongSeq_1,            --  CORBA.ULongLongSeq
      RE_CharSeq_1,                 --  CORBA.CharSeq
      RE_WCharSeq_1,                --  CORBA.WCharSeq
      RE_StringSeq_1,               --  CORBA.StringSeq
      RE_WStringSeq_1,              --  CORBA.WStringSeq
      RE_BooleanSeq_1,              --  CORBA.BooleanSeq
      RE_OctetSeq_1,                --  CORBA.OctetSeq
      RE_PolicyList_1,              --  CORBA.PolicyList

      --  CORBA Sequence types

      RE_AnySeq_2,                  --  CORBA.IDL_Sequences.AnySeq
      RE_FloatSeq_2,                --  CORBA.IDL_Sequences.FloatSeq
      RE_DoubleSeq_2,               --  CORBA.IDL_Sequences.DoubleSeq
      RE_LongDoubleSeq_2,           --  CORBA.IDL_Sequences.LongDoubleSeq
      RE_ShortSeq_2,                --  CORBA.IDL_Sequences.ShortSeq
      RE_LongSeq_2,                 --  CORBA.IDL_Sequences.LongSeq
      RE_LongLongSeq_2,             --  CORBA.IDL_Sequences.LongLongSeq
      RE_UShortSeq_2,               --  CORBA.IDL_Sequences.UShort
      RE_ULongSeq_2,                --  CORBA.IDL_Sequences.ULongSeq
      RE_ULongLongSeq_2,            --  CORBA.IDL_Sequences.ULongLongSeq
      RE_CharSeq_2,                 --  CORBA.IDL_Sequences.CharSeq
      RE_WCharSeq_2,                --  CORBA.IDL_Sequences.WCharSeq
      RE_StringSeq_2,               --  CORBA.IDL_Sequences.StringSeq
      RE_WStringSeq_2,              --  CORBA.IDL_Sequences.WStringSeq
      RE_BooleanSeq_2,              --  CORBA.IDL_Sequences.BooleanSeq
      RE_OctetSeq_2,                --  CORBA.IDL_Sequences.OctetSeq
      RE_PolicyList_2,              --  CORBA.Policy.PolicyList

      --  End of the CORBA entities declared in orb.idl that may be
      --  invoked in user idl files

      RE_TC_AnySeq,                 --  CORBA.IDL_Sequences.Helper.TC_AnySeq
      RE_TC_FloatSeq,               --  CORBA.IDL_Sequences.Helper.TC_FloatSeq
      RE_TC_DoubleSeq,              --  CORBA.IDL_Sequences.Helper.TC_DoubleSeq
      RE_TC_LongDoubleSeq,          --  CORBA.IDL_Sequences.Helper.
      --                                   TC_LongDoubleSeq
      RE_TC_ShortSeq,               --  CORBA.IDL_Sequences.Helper.TC_ShortSeq
      RE_TC_LongSeq,                --  CORBA.IDL_Sequences.Helper.TC_LongSeq
      RE_TC_LongLongSeq,            --  CORBA.IDL_Sequences.Helper.
      --                                   TC_LongLongSeq
      RE_TC_UShortSeq,              --  CORBA.IDL_Sequences.Helper.TC_UShort
      RE_TC_ULongSeq,               --  CORBA.IDL_Sequences.Helper.TC_ULongSeq
      RE_TC_ULongLongSeq,           --  CORBA.IDL_Sequences.Helper.
      --                                   TC_ULongLongSeq
      RE_TC_CharSeq,                --  CORBA.IDL_Sequences.Helper.TC_CharSeq
      RE_TC_WCharSeq,               --  CORBA.IDL_Sequences.Helper.TC_WCharSeq
      RE_TC_StringSeq,              --  CORBA.IDL_Sequences.Helper.TC_StringSeq
      RE_TC_WStringSeq,             --  CORBA.IDL_Sequences.Helper.
      --                                   TC_WStringSeq
      RE_TC_BooleanSeq,             --  CORBA.IDL_Sequences.Helper.
      --                                   TC_BooleanSeq
      RE_TC_OctetSeq,               --  CORBA.IDL_Sequences.Helper.TC_OctetSeq
      RE_From_Any_4,                --  CORBA.IDL_Sequences.Helper.From_Any
      RE_To_Any_4,                  --  CORBA.IDL_Sequences.Helper.To_Any
      RE_Wrap_4,                    --  CORBA.IDL_Sequences.Helper.Wrap
      RE_Is_Equivalent,             --  CORBA.Is_Equivalent
      RE_TC_Any,                    --  CORBA.TC_Any
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
      RE_TC_TypeCode,               --  CORBA.TC_TypeCode
      RE_TC_Null,                   --  CORBA.TC_Null
      RE_TC_Buffer,                 --  PolyORB.Any.TC_Buffer
      RE_TC_RepositoryId,           --  CORBA.Helper.TC_RepositoryId
      RE_TC_Identifier,             --  CORBA.Helper.TC_Identifier
      RE_TC_ScopedName,             --  CORBA.Helper.TC_ScopedName
      RE_TC_Visibility,             --  CORBA.Helper.TC_Visibility
      RE_TC_PolicyType,             --  CORBA.Helper.TC_PolicyType
      RE_From_Any_2,                --  CORBA.Helper.From_Any
      RE_To_Any_2,                  --  CORBA.Helper.To_Any
      RE_To_Standard_String,        --  CORBA.To_Standard_String
      RE_To_Standard_Wide_String,   --  CORBA.To_Standard_Wide_String
      RE_IDL_Exception_Members,     --  CORBA.IDL_Exception_Members
      RE_Object_2,                  --  CORBA.Local.Object
      RE_Object_Is_Nil,             --  CORBA.Object_Is_Nil
      RE_Raise_Inv_Objref,          --  CORBA.Raise_Inv_Objref
      RE_Raise_Bad_Operation,       --  CORBA.Raise_Bad_Operation
      RE_Raise_Bad_Param,           --  CORBA.Raise_Bad_Param
      RE_To_CORBA_String,           --  CORBA.To_CORBA_String
      RE_To_CORBA_Wide_String,      --  CORBA.To_CORBA_Wide_String
      RE_Ref_1,                     --  CORBA.AbstractBase.Ref
      RE_Ref_8,                     --  CORBA.Context.Ref
      RE_Set_Type,                  --  CORBA.Internals.Set_Type
      RE_Get_Empty_Any_Aggregate,   --  CORBA.Internals.Get_Empty_Any_Agregate
      RE_Add_Aggregate_Element,     --  CORBA.Internals.Add_Aggregate_Element
      RE_Get_Wrapper_Any,           --  CORBA.Internals.Get_Wrapper_Any
      RE_To_CORBA_Any,              --  CORBA.Internals.To_CORBA_Any
      RE_To_PolyORB_Any,            --  CORBA.Internals.To_PolyORB_Any
      RE_Buff_Access_To_Ulong,      --  Conversion
      RE_Move_Any_Value,            --  CORBA.Internals.Move_Any_Value
      RE_Add_Item_0,                --  CORBA.NVList.Add_Item
      RE_Ref_4,                     --  CORBA.NVList.Ref
      RE_Clone_Out_Args,            --  CORBA.NVList.Internals.Clone_Out_Args
      RE_Is_Nil,                    --  CORBA.Object.Is_Nil
      RE_Ref_2,                     --  CORBA.Object.Ref
      RE_Object_Of,                 --  CORBA.Object.Object_Of
      RE_Is_A,                      --  CORBA.Object.Is_A
      RE_From_Any_1,                --  CORBA.Object.Helper.From_Any
      RE_TC_Object_0,               --  CORBA.Object.Helper.TC_Object
      RE_To_Any_3,                  --  CORBA.Object.Helper.To_Any
      RE_Wrap_3,                    --  CORBA.Object.Helper.Wrap
      RE_To_PolyORB_Ref,            --  CORBA.Object.Internals.To_PolyORB_Ref
      RE_To_CORBA_Ref,              --  CORBA.Object.Internals.To_CORBA_Ref
      RE_Ref_6,                     --  CORBA.Policy.Ref
      RE_Object_3,                  --  CORBA.Policy.Impl.Object
      RE_Local_Ref,                 --  CORBA.Current.Local_Ref
      RE_Object_4,                  --  CORBA.Current.Impl.Object
      RE_Object_5,                  --  CORBA.Object.Impl.Object
      RE_Object_6,                  --  CORBA.TypeCode.Impl.Object
      RE_Create_List,               --  CORBA.ORB.Create_List,
      RE_Get_Default_Context,       --  CORBA.ORB.Get_Default_Context
      RE_Object,                    --  CORBA.TypeCode.Object
      RE_Add_Parameter,             --  CORBA.TypeCode.Internals.Add_Parameter
      RE_Build_Sequence_TC,         --  CORBA.TypeCode.
      --                            --    Internals.Build_Sequence_TC
      RE_To_CORBA_Object,           --  CORBA.TypeCode.
      --                            --    Internals.To_CORBA_Object
      RE_To_PolyORB_Object,         --  CORBA.TypeCode.
      --                            --    Internals.To_PolyORB_Object
      RE_Arguments_1,               --  CORBA.ServerRequest.Arguments
      RE_Object_Ptr,                --  CORBA.ServerRequest.Object_ptr
      RE_Operation,                 --  CORBA.ServerRequest.Operation
      RE_Set_Exception,             --  CORBA.ServerRequest.Set_Exception
      RE_Set_Result,                --  CORBA.ServerRequest.Set_Result

      --  Begin of the aligned entities (types) used for marshalling

      RE_Float_10,                  --  PolyORB.Aligned_Types.Float
      RE_Double_10,                 --  PolyORB.Aligned_Types.Double
      RE_Long_Double_10,            --  PolyORB.Aligned_Types.Long_Double
      RE_Short_10,                  --  PolyORB.Aligned_Types.Short
      RE_Long_10,                   --  PolyORB.Aligned_Types.Long
      RE_Long_Long_10,              --  PolyORB.Aligned_Types.Long_Long
      RE_Unsigned_Short_10,         --  PolyORB.Aligned_Types.Unsigned_Short
      RE_Unsigned_Long_10,          --  PolyORB.Aligned_Types.Unsigned_Long
      RE_Unsigned_Long_Long_10,     --  PolyORB.Aligned_Types.
      --                                   Unsigned_Long_Long
      RE_Char_10,                   --  PolyORB.Aligned_Types.Char
      RE_Wchar_10,                  --  PolyORB.Aligned_Types.Wchar
      RE_String_10,                 --  PolyORB.Aligned_Types.String
      RE_Wide_String_10,            --  PolyORB.Aligned_Types.Wide_String
      RE_Boolean_10,                --  PolyORB.Aligned_Types.Boolean
      RE_Octet_10,                  --  PolyORB.Aligned_Types.Octet
      RE_Sequence_10,               --  PolyORB.Aligned_Types.Sequence
      RE_Fixed_Point_10,            --  PolyORB.Aligned_Types.Fixed_Point
      RE_Set_Note,                  --  PolyORB.Annotations.Set_Note
      RE_Aggregate_Content,         --  PolyORB.Any.Agregate_Content
      RE_Any_1,                     --  PolyORB.Any.Any
      RE_Any_Container,             --  PolyORB.Any.Any_Container
      RE_By_Value,                  --  PolyORB.Any.By_Value
      RE_By_Reference,              --  PolyORB.Any.By_Reference
      RE_From_Any_3,                --  PolyORB.Any.From_Any
      RE_Get_Aggregate_Element_2,   --  PolyORB.Any.Get_Aggregate_Element
      RE_Get_Container,             --  PolyORB.Any.Get_Container
      RE_Get_Value,                 --  PolyORB.Any.Get_Value
      RE_Content,                   --  PolyORB.Any.Content
      RE_Content_Ptr,               --  PolyORB.Any.Content_Ptr
      RE_NamedValue,                --  PolyORB.Any.NamedValue
      RE_Is_Empty,                  --  PolyORB.Any.Is_Empty
      RE_Mechanism,                 --  PolyORB.Any.Mechanism
      RE_ARG_IN_1,                  --  PolyORB.Any.ARG_IN
      RE_ARG_OUT_1,                 --  PolyORB.Any.ARG_OUT
      RE_ARG_INOUT_1,               --  PolyORB.Any.ARG_INOUT
      RE_TC_Unsigned_Long_1,        --  PolyORB.Any.TC_Unsigned_Long
      RE_Set_Type_1,                --  PolyORB.Any.Set_Type
      RE_Set_Value,                 --  PolyORB.Any.Set_Type
      RE_Wrap_1,                    --  PolyORB.Any.Wrap
      RE_Ref_3,                     --  PolyORB.Any.NVList.Ref
      RE_Create,                    --  PolyORB.Any.NVList.Create
      RE_Add_Item_1,                --  PolyORB.Any.NVList.Add_Item
      RE_Build_Bounded_String_TC,   --  PolyORB.Any.TypeCode.
      --                                  Build_Bounded_String_TC
      RE_Build_Bounded_Wide_String_TC, --  PolyORB.Any.TypeCode.
      --                                  Build_Bounded_Wide_String_TC

      RE_Object_7,                  --  PolyORB.Any.TypeCode.Object
      RE_TC_Object_1,               --  PolyORB.Any.TypeCode.TC_Object
      RE_TC_Alias,                  --  PolyORB.Any.TypeCode.TC_Alias
      RE_TC_Enum,                   --  PolyORB.Any.TypeCode.TC_Enum
      RE_TC_Except,                 --  PolyORB.Any.TypeCode.TC_Except
      RE_TC_Struct,                 --  PolyORB.Any.TypeCode.TC_Struct
      RE_TC_Array,                  --  PolyORB.Any.TypeCode.TC_Array
      RE_TC_Union,                  --  PolyORB.Any.TypeCode.TC_Union
      RE_TC_Fixed,                  --  PolyORB.Any.TypeCode.TC_Fixed
      RE_Register_Exception,        --  PolyORB.Any.TypeCode.Register_Exception
      RE_Buffer_Access,             --  PolyORB.Buffers.Buffer_Access
      RE_Buffer_Type,               --  PolyORB.Buffers.Buffer_Type
      RE_Align_Position,            --  PolyORB.Buffers.Align_Position
      RE_Alignment_Type,            --  PolyORB.Buffers.Alignment_Type
      RE_Pad_Align,                 --  PolyORB.Buffers.Pad_Align
      RE_Client_Entity,             --  PolyORB.Protocols.GIOP.Client_Entity
      RE_Server_Entity,             --  PolyORB.Protocols.GIOP.Server_Entity
      RE_CDR_Representation_Access, --  PolyORB.Representation.CDR.
      --                                 CDR_Representation_Access
      RE_Bind,                      --  PolyORB.Binding_Data.Bind
      RE_Get_Request_QoS,           --  PolyORB.Request_QoS.Get_Request_QoS
      RE_Binding_Object_Access,     --  PolyORB_Binding_Object.
      --                                  Binding_Object_Access
      RE_The_ORB,                   --  PolyORB.Setup.The_ORB
      RE_Ref_10,                    --  PolyORB.Smart_Pointers.Ref
      RE_GIOP_Session,              --  PolyORB.Protocols.GIOP.GIOP_Session
      RE_Get_Component,             --  PolyORB.Binding_Object.Get_Component
      RE_Get_Profile,               --  PolyORB.Binding_Object.Get_Profile
      RE_Entity_Of,                 --  PolyORB.Smart_Pointers.Entity_Of
      RE_Component_Access,          --  PolyORB_Components.Component_Access
      RE_Profile_Access,            --  PolyORB.Binding_Data.Profile_Access
      RE_Get_Representation,        --  PolyORB.Protocols.GIOP.
      --                                  Get_Representation
      RE_Get_Buffer,                --  PolyORB.Protocols.GIOP.
      --                                  Get_Buffer
      RE_Get_GIOP_Version,          --  PolyORB.Binding_Data.GIOP
      RE_Negotiate_Code_Set_And_Update_Session,
      --                                PolyORB.Protocols.GIOP.GIOP_1_0.
      --                                 Negociate_Code_Set_And_Update_Session
      RE_Release,                   --  PolyORB.buffers.Release
      RE_Entity_Role,               --  PolyORB.Protocols.GIOP.Entity_Role
      RE_Operation_Payload,         --  PolyORB.Protocols.GIOP.
      --                                   Operation_Payload
      RE_CDR_Representation,        --  PolyORB.Representations.
      --                                   CDR.CDR_Representation
      RE_Error_Container,           --  PolyORB.Errors.Error_Container
      RE_Found,                     --  PolyORB.Errors.Found
      RE_User_Get_Members,          --  PolyORB.Exceptions.User_get_Members
      RE_User_Raise_Exception,      --  PolyORB.Exceptions.User_Raise_Exception
      RE_Module_Info,               --  PolyORB.Initialization.Module_Info
      RE_Register_Module,           --  PolyORB.Initialization.Register_Module
      RE_Ref_9,                     --  PolyORB.References.Ref
      RE_Marshall_1,                --  PolyORB.Representations.CDR.Marshall
      RE_Marshall_2,                --  PolyORB.Representations.CDR.Common.
      --                                   Marshall
      RE_Unmarshall_1,              --  PolyORB.Representations.CDR.Unmarshall
      RE_Unmarshall_2,              --  PolyORB.Representations.CDR.Common.
      --                                   Unmarshall
      RE_Pad_Compute,               --  PolyORB.Buffers.Optimization.
                                    --     Pad_Compute
      RE_Type_Size,                 --  PolyORB.Buffers.Optimization.
                                    --     Type_Size
      RE_CDR_Position,              --  PolyORB.Buffers.CDR_Position
      RE_Length,                    --  PolyORB.Buffers.Length
      RE_Preallocate_Buffer,        --  PolyORB.Buffers.Optimization.
                                    --     Preallocate_Buffer
      RE_Extract_Data,              --  PolyORB.Buffers.Extract_Data
      RE_Insert_Raw_Data,           --  PolyIRB.buffers.Optimization.
                                    --     Insert_Raw_Data
      RE_Opaque_Pointer,            --  PolyORB.Opaque.Opaque_Pointer
      RE_Arguments_2,               --  PolyORB.Requests.Arguments
      RE_Request_Access,            --  PolyORB.Requests.Request_Access
      RE_Request_Args,              --  PolyORB.Requests.Request_Args
      RE_Request_Args_Access,       --  PolyORB.Requests.Request_Args_Access
      RE_Request_Payload,           --  PolyORB.Requests.Request_Payload
      RE_Request_Payload_Access,    --  PolyORB.Requests.Request_Payload_Access
      RE_Create_Request,            --  PolyORB.Requests.Create_Request
      RE_Flags,                     --  PolyORB.Requests.Flags
      RE_Sync_None,                 --  PolyORB.Requests.Sync_None
      RE_Sync_With_Transport,       --  PolyORB.Requests.Sync_With_Transport
      RE_Sync_With_Server,          --  PolyORB.Requests.Sync_With_Server
      RE_Sync_With_Target,          --  PolyORB.Requests.Sync_With_Target
      RE_Sync_Call_Back,            --  PolyORB.Requests.Sync_Call_Back
      RE_Destroy_Request,           --  PolyORB.Requests.Destroy_Request
      RE_CORBA_Helper_1,            --  PolyORB.Sequences.Bounded.CORBA_Helper
      RE_CORBA_Helper_2,            --  PolyORB.Sequences.Unbounded.
      --                                   CORBA_Helper
      RE_Identifier,                --  PolyORB.Types.Identifier
      RE_Long_1,                    --  PolyORB.Types.Long
      RE_Long_Long_1,               --  PolyORB.Types.Long_Long
      RE_Short_1,                   --  PolyORB.Types.Short
      RE_Unsigned_Long_Long_1,      --  PolyORB.Types.Unisigned_Long_Long
      RE_Unsigned_Short_1,          --  PolyORB.Types.Unisigned_Short
      RE_Float_1,                   --  PolyORB.Types.Float
      RE_Double_1,                  --  PolyORB.Types.Double
      RE_Long_Double_1,             --  PolyORB.Types.Long_Double
      RE_Char_1,                    --  PolyORB.Types.Char
      RE_Wchar_1,                   --  PolyORB.Types.Wchar
      RE_Octet_1,                   --  PolyORB.Types.Octet
      RE_Boolean_1,                 --  PolyORB.Types.Boolean
      RE_Wide_String_1,             --  PolyORB.Types.Wide_String
      RE_String_1,                  --  PolyORB.Types.String
      RE_Unsigned_Long_1,           --  PolyORB.Types.Unsigned_Long
      RE_To_PolyORB_String,         --  PolyORB.Types.To_PolyORB_String
      RE_To_PolyORB_Wide_String,    --  PolyORB.Types.To_PolyORB_Wide_String
      RE_To_Standard_String_1,      --  PolyORB.Types.To_Standard_String
      RE_To_Standard_Wide_String_1, --  PolyORB.Types.To_Standard_Wide_String
      RE_Get_Domain_Managers,       --  PolyORB.CORBA_P.Domain_Management.
      --                                   Get_Domain_Managers
      RE_Get_Interface_Definition,  --  PolyORB.CORBA_P.IR_Hooks.
      --                                   Get_Interface_Definition
      RE_Client_Invoke,             --  PolyORB.CORBA_P.
      --                                   Interceptors_Hooks.Client_Invoke
      RE_Extract_Ada_Exception_Information, --  PolyORB.CORBA_P.
      --                         Exceptions.Extract_Ada_Exception_Information
      RE_Set_Ada_Exception_Information, --  PolyORB.CORBA_P.
      --                         Exceptions.Set_Ada_Exception_Information
      RE_System_Exception_To_Any,   --  PolyORB.CORBA_P.
      --                                   Exceptions.System_Exception_To_Any
      RE_Raise_From_Any,            --  PolyORB.CORBA_P.
      --                                   Exceptions.Raise_From_Any
      RE_Raise_From_Error,          --  PolyORB.CORBA_P.
      --                                   Exceptions.Raise_From_Error
      RE_Add,                       --  PolyORB.Utils.Strings."+"
      RE_And,                       --  PolyORB.Utils.Strings.Lists."&"
      RE_Empty,                     --  PolyORB.Utils.Strings.Lists.Empty
      RE_Register_Skeleton,         --  PortableServer.Internals.
      --                                   Register_Skeleton
      RE_Servant,                   --  PortableServer.Servant
      RE_Servant_Base,              --  PortableServer.Servant_Base
      RE_Nul,                       --  Standard.ASCII.Nul
      RE_Boolean_2,                 --  Standard.Boolean
      RE_Integer,                   --  Standard.Integer
      RE_Natural,                   --  Standard.Natural
      RE_Positive,                  --  Standard.Positive
      RE_String_2);                 --  Standard.String

   --  Predefined CORBA interfaces that may be used directly in IDL
   --  files
   subtype CORBA_Predefined_RU is RU_Id range
     RU_CORBA_Object .. RU_CORBA_TypeCode;

   --  Predefined CORBA entities that may be used directly in IDL
   --  files
   subtype CORBA_Predefined_RE is RE_Id range
     RE_Any .. RE_PolicyList_2;

   CORBA_Predefined_RU_Table : constant array (CORBA_Predefined_RU) of RE_Id
     := (RU_CORBA_Object        => RE_Ref_2,
         RU_CORBA_OObject       => RE_Ref_2,
         RU_CORBA_Current       => RE_Local_Ref,
         RU_CORBA_Policy        => RE_Ref_6,
         RU_CORBA_DomainManager => RE_Ref_11,
         RU_CORBA_TypeCode      => RE_Object);

   CORBA_Predefined_Implem_Table : constant
     array (CORBA_Predefined_RU) of RE_Id
     := (RU_CORBA_Object        => RE_Object_5,
         RU_CORBA_OObject       => RE_Object_5,
         RU_CORBA_Current       => RE_Object_4,
         RU_CORBA_Policy        => RE_Object_3,
         RU_CORBA_DomainManager => RE_Object_8,
         RU_CORBA_TypeCode      => RE_Object_6);

   CORBA_Predefined_RE_Table : constant array (CORBA_Predefined_RE) of RE_Id
     := (RE_Any                => RE_Any,
         RE_Identifier_0       => RE_Identifier_0,
         RE_RepositoryId       => RE_RepositoryId,
         RE_ScopedName         => RE_ScopedName,
         RE_Visibility         => RE_Visibility,
         RE_PolicyType         => RE_PolicyType,
         RE_Float              => RE_Float,
         RE_Double             => RE_Double,
         RE_Long_Double        => RE_Long_Double,
         RE_Short              => RE_Short,
         RE_Long               => RE_Long,
         RE_Long_Long          => RE_Long_Long,
         RE_Unsigned_Short     => RE_Unsigned_Short,
         RE_Unsigned_Long      => RE_Unsigned_Long,
         RE_Unsigned_Long_Long => RE_Unsigned_Long_Long,
         RE_Char               => RE_Char,
         RE_WChar              => RE_WChar,
         RE_String_0           => RE_String_0,
         RE_Wide_String        => RE_Wide_String,
         RE_Boolean            => RE_Boolean,
         RE_Octet              => RE_Octet,
         RE_AnySeq_1           => RE_AnySeq_2,
         RE_FloatSeq_1         => RE_FloatSeq_2,
         RE_DoubleSeq_1        => RE_DoubleSeq_2,
         RE_LongDoubleSeq_1    => RE_LongDoubleSeq_2,
         RE_ShortSeq_1         => RE_ShortSeq_2,
         RE_LongSeq_1          => RE_LongSeq_2,
         RE_LongLongSeq_1      => RE_LongLongSeq_2,
         RE_UShortSeq_1        => RE_UShortSeq_2,
         RE_ULongSeq_1         => RE_ULongSeq_2,
         RE_ULongLongSeq_1     => RE_ULongLongSeq_2,
         RE_CharSeq_1          => RE_CharSeq_2,
         RE_WCharSeq_1         => RE_WCharSeq_2,
         RE_StringSeq_1        => RE_StringSeq_2,
         RE_WStringSeq_1       => RE_WStringSeq_2,
         RE_BooleanSeq_1       => RE_BooleanSeq_2,
         RE_OctetSeq_1         => RE_OctetSeq_2,
         RE_PolicyList_1       => RE_PolicyList_2,
         RE_AnySeq_2           => RE_AnySeq_2,
         RE_FloatSeq_2         => RE_FloatSeq_2,
         RE_DoubleSeq_2        => RE_DoubleSeq_2,
         RE_LongDoubleSeq_2    => RE_LongDoubleSeq_2,
         RE_ShortSeq_2         => RE_ShortSeq_2,
         RE_LongSeq_2          => RE_LongSeq_2,
         RE_LongLongSeq_2      => RE_LongLongSeq_2,
         RE_UShortSeq_2        => RE_UShortSeq_2,
         RE_ULongSeq_2         => RE_ULongSeq_2,
         RE_ULongLongSeq_2     => RE_ULongLongSeq_2,
         RE_CharSeq_2          => RE_CharSeq_2,
         RE_WCharSeq_2         => RE_WCharSeq_2,
         RE_StringSeq_2        => RE_StringSeq_2,
         RE_WStringSeq_2       => RE_WStringSeq_2,
         RE_BooleanSeq_2       => RE_BooleanSeq_2,
         RE_OctetSeq_2         => RE_OctetSeq_2,
         RE_PolicyList_2       => RE_PolicyList_2);

   RE_Unit_Table : constant array (RE_Id) of RU_Id
     := (RE_Null                      => RU_Null,
         RE_Ref_0                     => RU_Null,
         RE_To_Any_1                  => RU_Null,
         RE_Boolean_0                 => RU_Null,
         RE_False                     => RU_Null,
         RE_Marshaller                => RU_Null,
         RE_True                      => RU_Null,
         RE_On                        => RU_Null,
         RE_Off                       => RU_Null,
         RE_Discriminant_Check        => RU_Null,
         RE_Range_Check               => RU_Null,
         RE_Convert                   => RU_Null,
         RE_Convert_Forward           => RU_Null,
         RE_Element_TC                => RU_Null,
         RE_LocalObject               => RU_Null,
         RE_Sequence_TC               => RU_Null,
         RE_TC_Bounded_String         => RU_Null,
         RE_TC_Bounded_Wide_String    => RU_Null,
         RE_Unmarshaller              => RU_Null,
         RE_Payload_Args              => RU_Null,
         RE_Length_2                  => RU_Null,
         RE_Get_Element               => RU_Null,
         RE_Exception_Occurrence      => RU_Ada_Exceptions,
         RE_Exception_Information     => RU_Ada_Exceptions,
         RE_Stream_Element_Count      => RU_Ada_Streams,
         RE_ARG_IN_0                  => RU_CORBA,
         RE_ARG_OUT_0                 => RU_CORBA,
         RE_ARG_INOUT_0               => RU_CORBA,
         RE_Default_Sys_Member        => RU_CORBA,
         RE_TC_Void                   => RU_CORBA,
         RE_To_Any_0                  => RU_CORBA,
         RE_From_Any_0                => RU_CORBA,
         RE_Wrap_2                    => RU_CORBA,
         RE_Get_Aggregate_Element     => RU_CORBA_Internals,
         RE_Get_Empty_Any             => RU_CORBA_Internals,
         RE_Any                       => RU_CORBA,
         RE_Identifier_0              => RU_CORBA,
         RE_RepositoryId              => RU_CORBA,
         RE_ScopedName                => RU_CORBA,
         RE_Visibility                => RU_CORBA,
         RE_PolicyType                => RU_CORBA,
         RE_Float                     => RU_CORBA,
         RE_Double                    => RU_CORBA,
         RE_Long_Double               => RU_CORBA,
         RE_Short                     => RU_CORBA,
         RE_Long                      => RU_CORBA,
         RE_Long_Long                 => RU_CORBA,
         RE_Unsigned_Short            => RU_CORBA,
         RE_Unsigned_Long             => RU_CORBA,
         RE_Unsigned_Long_Long        => RU_CORBA,
         RE_Char                      => RU_CORBA,
         RE_WChar                     => RU_CORBA,
         RE_String_0                  => RU_CORBA,
         RE_Wide_String               => RU_CORBA,
         RE_Boolean                   => RU_CORBA,
         RE_Octet                     => RU_CORBA,
         RE_AnySeq_1                  => RU_CORBA,
         RE_FloatSeq_1                => RU_CORBA,
         RE_DoubleSeq_1               => RU_CORBA,
         RE_LongDoubleSeq_1           => RU_CORBA,
         RE_ShortSeq_1                => RU_CORBA,
         RE_LongSeq_1                 => RU_CORBA,
         RE_LongLongSeq_1             => RU_CORBA,
         RE_UShortSeq_1               => RU_CORBA,
         RE_ULongSeq_1                => RU_CORBA,
         RE_ULongLongSeq_1            => RU_CORBA,
         RE_CharSeq_1                 => RU_CORBA,
         RE_WCharSeq_1                => RU_CORBA,
         RE_StringSeq_1               => RU_CORBA,
         RE_WStringSeq_1              => RU_CORBA,
         RE_BooleanSeq_1              => RU_CORBA,
         RE_OctetSeq_1                => RU_CORBA,
         RE_PolicyList_1              => RU_CORBA,
         RE_Float_10                  => RU_PolyORB_Aligned_Types,
         RE_Double_10                 => RU_PolyORB_Aligned_Types,
         RE_Long_Double_10            => RU_PolyORB_Aligned_Types,
         RE_Short_10                  => RU_PolyORB_Aligned_Types,
         RE_Long_10                   => RU_PolyORB_Aligned_Types,
         RE_Long_Long_10              => RU_PolyORB_Aligned_Types,
         RE_Unsigned_Short_10         => RU_PolyORB_Aligned_Types,
         RE_Unsigned_Long_10          => RU_PolyORB_Aligned_Types,
         RE_Unsigned_Long_Long_10     => RU_PolyORB_Aligned_Types,
         RE_Char_10                   => RU_PolyORB_Aligned_Types,
         RE_Wchar_10                  => RU_PolyORB_Aligned_Types,
         RE_String_10                 => RU_PolyORB_Aligned_Types,
         RE_Wide_String_10            => RU_PolyORB_Aligned_Types,
         RE_Boolean_10                => RU_PolyORB_Aligned_Types,
         RE_Octet_10                  => RU_PolyORB_Aligned_Types,
         RE_Sequence_10               => RU_PolyORB_Aligned_Types,
         RE_Fixed_Point_10            => RU_PolyORB_Aligned_Types,
         RE_AnySeq_2                  => RU_CORBA_IDL_Sequences,
         RE_FloatSeq_2                => RU_CORBA_IDL_Sequences,
         RE_DoubleSeq_2               => RU_CORBA_IDL_Sequences,
         RE_LongDoubleSeq_2           => RU_CORBA_IDL_Sequences,
         RE_ShortSeq_2                => RU_CORBA_IDL_Sequences,
         RE_LongSeq_2                 => RU_CORBA_IDL_Sequences,
         RE_LongLongSeq_2             => RU_CORBA_IDL_Sequences,
         RE_UShortSeq_2               => RU_CORBA_IDL_Sequences,
         RE_ULongSeq_2                => RU_CORBA_IDL_Sequences,
         RE_ULongLongSeq_2            => RU_CORBA_IDL_Sequences,
         RE_CharSeq_2                 => RU_CORBA_IDL_Sequences,
         RE_WCharSeq_2                => RU_CORBA_IDL_Sequences,
         RE_StringSeq_2               => RU_CORBA_IDL_Sequences,
         RE_WStringSeq_2              => RU_CORBA_IDL_Sequences,
         RE_BooleanSeq_2              => RU_CORBA_IDL_Sequences,
         RE_OctetSeq_2                => RU_CORBA_IDL_Sequences,
         RE_PolicyList_2              => RU_CORBA_Policy,
         RE_TC_AnySeq                 => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_FloatSeq               => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_DoubleSeq              => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_LongDoubleSeq          => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_ShortSeq               => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_LongSeq                => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_LongLongSeq            => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_UShortSeq              => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_ULongSeq               => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_ULongLongSeq           => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_CharSeq                => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_WCharSeq               => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_StringSeq              => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_WStringSeq             => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_BooleanSeq             => RU_CORBA_IDL_Sequences_Helper,
         RE_TC_OctetSeq               => RU_CORBA_IDL_Sequences_Helper,
         RE_From_Any_4                => RU_CORBA_IDL_Sequences_Helper,
         RE_To_Any_4                  => RU_CORBA_IDL_Sequences_Helper,
         RE_Wrap_4                    => RU_CORBA_IDL_Sequences_Helper,
         RE_Is_Equivalent             => RU_CORBA,
         RE_TC_Any                    => RU_CORBA,
         RE_TC_Float                  => RU_CORBA,
         RE_TC_Double                 => RU_CORBA,
         RE_TC_Long_Double            => RU_CORBA,
         RE_TC_Short                  => RU_CORBA,
         RE_TC_Long                   => RU_CORBA,
         RE_TC_Long_Long              => RU_CORBA,
         RE_TC_Unsigned_Short         => RU_CORBA,
         RE_TC_Unsigned_Long          => RU_CORBA,
         RE_TC_Unsigned_Long_Long     => RU_CORBA,
         RE_TC_Char                   => RU_CORBA,
         RE_TC_WChar                  => RU_CORBA,
         RE_TC_String                 => RU_CORBA,
         RE_TC_Wide_String            => RU_CORBA,
         RE_TC_Boolean                => RU_CORBA,
         RE_TC_Octet                  => RU_CORBA,
         RE_TC_TypeCode               => RU_CORBA,
         RE_TC_Null                   => RU_CORBA,
         RE_TC_Buffer                 => RU_PolyORB_Any,
         RE_TC_RepositoryId           => RU_CORBA_Helper,
         RE_TC_Identifier             => RU_CORBA_Helper,
         RE_TC_ScopedName             => RU_CORBA_Helper,
         RE_TC_Visibility             => RU_CORBA_Helper,
         RE_TC_PolicyType             => RU_CORBA_Helper,
         RE_From_Any_2                => RU_CORBA_Helper,
         RE_To_Any_2                  => RU_CORBA_Helper,
         RE_To_Standard_String        => RU_CORBA,
         RE_To_Standard_Wide_String   => RU_CORBA,
         RE_IDL_Exception_Members     => RU_CORBA,
         RE_Object_Is_Nil             => RU_CORBA,
         RE_To_CORBA_String           => RU_CORBA,
         RE_To_CORBA_Wide_String      => RU_CORBA,
         RE_Raise_Bad_Operation       => RU_CORBA,
         RE_Raise_Inv_Objref          => RU_CORBA,
         RE_Raise_Bad_Param           => RU_CORBA,
         RE_Ref_11                    => RU_CORBA_DomainManager,
         RE_Object_8                  => RU_CORBA_DomainManager_Impl,
         RE_From_Any_5                => RU_CORBA_DomainManager_Helper,
         RE_To_Any_5                  => RU_CORBA_DomainManager_Helper,
         RE_TC_DomainManager          => RU_CORBA_DomainManager_Helper,
         RE_Create_List_1             => RU_CORBA_ExceptionList,
         RE_Add_1                     => RU_CORBA_ExceptionList,
         RE_Ref_5                     => RU_CORBA_ExceptionList,
         RE_To_PolyORB_Ref_1          => RU_CORBA_ExceptionList_Internals,
         RE_Set_Type                  => RU_CORBA_Internals,
         RE_Get_Empty_Any_Aggregate   => RU_CORBA_Internals,
         RE_Add_Aggregate_Element     => RU_CORBA_Internals,
         RE_Get_Wrapper_Any           => RU_CORBA_Internals,
         RE_Ref_1                     => RU_CORBA_AbstractBase,
         RE_Ref_8                     => RU_CORBA_Context,
         RE_To_CORBA_Any              => RU_CORBA_Internals,
         RE_To_PolyORB_Any            => RU_CORBA_Internals,
         RE_Buff_Access_To_Ulong      => RU_PolyORB_Buffers_Optimization,
         RE_Move_Any_Value            => RU_CORBA_Internals,
         RE_Object_2                  => RU_CORBA_Local,
         RE_Add_Item_0                => RU_CORBA_NVList,
         RE_Ref_4                     => RU_CORBA_NVList,
         RE_Clone_Out_Args            => RU_CORBA_NVList_Internals,
         RE_Ref_2                     => RU_CORBA_Object,
         RE_Object_Of                 => RU_CORBA_Object,
         RE_Is_A                      => RU_CORBA_Object,
         RE_Is_Nil                    => RU_CORBA_Object,
         RE_To_PolyORB_Ref            => RU_CORBA_Object_Internals,
         RE_To_CORBA_Ref              => RU_CORBA_Object_Internals,
         RE_From_Any_1                => RU_CORBA_Object_Helper,
         RE_TC_Object_0               => RU_CORBA_Object_Helper,
         RE_To_Any_3                  => RU_CORBA_Object_Helper,
         RE_Wrap_3                    => RU_CORBA_Object_Helper,
         RE_Ref_6                     => RU_CORBA_Policy,
         RE_Object_3                  => RU_CORBA_Policy_Impl,
         RE_Local_Ref                 => RU_CORBA_Current,
         RE_Object_4                  => RU_CORBA_Current_Impl,
         RE_Object_5                  => RU_CORBA_Object_Impl,
         RE_Object_6                  => RU_CORBA_TypeCode_Impl,
         RE_Create_List               => RU_CORBA_ORB,
         RE_Get_Default_Context       => RU_CORBA_ORB,
         RE_Arguments_1               => RU_CORBA_ServerRequest,
         RE_Object_Ptr                => RU_CORBA_ServerRequest,
         RE_Operation                 => RU_CORBA_ServerRequest,
         RE_Set_Exception             => RU_CORBA_ServerRequest,
         RE_Set_Result                => RU_CORBA_ServerRequest,
         RE_Object                    => RU_CORBA_TypeCode,
         RE_Add_Parameter             => RU_CORBA_TypeCode_Internals,
         RE_Build_Sequence_TC         => RU_CORBA_TypeCode_Internals,
         RE_To_CORBA_Object           => RU_CORBA_TypeCode_Internals,
         RE_To_PolyORB_Object         => RU_CORBA_TypeCode_Internals,
         RE_Set_Note                  => RU_PolyORB_Annotations,
         RE_Aggregate_Content         => RU_PolyORB_Any,
         RE_Any_1                     => RU_PolyORB_Any,
         RE_Any_Container             => RU_PolyORB_Any,
         RE_By_Value                  => RU_PolyORB_Any,
         RE_By_Reference              => RU_PolyORB_Any,
         RE_Content                   => RU_PolyORB_Any,
         RE_Content_Ptr               => RU_PolyORB_Any,
         RE_From_Any_3                => RU_PolyORB_Any,
         RE_Get_Aggregate_Element_2   => RU_PolyORB_Any,
         RE_Get_Container             => RU_PolyORB_Any,
         RE_Get_Value                 => RU_PolyORB_Any,
         RE_Is_Empty                  => RU_PolyORB_Any,
         RE_Mechanism                 => RU_PolyORB_Any,
         RE_NamedValue                => RU_PolyORB_Any,
         RE_ARG_IN_1                  => RU_PolyORB_Any,
         RE_ARG_OUT_1                 => RU_PolyORB_Any,
         RE_ARG_INOUT_1               => RU_PolyORB_Any,
         RE_TC_Unsigned_Long_1        => RU_PolyORB_Any,
         RE_Set_Type_1                => RU_PolyORB_Any,
         RE_Set_Value                 => RU_PolyORB_Any,
         RE_Wrap_1                    => RU_PolyORB_Any,
         RE_Ref_3                     => RU_PolyORB_Any_NVList,
         RE_Create                    => RU_PolyORB_Any_NVList,
         RE_Add_Item_1                => RU_PolyORB_Any_NVList,
         RE_Build_Bounded_String_TC   => RU_PolyORB_Any_TypeCode,
         RE_Build_Bounded_Wide_String_TC =>
           RU_PolyORB_Any_TypeCode,
         RE_Object_7                  => RU_PolyORB_Any_TypeCode,
         RE_TC_Alias                  => RU_PolyORB_Any_TypeCode,
         RE_TC_Array                  => RU_PolyORB_Any_TypeCode,
         RE_TC_Enum                   => RU_PolyORB_Any_TypeCode,
         RE_TC_Except                 => RU_PolyORB_Any_TypeCode,
         RE_TC_Object_1               => RU_PolyORB_Any_TypeCode,
         RE_TC_Struct                 => RU_PolyORB_Any_TypeCode,
         RE_TC_Union                  => RU_PolyORB_Any_TypeCode,
         RE_TC_Fixed                  => RU_PolyORB_Any_TypeCode,
         RE_Buffer_Access             => RU_PolyORB_Buffers,
         RE_Buffer_Type               => RU_PolyORB_Buffers,
         RE_Align_Position            => RU_PolyORB_Buffers,
         RE_Alignment_Type            => RU_PolyORB_Buffers,
         RE_Pad_Align                 => RU_PolyORB_Buffers,
         RE_Client_Entity             => RU_PolyORB_Protocols_GIOP,
         RE_Server_Entity             => RU_PolyORB_Protocols_GIOP,
         RE_Negotiate_Code_Set_And_Update_Session =>
           RU_PolyORB_Protocols_GIOP_GIOP_1_2,
         RE_Release                   => RU_PolyORB_Buffers,
         RE_CDR_Representation_Access => RU_PolyORB_Representations_CDR,
         RE_Bind                      => RU_PolyORB_References_Binding,
         RE_Get_Request_QoS           => RU_PolyORB_Request_QoS,
         RE_Binding_Object_Access     => RU_PolyORB_Binding_Objects,
         RE_The_ORB                   => RU_PolyORB_Setup,
         RE_Ref_10                    => RU_PolyORB_Smart_Pointers,
         RE_GIOP_Session              => RU_PolyORB_Protocols_GIOP,
         RE_Get_Component             => RU_PolyORB_Binding_Objects,
         RE_Get_Profile               => RU_PolyORB_Binding_Objects,
         RE_Entity_Of                 => RU_PolyORB_Smart_Pointers,
         RE_Get_Representation        => RU_PolyORB_Protocols_GIOP,
         RE_Get_Buffer                => RU_PolyORB_Protocols_GIOP,
         RE_Component_Access          => RU_PolyORB_Components,
         RE_Profile_Access            => RU_PolyORB_Binding_Data,
         RE_Get_GIOP_Version          => RU_PolyORB_Binding_Data_GIOP,
         RE_Entity_Role               => RU_PolyORB_Protocols_GIOP,
         RE_Operation_Payload         => RU_PolyORB_Protocols_GIOP,
         RE_Error_Container           => RU_PolyORB_Errors,
         RE_Found                     => RU_PolyORB_Errors,
         RE_Register_Exception        => RU_PolyORB_Exceptions,
         RE_User_Get_Members          => RU_PolyORB_Exceptions,
         RE_User_Raise_Exception      => RU_PolyORB_Exceptions,
         RE_Raise_From_Any            => RU_PolyORB_CORBA_P_Exceptions,
         RE_Extract_Ada_Exception_Information =>
           RU_PolyORB_CORBA_P_Exceptions,
         RE_Set_Ada_Exception_Information =>
           RU_PolyORB_CORBA_P_Exceptions,
         RE_Raise_From_Error          => RU_PolyORB_CORBA_P_Exceptions,
         RE_System_Exception_To_Any   => RU_PolyORB_CORBA_P_Exceptions,
         RE_Get_Domain_Managers       => RU_PolyORB_CORBA_P_Domain_Management,
         RE_Get_Interface_Definition  => RU_PolyORB_CORBA_P_IR_Hooks,
         RE_Client_Invoke             => RU_PolyORB_CORBA_P_Interceptors_Hooks,
         RE_Module_Info               => RU_PolyORB_Initialization,
         RE_Register_Module           => RU_PolyORB_Initialization,
         RE_Ref_9                     => RU_PolyORB_References,
         RE_CDR_Representation        => RU_PolyORB_Representations_CDR,
         RE_Marshall_1                => RU_PolyORB_Representations_CDR,
         RE_Marshall_2                => RU_PolyORB_Representations_CDR_Common,
         RE_Unmarshall_1              => RU_PolyORB_Representations_CDR,
         RE_Unmarshall_2              => RU_PolyORB_Representations_CDR_Common,

         RE_Pad_Compute               => RU_PolyORB_Buffers_Optimization,
         RE_Type_Size                 => RU_PolyORB_Buffers_Optimization,
         RE_CDR_Position              => RU_PolyORB_Buffers,
         RE_Length                    => RU_PolyORB_Buffers,
         RE_Preallocate_Buffer        => RU_PolyORB_Buffers_Optimization,
         RE_Extract_Data              => RU_PolyORB_Buffers,
         RE_Insert_Raw_Data           => RU_PolyORB_Buffers_Optimization,
         RE_Opaque_Pointer            => RU_PolyORB_Opaque,

         RE_Arguments_2               => RU_PolyORB_Requests,
         RE_Request_Access            => RU_PolyORB_Requests,
         RE_Request_Args              => RU_PolyORB_Requests,
         RE_Request_Args_Access       => RU_PolyORB_Requests,
         RE_Request_Payload           => RU_PolyORB_Requests,
         RE_Request_Payload_Access    => RU_PolyORB_Requests,
         RE_Create_Request            => RU_PolyORB_Requests,
         RE_Destroy_Request           => RU_PolyORB_Requests,
         RE_Flags                     => RU_PolyORB_Requests,
         RE_Sync_None                 => RU_PolyORB_Requests,
         RE_Sync_With_Transport       => RU_PolyORB_Requests,
         RE_Sync_With_Server          => RU_PolyORB_Requests,
         RE_Sync_With_Target          => RU_PolyORB_Requests,
         RE_Sync_Call_Back            => RU_PolyORB_Requests,
         RE_CORBA_Helper_1            =>
           RU_PolyORB_Sequences_Bounded_CORBA_Helper,
         RE_CORBA_Helper_2            =>
           RU_PolyORB_Sequences_Unbounded_CORBA_Helper,
         RE_Identifier                => RU_PolyORB_Types,
         RE_Long_1                    => RU_PolyORB_Types,
         RE_Short_1                   => RU_PolyORB_Types,
         RE_Long_Long_1               => RU_PolyORB_Types,
         RE_Unsigned_Long_Long_1      => RU_PolyORB_Types,
         RE_Unsigned_Short_1          => RU_PolyORB_Types,
         RE_Float_1                   => RU_PolyORB_Types,
         RE_Double_1                  => RU_PolyORB_Types,
         RE_Long_Double_1             => RU_PolyORB_Types,
         RE_Char_1                    => RU_PolyORB_Types,
         RE_Wchar_1                   => RU_PolyORB_Types,
         RE_Octet_1                   => RU_PolyORB_Types,
         RE_Boolean_1                 => RU_PolyORB_Types,
         RE_Wide_String_1             => RU_PolyORB_Types,
         RE_String_1                  => RU_PolyORB_Types,
         RE_Unsigned_Long_1           => RU_PolyORB_Types,
         RE_To_PolyORB_String         => RU_PolyORB_Types,
         RE_To_PolyORB_Wide_String    => RU_PolyORB_Types,
         RE_To_Standard_String_1      => RU_PolyORB_Types,
         RE_To_Standard_Wide_String_1 => RU_PolyORB_Types,
         RE_Add                       => RU_PolyORB_Utils_Strings,
         RE_And                       => RU_PolyORB_Utils_Strings_Lists,
         RE_Empty                     => RU_PolyORB_Utils_Strings_Lists,
         RE_Register_Skeleton         => RU_PortableServer_Internals,
         RE_Servant                   => RU_PortableServer,
         RE_Servant_Base              => RU_PortableServer,
         RE_Nul                       => RU_Standard_ASCII,
         RE_Boolean_2                 => RU_Standard,
         RE_Positive                  => RU_Standard,
         RE_Integer                   => RU_Standard,
         RE_Natural                   => RU_Standard,
         RE_String_2                  => RU_Standard);

   procedure Initialize;

   function RE
     (Id     : RE_Id;
      Withed : Boolean := True)
     return Node_Id;
   --  Return a designator for entity Id

   function RU
     (Id     : RU_Id;
      Withed : Boolean := True)
     return Node_Id;
   --  Return a node for Unit id

end Backend.BE_CORBA_Ada.Runtime;
