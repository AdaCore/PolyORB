with Types; use Types;

package Backend.BE_Ada.Runtime is

   --  Runtime Units

   type RU_Id is
     (RU_Null,
      RU_Ada,
      RU_Ada_Exceptions,
      RU_CORBA,
      RU_CORBA_AbstractBase,
      RU_CORBA_Object,
      RU_CORBA_Object_Helper,
      RU_CORBA_TypeCode,
      RU_PolyORB,
      RU_PolyORB_Any,
      RU_PolyORB_Any_NVList,
      RU_PolyORB_Requests,
      RU_PolyORB_Types,
      RU_PolyORB_CORBA_P,
      RU_PolyORB_CORBA_P_Interceptors_Hooks,
      RU_PolyORB_CORBA_P_Exceptions,
      RU_Standard,
      RU_PortableServer);

   --  Runtime Entities

   type RE_Id is
     (RE_Ref_0,                     --  Ref
      RE_To_Any_1,                  --  CORBA.To_Any
      RE_Get_Empty_Any_1,           --  CORBA.Get_Empty_Any
      RE_Exception_Occurrence,      --  Ada.Exceptions.Exception_Occurrence
      RE_TC_Void,                   --  CORBA.TC_Void
      RE_Any,                       --  CORBA.Any
      RE_To_Any_0,                  --  CORBA.To_Any
      RE_Get_Empty_Any_0,           --  CORBA.Get_Empty_Any
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
      RE_String_1,                  --  CORBA.String
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
      RE_IDL_Exception_Members,     --  CORBA.IDL_Exception_Members
      RE_Object_Is_Nil,             --  CORBA.Object_Is_Nil
      RE_Raise_Inv_Objref,          --  CORBA.Raise_Inv_Objref
      RE_Raise_Bad_Param,           --  CORBA.Raise_Bad_Param
      RE_To_PolyORB_Ref,            --  CORBA.To_PolyORB_Ref
      RE_To_CORBA_String,           --  CORBA.To_CORBA_String
      RE_Ref_1,                     --  CORBA.AbstractBase.Ref
      RE_Set_Type,                  --  CORBA.Set_Type
      RE_Get_Empty_Any_Aggregate,   --  CORBA.Get_Empty_Any_Agregate
      RE_Add_Aggregate_Element,     --  CORBA.Add_Aggregate_Element
      RE_Is_Nil,                    --  CORBA.Object.Is_Nil
      RE_Ref_2,                     --  CORBA.Object.Ref
      RE_Object_Of,                 --  CORBA.Object.Object_Of
      RE_Is_A,                      --  CORBA.Object.Is_A
      RE_To_Any_3,                  --  CORBA.Object.Helper.To_Any
      RE_From_Any,                  --  CORBA.Object.Helper.From_Any
      RE_Object,                    --  CORBA.TypeCode.Object
      RE_To_CORBA_Object,     --  CORBA.TypeCode.Internals.To_CORBA_Object
      RE_NamedValue,                --  PolyORB.Any.NamedValue
      RE_Is_Empty,                  --  PolyORB.Any.Is_Empty
      RE_Ref_3,                     --  PolyORB.Any.NVList.Ref
      RE_Create,                    --  PolyORB.Any.NVList.Create
      RE_TC_Object,                 --  PolyORB.Any.TypeCode.TC_Object
      RE_TC_Alias,                  --  PolyORB.Any.TypeCode.TC_Alias
      RE_TC_Enum,                   --  PolyORB.Any.TypeCode.TC_Enum
      RE_TC_Struct,                 --  PolyORB.Any.TypeCode.TC_Struct
      RE_TC_Array,                  --  PolyORB.Any.TypeCode.TC_Array
      RE_Request_Access,            --  PolyORB.Requests.Request_Access
      RE_Create_Request,            --  PolyORB.Requests.Create_Request
      RE_Flags,                     --  PolyORB.Requests.Flags
      RE_Destroy_Request,           --  PolyORB.Requests.Destroy_Request
      RE_Identifier,                --  PolyORB.Types.Identifier
      RE_To_PolyORB_String,         --  PolyORB.Types.To_PolyORB_String
      RE_Client_Invoke,    --  PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
      RE_Raise_From_Any,   --  PolyORB.CORBA_P.Exceptions.Raise_From_Any
      RE_String_2,                  --  Standard.String
      RE_Servant_Base);             --  PortableServer.Servant_Base

   RE_Unit_Table : array (RE_Id) of RU_Id

     := (RE_Ref_0                 => RU_Null,
         RE_To_Any_1              => RU_Null,
         RE_Get_Empty_Any_1       => RU_Null,
         RE_Exception_Occurrence  => RU_Ada_Exceptions,
         RE_TC_Void               => RU_CORBA,
         RE_Any                   => RU_CORBA,
         RE_To_Any_0              => RU_CORBA,
         RE_Get_Empty_Any_0       => RU_CORBA,
         RE_Float                 => RU_CORBA,
         RE_Double                => RU_CORBA,
         RE_Long_Double           => RU_CORBA,
         RE_Short                 => RU_CORBA,
         RE_Long                  => RU_CORBA,
         RE_Long_Long             => RU_CORBA,
         RE_Unsigned_Short        => RU_CORBA,
         RE_Unsigned_Long         => RU_CORBA,
         RE_Unsigned_Long_Long    => RU_CORBA,
         RE_Char                  => RU_CORBA,
         RE_WChar                 => RU_CORBA,
         RE_String_1              => RU_CORBA,
         RE_Wide_String           => RU_CORBA,
         RE_Boolean               => RU_CORBA,
         RE_Octet                 => RU_CORBA,
         RE_TC_Float              => RU_CORBA,
         RE_TC_Double             => RU_CORBA,
         RE_TC_Long_Double        => RU_CORBA,
         RE_TC_Short              => RU_CORBA,
         RE_TC_Long               => RU_CORBA,
         RE_TC_Long_Long          => RU_CORBA,
         RE_TC_Unsigned_Short     => RU_CORBA,
         RE_TC_Unsigned_Long      => RU_CORBA,
         RE_TC_Unsigned_Long_Long => RU_CORBA,
         RE_TC_Char               => RU_CORBA,
         RE_TC_WChar              => RU_CORBA,
         RE_TC_String             => RU_CORBA,
         RE_TC_Wide_String        => RU_CORBA,
         RE_TC_Boolean            => RU_CORBA,
         RE_TC_Octet              => RU_CORBA,
         RE_IDL_Exception_Members => RU_CORBA,
         RE_Object_Is_Nil         => RU_CORBA,
         RE_To_CORBA_String       => RU_CORBA,
         RE_Raise_Inv_Objref      => RU_CORBA,
         RE_Raise_Bad_Param       => RU_CORBA,
         RE_To_PolyORB_Ref        => RU_CORBA,
         RE_Ref_1                 => RU_CORBA_AbstractBase,
         RE_Set_Type              => RU_CORBA,
         RE_Get_Empty_Any_Aggregate => RU_CORBA,
         RE_Add_Aggregate_Element => RU_CORBA,
         RE_Ref_2                 => RU_CORBA_Object,
         RE_Object_Of             => RU_CORBA_Object,
         RE_Is_A                  => RU_CORBA_Object,
         RE_To_Any_3              => RU_CORBA_Object_Helper,
         RE_From_Any              => RU_CORBA_Object_Helper,
         RE_Is_Nil                => RU_CORBA_Object,
         RE_Object                => RU_CORBA,
         RE_To_CORBA_Object       => RU_CORBA,
         RE_NamedValue            => RU_PolyORB_Any,
         RE_Is_Empty              => RU_PolyORB_Any,
         RE_Ref_3                 => RU_PolyORB_Any_NVList,
         RE_Create                => RU_PolyORB_Any_NVList,
         RE_TC_Object             => RU_PolyORB_Any,
         RE_TC_Alias              => RU_PolyORB_Any,
         RE_TC_Enum               => RU_PolyORB_Any,
         RE_TC_Struct             => RU_PolyORB_Any,
         RE_TC_Array              => RU_PolyORB_Any,
         RE_Request_Access        => RU_PolyORB_Requests,
         RE_Create_Request        => RU_PolyORB_Requests,
         RE_Flags                 => RU_PolyORB_Requests,
         RE_Destroy_Request       => RU_PolyORB_Requests,
         RE_Identifier            => RU_PolyORB_Types,
         RE_To_PolyORB_String     => RU_PolyORB_Types,
         RE_Client_Invoke         => RU_PolyORB_CORBA_P_Interceptors_Hooks,
         RE_Raise_From_Any      => RU_PolyORB_CORBA_P_Exceptions,
         RE_String_2              => RU_Standard,
         RE_Servant_Base          => RU_PortableServer);

   procedure Initialize;

   function RE (Id : RE_Id; Witheded : Boolean := True) return Node_Id;
   --  Return a designator for entity Id

   function RU (Id : RU_Id) return Node_Id;
   --  Return a node for Unit id.

end Backend.BE_Ada.Runtime;
