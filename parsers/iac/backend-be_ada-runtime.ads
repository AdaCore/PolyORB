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
      RU_CORBA_TypeCode,
      RU_PolyORB,
      RU_PolyORB_Any,
      RU_PolyORB_Any_NVList,
      RU_PolyORB_Any_TypeCode,
      RU_PolyORB_Requests,
      RU_PolyORB_Types,
      RU_Standard);

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
      RE_Octet,                     --  CORBA.Boolean
      RE_IDL_Exception_Members,     --  CORBA.IDL_Exception_Members
      RE_Object_Is_Nil,             --  CORBA.Object_Is_Nil
      RE_Raise_Inv_Objref,          --  CORBA.Raise_Inv_Objref
      RE_To_CORBA_String,           --  CORBA.To_CORBA_String
      RE_Ref_1,                     --  CORBA.AbstractBase.Ref
      RE_Is_Nil,                    --  CORBA.Object.Is_Nil
      RE_Ref_2,                     --  CORBA.Object.Ref
      RE_Object,                    --  CORBA.TypeCode.Object
      RE_NamedValue,                --  PolyORB.Any.NamedValue
      RE_Ref_3,                     --  PolyORB.Any.NVList.Ref
      RE_Create,                    --  PolyORB.Any.NVList.Create
      RE_TC_Object,                 --  PolyORB.Any.TypeCode.TC_Object
      RE_Request_Access,            --  PolyORB.Requests.Request_Access
      RE_Identifier,                --  PolyORB.Types.Identifier
      RE_To_PolyORB_String,         --  PolyORB.Types.To_PolyORB_String
      RE_String_2);                 --  Standard.String

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
         RE_IDL_Exception_Members => RU_CORBA,
         RE_Object_Is_Nil         => RU_CORBA,
         RE_To_CORBA_String       => RU_CORBA,
         RE_Raise_Inv_Objref      => RU_CORBA,
         RE_Ref_1                 => RU_CORBA_AbstractBase,
         RE_Ref_2                 => RU_CORBA_Object,
         RE_Is_Nil                => RU_CORBA_Object,
         RE_Object                => RU_CORBA_TypeCode,
         RE_NamedValue            => RU_PolyORB_Any,
         RE_Ref_3                 => RU_PolyORB_Any_NVList,
         RE_Create                => RU_PolyORB_Any_NVList,
         RE_TC_Object             => RU_PolyORB_Any_TypeCode,
         RE_Request_Access        => RU_PolyORB_Requests,
         RE_Identifier            => RU_PolyORB_Types,
         RE_To_PolyORB_String     => RU_PolyORB_Types,
         RE_String_2              => RU_Standard);

   procedure Initialize;

   function RE (Id : RE_Id) return Node_Id;
   --  Return a designator for entity Id
   function RU (Id : RU_Id) return Node_Id;
   --  Return a node for Unit id.

end Backend.BE_Ada.Runtime;
