----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

package CORBA.Repository_Root.Helper is

   pragma Elaborate_Body;

   TC_DefinitionKind : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.DefinitionKind;

   function To_Any (Item : in CORBA.Repository_Root.DefinitionKind)
      return CORBA.Any;

   TC_VersionSpec : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.VersionSpec;

   function To_Any (Item : in CORBA.Repository_Root.VersionSpec)
      return CORBA.Any;

--   TC_IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward : CORBA.TypeCode.Object :=
--      CORBA.TypeCode.TC_Sequence;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward.Sequence;
--
--   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward.Sequence)
--      return CORBA.Any;
--
--   TC_ContainedSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
--     TC_Alias;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.ContainedSeq;
--
--   function To_Any (Item : in CORBA.Repository_Root.ContainedSeq)
--      return CORBA.Any;
--
--   TC_IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward : CORBA.TypeCode.Object :=
--      CORBA.TypeCode.TC_Sequence;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward.Sequence;
--
--   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward.Sequence)
--      return CORBA.Any;
--
--   TC_InterfaceDefSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
--      TC_Alias;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.InterfaceDefSeq;
--
--   function To_Any (Item : in CORBA.Repository_Root.InterfaceDefSeq)
--      return CORBA.Any;
--
--   TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward : CORBA.TypeCode.Object :=
--      CORBA.TypeCode.TC_Sequence;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward.Sequence;
--
--   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward.Sequence)
--      return CORBA.Any;
--
--   TC_ValueDefSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
--      TC_Alias;
--
--   function From_Any (Item : in CORBA.Any)
--      return CORBA.Repository_Root.ValueDefSeq;
--
--   function To_Any (Item : in CORBA.Repository_Root.ValueDefSeq)
--      return CORBA.Any;

   TC_OperationMode : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.OperationMode;

   function To_Any (Item : in CORBA.Repository_Root.OperationMode)
      return CORBA.Any;

   TC_ParameterMode : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ParameterMode;

   function To_Any (Item : in CORBA.Repository_Root.ParameterMode)
      return CORBA.Any;

   TC_ParameterDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ParameterDescription;

   function To_Any (Item : in CORBA.Repository_Root.ParameterDescription)
      return CORBA.Any;

   TC_Visibility : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.Visibility;

   function To_Any (Item : in CORBA.Repository_Root.Visibility)
      return CORBA.Any;

   TC_ValueMember : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ValueMember;

   function To_Any (Item : in CORBA.Repository_Root.ValueMember)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward.Sequence)
      return CORBA.Any;

   TC_ExceptionDefSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ExceptionDefSeq;

   function To_Any (Item : in CORBA.Repository_Root.ExceptionDefSeq)
      return CORBA.Any;

   TC_StructMember : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.StructMember;

   function To_Any (Item : in CORBA.Repository_Root.StructMember)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_StructMember : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_StructMember.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_StructMember.Sequence)
      return CORBA.Any;

   TC_StructMemberSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.StructMemberSeq;

   function To_Any (Item : in CORBA.Repository_Root.StructMemberSeq)
      return CORBA.Any;

   TC_Initializer : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.Initializer;

   function To_Any (Item : in CORBA.Repository_Root.Initializer)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_Initializer : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Initializer.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_Initializer.Sequence)
      return CORBA.Any;

   TC_InitializerSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.InitializerSeq;

   function To_Any (Item : in CORBA.Repository_Root.InitializerSeq)
      return CORBA.Any;

   TC_UnionMember : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.UnionMember;

   function To_Any (Item : in CORBA.Repository_Root.UnionMember)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_UnionMember : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_UnionMember.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_UnionMember.Sequence)
      return CORBA.Any;

   TC_UnionMemberSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.UnionMemberSeq;

   function To_Any (Item : in CORBA.Repository_Root.UnionMemberSeq)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Identifier : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Identifier.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Identifier.Sequence)
      return CORBA.Any;

   TC_EnumMemberSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.EnumMemberSeq;

   function To_Any (Item : in CORBA.Repository_Root.EnumMemberSeq)
      return CORBA.Any;

   TC_PrimitiveKind : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.PrimitiveKind;

   function To_Any (Item : in CORBA.Repository_Root.PrimitiveKind)
      return CORBA.Any;

   TC_ModuleDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ModuleDescription;

   function To_Any (Item : in CORBA.Repository_Root.ModuleDescription)
      return CORBA.Any;

   TC_ConstantDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ConstantDescription;

   function To_Any (Item : in CORBA.Repository_Root.ConstantDescription)
      return CORBA.Any;

   TC_TypeDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.TypeDescription;

   function To_Any (Item : in CORBA.Repository_Root.TypeDescription)
      return CORBA.Any;

   TC_ExceptionDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ExceptionDescription;

   function To_Any (Item : in CORBA.Repository_Root.ExceptionDescription)
      return CORBA.Any;

   TC_AttributeMode : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Enum;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.AttributeMode;

   function To_Any (Item : in CORBA.Repository_Root.AttributeMode)
      return CORBA.Any;

   TC_AttributeDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.AttributeDescription;

   function To_Any (Item : in CORBA.Repository_Root.AttributeDescription)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ParameterDescription.Sequence)
      return CORBA.Any;

   TC_ParDescriptionSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ParDescriptionSeq;

   function To_Any (Item : in CORBA.Repository_Root.ParDescriptionSeq)
      return CORBA.Any;

   TC_ContextIdentifier : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ContextIdentifier;

   function To_Any (Item : in CORBA.Repository_Root.ContextIdentifier)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ContextIdentifier.Sequence)
      return CORBA.Any;

   TC_ContextIdSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ContextIdSeq;

   function To_Any (Item : in CORBA.Repository_Root.ContextIdSeq)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription.Sequence)
      return CORBA.Any;

   TC_ExcDescriptionSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ExcDescriptionSeq;

   function To_Any (Item : in CORBA.Repository_Root.ExcDescriptionSeq)
      return CORBA.Any;

   TC_OperationDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.OperationDescription;

   function To_Any (Item : in CORBA.Repository_Root.OperationDescription)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_RepositoryId : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_RepositoryId.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_RepositoryId.Sequence)
      return CORBA.Any;

   TC_RepositoryIdSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.RepositoryIdSeq;

   function To_Any (Item : in CORBA.Repository_Root.RepositoryIdSeq)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_OperationDescription.Sequence)
      return CORBA.Any;

   TC_OpDescriptionSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.OpDescriptionSeq;

   function To_Any (Item : in CORBA.Repository_Root.OpDescriptionSeq)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_AttributeDescription.Sequence)
      return CORBA.Any;

   TC_AttrDescriptionSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.AttrDescriptionSeq;

   function To_Any (Item : in CORBA.Repository_Root.AttrDescriptionSeq)
      return CORBA.Any;

   TC_InterfaceDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.InterfaceDescription;

   function To_Any (Item : in CORBA.Repository_Root.InterfaceDescription)
      return CORBA.Any;

   TC_IDL_SEQUENCE_CORBA_Repository_Root_ValueMember : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Sequence;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueMember.Sequence;

   function To_Any (Item : in CORBA.Repository_Root.IDL_SEQUENCE_CORBA_Repository_Root_ValueMember.Sequence)
      return CORBA.Any;

   TC_ValueMemberSeq : CORBA.TypeCode.Object := CORBA.TypeCode.
      TC_Alias;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ValueMemberSeq;

   function To_Any (Item : in CORBA.Repository_Root.ValueMemberSeq)
      return CORBA.Any;

   TC_ValueDescription : CORBA.TypeCode.Object :=
      CORBA.TypeCode.TC_Struct;

   function From_Any (Item : in CORBA.Any)
      return CORBA.Repository_Root.ValueDescription;

   function To_Any (Item : in CORBA.Repository_Root.ValueDescription)
      return CORBA.Any;

end CORBA.Repository_Root.Helper;
