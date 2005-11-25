------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                C O R B A . R E P O S I T O R Y _ R O O T                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Implementation Note: this package defines data types introduced by
--  the specification of the Interface Repository. OMG Issue #3639:
--  The Ada mapping of the Interface Repository states that these
--  types cannot be added to the CORBA package because this would
--  introduce a circular dependence between CORBA and CORBA.Forward
--  package.
--
--  Thus, we follow the proposed correction:
--
--      "The types defined within module CORBA by the Interface
--      Repository Specification (formal/99-10-07, p10-56 to 10-68),
--      except the TypeCode and ORB interfaces, shall be mapped to a
--      (child) library package CORBA.Repository_Root."
--
--  In addition to this correction, we retained standard names for
--  types: all declared type ids are prefixed by CORBA, not
--  CORBA.Repository_Root.

with CORBA.Forward;
pragma Elaborate_All (CORBA.Forward);
with CORBA.Sequences.Unbounded;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

package CORBA.Repository_Root is

   --  Implementation Notes: these three forward declarations below is
   --  not declared in CORBA_InterfaceRepository.idl file, but it is
   --  required for child packages.

   package ArrayDef_Forward is new CORBA.Forward;

   package AttributeDef_Forward is new CORBA.Forward;

   package Contained_Forward is new CORBA.Forward;

   package Container_Forward is new CORBA.Forward;

   package FixedDef_Forward is new CORBA.Forward;

   package IRObject_Forward is new CORBA.Forward;

   package OperationDef_Forward is new CORBA.Forward;

   package PrimitiveDef_Forward is new CORBA.Forward;

   package Repository_Forward is new CORBA.Forward;

   package SequenceDef_Forward is new CORBA.Forward;

   package StringDef_Forward is new CORBA.Forward;

   package TypedefDef_Forward is new CORBA.Forward;

   package ValueMemberDef_Forward is new CORBA.Forward;

   package WstringDef_Forward is new CORBA.Forward;

   --  enum DefinitionKind

   type DefinitionKind is
     (dk_none,
      dk_all,
      dk_Attribute,
      dk_Constant,
      dk_Exception,
      dk_Interface,
      dk_Module,
      dk_Operation,
      dk_Typedef,
      dk_Alias,
      dk_Struct,
      dk_Union,
      dk_Enum,
      dk_Primitive,
      dk_String,
      dk_Sequence,
      dk_Array,
      dk_Repository,
      dk_Wstring,
      dk_Fixed,
      dk_Value,
      dk_ValueBox,
      dk_ValueMember,
      dk_Native,
      dk_AbstractInterface,
      dk_LocalInterface,
      dk_Component,
      dk_Home,
      dk_Factory,
      dk_Finder,
      dk_Emits,
      dk_Publishes,
      dk_Consumes,
      dk_Provides,
      dk_Uses,
      dk_Event);

   --  typedef VersionSpec

   type VersionSpec is new CORBA.String;

   package ModuleDef_Forward is new CORBA.Forward;

   package ConstantDef_Forward is new CORBA.Forward;

   package IDLType_Forward is new CORBA.Forward;

   package StructDef_Forward is new CORBA.Forward;

   package UnionDef_Forward is new CORBA.Forward;

   package EnumDef_Forward is new CORBA.Forward;

   package AliasDef_Forward is new CORBA.Forward;

   package ExceptionDef_Forward is new CORBA.Forward;

   package NativeDef_Forward is new CORBA.Forward;

   package InterfaceDef_Forward is new CORBA.Forward;

   --  typedef InterfaceDefSeq

   package IDL_SEQUENCE_CORBA_InterfaceDef_Forward is
      new CORBA.Sequences.Unbounded (InterfaceDef_Forward.Ref);

   type InterfaceDefSeq is
      new IDL_SEQUENCE_CORBA_InterfaceDef_Forward.Sequence;

   package ValueDef_Forward is new CORBA.Forward;

   --  typedef ValueDefSeq

   package IDL_SEQUENCE_CORBA_ValueDef_Forward is
      new CORBA.Sequences.Unbounded (ValueDef_Forward.Ref);

   type ValueDefSeq is new IDL_SEQUENCE_CORBA_ValueDef_Forward.Sequence;

   package ValueBoxDef_Forward is new CORBA.Forward;

   package AbstractInterfaceDef_Forward is new CORBA.Forward;

   --  typedef AbstractInterfaceDefSeq

   package IDL_SEQUENCE_CORBA_AbstractInterfaceDef_Forward is
      new CORBA.Sequences.Unbounded (AbstractInterfaceDef_Forward.Ref);

   type AbstractInterfaceDefSeq is
      new IDL_SEQUENCE_CORBA_AbstractInterfaceDef_Forward.Sequence;

   package LocalInterfaceDef_Forward is new CORBA.Forward;

   --  typedef LocalInterfaceDefSeq

   package IDL_SEQUENCE_CORBA_LocalInterfaceDef_Forward is
      new CORBA.Sequences.Unbounded (LocalInterfaceDef_Forward.Ref);

   type LocalInterfaceDefSeq is
      new IDL_SEQUENCE_CORBA_LocalInterfaceDef_Forward.Sequence;

   package ExtInterfaceDef_Forward is new CORBA.Forward;

   --  typedef ExtInterfaceDefSeq

   package IDL_SEQUENCE_CORBA_ExtInterfaceDef_Forward is
      new CORBA.Sequences.Unbounded (ExtInterfaceDef_Forward.Ref);

   type ExtInterfaceDefSeq is
      new IDL_SEQUENCE_CORBA_ExtInterfaceDef_Forward.Sequence;

   package ExtValueDef_Forward is new CORBA.Forward;

   --  typedef ExtValueDefSeq

   package IDL_SEQUENCE_CORBA_ExtValueDef_Forward is
      new CORBA.Sequences.Unbounded (ExtValueDef_Forward.Ref);

   type ExtValueDefSeq is
      new IDL_SEQUENCE_CORBA_ExtValueDef_Forward.Sequence;

   --  ExtAbstractInterfaceDef forward declaration

   package ExtAbstractInterfaceDef_Forward is new CORBA.Forward;

   --  typedef ExtAbstractInterfaceDefSeq

   package IDL_SEQUENCE_CORBA_ExtAbstractInterfaceDef_Forward is
      new CORBA.Sequences.Unbounded (ExtAbstractInterfaceDef_Forward.Ref);

   type ExtAbstractInterfaceDefSeq is
      new IDL_SEQUENCE_CORBA_ExtAbstractInterfaceDef_Forward.Sequence;

   --  ExtLocalInterfaceDef forward declaration

   package ExtLocalInterfaceDef_Forward is new CORBA.Forward;

   --  typedef ExtLocalInterfaceDefSeq

   package IDL_SEQUENCE_CORBA_ExtLocalInterfaceDef_Forward is
      new CORBA.Sequences.Unbounded (ExtLocalInterfaceDef_Forward.Ref);

   type ExtLocalInterfaceDefSeq is
      new IDL_SEQUENCE_CORBA_ExtLocalInterfaceDef_Forward.Sequence;

   --  typedef ContainedSeq

   package IDL_SEQUENCE_CORBA_Contained_Forward is
      new CORBA.Sequences.Unbounded (Contained_Forward.Ref);

   type ContainedSeq is
      new IDL_SEQUENCE_CORBA_Contained_Forward.Sequence;

   --  struct StructMember

   type StructMember is record
      Name     : CORBA.Identifier;
      IDL_Type : CORBA.TypeCode.Object;
      Type_Def : CORBA.Repository_Root.IDLType_Forward.Ref;
   end record;

   --  typedef StructMemberSeq

   package IDL_SEQUENCE_CORBA_StructMember is
      new CORBA.Sequences.Unbounded (StructMember);

   type StructMemberSeq is
      new IDL_SEQUENCE_CORBA_StructMember.Sequence;

   --  struct Initializer

   type Initializer is record
      Members : CORBA.Repository_Root.StructMemberSeq;
      Name    : CORBA.Identifier;
   end record;

   --  typedef InitializerSeq

   package IDL_SEQUENCE_CORBA_Initializer is
      new CORBA.Sequences.Unbounded (Initializer);

   type InitializerSeq is
      new IDL_SEQUENCE_CORBA_Initializer.Sequence;

   --  struct UnionMember

   type UnionMember is record
      Name     : CORBA.Identifier;
      Label    : CORBA.Any;
      IDL_Type : CORBA.TypeCode.Object;
      Type_Def : CORBA.Repository_Root.IDLType_Forward.Ref;
   end record;

   --  struct ExceptionDescription

   type ExceptionDescription is record
      Name       : CORBA.Identifier;
      Id         : CORBA.RepositoryId;
      Defined_In : CORBA.RepositoryId;
      Version    : CORBA.Repository_Root.VersionSpec;
      IDL_Type   : CORBA.TypeCode.Object;
   end record;

   --  typedef ExcDescriptionSeq

   package IDL_SEQUENCE_CORBA_ExceptionDescription is
      new CORBA.Sequences.Unbounded (ExceptionDescription);

   type ExcDescriptionSeq is
      new IDL_SEQUENCE_CORBA_ExceptionDescription.Sequence;

   --  struct ExtInitializer

   type ExtInitializer is record
      Members    : CORBA.Repository_Root.StructMemberSeq;
      Exceptions : CORBA.Repository_Root.ExcDescriptionSeq;
      Name       : CORBA.Identifier;
   end record;

   --  typedef ExtInitializerSeq

   package IDL_SEQUENCE_CORBA_ExtInitializer is
      new CORBA.Sequences.Unbounded (ExtInitializer);

   type ExtInitializerSeq is
      new IDL_SEQUENCE_CORBA_ExtInitializer.Sequence;

   --  typedef UnionMemberSeq

   package IDL_SEQUENCE_CORBA_UnionMember is
      new CORBA.Sequences.Unbounded (UnionMember);

   type UnionMemberSeq is new IDL_SEQUENCE_CORBA_UnionMember.Sequence;

   --  typedef EnumMemberSeq

   package IDL_SEQUENCE_CORBA_Identifier is
      new CORBA.Sequences.Unbounded (CORBA.Identifier);

   type EnumMemberSeq is
      new IDL_SEQUENCE_CORBA_Identifier.Sequence;

   --  enum PrimitiveKind

   type PrimitiveKind is
     (pk_null,
      pk_void,
      pk_short,
      pk_long,
      pk_ushort,
      pk_ulong,
      pk_float,
      pk_double,
      pk_boolean,
      pk_char,
      pk_octet,
      pk_any,
      pk_TypeCode,
      pk_Principal,
      pk_string,
      pk_objref,
      pk_longlong,
      pk_ulonglong,
      pk_longdouble,
      pk_wchar,
      pk_wstring,
      pk_value_base);

   --  struct ModuleDescription

   type ModuleDescription is record
      Name       : CORBA.Identifier;
      Id         : CORBA.RepositoryId;
      Defined_In : CORBA.RepositoryId;
      Version    : CORBA.Repository_Root.VersionSpec;
   end record;

   --  struct ConstantDescription

   type ConstantDescription is record
      Name       : CORBA.Identifier;
      Id         : CORBA.RepositoryId;
      Defined_In : CORBA.RepositoryId;
      Version    : CORBA.Repository_Root.VersionSpec;
      IDL_Type   : CORBA.TypeCode.Object;
      Value      : CORBA.Any;
   end record;

   --  struct TypeDescription

   type TypeDescription is record
      Name       : CORBA.Identifier;
      Id         : CORBA.RepositoryId;
      Defined_In : CORBA.RepositoryId;
      Version    : CORBA.Repository_Root.VersionSpec;
      IDL_type   : CORBA.TypeCode.Object;
   end record;

   --  enum AttributeMode

   type AttributeMode is
     (ATTR_NORMAL,
      ATTR_READONLY);

   --  struct AttributeDescription

   type AttributeDescription is record
      Name       : CORBA.Identifier;
      Id         : CORBA.RepositoryId;
      Defined_In : CORBA.RepositoryId;
      Version    : CORBA.Repository_Root.VersionSpec;
      IDL_Type   : CORBA.TypeCode.Object;
      Mode       : CORBA.Repository_Root.AttributeMode;
   end record;

   --  struct ExtAttributeDescription

   type ExtAttributeDescription is record
      Name           : CORBA.Identifier;
      Id             : CORBA.RepositoryId;
      Defined_In     : CORBA.RepositoryId;
      Version        : CORBA.Repository_Root.VersionSpec;
      IDL_Type       : CORBA.TypeCode.Object;
      Mode           : CORBA.Repository_Root.AttributeMode;
      Get_Exceptions : CORBA.Repository_Root.ExcDescriptionSeq;
      Put_Exceptions : CORBA.Repository_Root.ExcDescriptionSeq;
   end record;

   --  enum OperationMode

   type OperationMode is
     (OP_NORMAL,
      OP_ONEWAY);

   --  enum ParameterMode

   type ParameterMode is
     (PARAM_IN,
      PARAM_OUT,
      PARAM_INOUT);

   --  struct ParameterDescription

   type ParameterDescription is record
      Name     : CORBA.Identifier;
      IDL_Type : CORBA.TypeCode.Object;
      Type_Def : CORBA.Repository_Root.IDLType_Forward.Ref;
      Mode     : CORBA.Repository_Root.ParameterMode;
   end record;

   --  typedef ParDescriptionSeq

   package IDL_SEQUENCE_CORBA_ParameterDescription is
      new CORBA.Sequences.Unbounded (ParameterDescription);

   type ParDescriptionSeq is
      new IDL_SEQUENCE_CORBA_ParameterDescription.Sequence;

   --  typedef ContextIdentifier

   type ContextIdentifier is new CORBA.Identifier;

   --  typedef ContextIdSeq

   package IDL_SEQUENCE_CORBA_ContextIdentifier is
      new CORBA.Sequences.Unbounded (ContextIdentifier);

   type ContextIdSeq is new IDL_SEQUENCE_CORBA_ContextIdentifier.Sequence;

   --  typedef ExceptionDefSeq

   package IDL_SEQUENCE_CORBA_ExceptionDef_Forward is
      new CORBA.Sequences.Unbounded (ExceptionDef_Forward.Ref);

   type ExceptionDefSeq is
      new IDL_SEQUENCE_CORBA_ExceptionDef_Forward.Sequence;

   --  struct OperationDescription

   type OperationDescription is record
      Name       : CORBA.Identifier;
      Id         : CORBA.RepositoryId;
      Defined_In : CORBA.RepositoryId;
      Version    : CORBA.Repository_Root.VersionSpec;
      Result     : CORBA.TypeCode.Object;
      Mode       : CORBA.Repository_Root.OperationMode;
      Contexts   : CORBA.Repository_Root.ContextIdSeq;
      Parameters : CORBA.Repository_Root.ParDescriptionSeq;
      Exceptions : CORBA.Repository_Root.ExcDescriptionSeq;
   end record;

   --  typedef RepositoryIdSeq

   package IDL_SEQUENCE_CORBA_RepositoryId is
      new CORBA.Sequences.Unbounded (RepositoryId);

   type RepositoryIdSeq is new IDL_SEQUENCE_CORBA_RepositoryId.Sequence;

   --  typedef OpDescriptionSeq

   package IDL_SEQUENCE_CORBA_OperationDescription is
      new CORBA.Sequences.Unbounded (OperationDescription);

   type OpDescriptionSeq is
      new IDL_SEQUENCE_CORBA_OperationDescription.Sequence;

   --  typedef AttrDescriptionSeq

   package IDL_SEQUENCE_CORBA_AttributeDescription is
      new CORBA.Sequences.Unbounded (AttributeDescription);

   type AttrDescriptionSeq is
      new IDL_SEQUENCE_CORBA_AttributeDescription.Sequence;

   --  typedef ExtAttrDescriptionSeq

   package IDL_SEQUENCE_CORBA_ExtAttributeDescription is
      new CORBA.Sequences.Unbounded (ExtAttributeDescription);

   type ExtAttrDescriptionSeq is
      new IDL_SEQUENCE_CORBA_ExtAttributeDescription.Sequence;

   --  struct InterfaceDescription

   type InterfaceDescription is record
      Name            : CORBA.Identifier;
      Id              : CORBA.RepositoryId;
      Defined_In      : CORBA.RepositoryId;
      Version         : CORBA.Repository_Root.VersionSpec;
      Base_Interfaces : CORBA.Repository_Root.RepositoryIdSeq;
      Is_Abstract     : CORBA.Boolean;
   end record;

   --  Implementation Note: the IDL-to-Ada mapping specifications
   --  states that the Visibility type is part of the CORBA package
   --  specification. However, this type is defined in the
   --  specification of the Interface Repository, and thus should be
   --  in this package. The definition of the Visibility type is in
   --  the CORBA package.

   --  typedef Visibility
   --
   --  type Visibility is new CORBA.Short;
   --
   --  PRIVATE_MEMBER : constant Visibility := 0;
   --  PUBLIC_MEMBER  : constant Visibility := 1;

   --  struct ValueMember

   type ValueMember is record
      Name       : CORBA.Identifier;
      Id         : CORBA.RepositoryId;
      Defined_In : CORBA.RepositoryId;
      Version    : CORBA.Repository_Root.VersionSpec;
      IDL_Type   : CORBA.TypeCode.Object;
      Type_Def   : CORBA.Repository_Root.IDLType_Forward.Ref;
      IDL_Access : CORBA.Visibility;
   end record;

   --  typedef ValueMemberSeq

   package IDL_SEQUENCE_CORBA_ValueMember is
      new CORBA.Sequences.Unbounded (ValueMember);

   type ValueMemberSeq is new IDL_SEQUENCE_CORBA_ValueMember.Sequence;

   --  struct ValueDescription

   type ValueDescription is record
      Name                 : CORBA.Identifier;
      Id                   : CORBA.RepositoryId;
      Is_Abstract          : CORBA.Boolean;
      Is_Custom            : CORBA.Boolean;
      Defined_In           : CORBA.RepositoryId;
      Version              : CORBA.Repository_Root.VersionSpec;
      Supported_Interfaces : CORBA.Repository_Root.RepositoryIdSeq;
      Abstract_Base_Values : CORBA.Repository_Root.RepositoryIdSeq;
      Is_Truncatable       : CORBA.Boolean;
      Base_Value           : CORBA.RepositoryId;
   end record;

   --  Repository Ids

   DefinitionKind_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/DefinitionKind:2.3";

   VersionSpec_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/VersionSpec:1.0";

   InterfaceDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/InterfaceDefSeq:1.0";

   ValueDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ValueDefSeq:1.0";

   AbstractInterfaceDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/AbstractInterfaceDefSeq:1.0";

   LocalInterfaceDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/LocalInterfaceDefSeq:1.0";

   ExtInterfaceDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExtInterfaceDefSeq:1.0";

   ExtValueDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExtValueDefSeq:1.0";

   ExtAbstractInterfaceDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExtAbstractInterfaceDefSeq:1.0";

   ExtLocalInterfaceDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExtLocalInterfaceDefSeq:1.0";

   ContainedSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ContainedSeq:1.0";

   StructMember_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/StructMember:1.0";

   StructMemberSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/StructMemberSeq:1.0";

   Initializer_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/Initializer:2.3";

   InitializerSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/InitializerSeq:1.0";

   UnionMember_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/UnionMember:1.0";

   ExceptionDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExceptionDescription:1.0";

   ExcDescriptionSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExcDescriptionSeq:1.0";

   ExtInitializer_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExtInitializer:1.0";

   ExtInitializerSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExtInitializerSeq:1.0";

   UnionMemberSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/UnionMemberSeq:1.0";

   EnumMemberSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/EnumMemberSeq:1.0";

   PrimitiveKind_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/PrimitiveKind:2.3";

   ModuleDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ModuleDescription:1.0";

   ConstantDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ConstantDescription:1.0";

   TypeDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/TypeDescription:1.0";

   AttributeMode_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/AttributeMode:1.0";

   AttributeDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/AttributeDescription:1.0";

   ExtAttributeDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExtAttributeDescription:1.0";

   OperationMode_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/OperationMode:1.0";

   ParameterMode_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ParameterMode:1.0";

   ParameterDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ParameterDescription:1.0";

   ParDescriptionSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ParDescriptionSeq:1.0";

   ContextIdentifier_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ContextIdentifier:1.0";

   ContextIdSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ContextIdSeq:1.0";

   ExceptionDefSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExceptionDefSeq:1.0";

   OperationDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/OperationDescription:1.0";

   RepositoryIdSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/RepositoryIdSeq:1.0";

   OpDescriptionSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/OpDescriptionSeq:1.0";

   AttrDescriptionSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/AttrDescriptionSeq:1.0";

   ExtAttrDescriptionSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ExtAttrDescriptionSeq:1.0";

   InterfaceDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/InterfaceDescription:2.3";

   Visibility_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/Visibility:1.0";

   ValueMember_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ValueMember:2.3";

   ValueMemberSeq_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ValueMemberSeq:1.0";

   ValueDescription_Repository_Id : constant Standard.String
     := "IDL:omg.org/CORBA/ValueDescription:2.3";

end CORBA.Repository_Root;
