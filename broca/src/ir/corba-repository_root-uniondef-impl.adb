----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Container;
with CORBA.Repository_Root.Container.Impl;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.UnionDef.Skel;
with CORBA.Repository_Root.IDLType.Impl;

package body CORBA.Repository_Root.UnionDef.Impl is

   ------------
   --  INIT  --
   ------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   IDL_Type : CORBA.TypeCode.Object;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
                   Container_View : CORBA.Repository_Root.Container.Impl.Object_Ptr;
                   Discriminator_Type : CORBA.TypeCode.Object;
                   Discriminator_Type_Def : CORBA.Repository_Root.IDLType.Ref;
                   Members : CORBA.Repository_Root.UnionMemberSeq) is
   begin
      Typedefdef.Impl.Init (Typedefdef.Impl.Object_Ptr (Self),
                            Real_Object,
                            Def_Kind,
                            Id,
                            Name,
                            Version,
                            Defined_In,
                            IDL_Type,
                            IDLType_View);
      Container.Impl.Init (Container_View,
                           Real_Object,
                           Def_Kind,
                           Contents);
      Self.Container_View := Container_View;
      Self.Discriminator_Type := Discriminator_Type;
      Self.Discriminator_Type_Def := Discriminator_Type_Def;
      Self.Members := Members;
   end Init;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------

   function Get_Container_View (Self : access Object)
     return CORBA.Repository_Root.Container.Impl.Object_Ptr is
   begin
      return Self.Container_View;
   end Get_Container_View;


   function get_discriminator_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin

      --  Insert implementation of get_discriminator_type

      return Result;
   end get_discriminator_type;


   function get_discriminator_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
      Result : CORBA.Repository_Root.IDLType.Ref;
   begin

      --  Insert implementation of get_discriminator_type_def

      return Result;
   end get_discriminator_type_def;


   procedure set_discriminator_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin

      --  Insert implementation of set_discriminator_type_def

      null;
   end set_discriminator_type_def;


   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.UnionMemberSeq
   is
      Result : CORBA.Repository_Root.UnionMemberSeq;
   begin

      --  Insert implementation of get_members

      return Result;
   end get_members;


   procedure set_members
     (Self : access Object;
      To : in CORBA.Repository_Root.UnionMemberSeq) is
   begin

      --  Insert implementation of set_members

      null;
   end set_members;


   function lookup
     (Self : access Object;
      search_name : in CORBA.ScopedName)
     return CORBA.Repository_Root.Contained.Ref
   is
      Result : CORBA.Repository_Root.Contained.Ref;
   begin

      --  Insert implementation of lookup

      return Result;
   end lookup;


   function contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq
   is
      Result : CORBA.Repository_Root.ContainedSeq;
   begin

      --  Insert implementation of contents

      return Result;
   end contents;


   function lookup_name
     (Self : access Object;
      search_name : in CORBA.Identifier;
      levels_to_search : in CORBA.Long;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq
   is
      Result : CORBA.Repository_Root.ContainedSeq;
   begin

      --  Insert implementation of lookup_name

      return Result;
   end lookup_name;


   function describe_contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean;
      max_returned_objs : in CORBA.Long)
     return CORBA.Repository_Root.Container.DescriptionSeq
   is
      Result : CORBA.Repository_Root.Container.DescriptionSeq;
   begin

      --  Insert implementation of describe_contents

      return Result;
   end describe_contents;


   function create_module
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.ModuleDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ModuleDef_Forward.Ref;
   begin

      --  Insert implementation of create_module

      return Result;
   end create_module;


   function create_constant
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_type : in CORBA.Repository_Root.IDLType_Forward.Ref;
      value : in CORBA.Any)
     return CORBA.Repository_Root.ConstantDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ConstantDef_Forward.Ref;
   begin

      --  Insert implementation of create_constant

      return Result;
   end create_constant;


   function create_struct
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.StructDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.StructDef_Forward.Ref;
   begin

      --  Insert implementation of create_struct

      return Result;
   end create_struct;


   function create_union
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      discriminator_type : in CORBA.Repository_Root.IDLType_Forward.Ref;
      members : in CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.Repository_Root.UnionDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.UnionDef_Forward.Ref;
   begin

      --  Insert implementation of create_union

      return Result;
   end create_union;


   function create_enum
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.Repository_Root.EnumDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.EnumDef_Forward.Ref;
   begin

      --  Insert implementation of create_enum

      return Result;
   end create_enum;


   function create_alias
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.AliasDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.AliasDef_Forward.Ref;
   begin

      --  Insert implementation of create_alias

      return Result;
   end create_alias;


   function create_interface
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      base_interfaces : in CORBA.Repository_Root.InterfaceDefSeq;
      is_abstract : in CORBA.Boolean)
     return CORBA.Repository_Root.InterfaceDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.InterfaceDef_Forward.Ref;
   begin

      --  Insert implementation of create_interface

      return Result;
   end create_interface;


   function create_value
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      is_custom : in CORBA.Boolean;
      is_abstract : in CORBA.Boolean;
      base_value : in CORBA.Repository_Root.ValueDef_Forward.Ref;
      is_truncatable : in CORBA.Boolean;
      abstract_base_values : in CORBA.Repository_Root.ValueDefSeq;
      supported_interfaces : in CORBA.Repository_Root.InterfaceDefSeq;
      initializers : in CORBA.Repository_Root.InitializerSeq)
     return CORBA.Repository_Root.ValueDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ValueDef_Forward.Ref;
   begin

      --  Insert implementation of create_value

      return Result;
   end create_value;


   function create_value_box
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type_def : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.ValueBoxDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ValueBoxDef_Forward.Ref;
   begin

      --  Insert implementation of create_value_box

      return Result;
   end create_value_box;


   function create_exception
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.ExceptionDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ExceptionDef_Forward.Ref;
   begin

      --  Insert implementation of create_exception

      return Result;
   end create_exception;


   function create_native
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.NativeDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.NativeDef_Forward.Ref;
   begin

      --  Insert implementation of create_native

      return Result;
   end create_native;

end CORBA.Repository_Root.UnionDef.Impl;
