----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.ORB.Typecode;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Container;
with CORBA.Repository_Root.Container.Impl;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.UnionDef.Skel;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;

with Broca.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.UnionDef.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : UnionDef_Forward.Ref)
     return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      Broca.Server_Tools.Reference_To_Servant
        (UnionDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return UnionDef_Forward.Ref is
      Ref : UnionDef.Ref;
   begin
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return UnionDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

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
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
                   Container_View : CORBA.Repository_Root.Container.Impl.Object_Ptr;
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
                            IDLType_View);
      Container.Impl.Init (Container_View,
                           Real_Object,
                           Def_Kind,
                           Contents);
      Self.Container_View := Container_View;
      Self.Discriminator_Type_Def := Discriminator_Type_Def;
      Initialize_Members (Self, Members);
   end Init;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------
   function Get_Container_View (Self : access Object)
     return CORBA.Repository_Root.Container.Impl.Object_Ptr is
   begin
      return Self.Container_View;
   end Get_Container_View;


   --------------------------
   --  Initialize_Members  --
   --------------------------
   procedure Initialize_Members (Self : access Object;
                                 Seq : in UnionMemberSeq) is
--      package UMS renames
--        IDL_SEQUENCE_CORBA_Repository_Root_UnionMember;
--      Memb_Array : UMS.Element_Array
--        := UMS.To_Element_Array (UMS.Sequence (Seq));
   begin
      --  FIXME>>>>>>>>>>>>>>>>>
      --  if we set the typecodes to TC_Void, we will loose
      --  the type of the members...

      --      for I in Memb_Array'Range loop
      --         Memb_Array (I).IDL_Type := CORBA.TC_Void;
      --      end loop;
      --      Self.Members := UnionMemberSeq (UMS.To_Sequence (Memb_Array));

       Self.Members := Seq;
   end Initialize_Members;


   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.Typecode.Create_Union_Tc (Get_Id (Self),
                                                 Get_Name (Self),
                                                 Get_Discriminator_Type (Self),
                                                 Self.Members);
   end get_type;


   function get_discriminator_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      Obj : Portableserver.Servant;
   begin
      Broca.Server_Tools.Reference_To_Servant (Self.Discriminator_Type_Def,
                                               Obj);
      return IDLType.Impl.Get_Type (IDLType.Impl.To_IDLType
                                    (IRObject.Impl.Object_Ptr (Obj)));
   end get_discriminator_type;


   function get_discriminator_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
   begin
      return Self.Discriminator_Type_Def;
   end get_discriminator_type_def;


   procedure set_discriminator_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Discriminator_Type_Def := To;
   end set_discriminator_type_def;


   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.UnionMemberSeq
   is
   begin
      return Self.Members;
   end get_members;


   procedure set_members
     (Self : access Object;
      To : in CORBA.Repository_Root.UnionMemberSeq) is
   begin
      Initialize_Members (Self, To);
   end set_members;


   --------------------------------
   --  Inherited from container  --
   --------------------------------
   function lookup
     (Self : access Object;
      search_name : in CORBA.ScopedName)
     return CORBA.Repository_Root.Contained.Ref
   is
   begin
      return Container.Impl.Lookup (Self.Container_View,
                                    Search_Name);
   end lookup;


   function contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq
   is
   begin
      return Container.Impl.Contents (Self.Container_View,
                                      Limit_Type,
                                      Exclude_Inherited);
   end contents;


   function lookup_name
     (Self : access Object;
      search_name : in CORBA.Identifier;
      levels_to_search : in CORBA.Long;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq
   is
   begin
      return Container.Impl.Lookup_Name (Self.Container_View,
                                         Search_Name,
                                         Levels_To_Search,
                                         Limit_Type,
                                         Exclude_Inherited);
   end lookup_name;


   function describe_contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean;
      max_returned_objs : in CORBA.Long)
     return CORBA.Repository_Root.Container.DescriptionSeq
   is
   begin
      return Container.Impl.Describe_Contents (Self.Container_View,
                                               Limit_Type,
                                               Exclude_Inherited,
                                               Max_Returned_Objs);
   end describe_contents;


   function create_module
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.ModuleDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Module (Self.Container_View,
                                           Id,
                                           Name,
                                           Version);
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
   begin
      return Container.Impl.Create_Constant (Self.Container_View,
                                             Id,
                                             Name,
                                             Version,
                                             IDL_Type,
                                             Value);
   end create_constant;


   function create_struct
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.StructDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Struct (Self.Container_View,
                                           Id,
                                           Name,
                                           Version,
                                           Members);
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
   begin
      return Container.Impl.Create_Union (Self.Container_View,
                                          Id,
                                          Name,
                                          Version,
                                          Discriminator_Type,
                                          Members);
   end create_union;


   function create_enum
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.Repository_Root.EnumDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Enum (Self.Container_View,
                                         Id,
                                         Name,
                                         Version,
                                         Members);
   end create_enum;


   function create_alias
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.AliasDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Alias (Self.Container_View,
                                          Id,
                                          Name,
                                          Version,
                                          Original_Type);
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
   begin
      return Container.Impl.Create_Interface (Self.Container_View,
                                              Id,
                                              Name,
                                              Version,
                                              Base_Interfaces,
                                              Is_Abstract);
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
   begin
      return Container.Impl.Create_Value (Self.Container_View,
                                          Id,
                                          Name,
                                          Version,
                                          Is_Custom,
                                          Is_Abstract,
                                          Base_Value,
                                          Is_Truncatable,
                                          Abstract_Base_Values,
                                          Supported_Interfaces,
                                          Initializers);
   end create_value;


   function create_value_box
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type_def : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.ValueBoxDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Value_Box (Self.Container_View,
                                              Id,
                                              Name,
                                              Version,
                                              Original_Type_Def);
   end create_value_box;


   function create_exception
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.ExceptionDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Exception (Self.Container_View,
                                              Id,
                                              Name,
                                              Version,
                                              Members);
   end create_exception;


   function create_native
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.NativeDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Native (Self.Container_View,
                                           Id,
                                           Name,
                                           Version);
   end create_native;

end CORBA.Repository_Root.UnionDef.Impl;
