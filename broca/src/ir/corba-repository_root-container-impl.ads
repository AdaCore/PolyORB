----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.IRObject.Impl;
pragma Elaborate_All (CORBA.Repository_Root.IRObject.Impl);

package CORBA.Repository_Root.Container.Impl is

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence);


   --  To transform a forward_ref in impl.object_ptr.
   function To_Object (Fw_Ref : Container_Forward.Ref)
     return Container.Impl.Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return Container_Forward.Ref;

   --  Our function to get the contents list.
   function Get_Contents (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;

   --  Our function to set the contents list.
   procedure Set_Contents
     (Self : access Object;
      New_List : in CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence);

   --  Procedure to append an element to the contents
   procedure Append_To_Contents (Self : access Object;
                                 Element : Contained.Impl.Object_Ptr);

   --  Procedure to delete an element from the contents
   procedure Delete_From_Contents (Self : access Object;
                                   Element : Contained.Impl.Object_Ptr);

   --  usefull for the multiple inhertance
   --  transform an IRObject to a container
   --  success is true if it is possible
   procedure To_Container
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_Ptr);

   --  should only be called if the cast is safe!
   function To_Container
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr)
     return Object_Ptr;


   --  Check if a node of this Id already exists in self.
   --  If yes, raise bad_param (2) and returns false.
   --  Returns true if no node was found.
   function Check_Id (Self : access Object;
                      Id : RepositoryId) return Boolean;

   --  Check if a node of this name already exists in self.
   --  If yes, rais bad_param (3) and returns false.
   --  Returns true if no node was found.
   function Check_Name (Self : access Object;
                        Name : Identifier) return Boolean;

   --  Check if a node of kind "Kind", can be created or moved in Self,
   --  according to the navigation and structure rules!
   --  raise bad_param(4) if not compliant and returns false.
   -- Retruns true if compliant.
   function Check_Structure (Self : access Object;
                             Kind : DefinitionKind) return Boolean;

   -------------
   -- IR spec --
   -------------
   function lookup
     (Self : access Object;
      search_name : in CORBA.ScopedName)
     return CORBA.Repository_Root.Contained.Ref;

   function contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq;

   function lookup_name
     (Self : access Object;
      search_name : in CORBA.Identifier;
      levels_to_search : in CORBA.Long;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq;

   function describe_contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean;
      max_returned_objs : in CORBA.Long)
     return CORBA.Repository_Root.Container.DescriptionSeq;

   function create_module
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.ModuleDef_Forward.Ref;

   function create_constant
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_type : in CORBA.Repository_Root.IDLType_Forward.Ref;
      value : in CORBA.Any)
     return CORBA.Repository_Root.ConstantDef_Forward.Ref;

   function create_struct
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.StructDef_Forward.Ref;

   function create_union
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      discriminator_type : in CORBA.Repository_Root.IDLType_Forward.Ref;
      members : in CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.Repository_Root.UnionDef_Forward.Ref;

   function create_enum
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.Repository_Root.EnumDef_Forward.Ref;

   function create_alias
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.AliasDef_Forward.Ref;

   function create_interface
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      base_interfaces : in CORBA.Repository_Root.InterfaceDefSeq;
      is_abstract : in CORBA.Boolean)
     return CORBA.Repository_Root.InterfaceDef_Forward.Ref;

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
     return CORBA.Repository_Root.ValueDef_Forward.Ref;

   function create_value_box
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type_def : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.ValueBoxDef_Forward.Ref;

   function create_exception
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.ExceptionDef_Forward.Ref;

   function create_native
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.NativeDef_Forward.Ref;

private

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with record
        Contents : CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
   end record;

end CORBA.Repository_Root.Container.Impl;
