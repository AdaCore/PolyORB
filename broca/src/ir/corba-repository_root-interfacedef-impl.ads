----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.OperationDef;
with CORBA.Repository_Root.AttributeDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
pragma Elaborate_All (CORBA.Repository_Root.IDLType.Impl);
with CORBA.Repository_Root.Contained.Impl;
pragma Elaborate_All (CORBA.Repository_Root.Contained.Impl);
with CORBA.Repository_Root.Container.Impl;
pragma Elaborate_All (CORBA.Repository_Root.Container.Impl);

package CORBA.Repository_Root.InterfaceDef.Impl is

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Absolute_Name : CORBA.ScopedName;
                   Containing_Repository :
                     CORBA.Repository_Root.Repository_Forward.Ref;
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_List;
                   IDL_Type : CORBA.TypeCode.Object;
                   Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Base_Interfaces : CORBA.Repository_Root.InterfaceDefSeq;
                   Is_Abstract : CORBA.Boolean);

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : InterfaceDef_Forward.Ref)
                       return Object_Ptr;

   --  For multiple inheritance, to access the different views
   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr;

   function Get_IDLType_View (Self : access Object)
     return CORBA.Repository_Root.IDLType.Impl.Object_Ptr;

   -- functions inherited from the secondary parents
   function get_base_interfaces
     (Self : access Object)
     return CORBA.Repository_Root.InterfaceDefSeq;

   procedure set_base_interfaces
     (Self : access Object;
      To : in CORBA.Repository_Root.InterfaceDefSeq);

   function get_is_abstract
     (Self : access Object)
     return CORBA.Boolean;

   procedure set_is_abstract
     (Self : access Object;
      To : in CORBA.Boolean);

   function is_a
     (Self : access Object;
      interface_id : in CORBA.RepositoryId)
     return CORBA.Boolean;

   function describe_interface
     (Self : access Object)
     return CORBA.Repository_Root.InterfaceDef.FullInterfaceDescription;

   function create_attribute
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_type : in CORBA.Repository_Root.IDLType.Ref;
      mode : in CORBA.Repository_Root.AttributeMode)
     return CORBA.Repository_Root.AttributeDef.Ref;

   function create_operation
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_result : in CORBA.Repository_Root.IDLType.Ref;
      mode : in CORBA.Repository_Root.OperationMode;
      params : in CORBA.Repository_Root.ParDescriptionSeq;
      exceptions : in CORBA.Repository_Root.ExceptionDefSeq;
      contexts : in CORBA.Repository_Root.ContextIdSeq)
     return CORBA.Repository_Root.OperationDef.Ref;

   function get_id
     (Self : access Object)
     return CORBA.RepositoryId;

   procedure set_id
     (Self : access Object;
      To : in CORBA.RepositoryId);

   function get_name
     (Self : access Object)
     return CORBA.Identifier;

   procedure set_name
     (Self : access Object;
      To : in CORBA.Identifier);

   function get_version
     (Self : access Object)
     return CORBA.Repository_Root.VersionSpec;

   procedure set_version
     (Self : access Object;
      To : in CORBA.Repository_Root.VersionSpec);

   function get_defined_in
     (Self : access Object)
     return CORBA.Repository_Root.Container_Forward.Ref;

   function get_absolute_name
     (Self : access Object)
     return CORBA.ScopedName;

   function get_containing_repository
     (Self : access Object)
     return CORBA.Repository_Root.Repository_Forward.Ref;

   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description;

   procedure move
     (Self : access Object;
      new_container : in CORBA.Repository_Root.Container_Forward.Ref;
      new_name : in CORBA.Identifier;
      new_version : in CORBA.Repository_Root.VersionSpec);

   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   --  Transform an InterfaceDefSeq into a RepositoryIdSeq
   function Get_RepositoryIdSeq (IntDefSeq : InterfaceDefSeq)
                                 return RepositoryIdSeq;

private

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with record
        Contained_View : CORBA.Repository_Root.Contained.Impl.Object_Ptr;
        IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
        Base_Interfaces : CORBA.Repository_Root.InterfaceDefSeq;
        Is_Abstract : CORBA.Boolean;
   end record;

end CORBA.Repository_Root.InterfaceDef.Impl;

