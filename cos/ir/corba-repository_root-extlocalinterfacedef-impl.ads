
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.ExtAttributeDef;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.InterfaceAttrExtension.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.LocalInterfaceDef.Impl;

package CORBA.Repository_Root.ExtLocalInterfaceDef.Impl is

   type Object is new LocalInterfaceDef.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   function describe_ext_interface
     (Self : access Object)
      return InterfaceAttrExtension.ExtFullInterfaceDescription;

   function create_ext_attribute
     (Self           : access Object;
      id             : in     RepositoryId;
      name           : in     Identifier;
      version        : in     VersionSpec;
      IDL_type       : in     IDLType.Ref;
      mode           : in     AttributeMode;
      get_exceptions : in     ExceptionDefSeq;
      set_exceptions : in     ExceptionDefSeq)
      return ExtAttributeDef.Ref;

   package Internals is

      procedure Init
        (Self                        : access Object'Class;
         Real_Object                 : in     IRObject.Impl.Object_Ptr;
         Def_Kind                    : in     DefinitionKind;
         Id                          : in     RepositoryId;
         Name                        : in     Identifier;
         Version                     : in     VersionSpec;
         Defined_In                  : in     Container_Forward.Ref;
         Contents                    : in
           Contained.Impl.Contained_Seq.Sequence;
         Contained_View              : in     Contained.Impl.Object_Ptr;
         IDLType_View                : in     IDLType.Impl.Object_Ptr;
         Base_Interfaces             : in     InterfaceDefSeq;
         InterfaceAttrExtension_View : in
           InterfaceAttrExtension.Impl.Object_Ptr);
      --  Recursively initialize object fields

   end Internals;

private

   type Object is new LocalInterfaceDef.Impl.Object with record
      InterfaceAttrExtension_View : InterfaceAttrExtension.Impl.Object_Ptr;
   end record;

end CORBA.Repository_Root.ExtLocalInterfaceDef.Impl;
