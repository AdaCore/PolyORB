
with CORBA.Repository_Root.Container.Impl;
with CORBA.Repository_Root.ExtAttributeDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IRObject.Impl;
with PortableServer;

package CORBA.Repository_Root.InterfaceAttrExtension.Impl is

   type Object is new PortableServer.Servant_Base with private;

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
        (Self        : access Object'Class;
         Real_Object : in     IRObject.Impl.Object_Ptr);
      --  Recursively initialize object fields

   end Internals;

private

   type Object is new PortableServer.Servant_Base with record
      Real : Container.Impl.Object_Ptr;
   end record;

end CORBA.Repository_Root.InterfaceAttrExtension.Impl;
