
with CORBA.Repository_Root.ExtAttributeDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.InterfaceAttrExtension;
with CORBA.Repository_Root.ExtAbstractInterfaceDef.Skel;
pragma Elaborate (CORBA.Repository_Root.ExtAbstractInterfaceDef.Skel);
pragma Warnings (Off, CORBA.Repository_Root.ExtAbstractInterfaceDef.Skel);

package body CORBA.Repository_Root.ExtAbstractInterfaceDef.Impl is

   --------------------------
   -- create_ext_attribute --
   --------------------------

   function create_ext_attribute
     (Self           : access Object;
      id             : in     RepositoryId;
      name           : in     Identifier;
      version        : in     VersionSpec;
      IDL_type       : in     IDLType.Ref;
      mode           : in     AttributeMode;
      get_exceptions : in     ExceptionDefSeq;
      set_exceptions : in     ExceptionDefSeq)
      return ExtAttributeDef.Ref
   is
   begin
      return
        InterfaceAttrExtension.Impl.create_ext_attribute
        (Self.InterfaceAttrExtension_View,
         id,
         name,
         version,
         IDL_type,
         mode,
         get_exceptions,
         set_exceptions);
   end create_ext_attribute;

   ----------------------------
   -- describe_ext_interface --
   ----------------------------

   function describe_ext_interface
     (Self : access Object)
      return InterfaceAttrExtension.ExtFullInterfaceDescription
   is
   begin
      return
        InterfaceAttrExtension.Impl.describe_ext_interface
        (Self.InterfaceAttrExtension_View);
   end describe_ext_interface;

   package body Internals is

      ----------
      -- Init --
      ----------

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
         Base_Interfaces             : in     AbstractInterfaceDefSeq;
         InterfaceAttrExtension_View : in
           InterfaceAttrExtension.Impl.Object_Ptr)
      is
      begin
         AbstractInterfaceDef.Impl.Internals.Init
           (AbstractInterfaceDef.Impl.Object_Ptr (Self),
            Real_Object,
            Def_Kind,
            Id,
            Name,
            Version,
            Defined_In,
            Contents,
            Contained_View,
            IDLType_View,
            Base_Interfaces);

         InterfaceAttrExtension.Impl.Internals.Init
           (InterfaceAttrExtension_View,
            Real_Object);

         Self.InterfaceAttrExtension_View := InterfaceAttrExtension_View;
      end Init;

   end Internals;

end CORBA.Repository_Root.ExtAbstractInterfaceDef.Impl;
