
package body CORBA.Repository_Root.LocalInterfaceDef.Impl is

   package body Internals is

      ----------
      -- Init --
      ----------

      procedure Init
        (Self            : access Object'Class;
         Real_Object     : in     IRObject.Impl.Object_Ptr;
         Def_Kind        : in     DefinitionKind;
         Id              : in     RepositoryId;
         Name            : in     Identifier;
         Version         : in     VersionSpec;
         Defined_In      : in     Container_Forward.Ref;
         Contents        : in     Contained.Impl.Contained_Seq.Sequence;
         Contained_View  : in     Contained.Impl.Object_Ptr;
         IDLType_View    : in     IDLType.Impl.Object_Ptr;
         Base_Interfaces : in     InterfaceDefSeq)
      is
      begin
         InterfaceDef.Impl.Init
           (InterfaceDef.Impl.Object_Ptr (Self),
            Real_Object,
            Def_Kind,
            Id,
            Name,
            Version,
            Defined_In,
            Contents,
            Contained_View,
            IDLType_View,
            Base_Interfaces,
            False);
      end Init;

   end Internals;

end CORBA.Repository_Root.LocalInterfaceDef.Impl;
