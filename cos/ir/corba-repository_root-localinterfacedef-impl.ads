
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.InterfaceDef.Impl;
with CORBA.Repository_Root.IRObject.Impl;

package CORBA.Repository_Root.LocalInterfaceDef.Impl is

   type Object is new InterfaceDef.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   package Internals is

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
         Base_Interfaces : in     InterfaceDefSeq);

   end Internals;

private

   type Object is new InterfaceDef.Impl.Object with null record;

end CORBA.Repository_Root.LocalInterfaceDef.Impl;
