
with CORBA.Repository_Root.AttributeDef.Impl;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IRObject.Impl;

package CORBA.Repository_Root.ExtAttributeDef.Impl is

   type Object is new AttributeDef.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   function get_get_exceptions
     (Self : access Object)
      return ExcDescriptionSeq;

   procedure set_get_exceptions
     (Self : access Object;
      To   : in     ExcDescriptionSeq);

   function get_set_exceptions
     (Self : access Object)
      return ExcDescriptionSeq;

   procedure set_set_exceptions
     (Self : access Object;
      To   : in     ExcDescriptionSeq);

   function describe_attribute
     (Self : access Object)
      return ExtAttributeDescription;

   package Internals is

      procedure Init
        (Self           : access Object'Class;
         Real_Object    : in     IRObject.Impl.Object_Ptr;
         Def_Kind       : in     DefinitionKind;
         Id             : in     RepositoryId;
         Name           : in     Identifier;
         Version        : in     VersionSpec;
         Defined_In     : in     Container_Forward.Ref;
         Type_Def       : in     IDLType.Ref;
         Mode           : in     AttributeMode;
         Get_Exceptions : in     ExceptionDefSeq;
         Set_Exceptions : in     ExceptionDefSeq);
      --  Recursively initialize object fields

   end Internals;

private

   type Object is new AttributeDef.Impl.Object with record
      Get_Exceptions : ExcDescriptionSeq;
      Set_Exceptions : ExcDescriptionSeq;
   end record;

end CORBA.Repository_Root.ExtAttributeDef.Impl;
