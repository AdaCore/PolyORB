----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained.Impl;
pragma Elaborate_All (CORBA.Repository_Root.Contained.Impl);

package CORBA.Repository_Root.ValueMemberDef.Impl is

   type Object is
     new CORBA.Repository_Root.Contained.Impl.Object with private;

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
                   IDL_Type : CORBA.TypeCode.Object;
                   Type_Def : CORBA.Repository_Root.IDLType.Ref;
                   IDL_Access : CORBA.Repository_Root.Visibility);

   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref;

   procedure set_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref);

   function get_access
     (Self : access Object)
     return CORBA.Repository_Root.Visibility;

   procedure set_access
     (Self : access Object;
      To : in CORBA.Repository_Root.Visibility);

   --  override this from contained
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description;

private

   type Object is
     new CORBA.Repository_Root.Contained.Impl.Object with record
        IDL_Type : CORBA.TypeCode.Object;
        Type_Def : CORBA.Repository_Root.IDLType.Ref;
        IDL_Access : CORBA.Repository_Root.Visibility;
   end record;

end CORBA.Repository_Root.ValueMemberDef.Impl;
