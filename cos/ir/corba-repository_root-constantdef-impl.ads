----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained.Impl;

package CORBA.Repository_Root.ConstantDef.Impl is

   type Object is
     new CORBA.Repository_Root.Contained.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  To transform a forward_ref in impl.object_ptr.
   function To_Object (Fw_Ref : ConstantDef_Forward.Ref)
                       return Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return ConstantDef_Forward.Ref;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Type_Def : CORBA.Repository_Root.IDLType.Ref;
                   Value : CORBA.Any);

   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref;

   procedure set_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref);

   function get_value
     (Self : access Object)
     return CORBA.Any;

   procedure set_value
     (Self : access Object;
      To : in CORBA.Any);

   --  override this from contained
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description;

private

   type Object is new CORBA.Repository_Root.Contained.Impl.Object with record
      Type_Def : CORBA.Repository_Root.IDLType.Ref;
      --  the Type attribute is the Type of the value
      Value : CORBA.Any;
   end record;

end CORBA.Repository_Root.ConstantDef.Impl;
