----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.TypedefDef.Impl;

package CORBA.Repository_Root.ValueBoxDef.Impl is

   type Object is
     new CORBA.Repository_Root.TypedefDef.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : ValueBoxDef_Forward.Ref)
                       return Object_Ptr;

    --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return ValueBoxDef_Forward.Ref;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Original_Type_Def : CORBA.Repository_Root.IDLType.Ref);

   --  overload the get_type from IDLType
   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_original_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref;

   procedure set_original_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref);

private

   type Object is
     new CORBA.Repository_Root.TypedefDef.Impl.Object with record
        Original_Type_Def : CORBA.Repository_Root.IDLType.Ref;
   end record;

end CORBA.Repository_Root.ValueBoxDef.Impl;
