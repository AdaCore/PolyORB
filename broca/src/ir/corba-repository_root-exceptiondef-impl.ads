----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Container.Impl;

package CORBA.Repository_Root.ExceptionDef.Impl is

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
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
                   Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
                   Members : CORBA.Repository_Root.StructMemberSeq);

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : ExceptionDef_Forward.Ref)
                       return Object_Ptr;

    --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return ExceptionDef_Forward.Ref;

   --  for accessing the secondary parents view
   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr;

   --  Set the members attribute while putting the "type" field
   --  of the member to TC_Void
   procedure Initialize_Members (Self : access Object;
                                 Seq : in StructMemberSeq);

   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object;

   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.StructMemberSeq;

   procedure set_members
     (Self : access Object;
      To : in CORBA.Repository_Root.StructMemberSeq);

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

   --  Transform a ExceptionDefSeq ExcDescriptionSeq
   function Get_ExcDescriptionSeq (ExcDefSeq : ExceptionDefSeq)
                                   return ExcDescriptionSeq;

private

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with record
        Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
        --  the Type will be computed dynamically ...
        Members : CORBA.Repository_Root.StructMemberSeq;
   end record;

end CORBA.Repository_Root.ExceptionDef.Impl;
