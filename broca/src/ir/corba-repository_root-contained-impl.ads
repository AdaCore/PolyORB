----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;
pragma Elaborate_All (CORBA.Repository_Root.IRObject.Impl);

package CORBA.Repository_Root.Contained.Impl is

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with private;

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
                   Absolute_Name : CORBA.ScopedName;
                   Containing_Repository :
                     CORBA.Repository_Root.Repository_Forward.Ref);

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : Contained_Forward.Ref)
                       return Object_Ptr;

   --  usefull for the multiple inhertance
   --  transform an IRObject to a container
   --  success is true if it is possible
   procedure To_Contained
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_Ptr);


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

   function get_defined_in
     (Self : access Object)
     return CORBA.RepositoryId;

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


   ------------------------------------
   -- A useful sequence of contained --
   ------------------------------------

   --  This package is used to store the content of the container.
   --  It is better to store the Objct_ptr instead of the ref_forward
   --  as it is declared in the corba.Repository_Root module.
   package Contained_Seq is new Sequences.Unbounded (Object_Ptr);

   --  return null if RepId not found in In_Seq
   function Lookup_Id
     (In_Seq : Contained_Seq.Sequence;
      Search_Id : CORBA.RepositoryId)
      return Object_Ptr;

private

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with record
        Id : CORBA.RepositoryId;
        Name : CORBA.Identifier;
        Version : CORBA.Repository_Root.VersionSpec;
        Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
        Absolute_Name : CORBA.ScopedName;
        Containing_Repository : CORBA.Repository_Root.Repository_Forward.Ref;
     end record;

end CORBA.Repository_Root.Contained.Impl;














