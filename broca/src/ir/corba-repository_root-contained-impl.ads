----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IRObject.Impl;

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
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref);

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : Contained_Forward.Ref)
                       return Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return Contained_Forward.Ref;

   --  usefull for the multiple inhertance
   --  transform an IRObject to a container
   --  success is true if it is possible
   procedure To_Contained
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_Ptr);

   --  should only be called if the cast is safe!
   function To_Contained
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr)
     return Object_Ptr;

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
   function Lookup_Id (In_Seq : Contained_Seq.Sequence;
                       Search_Id : CORBA.RepositoryId)
                       return Object_Ptr;

   --  Look for the given scopedName in the Sequence.
   --  Returns  nil object reference if not found.
   --  The Name should not begin with ::
   function Lookup_ScopedName (In_Seq : Contained_Seq.Sequence;
                               Name : ScopedName) return Object_Ptr;

   --  Look for the given name in the given contained sequence
   --  Check also if the definition_kind correspond to the limit
   --  Returns the result in a ContainedSeq
   function Lookup_Name (In_Seq : Contained_Seq.Sequence;
                         Name : Identifier;
                         Limit_Type : DefinitionKind) return ContainedSeq;

   --  This function returns the In_Seq as containedSeq if limit_type is dk_all
   --  else it returns the specific limit_type containeds within In_Seq.
   function Contents (In_Seq : Contained_Seq.Sequence;
                      Limit_Type : DefinitionKind) return ContainedSeq;

   --  This procedure removes the twins
   procedure Simplify_ContainedSeq (In_Seq : in out ContainedSeq);

   --  A usefull function to transform a Contained_Seq.Sequence into a ContainedSeq
   function To_ContainedSeq
     (In_Seq : Contained_Seq.Sequence)
      return  CORBA.Repository_Root.ContainedSeq;

   --  A usefull function to transform a Contained_Seq.Sequence into a ContainedSeq
   function To_Contained_Sequence
     (In_Seq : ContainedSeq)
      return  Contained_Seq.Sequence;

private

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with record
        Id : CORBA.RepositoryId;
        Name : CORBA.Identifier;
        Version : CORBA.Repository_Root.VersionSpec;
        Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
        --  The absolute_name and containing_repository fields are
        --  computed dynamically.
     end record;

end CORBA.Repository_Root.Contained.Impl;















