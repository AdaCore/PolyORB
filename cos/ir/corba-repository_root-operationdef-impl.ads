----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained.Impl;

package CORBA.Repository_Root.OperationDef.Impl is

   type Object is
     new CORBA.Repository_Root.Contained.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Result_Def : CORBA.Repository_Root.IDLType.Ref;
                   Params : CORBA.Repository_Root.ParDescriptionSeq;
                   Mode : CORBA.Repository_Root.OperationMode;
                   Contexts : CORBA.Repository_Root.ContextIdSeq;
                   Exceptions : CORBA.Repository_Root.ExceptionDefSeq);

   function get_result
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_result_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref;

   procedure set_result_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref);

   function get_params
     (Self : access Object)
     return CORBA.Repository_Root.ParDescriptionSeq;

   procedure set_params
     (Self : access Object;
      To : in CORBA.Repository_Root.ParDescriptionSeq);

   function get_mode
     (Self : access Object)
     return CORBA.Repository_Root.OperationMode;

   procedure set_mode
     (Self : access Object;
      To : in CORBA.Repository_Root.OperationMode);

   function get_contexts
     (Self : access Object)
     return CORBA.Repository_Root.ContextIdSeq;

   procedure set_contexts
     (Self : access Object;
      To : in CORBA.Repository_Root.ContextIdSeq);

   function get_exceptions
     (Self : access Object)
     return CORBA.Repository_Root.ExceptionDefSeq;

   procedure set_exceptions
     (Self : access Object;
      To : in CORBA.Repository_Root.ExceptionDefSeq);

   --  override this from contained
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description;

private

   type Object is new CORBA.Repository_Root.Contained.Impl.Object with record
      --  the Result is the type of the result_def
      Result_Def : CORBA.Repository_Root.IDLType.Ref;
      Params : CORBA.Repository_Root.ParDescriptionSeq;
      Mode : CORBA.Repository_Root.OperationMode;
      Contexts : CORBA.Repository_Root.ContextIdSeq;
      Exceptions : CORBA.Repository_Root.ExceptionDefSeq;
   end record;

end CORBA.Repository_Root.OperationDef.Impl;
