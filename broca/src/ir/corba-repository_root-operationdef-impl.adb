----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.OperationDef.Skel;
with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.ExceptionDef.Impl;

package body CORBA.Repository_Root.OperationDef.Impl is

   ----------------------
   --  Procedure init  --
   ----------------------
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
                     CORBA.Repository_Root.Repository_Forward.Ref;
                   Result : CORBA.TypeCode.Object;
                   Result_Def : CORBA.Repository_Root.IDLType.Ref;
                   Params : CORBA.Repository_Root.ParDescriptionSeq;
                   Mode : CORBA.Repository_Root.OperationMode;
                   Contexts : CORBA.Repository_Root.ContextIdSeq;
                   Exceptions : CORBA.Repository_Root.ExceptionDefSeq) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In,
                           Absolute_Name,
                           Containing_Repository);
      Self.Result := Result;
      Self.Result_Def := Result_Def;
      Self.Params := Params;
      Self.Mode := Mode;
      Self.Contexts := Contexts;
      Self.Exceptions := Exceptions;
   end Init;


   function get_result
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin

      --  Insert implementation of get_result

      return Result;
   end get_result;


   function get_result_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
      Result : CORBA.Repository_Root.IDLType.Ref;
   begin

      --  Insert implementation of get_result_def

      return Result;
   end get_result_def;


   procedure set_result_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin

      --  Insert implementation of set_result_def

      null;
   end set_result_def;


   function get_params
     (Self : access Object)
     return CORBA.Repository_Root.ParDescriptionSeq
   is
      Result : CORBA.Repository_Root.ParDescriptionSeq;
   begin

      --  Insert implementation of get_params

      return Result;
   end get_params;


   procedure set_params
     (Self : access Object;
      To : in CORBA.Repository_Root.ParDescriptionSeq) is
   begin

      --  Insert implementation of set_params

      null;
   end set_params;


   function get_mode
     (Self : access Object)
     return CORBA.Repository_Root.OperationMode
   is
      Result : CORBA.Repository_Root.OperationMode;
   begin

      --  Insert implementation of get_mode

      return Result;
   end get_mode;


   procedure set_mode
     (Self : access Object;
      To : in CORBA.Repository_Root.OperationMode) is
   begin

      --  Insert implementation of set_mode

      null;
   end set_mode;


   function get_contexts
     (Self : access Object)
     return CORBA.Repository_Root.ContextIdSeq
   is
      Result : CORBA.Repository_Root.ContextIdSeq;
   begin

      --  Insert implementation of get_contexts

      return Result;
   end get_contexts;


   procedure set_contexts
     (Self : access Object;
      To : in CORBA.Repository_Root.ContextIdSeq) is
   begin

      --  Insert implementation of set_contexts

      null;
   end set_contexts;


   function get_exceptions
     (Self : access Object)
     return CORBA.Repository_Root.ExceptionDefSeq
   is
      Result : CORBA.Repository_Root.ExceptionDefSeq;
   begin

      --  Insert implementation of get_exceptions

      return Result;
   end get_exceptions;


   procedure set_exceptions
     (Self : access Object;
      To : in CORBA.Repository_Root.ExceptionDefSeq) is
   begin

      --  Insert implementation of set_exceptions

      null;
   end set_exceptions;


   ----------------
   --  Describe  --
   ----------------
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
     is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.OperationDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Get_Defined_In (Self),
               Version => Get_Version (Self),
               Result => Self.Result,
               Mode => Self.Mode,
               Contexts => Self.Contexts,
               Parameters => Self.Params,
               Exceptions => ExceptionDef.Impl.Get_ExcDescriptionSeq
               (Self.Exceptions));
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end Describe;

end CORBA.Repository_Root.OperationDef.Impl;

