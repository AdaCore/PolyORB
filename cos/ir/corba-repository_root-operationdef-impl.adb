pragma Warnings (Off);
----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.ExceptionDef.Impl;
with CORBA.Repository_Root.IRObject.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

with CORBA.Repository_Root.OperationDef.Skel;

package body CORBA.Repository_Root.OperationDef.Impl is

   ----------------------
   --  Procedure init  --
   ----------------------
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
                   Exceptions : CORBA.Repository_Root.ExceptionDefSeq) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
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
   begin
      return IDLType.Get_Type (get_result_def (Self));
   end get_result;


   function get_result_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
   begin
      return Self.Result_Def;
   end get_result_def;


   procedure set_result_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Result_Def := To;
   end set_result_def;


   function get_params
     (Self : access Object)
     return CORBA.Repository_Root.ParDescriptionSeq
   is
   begin
      return Self.Params;
   end get_params;


   procedure set_params
     (Self : access Object;
      To : in CORBA.Repository_Root.ParDescriptionSeq) is
   begin
      Self.Params := To;
   end set_params;


   function get_mode
     (Self : access Object)
     return CORBA.Repository_Root.OperationMode
   is
   begin
      return Self.Mode;
   end get_mode;


   procedure set_mode
     (Self : access Object;
      To : in CORBA.Repository_Root.OperationMode) is
   begin
      Self.Mode := To;
   end set_mode;


   function get_contexts
     (Self : access Object)
     return CORBA.Repository_Root.ContextIdSeq
   is
   begin
      return Self.Contexts;
   end get_contexts;


   procedure set_contexts
     (Self : access Object;
      To : in CORBA.Repository_Root.ContextIdSeq) is
   begin
      Self.Contexts := To;
   end set_contexts;


   function get_exceptions
     (Self : access Object)
     return CORBA.Repository_Root.ExceptionDefSeq
   is
   begin
      return Self.Exceptions;
   end get_exceptions;


   procedure set_exceptions
     (Self : access Object;
      To : in CORBA.Repository_Root.ExceptionDefSeq) is
   begin
      Self.Exceptions := To;
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
               Result => Get_Result (Self),
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




