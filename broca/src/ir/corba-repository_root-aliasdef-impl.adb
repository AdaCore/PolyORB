----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.AliasDef.Skel;

package body CORBA.Repository_Root.AliasDef.Impl is


   ------------
   --  INIT  --
   ------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   IDL_Type : CORBA.TypeCode.Object;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Original_Type_Def : CORBA.Repository_Root.IDLType.Ref) is
   begin
      Typedefdef.Impl.Init (Typedefdef.Impl.Object_Ptr (Self),
                            Real_Object,
                            Def_Kind,
                            Id,
                            Name,
                            Version,
                            Defined_In,
                            IDL_Type,
                            IDLType_View);
      Self.Original_Type_Def := Original_Type_Def;
   end Init;


   function get_original_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
      Result : CORBA.Repository_Root.IDLType.Ref;
   begin

      --  Insert implementation of get_original_type_def

      return Result;
   end get_original_type_def;


   procedure set_original_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin

      --  Insert implementation of set_original_type_def

      null;
   end set_original_type_def;

end CORBA.Repository_Root.AliasDef.Impl;
