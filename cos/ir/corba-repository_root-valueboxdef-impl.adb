pragma Warnings (Off);
----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

--  with CORBA.ORB;
with PolyORB;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;

with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.ValueBoxDef.Impl is


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : ValueBoxDef_Forward.Ref)
                       return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (ValueBoxDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return ValueBoxDef.Impl.Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return ValueBoxDef_Forward.Ref is
      Ref : ValueBoxDef.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Ref);
      return ValueBoxDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ------------
   --  INIT  --
   ------------
   procedure Init
     (Self : access Object;
      Real_Object : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Def_Kind : CORBA.Repository_Root.DefinitionKind;
      Id : CORBA.RepositoryId;
      Name : CORBA.Identifier;
      Version : CORBA.Repository_Root.VersionSpec;
      Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
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
                            IDLType_View);
      Self.Original_Type_Def := Original_Type_Def;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
      Obj : Portableserver.Servant;
      Orig_TC : CORBA.TypeCode.Object;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (Self.Original_Type_Def,
                                               Obj);
      Orig_TC := IDLType.Impl.Get_Type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr (Obj)));

      --  return  CORBA.ORB.Create_Value_Box_Tc (Get_Id (Self),
      --                                              Get_Name (Self),
      --                                              Orig_TC);
      raise PolyORB.Not_Implemented;
      return get_type (Self);
   end get_type;


   function get_original_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
   begin
      return Self.Original_Type_Def;
   end get_original_type_def;


   procedure set_original_type_def
     (Self : access Object;
      To : in CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Original_Type_Def := To;
   end set_original_type_def;

end CORBA.Repository_Root.ValueBoxDef.Impl;
