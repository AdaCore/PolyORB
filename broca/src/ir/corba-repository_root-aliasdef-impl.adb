----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Impl;
with CORBA.ORB.Typecode;

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.AliasDef.Skel;

package body CORBA.Repository_Root.AliasDef.Impl is


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : AliasDef_Forward.Ref)
     return Object_Ptr is
   begin
      return Object_Ptr
        (AliasDef.Object_Of
         (AliasDef.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return AliasDef_Forward.Ref is
      Ref : AliasDef.Ref;
   begin
      Set (Ref, CORBA.Impl.Object_Ptr (Obj));
      return AliasDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

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
      Orig_TC : CORBA.TypeCode.Object :=  IDLType.Impl.Get_Type
        (IDLType.Impl.Object_Ptr
         (IDLType.Object_Of (Self.Original_Type_Def)));
   begin
      return  CORBA.ORB.Typecode.Create_Alias_Tc (Get_Id (Self),
                                                  Get_Name (Self),
                                                  Orig_TC);
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

end CORBA.Repository_Root.AliasDef.Impl;
