----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.TypedefDef.Skel;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.Helper;

package body CORBA.Repository_Root.TypedefDef.Impl is

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
                  IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
      IDLType.Impl.Init (IDLType_View,
                         Real_Object,
                         Def_Kind);
      Self.IDLType_View := IDLType_View;
   end Init;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------

   function Get_IDLType_View (Self : access Object)
     return CORBA.Repository_Root.IDLType.Impl.Object_Ptr is
   begin
      return Self.IDLType_View;
   end Get_IDLType_View;

   -----------------------------
   --  Inherited from IDLType --
   -----------------------------
   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
   begin
      return IDLType.Impl.Get_Type (Self.IDLType_View);
   end get_type;


   ----------------
   --  Describe  --
   ----------------
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
     is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.TypeDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Get_Defined_In (Self),
               Version => Get_Version (Self),
               IDL_Type => IDLType.Impl.Get_Type (Self.IDLType_View));
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end Describe;


end CORBA.Repository_Root.TypedefDef.Impl;
