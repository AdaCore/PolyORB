----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.ModuleDef.Skel;
with CORBA.Repository_Root.Helper;

package body CORBA.Repository_Root.ModuleDef.Impl is


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
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_List;
                   Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr)
   is
   begin
      Container.Impl.Init (Container.Impl.Object_Ptr (Self),
                           Real_Object,
                           Def_Kind,
                           Contents);
      Contained.Impl.Init (Contained_View,
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In,
                           Absolute_Name,
                           Containing_Repository);
      Self.Contained_View := Contained_View;
   end Init;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------

   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr is
   begin
      return Self.Contained_View;
   end Get_Contained_View;


   function get_id
     (Self : access Object)
     return CORBA.RepositoryId
   is
      Result : CORBA.RepositoryId;
   begin

      --  Insert implementation of get_id

      return Result;
   end get_id;


   procedure set_id
     (Self : access Object;
      To : in CORBA.RepositoryId) is
   begin

      --  Insert implementation of set_id

      null;
   end set_id;


   function get_name
     (Self : access Object)
     return CORBA.Identifier
   is
      Result : CORBA.Identifier;
   begin

      --  Insert implementation of get_name

      return Result;
   end get_name;


   procedure set_name
     (Self : access Object;
      To : in CORBA.Identifier) is
   begin

      --  Insert implementation of set_name

      null;
   end set_name;


   function get_version
     (Self : access Object)
     return CORBA.Repository_Root.VersionSpec
   is
      Result : CORBA.Repository_Root.VersionSpec;
   begin

      --  Insert implementation of get_version

      return Result;
   end get_version;


   procedure set_version
     (Self : access Object;
      To : in CORBA.Repository_Root.VersionSpec) is
   begin

      --  Insert implementation of set_version

      null;
   end set_version;


   function get_defined_in
     (Self : access Object)
     return CORBA.Repository_Root.Container_Forward.Ref
   is
      Result : CORBA.Repository_Root.Container_Forward.Ref;
   begin

      --  Insert implementation of get_defined_in

      return Result;
   end get_defined_in;


   function get_absolute_name
     (Self : access Object)
     return CORBA.ScopedName
   is
      Result : CORBA.ScopedName;
   begin

      --  Insert implementation of get_absolute_name

      return Result;
   end get_absolute_name;


   function get_containing_repository
     (Self : access Object)
     return CORBA.Repository_Root.Repository_Forward.Ref
   is
      Result : CORBA.Repository_Root.Repository_Forward.Ref;
   begin

      --  Insert implementation of get_containing_repository

      return Result;
   end get_containing_repository;


   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
   is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.ModuleDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Contained.Impl.Get_Defined_In
               (Self.Contained_View),
               Version => Get_Version (Self));
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end describe;


   procedure move
     (Self : access Object;
      new_container : in CORBA.Repository_Root.Container_Forward.Ref;
      new_name : in CORBA.Identifier;
      new_version : in CORBA.Repository_Root.VersionSpec) is
   begin

      --  Insert implementation of move

      null;
   end move;

end CORBA.Repository_Root.ModuleDef.Impl;
