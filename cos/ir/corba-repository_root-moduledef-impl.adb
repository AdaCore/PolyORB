pragma Warnings (Off);
----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Ada.Tags;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.ModuleDef.Skel;
with CORBA.Repository_Root.Helper;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.ModuleDef.Impl is


   -----------
   -- Debug --
   -----------

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("moduledef.impl");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   package L2 is new PolyORB.Log.Facility_Log ("moduledef.impl_method_trace");
   procedure O2 (Message : in Standard.String; Level : Log_Level := Debug)
     renames L2.Output;


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : ModuleDef_Forward.Ref)
                       return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      pragma Debug (O2 ("to_object (moduledef)"));
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (ModuleDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return ModuleDef_Forward.Ref is
      Ref : ModuleDef.Ref;
   begin
      pragma Debug (O2 ("to_forward (moduledef)"));
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      pragma Debug (O ("before return (to_forward)"));
      return ModuleDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

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
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
                   Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr)
   is
   begin
      pragma Debug (O2 ("init (moduledef)"));
      Container.Impl.Init (Container.Impl.Object_Ptr (Self),
                           Real_Object,
                           Def_Kind,
                           Contents);
      pragma Debug (O ("Type of the defined_in : " &
                       Ada.Tags.External_Tag (Container.Impl.To_Object (Defined_In).all'Tag)));
      Contained.Impl.Init (Contained_View,
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
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


   --------------------------------
   --  inherited from Contained  --
   --------------------------------

   function get_id
     (Self : access Object)
     return CORBA.RepositoryId
   is
   begin
      return Contained.Impl.Get_Id (Self.Contained_View);
   end get_id;


   procedure set_id
     (Self : access Object;
      To : in CORBA.RepositoryId) is
   begin
      Contained.Impl.Set_Id (Self.Contained_View, To);
   end set_id;


   function get_name
     (Self : access Object)
     return CORBA.Identifier
   is
   begin
      return Contained.Impl.Get_Name (Self.Contained_View);
   end get_name;


   procedure set_name
     (Self : access Object;
      To : in CORBA.Identifier) is
   begin
      Contained.Impl.Set_Name (Self.Contained_View, To);
   end set_name;


   function get_version
     (Self : access Object)
     return CORBA.Repository_Root.VersionSpec
   is
   begin
      return Contained.Impl.Get_Version (Self.Contained_View);
   end get_version;


   procedure set_version
     (Self : access Object;
      To : in CORBA.Repository_Root.VersionSpec) is
   begin
      Contained.Impl.Set_Version (Self.Contained_View, To);
   end set_version;


   function get_defined_in
     (Self : access Object)
     return CORBA.Repository_Root.Container_Forward.Ref
   is
   begin
       return Contained.Impl.Get_Defined_In (Self.Contained_View);
   end get_defined_in;


   function get_absolute_name
     (Self : access Object)
      return CORBA.ScopedName
   is
      use Contained.Impl;
   begin
      pragma Debug (O ("get_absolute_name : enter"));
      if (Self.Contained_View = null) then
         null;
         pragma Debug (O ("get_absolute_name : Contained_view is null"));
      end if;
      return Contained.Impl.Get_Absolute_Name (Self.Contained_View);
   end get_absolute_name;


   function get_containing_repository
     (Self : access Object)
     return CORBA.Repository_Root.Repository_Forward.Ref
   is
   begin
      return Contained.Impl.Get_Containing_Repository (Self.Contained_View);
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
      Contained.Impl.Move (Self.Contained_View,
                           New_Container,
                           New_Name,
                           New_Version);
   end move;

end CORBA.Repository_Root.ModuleDef.Impl;
