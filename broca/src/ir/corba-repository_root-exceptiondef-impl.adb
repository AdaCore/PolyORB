----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.ORB.Typecode;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.ExceptionDef.Skel;
with CORBA.Repository_Root.Container.Impl;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Helper;

with Broca.Server_Tools;
with PortableServer;

package body CORBA.Repository_Root.ExceptionDef.Impl is

   package ExcDef renames IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDef_Forward;
   package ExcDes renames IDL_SEQUENCE_CORBA_Repository_Root_ExceptionDescription;

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
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
                   Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
                   Members : CORBA.Repository_Root.StructMemberSeq) is
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
                           Defined_In);
      Self.Contained_View := Contained_View;
      Initialize_Members (Self, Members);
   end Init;

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : ExceptionDef_Forward.Ref)
                       return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      Broca.Server_Tools.Reference_To_Servant
        (ExceptionDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return ExceptionDef.Impl.Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return ExceptionDef_Forward.Ref is
      Ref : ExceptionDef.Ref;
   begin
      Broca.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return ExceptionDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------

   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr is
   begin
      return Self.Contained_View;
   end Get_Contained_View;

   --------------------------
   --  Initialize_Members  --
   --------------------------
   procedure Initialize_Members (Self : access Object;
                                 Seq : in StructMemberSeq) is
--      package SMS renames
--        IDL_SEQUENCE_CORBA_Repository_Root_StructMember;
--      Memb_Array : SMS.Element_Array
--        := SMS.To_Element_Array (SMS.Sequence (Seq));
   begin
      --  FIXME>>>>>>>>>>>>>>>>>
      --  if we set the typecodes to TC_Void, we will loose
      --  the type of the members...

--      for I in Memb_Array'Range loop
--         Memb_Array (I).IDL_Type := CORBA.TC_Void;
--      end loop;
--      Self.Members := StructMemberSeq (SMS.To_Sequence (Memb_Array));

      Self.Members := Seq;
   end Initialize_Members;


   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.TypeCode.Create_Exception_TC (Get_Id (Self),
                                                     Get_Name (Self),
                                                     Self.Members);
   end get_type;


   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.StructMemberSeq
   is
      Result : CORBA.Repository_Root.StructMemberSeq;
   begin
      return Self.Members;
   end get_members;


   procedure set_members
     (Self : access Object;
      To : in CORBA.Repository_Root.StructMemberSeq) is
   begin
      Initialize_Members (Self, To);
   end set_members;


   ------------------------------
   --  inherited by contained  --
   ------------------------------
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
   begin
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
      Desc : CORBA.Repository_Root.ExceptionDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Contained.Impl.Get_Defined_In
               (Self.Contained_View),
               Version => Get_Version (Self),
               IDL_Type => Get_Type (Self));
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


   -----------------------------
   --  Get_ExcDescritpionSeq  --
   -----------------------------
   function Get_ExcDescriptionSeq (ExcDefSeq : ExceptionDefSeq)
                                   return ExcDescriptionSeq
   is
      Result : ExcDescriptionSeq;
      Exc_Array : ExcDef.Element_Array
        := ExcDef.To_Element_Array (ExcDef.Sequence (ExcDefSeq));
   begin
      for I in Exc_Array'Range loop
         declare
            Exc : Object_Ptr
              := To_Object (Exc_Array (I));
            Des : ExceptionDescription;
         begin
            Des := (Name => Get_Name (Exc),
                    Id => Get_Id (Exc),
                    Defined_In => Contained.Impl.Get_Defined_In
                    (Get_Contained_View (Exc)),
                    Version => Get_Version (Exc),
                    IDL_Type => Get_Type (Exc));
            ExcDes.Append (ExcDes.Sequence (Result), Des);
         end;
      end loop;
      return Result;
   end;

end CORBA.Repository_Root.ExceptionDef.Impl;

