----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------
with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.OperationDef;
with CORBA.Repository_Root.AttributeDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.InterfaceDef.Skel;
with CORBA.Repository_Root.Helper;

package body CORBA.Repository_Root.InterfaceDef.Impl is

   package IntDef renames IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;
   package IdSeq renames IDL_SEQUENCE_CORBA_RepositoryId;

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
                   Absolute_Name : CORBA.ScopedName;
                   Containing_Repository :
                     CORBA.Repository_Root.Repository_Forward.Ref;
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_List;
                   IDL_Type : CORBA.TypeCode.Object;
                   Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Base_Interfaces : CORBA.Repository_Root.InterfaceDefSeq;
                   Is_Abstract : CORBA.Boolean) is
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
      IDLType.Impl.Init (IDLType_View,
                         Real_Object,
                         Def_Kind,
                          IDL_Type);
      Self.Contained_View := Contained_View;
      Self.IDLType_View := IDLType_View;
      Self.Is_Abstract := Is_Abstract;
      Self.Base_Interfaces := Base_Interfaces;
   end Init;


   -----------------
   --  To_Object  --
    -----------------
   function To_Object (Fw_Ref : InterfaceDef_Forward.Ref)
                       return Object_Ptr is
   begin
      return InterfaceDef.Impl.Object_Ptr
        (InterfaceDef.Object_Of
         (InterfaceDef.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------

   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr is
   begin
      return Self.Contained_View;
   end Get_Contained_View;

   function Get_IDLType_View (Self : access Object)
     return CORBA.Repository_Root.IDLType.Impl.Object_Ptr is
   begin
      return Self.IDLType_View;
   end Get_IDLType_View;



   function get_base_interfaces
     (Self : access Object)
     return CORBA.Repository_Root.InterfaceDefSeq
   is
      Result : CORBA.Repository_Root.InterfaceDefSeq;
   begin

      --  Insert implementation of get_base_interfaces

      return Result;
   end get_base_interfaces;


   procedure set_base_interfaces
     (Self : access Object;
      To : in CORBA.Repository_Root.InterfaceDefSeq) is
   begin

      --  Insert implementation of set_base_interfaces

      null;
   end set_base_interfaces;


   function get_is_abstract
     (Self : access Object)
     return CORBA.Boolean
   is
      Result : CORBA.Boolean;
   begin

      --  Insert implementation of get_is_abstract

      return Result;
   end get_is_abstract;


   procedure set_is_abstract
     (Self : access Object;
      To : in CORBA.Boolean) is
   begin

      --  Insert implementation of set_is_abstract

      null;
   end set_is_abstract;


   function is_a
     (Self : access Object;
      interface_id : in CORBA.RepositoryId)
     return CORBA.Boolean
   is
      Result : CORBA.Boolean;
   begin

      --  Insert implementation of is_a

      return Result;
   end is_a;


   function describe_interface
     (Self : access Object)
     return CORBA.Repository_Root.InterfaceDef.FullInterfaceDescription
   is
      Result : CORBA.Repository_Root.InterfaceDef.FullInterfaceDescription;
   begin

      --  Insert implementation of describe_interface

      return Result;
   end describe_interface;


   function create_attribute
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_type : in CORBA.Repository_Root.IDLType.Ref;
      mode : in CORBA.Repository_Root.AttributeMode)
     return CORBA.Repository_Root.AttributeDef.Ref
   is
      Result : CORBA.Repository_Root.AttributeDef.Ref;
   begin

      --  Insert implementation of create_attribute

      return Result;
   end create_attribute;


   function create_operation
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_result : in CORBA.Repository_Root.IDLType.Ref;
      mode : in CORBA.Repository_Root.OperationMode;
      params : in CORBA.Repository_Root.ParDescriptionSeq;
      exceptions : in CORBA.Repository_Root.ExceptionDefSeq;
      contexts : in CORBA.Repository_Root.ContextIdSeq)
     return CORBA.Repository_Root.OperationDef.Ref
   is
      Result : CORBA.Repository_Root.OperationDef.Ref;
   begin

      --  Insert implementation of create_operation

      return Result;
   end create_operation;


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
      Desc : CORBA.Repository_Root.InterfaceDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Contained.Impl.Get_Defined_In
               (Self.Contained_View),
               Version => Get_Version (Self),
               Base_Interfaces => Get_RepositoryIdSeq (Self.Base_Interfaces),
               Is_Abstract => Self.Is_Abstract);
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


   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      Result : CORBA.TypeCode.Object;
   begin

      --  Insert implementation of get_type

      return Result;
   end get_type;

   ---------------------------
   --  Get_RepositoryIdSeq  --
   ---------------------------
   function Get_RepositoryIdSeq (IntDefSeq : InterfaceDefSeq)
                                 return RepositoryIdSeq
   is
      Result : RepositoryIdSeq;
      Int_Array : IntDef.Element_Array
        := IntDef.To_Element_Array (IntDef.Sequence (IntDefSeq));
   begin
      for I in Int_Array'Range loop
         declare
            Int : Object_Ptr
              := To_Object (Int_Array (I));
         begin
            IdSeq.Append (IdSeq.Sequence (Result), Get_Id (Int));
         end;
      end loop;
      return Result;
   end;

end CORBA.Repository_Root.InterfaceDef.Impl;

