----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.OperationDef;
with CORBA.Repository_Root.AttributeDef;
with CORBA.Repository_Root.ValueMemberDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.ValueDef.Skel;
with CORBA.Repository_Root.InterfaceDef;
with CORBA.Repository_Root.InterfaceDef.Impl;
with CORBA.Repository_Root.Helper;

package body CORBA.Repository_Root.ValueDef.Impl is

   package ValDef renames IDL_SEQUENCE_CORBA_Repository_Root_ValueDef_Forward;
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
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
                   IDL_Type : CORBA.TypeCode.Object;
                   Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Supported_Interfaces : CORBA.Repository_Root.InterfaceDefSeq;
                   Initializers : CORBA.Repository_Root.InitializerSeq;
                   Base_Value : CORBA.Repository_Root.ValueDef.Ref;
                   Abstract_Base_Values : CORBA.Repository_Root.ValueDefSeq;
                   Is_Abstract : CORBA.Boolean;
                   Is_Custom : CORBA.Boolean;
                   Is_Truncatable : CORBA.Boolean) is
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
       Self.Supported_Interfaces := Supported_Interfaces;
       Self.Initializers := Initializers;
       Self.Base_Value := Base_Value;
       Self.Abstract_Base_Values := Abstract_Base_Values;
       Self.Is_Abstract := Is_Abstract;
       Self.Is_Custom := Is_Custom;
       Self.Is_Truncatable := Is_Truncatable;
   end Init;

   -----------------
   --  To_Object  --
    -----------------
   function To_Object (Fw_Ref : ValueDef_Forward.Ref)
                       return Object_Ptr is
   begin
      return ValueDef.Impl.Object_Ptr
        (ValueDef.Object_Of
         (ValueDef.Convert_Forward.To_Ref
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


   function get_supported_interfaces
     (Self : access Object)
     return CORBA.Repository_Root.InterfaceDefSeq
   is
      Result : CORBA.Repository_Root.InterfaceDefSeq;
   begin

      --  Insert implementation of get_supported_interfaces

      return Result;
   end get_supported_interfaces;


   procedure set_supported_interfaces
     (Self : access Object;
      To : in CORBA.Repository_Root.InterfaceDefSeq) is
   begin

      --  Insert implementation of set_supported_interfaces

      null;
   end set_supported_interfaces;


   function get_initializers
     (Self : access Object)
     return CORBA.Repository_Root.InitializerSeq
   is
      Result : CORBA.Repository_Root.InitializerSeq;
   begin

      --  Insert implementation of get_initializers

      return Result;
   end get_initializers;


   procedure set_initializers
     (Self : access Object;
      To : in CORBA.Repository_Root.InitializerSeq) is
   begin

      --  Insert implementation of set_initializers

      null;
   end set_initializers;


   function get_base_value
     (Self : access Object)
     return CORBA.Repository_Root.ValueDef.Ref
   is
      Result : CORBA.Repository_Root.ValueDef.Ref;
   begin

      --  Insert implementation of get_base_value

      return Result;
   end get_base_value;


   procedure set_base_value
     (Self : access Object;
      To : in CORBA.Repository_Root.ValueDef.Ref) is
   begin

      --  Insert implementation of set_base_value

      null;
   end set_base_value;


   function get_abstract_base_values
     (Self : access Object)
     return CORBA.Repository_Root.ValueDefSeq
   is
      Result : CORBA.Repository_Root.ValueDefSeq;
   begin

      --  Insert implementation of get_abstract_base_values

      return Result;
   end get_abstract_base_values;


   procedure set_abstract_base_values
     (Self : access Object;
      To : in CORBA.Repository_Root.ValueDefSeq) is
   begin

      --  Insert implementation of set_abstract_base_values

      null;
   end set_abstract_base_values;


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


   function get_is_custom
     (Self : access Object)
     return CORBA.Boolean
   is
      Result : CORBA.Boolean;
   begin

      --  Insert implementation of get_is_custom

      return Result;
   end get_is_custom;


   procedure set_is_custom
     (Self : access Object;
      To : in CORBA.Boolean) is
   begin

      --  Insert implementation of set_is_custom

      null;
   end set_is_custom;


   function get_is_truncatable
     (Self : access Object)
     return CORBA.Boolean
   is
      Result : CORBA.Boolean;
   begin

      --  Insert implementation of get_is_truncatable

      return Result;
   end get_is_truncatable;


   procedure set_is_truncatable
     (Self : access Object;
      To : in CORBA.Boolean) is
   begin

      --  Insert implementation of set_is_truncatable

      null;
   end set_is_truncatable;


   function is_a
     (Self : access Object;
      id : in CORBA.RepositoryId)
     return CORBA.Boolean
   is
      Result : CORBA.Boolean;
   begin

      --  Insert implementation of is_a

      return Result;
   end is_a;


   function describe_value
     (Self : access Object)
     return CORBA.Repository_Root.ValueDef.FullValueDescription
   is
      Result : CORBA.Repository_Root.ValueDef.FullValueDescription;
   begin

      --  Insert implementation of describe_value

      return Result;
   end describe_value;


   function create_value_member
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_type : in CORBA.Repository_Root.IDLType.Ref;
      IDL_access : in CORBA.Repository_Root.Visibility)
     return CORBA.Repository_Root.ValueMemberDef.Ref
   is
      Result : CORBA.Repository_Root.ValueMemberDef.Ref;
   begin

      --  Insert implementation of create_value_member

      return Result;
   end create_value_member;


   function create_attribute
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_type_1 : in CORBA.Repository_Root.IDLType.Ref;
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
      Desc : CORBA.Repository_Root.ValueDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Is_Abstract => Self.Is_Abstract,
               Is_Custom => Self.Is_Custom,
               Defined_In => Contained.Impl.Get_Defined_In
               (Self.Contained_View),
               Version => Get_Version (Self),
               Supported_Interfaces => InterfaceDef.Impl.Get_RepositoryIdSeq
               (Self.Supported_Interfaces),
               Abstract_Base_Values => Get_RepositoryIdSeq
               (Self.Abstract_Base_Values),
               Is_Truncatable => Self.Is_Truncatable,
               Base_Value => Get_Id (ValueDef.Impl.Object_Ptr
                                     (ValueDef.Object_Of (Self.Base_Value))));
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
   function Get_RepositoryIdSeq (ValDefSeq : ValueDefSeq)
                                 return RepositoryIdSeq
   is
      Result : RepositoryIdSeq;
      Val_Array : ValDef.Element_Array
        := ValDef.To_Element_Array (ValDef.Sequence (ValDefSeq));
   begin
      for I in Val_Array'Range loop
         declare
            Val : Object_Ptr
              := To_Object (Val_Array (I));
         begin
            IdSeq.Append (IdSeq.Sequence (Result), Get_Id (Val));
         end;
      end loop;
      return Result;
   end;

end CORBA.Repository_Root.ValueDef.Impl;











