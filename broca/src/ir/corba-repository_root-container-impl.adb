----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Ada.Strings.Unbounded;
with CORBA.AbstractBase;
with CORBA.Impl;

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Container.Skel;
with CORBA.Repository_Root.StructDef.Impl;
with CORBA.Repository_Root.UnionDef.Impl;
with CORBA.Repository_Root.Repository.Impl;
with CORBA.Repository_Root.InterfaceDef.Impl;
with CORBA.Repository_Root.ValueDef.Impl;

with Broca.Exceptions;
with Broca.Debug;

package body CORBA.Repository_Root.Container.Impl is

   -----------
   -- Debug --
   -----------

   Flag : constant Natural
     := Broca.Debug.Is_Active ("container.impl");
   procedure O is new Broca.Debug.Output (Flag);

   Flag2 : constant Natural
     := Broca.Debug.Is_Active ("container.impl_method_trace");
   procedure O2 is new Broca.Debug.Output (Flag2);


   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : Corba.Repository_Root.DefinitionKind;
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence) is
   begin
      IRObject.Impl.Init (IRObject.Impl.Object_Ptr (Self), Real_Object, Def_Kind);
      Self.Contents := Contents;
   end Init;


   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : Container_Forward.Ref)
                       return Container.Impl.Object_Ptr is
   begin
      return Container.Impl.Object_Ptr
        (Container.Object_Of
         (Container.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ------------------------------------------
   --  manipulation of the contents field  --
   ------------------------------------------
   function Get_Contents
     (Self : access Object)
      return CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence is
   begin
      return Self.Contents;
   end Get_Contents;

   procedure Set_Contents
     (Self : access Object;
      New_List : in CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence) is
   begin
      Self.Contents := New_List;
   end Set_Contents;

   procedure Append_To_Contents (Self : access Object;
                                 Element : Contained.Impl.Object_Ptr)
   is
   begin
      Contained.Impl.Contained_Seq.Append (Self.Contents,
                                           Element);
   end Append_To_Contents;

   procedure Delete_From_Contents (Self : access Object;
                                   Element : Contained.Impl.Object_Ptr)
   is
      Index : Positive;
      Cont_Array : Contained.Impl.Contained_Seq.Element_Array (1 .. 1);
   begin
      Cont_Array (1) := Element;
      Index := Contained.Impl.Contained_Seq.Index
        (Self.Contents,
         Cont_Array);
      Contained.Impl.Contained_Seq.Delete (Self.Contents,
                                           Index,
                                           Natural (Index));
   end Delete_From_Contents;

   --------------------
   --  To_Container  --
   --------------------
   procedure To_Container
     (Self : IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_ptr)
   is
   begin
      Success := True;
      case IRObject.Impl.Get_Def_Kind
        (Self) is
         when
           Dk_Attribute  |
           Dk_Constant   |
           Dk_Operation  |
           Dk_Typedef    |
           Dk_Alias      |
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_Enum       |
           Dk_ValueBox   |
           dk_ValueMember|
           dk_Native     |
           Dk_All        |
           Dk_None       =>
            Success := False;
            Result := null;
         when
           --  inherited types
           Dk_Repository |
           Dk_Value      |
           Dk_Module     |
           Dk_Exception  |
           Dk_Interface  =>
            Result := Object_Ptr (Self);
         when
           -- types containing a "container_view" field
           Dk_Struct     =>
            declare
               Interm : Structdef.Impl.Object_Ptr :=
                 Structdef.Impl.Object_Ptr (Self);
            begin
               Result := Structdef.Impl.Get_Container_View (Interm);
            end;
         when
           -- types containing a "container_view" field
           Dk_Union      =>
            declare
               Interm : Uniondef.Impl.Object_Ptr :=
                 Uniondef.Impl.Object_Ptr (Self);
            begin
               Result := Uniondef.Impl.Get_Container_View (Interm);
            end;
      end case;
      return;
   end To_Container;

   function To_Container
     (Self : IRObject.Impl.Object_Ptr)
      return Object_ptr
   is
   begin
      case IRObject.Impl.Get_Def_Kind
        (Self) is
         when
           Dk_Attribute  |
           Dk_Constant   |
           Dk_Operation  |
           Dk_Typedef    |
           Dk_Alias      |
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      |
           Dk_Enum       |
           Dk_ValueBox   |
           dk_ValueMember|
           dk_Native     |
           Dk_All        |
           Dk_None       =>
            Broca.Exceptions.Raise_Internal;
            return null;
         when
           --  inherited types
           Dk_Repository |
           Dk_Value      |
           Dk_Module     |
           Dk_Exception  |
           Dk_Interface  =>
            return Object_Ptr (Self);
         when
           -- types containing a "container_view" field
           Dk_Struct     =>
            declare
               Interm : Structdef.Impl.Object_Ptr :=
                 Structdef.Impl.Object_Ptr (Self);
            begin
               return Structdef.Impl.Get_Container_View (Interm);
            end;
         when
           -- types containing a "container_view" field
           Dk_Union      =>
            declare
               Interm : Uniondef.Impl.Object_Ptr :=
                 Uniondef.Impl.Object_Ptr (Self);
            begin
               return Uniondef.Impl.Get_Container_View (Interm);
            end;
      end case;
   end To_Container;

   -------------
   -- IR spec --
   -------------

   function lookup
     (Self : access Object;
      search_name : in CORBA.ScopedName)
     return CORBA.Repository_Root.Contained.Ref
   is
      Result_Obj : Contained.Impl.Object_Ptr := null;
      Result : CORBA.Repository_Root.Contained.Ref;
      use Contained.Impl;
      use Ada.Strings.Unbounded;
   begin
      --  if it begins with :: then lookup in all the repository
      if Head (Unbounded_String (Search_Name), 2) = "::" then
         declare
            New_Search : ScopedName
              := ScopedName (Tail (Unbounded_String (Search_Name),
                                   Length (Unbounded_String (Search_Name)) - 2));
         begin
            if Get_Def_Kind (Self) = Dk_Repository then
               Result_Obj := Lookup_ScopedName (Self.Contents,
                                                New_Search);
            else
               Result_Obj := Lookup_ScopedName
                 (Repository.Impl.Get_Contents
                  (Repository.Impl.To_Object
                   (Get_Containing_Repository
                    (To_Contained (Get_Real_Object (Self))))),
                  New_Search);
            end if;
         end;
      else
         Result_Obj := Lookup_ScopedName (Self.Contents,
                                          Search_Name);
      end if;
      --  return a Nil_ref if result_obj is null.
      Contained.Set (Result,
                     CORBA.Impl.Object_Ptr (Result_Obj));
      return Result;
   end lookup;


   function contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq
   is
      Result : CORBA.Repository_Root.ContainedSeq;
   begin

      --  Insert implementation of contents

      return Result;
   end contents;


   function lookup_name
     (Self : access Object;
      search_name : in CORBA.Identifier;
      levels_to_search : in CORBA.Long;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq
   is
      package Contained_For_Seq renames IDL_SEQUENCE_CORBA_Repository_Root_Contained_Forward;
      Result : CORBA.Repository_Root.ContainedSeq;
   begin
      Result := Contained.Impl.Lookup_Name (Self.Contents,
                                            Search_Name,
                                            Limit_Type);

      if not Exclude_Inherited then
         case Get_Def_Kind (Self) is
            when Dk_Interface =>
               declare
                  package IDF renames IDL_SEQUENCE_CORBA_Repository_Root_InterfaceDef_Forward;
                  IntDefSeq : InterfaceDefSeq
                    := InterfaceDef.Impl.Get_Base_Interfaces
                    (InterfaceDef.Impl.Object_Ptr (Get_Real_Object (Self)));
                  Int_Array : IDF.Element_Array
                    := IDF.To_Element_Array (IDF.Sequence (IntDefSeq));
               begin
                  for I in Int_Array'Range loop
                     declare
                        Int : InterfaceDef.Impl.Object_Ptr
                          := InterfaceDef.Impl.To_Object (Int_Array (I));
                        Res : ContainedSeq;
                     begin
                        --  we will get all the definition of the inherited interface
                        Res := Lookup_Name (Object_Ptr (Int),
                                            Search_Name,
                                            -1,
                                            Limit_Type,
                                            Exclude_Inherited);
                        Contained_For_Seq.Append
                          (Contained_For_Seq.Sequence (Result),
                           Contained_For_Seq.Sequence (Res));
                     end;
                  end loop;
               end;
            when Dk_Value =>
               declare
               begin
                  null;
               end;
            when others =>
               null;
         end case;
      end if;

      return Result;
   end lookup_name;


   function describe_contents
     (Self : access Object;
      limit_type : in CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : in CORBA.Boolean;
      max_returned_objs : in CORBA.Long)
     return CORBA.Repository_Root.Container.DescriptionSeq
   is
      Result : CORBA.Repository_Root.Container.DescriptionSeq;
   begin

      --  Insert implementation of describe_contents

      return Result;
   end describe_contents;


   function create_module
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.ModuleDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ModuleDef_Forward.Ref;
   begin

      --  Insert implementation of create_module

      return Result;
   end create_module;


   function create_constant
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      IDL_type : in CORBA.Repository_Root.IDLType_Forward.Ref;
      value : in CORBA.Any)
     return CORBA.Repository_Root.ConstantDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ConstantDef_Forward.Ref;
   begin

      --  Insert implementation of create_constant

      return Result;
   end create_constant;


   function create_struct
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.StructDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.StructDef_Forward.Ref;
   begin

      --  Insert implementation of create_struct

      return Result;
   end create_struct;


   function create_union
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      discriminator_type : in CORBA.Repository_Root.IDLType_Forward.Ref;
      members : in CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.Repository_Root.UnionDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.UnionDef_Forward.Ref;
   begin

      --  Insert implementation of create_union

      return Result;
   end create_union;


   function create_enum
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.Repository_Root.EnumDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.EnumDef_Forward.Ref;
   begin

      --  Insert implementation of create_enum

      return Result;
   end create_enum;


   function create_alias
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.AliasDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.AliasDef_Forward.Ref;
   begin

      --  Insert implementation of create_alias

      return Result;
   end create_alias;


   function create_interface
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      base_interfaces : in CORBA.Repository_Root.InterfaceDefSeq;
      is_abstract : in CORBA.Boolean)
     return CORBA.Repository_Root.InterfaceDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.InterfaceDef_Forward.Ref;
   begin

      --  Insert implementation of create_interface

      return Result;
   end create_interface;


   function create_value
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      is_custom : in CORBA.Boolean;
      is_abstract : in CORBA.Boolean;
      base_value : in CORBA.Repository_Root.ValueDef_Forward.Ref;
      is_truncatable : in CORBA.Boolean;
      abstract_base_values : in CORBA.Repository_Root.ValueDefSeq;
      supported_interfaces : in CORBA.Repository_Root.InterfaceDefSeq;
      initializers : in CORBA.Repository_Root.InitializerSeq)
     return CORBA.Repository_Root.ValueDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ValueDef_Forward.Ref;
   begin

      --  Insert implementation of create_value

      return Result;
   end create_value;


   function create_value_box
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      original_type_def : in CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.ValueBoxDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ValueBoxDef_Forward.Ref;
   begin

      --  Insert implementation of create_value_box

      return Result;
   end create_value_box;


   function create_exception
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec;
      members : in CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.ExceptionDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.ExceptionDef_Forward.Ref;
   begin

      --  Insert implementation of create_exception

      return Result;
   end create_exception;


   function create_native
     (Self : access Object;
      id : in CORBA.RepositoryId;
      name : in CORBA.Identifier;
      version : in CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.NativeDef_Forward.Ref
   is
      Result : CORBA.Repository_Root.NativeDef_Forward.Ref;
   begin

      --  Insert implementation of create_native

      return Result;
   end create_native;

end CORBA.Repository_Root.Container.Impl;
