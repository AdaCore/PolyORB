------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.STRUCTDEF.IMPL                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (Off);

with CORBA.ORB.TypeCode;
with PortableServer;

with CORBA.Repository_Root.StructDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.StructDef.Skel);

with PolyORB.CORBA_P.Server_Tools;

package body CORBA.Repository_Root.StructDef.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : StructDef_Forward.Ref)
     return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (StructDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return StructDef_Forward.Ref is
      Ref : StructDef.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return StructDef.Convert_Forward.To_Forward (Ref);
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
      Contents : CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
      Container_View : CORBA.Repository_Root.Container.Impl.Object_Ptr;
      Members : CORBA.Repository_Root.StructMemberSeq) is
   begin
      Typedefdef.Impl.Init (Typedefdef.Impl.Object_Ptr (Self),
                            Real_Object,
                            Def_Kind,
                            Id,
                            Name,
                            Version,
                            Defined_In,
                            IDLType_View);
      Container.Impl.Init (Container_View,
                           Real_Object,
                           Def_Kind,
                           Contents);
      Self.Container_View := Container_View;
      Initialize_Members (Self, Members);
   end Init;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------

   function Get_Container_View (Self : access Object)
      return CORBA.Repository_Root.Container.Impl.Object_Ptr is
   begin
      return Self.Container_View;
   end Get_Container_View;

   --------------------------
   --  Initialize_Members  --
   --------------------------
   procedure Initialize_Members (Self : access Object;
                                 Seq : StructMemberSeq) is
--      package SMS renames
--        IDL_SEQUENCE_CORBA_Repository_Root_StructMember;
--      Memb_Array : SMS.Element_Array
--        := SMS.To_Element_Array (SMS.Sequence (Seq));
   begin
      --  FIXME>>>>>>>>>>>>>>>>>
      --  if we set the typecodes to TC_Void, we will loose
      --  the type of the members...

      --  for I in Memb_Array'Range loop
      --         Memb_Array (I).IDL_Type := CORBA.TC_Void;
      --  end loop;
      --  Self.Members := StructMemberSeq (SMS.To_Sequence (Memb_Array));

      Self.Members := Seq;
   end Initialize_Members;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return  CORBA.ORB.TypeCode.Create_Struct_Tc
        (Get_Id (Self), Get_Name (Self), Self.Members);
   end get_type;

   ----------------
   --   members  --
   ----------------
   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.StructMemberSeq
   is
   begin
      return Self.Members;
   end get_members;

   procedure set_members
     (Self : access Object;
      To : CORBA.Repository_Root.StructMemberSeq) is
   begin
      Initialize_Members (Self, To);
   end set_members;

   --------------------------------
   --  Inherited from container  --
   --------------------------------
   function lookup
     (Self : access Object;
      search_name : CORBA.ScopedName)
     return CORBA.Repository_Root.Contained.Ref
   is
   begin
      return Container.Impl.Lookup (Self.Container_View,
                                    Search_Name);
   end lookup;

   function contents
     (Self : access Object;
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq
   is
   begin
      return Container.Impl.Contents (Self.Container_View,
                                      Limit_Type,
                                      Exclude_Inherited);
   end contents;

   function lookup_name
     (Self : access Object;
      search_name : CORBA.Identifier;
      levels_to_search : CORBA.Long;
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq
   is
   begin
      return Container.Impl.Lookup_Name (Self.Container_View,
                                         Search_Name,
                                         Levels_To_Search,
                                         Limit_Type,
                                         Exclude_Inherited);
   end lookup_name;

   function describe_contents
     (Self : access Object;
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean;
      max_returned_objs : CORBA.Long)
     return CORBA.Repository_Root.Container.DescriptionSeq
   is
   begin
      return Container.Impl.Describe_Contents (Self.Container_View,
                                               Limit_Type,
                                               Exclude_Inherited,
                                               Max_Returned_Objs);
   end describe_contents;

   function create_module
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.ModuleDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Module (Self.Container_View,
                                           Id,
                                           Name,
                                           Version);
   end create_module;

   function create_constant
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      IDL_type : CORBA.Repository_Root.IDLType_Forward.Ref;
      value : CORBA.Any)
     return CORBA.Repository_Root.ConstantDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Constant (Self.Container_View,
                                             Id,
                                             Name,
                                             Version,
                                             IDL_Type,
                                             Value);
   end create_constant;

   function create_struct
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.StructDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Struct (Self.Container_View,
                                           Id,
                                           Name,
                                           Version,
                                           Members);
   end create_struct;

   function create_union
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      discriminator_type : CORBA.Repository_Root.IDLType_Forward.Ref;
      members : CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.Repository_Root.UnionDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Union (Self.Container_View,
                                          Id,
                                          Name,
                                          Version,
                                          Discriminator_Type,
                                          Members);
   end create_union;

   function create_enum
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.Repository_Root.EnumDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Enum (Self.Container_View,
                                         Id,
                                         Name,
                                         Version,
                                         Members);
   end create_enum;

   function create_alias
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      original_type : CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.AliasDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Alias (Self.Container_View,
                                          Id,
                                          Name,
                                          Version,
                                          Original_Type);
   end create_alias;

   function create_interface
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      base_interfaces : CORBA.Repository_Root.InterfaceDefSeq;
      is_abstract : CORBA.Boolean)
     return CORBA.Repository_Root.InterfaceDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Interface (Self.Container_View,
                                              Id,
                                              Name,
                                              Version,
                                              Base_Interfaces,
                                              Is_Abstract);
   end create_interface;

   function create_value
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      is_custom : CORBA.Boolean;
      is_abstract : CORBA.Boolean;
      base_value : CORBA.Repository_Root.ValueDef_Forward.Ref;
      is_truncatable : CORBA.Boolean;
      abstract_base_values : CORBA.Repository_Root.ValueDefSeq;
      supported_interfaces : CORBA.Repository_Root.InterfaceDefSeq;
      initializers : CORBA.Repository_Root.InitializerSeq)
     return CORBA.Repository_Root.ValueDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Value (Self.Container_View,
                                          Id,
                                          Name,
                                          Version,
                                          Is_Custom,
                                          Is_Abstract,
                                          Base_Value,
                                          Is_Truncatable,
                                          Abstract_Base_Values,
                                          Supported_Interfaces,
                                          Initializers);
   end create_value;

   function create_value_box
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      original_type_def : CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.ValueBoxDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Value_Box (Self.Container_View,
                                              Id,
                                              Name,
                                              Version,
                                              Original_Type_Def);
   end create_value_box;

   function create_exception
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.ExceptionDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Exception (Self.Container_View,
                                              Id,
                                              Name,
                                              Version,
                                              Members);
   end create_exception;

   function create_native
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.NativeDef_Forward.Ref
   is
   begin
      return Container.Impl.Create_Native (Self.Container_View,
                                           Id,
                                           Name,
                                           Version);
   end create_native;

   -------------------------------
   -- create_abstract_interface --
   -------------------------------

   function create_abstract_interface
     (Self            : access Object;
      id              : RepositoryId;
      name            : Identifier;
      version         : VersionSpec;
      base_interfaces : AbstractInterfaceDefSeq)
     return AbstractInterfaceDef_Forward.Ref
   is
   begin
      return
        Container.Impl.create_abstract_interface
        (Self.Container_View, id, name, version, base_interfaces);
   end create_abstract_interface;

   ----------------------------
   -- create_local_interface --
   ----------------------------

   function create_local_interface
     (Self            : access Object;
      id              : RepositoryId;
      name            : Identifier;
      version         : VersionSpec;
      base_interfaces : InterfaceDefSeq)
     return LocalInterfaceDef_Forward.Ref
   is
   begin
      return
        Container.Impl.create_local_interface
        (Self.Container_View, id, name, version, base_interfaces);
   end create_local_interface;

end CORBA.Repository_Root.StructDef.Impl;
