------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.STRUCTDEF.IMPL                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Repository_Root.Container;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.Container.Impl;
with CORBA.Repository_Root.TypedefDef.Impl;

package CORBA.Repository_Root.StructDef.Impl is

   type Object is
     new CORBA.Repository_Root.TypedefDef.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  To transform a forward_ref in impl.object_ptr.
   function To_Object (Fw_Ref : StructDef_Forward.Ref)
                       return Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return StructDef_Forward.Ref;

   --  method used to initialize recursively the object fields.
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
       Members : CORBA.Repository_Root.StructMemberSeq);

   --  For multiple inheritance, to access the different views
   function Get_Container_View (Self : access Object)
     return CORBA.Repository_Root.Container.Impl.Object_Ptr;

   --  Set the members attribute while putting the "type" field
   --  of the member to TC_Void
   procedure Initialize_Members (Self : access Object;
                                 Seq : StructMemberSeq);

   --  overload the get_type from IDLType
   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.StructMemberSeq;

   procedure set_members
     (Self : access Object;
      To : CORBA.Repository_Root.StructMemberSeq);

   function lookup
     (Self : access Object;
      search_name : CORBA.ScopedName)
     return CORBA.Repository_Root.Contained.Ref;

   function contents
     (Self : access Object;
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq;

   function lookup_name
     (Self : access Object;
      search_name : CORBA.Identifier;
      levels_to_search : CORBA.Long;
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean)
     return CORBA.Repository_Root.ContainedSeq;

   function describe_contents
     (Self : access Object;
      limit_type : CORBA.Repository_Root.DefinitionKind;
      exclude_inherited : CORBA.Boolean;
      max_returned_objs : CORBA.Long)
     return CORBA.Repository_Root.Container.DescriptionSeq;

   function create_module
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.ModuleDef_Forward.Ref;

   function create_constant
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      IDL_type : CORBA.Repository_Root.IDLType_Forward.Ref;
      value : CORBA.Any)
     return CORBA.Repository_Root.ConstantDef_Forward.Ref;

   function create_struct
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.StructDef_Forward.Ref;

   function create_union
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      discriminator_type : CORBA.Repository_Root.IDLType_Forward.Ref;
      members : CORBA.Repository_Root.UnionMemberSeq)
     return CORBA.Repository_Root.UnionDef_Forward.Ref;

   function create_enum
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.EnumMemberSeq)
     return CORBA.Repository_Root.EnumDef_Forward.Ref;

   function create_alias
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      original_type : CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.AliasDef_Forward.Ref;

   function create_interface
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      base_interfaces : CORBA.Repository_Root.InterfaceDefSeq;
      is_abstract : CORBA.Boolean)
     return CORBA.Repository_Root.InterfaceDef_Forward.Ref;

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
     return CORBA.Repository_Root.ValueDef_Forward.Ref;

   function create_value_box
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      original_type_def : CORBA.Repository_Root.IDLType_Forward.Ref)
     return CORBA.Repository_Root.ValueBoxDef_Forward.Ref;

   function create_exception
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      members : CORBA.Repository_Root.StructMemberSeq)
     return CORBA.Repository_Root.ExceptionDef_Forward.Ref;

   function create_native
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec)
     return CORBA.Repository_Root.NativeDef_Forward.Ref;

   function create_abstract_interface
     (Self            : access Object;
      id              : RepositoryId;
      name            : Identifier;
      version         : VersionSpec;
      base_interfaces : AbstractInterfaceDefSeq)
      return AbstractInterfaceDef_Forward.Ref;

   function create_local_interface
     (Self            : access Object;
      id              : RepositoryId;
      name            : Identifier;
      version         : VersionSpec;
      base_interfaces : InterfaceDefSeq)
      return LocalInterfaceDef_Forward.Ref;

--  Implementation Notes: create_ext_value commented out because of error
--  in idlac/ALM (see CORBA_InterfaceRepository.idl)
--   function create_ext_value
--     (Self                 : access Object;
--      id                   : RepositoryId;
--      name                 : Identifier;
--      version              : VersionSpec;
--      is_custom            : CORBA.Boolean;
--      is_abstract          : CORBA.Boolean;
--      base_value           : ValueDef_Forward.Ref;
--      is_truncatable       : CORBA.Boolean;
--      abstract_base_values : ValueDefSeq;
--      supported_interfaces : InterfaceDefSeq;
--      initializers         : ExtInitializerSeq)
--      return ExtValueDef_Forward.Ref;

private

   type Object is new CORBA.Repository_Root.TypedefDef.Impl.Object with record
      Container_View : CORBA.Repository_Root.Container.Impl.Object_Ptr;
      Members : CORBA.Repository_Root.StructMemberSeq;
   end record;

end CORBA.Repository_Root.StructDef.Impl;
