------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.CONTAINER.IMPL                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.IRObject.Impl;

package CORBA.Repository_Root.Container.Impl is

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init
     (Self : access Object;
      Real_Object : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Def_Kind : CORBA.Repository_Root.DefinitionKind;
      Contents : CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence);

   --  To transform a forward_ref in impl.object_ptr.
   function To_Object (Fw_Ref : Container_Forward.Ref)
     return Container.Impl.Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return Container_Forward.Ref;

   --  Our function to get the contents list.
   function Get_Contents (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;

   --  Our function to set the contents list.
   procedure Set_Contents
     (Self : access Object;
      New_List : in
        CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence);

   --  Procedure to append an element to the contents
   procedure Append_To_Contents (Self : access Object;
                                 Element : Contained.Impl.Object_Ptr);

   --  Procedure to delete an element from the contents
   procedure Delete_From_Contents (Self : access Object;
                                   Element : Contained.Impl.Object_Ptr);

   --  usefull for the multiple inhertance
   --  transform an IRObject to a container
   --  success is true if it is possible
   procedure To_Container
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Success : out Boolean;
      Result : out Object_Ptr);

   --  should only be called if the cast is safe!
   function To_Container
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr)
     return Object_Ptr;

   --  Check if a node of this Id already exists in self.
   --  If yes, raise BAD_PARAM (Minor => 2).
   procedure Check_Id
     (Self : access Object;
      Id   : RepositoryId);

   --  Check if a node of this name already exists in self.
   --  If yes, raise BAD_PARAM (Minor => 3).
   procedure Check_Name
     (Self : access Object;
      Name : Identifier);

   --  Check if a node of kind "Kind", can be created or moved in Self,
   --  according to the navigation and structure rules.
   --  Raise BAD_PARAM (Minor => 4) if not compliant.
   procedure Check_Structure
     (Self : access Object;
      Kind : DefinitionKind);

   -------------
   -- IR spec --
   -------------
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

   type Object is new CORBA.Repository_Root.IRObject.Impl.Object with record
      Contents : CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
   end record;

end CORBA.Repository_Root.Container.Impl;
