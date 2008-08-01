------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  C O R B A . R E P O S I T O R Y _ R O O T . V A L U E D E F . I M P L   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.OperationDef;
with CORBA.Repository_Root.AttributeDef;
with CORBA.Repository_Root.ValueMemberDef;
with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Container.Impl;

package CORBA.Repository_Root.ValueDef.Impl is

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  method used to initialize recursively the object fields.
   procedure Init
     (Self : access Object;
      Real_Object : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Def_Kind : CORBA.Repository_Root.DefinitionKind;
      Id : CORBA.RepositoryId;
      Name : CORBA.Identifier;
      Version : CORBA.Repository_Root.VersionSpec;
      Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
      Contents : CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
      Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
      IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
      Supported_Interfaces : CORBA.Repository_Root.InterfaceDefSeq;
      Initializers : CORBA.Repository_Root.InitializerSeq;
      Base_Value : CORBA.Repository_Root.ValueDef.Ref;
      Abstract_Base_Values : CORBA.Repository_Root.ValueDefSeq;
      Is_Abstract : CORBA.Boolean;
      Is_Custom : CORBA.Boolean;
      Is_Truncatable : CORBA.Boolean);

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : ValueDef_Forward.Ref)
                       return Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return ValueDef_Forward.Ref;

   --  For multiple inheritance, to access the different views
   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr;

   function Get_IDLType_View (Self : access Object)
     return CORBA.Repository_Root.IDLType.Impl.Object_Ptr;

   --  functions inherited from the secondary parents
   function get_supported_interfaces
     (Self : access Object)
     return CORBA.Repository_Root.InterfaceDefSeq;

   procedure set_supported_interfaces
     (Self : access Object;
      To : CORBA.Repository_Root.InterfaceDefSeq);

   function get_initializers
     (Self : access Object)
     return CORBA.Repository_Root.InitializerSeq;

   procedure set_initializers
     (Self : access Object;
      To : CORBA.Repository_Root.InitializerSeq);

   function get_base_value
     (Self : access Object)
     return CORBA.Repository_Root.ValueDef.Ref;

   procedure set_base_value
     (Self : access Object;
      To : CORBA.Repository_Root.ValueDef.Ref);

   function get_abstract_base_values
     (Self : access Object)
     return CORBA.Repository_Root.ValueDefSeq;

   procedure set_abstract_base_values
     (Self : access Object;
      To : CORBA.Repository_Root.ValueDefSeq);

   function get_is_abstract
     (Self : access Object)
     return CORBA.Boolean;

   procedure set_is_abstract
     (Self : access Object;
      To : CORBA.Boolean);

   function get_is_custom
     (Self : access Object)
     return CORBA.Boolean;

   procedure set_is_custom
     (Self : access Object;
      To : CORBA.Boolean);

   function get_is_truncatable
     (Self : access Object)
     return CORBA.Boolean;

   procedure set_is_truncatable
     (Self : access Object;
      To : CORBA.Boolean);

   function is_a
     (Self : access Object;
      id : CORBA.RepositoryId)
     return CORBA.Boolean;

   function describe_value
     (Self : access Object)
     return CORBA.Repository_Root.ValueDef.FullValueDescription;

   function create_value_member
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      IDL_type : CORBA.Repository_Root.IDLType.Ref;
      IDL_access : CORBA.Visibility)
     return CORBA.Repository_Root.ValueMemberDef.Ref;

   function create_attribute
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      IDL_type_1 : CORBA.Repository_Root.IDLType.Ref;
      mode : CORBA.Repository_Root.AttributeMode)
     return CORBA.Repository_Root.AttributeDef.Ref;

   function create_operation
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      IDL_result : CORBA.Repository_Root.IDLType.Ref;
      mode : CORBA.Repository_Root.OperationMode;
      params : CORBA.Repository_Root.ParDescriptionSeq;
      exceptions : CORBA.Repository_Root.ExceptionDefSeq;
      contexts : CORBA.Repository_Root.ContextIdSeq)
     return CORBA.Repository_Root.OperationDef.Ref;

   function get_id
     (Self : access Object)
     return CORBA.RepositoryId;

   procedure set_id
     (Self : access Object;
      To : CORBA.RepositoryId);

   function get_name
     (Self : access Object)
     return CORBA.Identifier;

   procedure set_name
     (Self : access Object;
      To : CORBA.Identifier);

   function get_version
     (Self : access Object)
     return CORBA.Repository_Root.VersionSpec;

   procedure set_version
     (Self : access Object;
      To : CORBA.Repository_Root.VersionSpec);

   function get_defined_in
     (Self : access Object)
     return CORBA.Repository_Root.Container_Forward.Ref;

   function get_absolute_name
     (Self : access Object)
     return CORBA.ScopedName;

   function get_containing_repository
     (Self : access Object)
     return CORBA.Repository_Root.Repository_Forward.Ref;

   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description;

   procedure move
     (Self : access Object;
      new_container : CORBA.Repository_Root.Container_Forward.Ref;
      new_name : CORBA.Identifier;
      new_version : CORBA.Repository_Root.VersionSpec);

   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   --  Transform the ValueDefSeq into a RepositoryIdSeq
   function Get_RepositoryIdSeq (ValDefSeq : ValueDefSeq)
                                 return RepositoryIdSeq;

private

   type Object is new CORBA.Repository_Root.Container.Impl.Object with record
      Contained_View : CORBA.Repository_Root.Contained.Impl.Object_Ptr;
      IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
      Supported_Interfaces : CORBA.Repository_Root.InterfaceDefSeq;
      Initializers : CORBA.Repository_Root.InitializerSeq;
      Base_Value : CORBA.Repository_Root.ValueDef.Ref;
      Abstract_Base_Values : CORBA.Repository_Root.ValueDefSeq;
      Is_Abstract : CORBA.Boolean;
      Is_Custom : CORBA.Boolean;
      Is_Truncatable : CORBA.Boolean;
   end record;

end CORBA.Repository_Root.ValueDef.Impl;
