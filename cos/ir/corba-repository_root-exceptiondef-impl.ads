------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 CORBA.REPOSITORY_ROOT.EXCEPTIONDEF.IMPL                  --
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

with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Container.Impl;

package CORBA.Repository_Root.ExceptionDef.Impl is

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
      Contained_View : CORBA.Repository_Root.Contained.Impl.Object_Ptr;
      Members : CORBA.Repository_Root.StructMemberSeq);

   --  Transform the forward to an impl.object.ptr.
   function To_Object (Fw_Ref : ExceptionDef_Forward.Ref)
                       return Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return ExceptionDef_Forward.Ref;

   --  for accessing the secondary parents view
   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr;

   --  Set the members attribute while putting the "type" field
   --  of the member to TC_Void
   procedure Initialize_Members (Self : access Object;
                                 Seq : StructMemberSeq);

   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object;

   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.StructMemberSeq;

   procedure set_members
     (Self : access Object;
      To : CORBA.Repository_Root.StructMemberSeq);

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

   --  Transform a ExceptionDefSeq ExcDescriptionSeq
   function Get_ExcDescriptionSeq (ExcDefSeq : ExceptionDefSeq)
                                   return ExcDescriptionSeq;

private

   type Object is new CORBA.Repository_Root.Container.Impl.Object with record
      Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
      --  the Type will be computed dynamically ...
      Members : CORBA.Repository_Root.StructMemberSeq;
   end record;

end CORBA.Repository_Root.ExceptionDef.Impl;
