------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.MODULEDEF.IMPL                    --
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

with CORBA.Repository_Root.Contained;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Container.Impl;

package CORBA.Repository_Root.ModuleDef.Impl is

   type Object is
     new CORBA.Repository_Root.Container.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  To transform a forward_ref in impl.object_ptr.
   function To_Object (Fw_Ref : ModuleDef_Forward.Ref)
                       return Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return ModuleDef_Forward.Ref;

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
       Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr);

   --  For multiple inheritance, to access the different views
   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr;

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

private

   type Object is new CORBA.Repository_Root.Container.Impl.Object with record
      Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
   end record;

end CORBA.Repository_Root.ModuleDef.Impl;
