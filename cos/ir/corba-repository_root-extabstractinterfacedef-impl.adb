------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           CORBA.REPOSITORY_ROOT.EXTABSTRACTINTERFACEDEF.IMPL             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.InterfaceAttrExtension;
with CORBA.Repository_Root.ExtAbstractInterfaceDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.ExtAbstractInterfaceDef.Skel);

package body CORBA.Repository_Root.ExtAbstractInterfaceDef.Impl is

   --------------------------
   -- create_ext_attribute --
   --------------------------

   function create_ext_attribute
     (Self           : access Object;
      id             : RepositoryId;
      name           : Identifier;
      version        : VersionSpec;
      IDL_type       : IDLType.Ref;
      mode           : AttributeMode;
      get_exceptions : ExceptionDefSeq;
      set_exceptions : ExceptionDefSeq)
      return ExtAttributeDef.Ref
   is
   begin
      return
        InterfaceAttrExtension.Impl.create_ext_attribute
        (Self.InterfaceAttrExtension_View,
         id,
         name,
         version,
         IDL_type,
         mode,
         get_exceptions,
         set_exceptions);
   end create_ext_attribute;

   ----------------------------
   -- describe_ext_interface --
   ----------------------------

   function describe_ext_interface
     (Self : access Object)
      return InterfaceAttrExtension.ExtFullInterfaceDescription
   is
   begin
      return
        InterfaceAttrExtension.Impl.describe_ext_interface
        (Self.InterfaceAttrExtension_View);
   end describe_ext_interface;

   package body Internals is

      ----------
      -- Init --
      ----------

      procedure Init
        (Self                        : access Object'Class;
         Real_Object                 : IRObject.Impl.Object_Ptr;
         Def_Kind                    : DefinitionKind;
         Id                          : RepositoryId;
         Name                        : Identifier;
         Version                     : VersionSpec;
         Defined_In                  : Container_Forward.Ref;
         Contents                    : Contained.Impl.Contained_Seq.Sequence;
         Contained_View              : Contained.Impl.Object_Ptr;
         IDLType_View                : IDLType.Impl.Object_Ptr;
         Base_Interfaces             : AbstractInterfaceDefSeq;
         InterfaceAttrExtension_View : InterfaceAttrExtension.Impl.Object_Ptr)
      is
      begin
         AbstractInterfaceDef.Impl.Internals.Init
           (AbstractInterfaceDef.Impl.Object_Ptr (Self),
            Real_Object,
            Def_Kind,
            Id,
            Name,
            Version,
            Defined_In,
            Contents,
            Contained_View,
            IDLType_View,
            Base_Interfaces);

         InterfaceAttrExtension.Impl.Internals.Init
           (InterfaceAttrExtension_View,
            Real_Object);

         Self.InterfaceAttrExtension_View := InterfaceAttrExtension_View;
      end Init;

   end Internals;

end CORBA.Repository_Root.ExtAbstractInterfaceDef.Impl;
