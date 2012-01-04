------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              CORBA.REPOSITORY_ROOT.LOCALINTERFACEDEF.IMPL                --
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

with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.InterfaceDef.Impl;
with CORBA.Repository_Root.IRObject.Impl;

package CORBA.Repository_Root.LocalInterfaceDef.Impl is

   type Object is new InterfaceDef.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   package Internals is

      procedure Init
        (Self            : access Object'Class;
         Real_Object     : IRObject.Impl.Object_Ptr;
         Def_Kind        : DefinitionKind;
         Id              : RepositoryId;
         Name            : Identifier;
         Version         : VersionSpec;
         Defined_In      : Container_Forward.Ref;
         Contents        : Contained.Impl.Contained_Seq.Sequence;
         Contained_View  : Contained.Impl.Object_Ptr;
         IDLType_View    : IDLType.Impl.Object_Ptr;
         Base_Interfaces : InterfaceDefSeq);

   end Internals;

private

   type Object is new InterfaceDef.Impl.Object with null record;

end CORBA.Repository_Root.LocalInterfaceDef.Impl;
