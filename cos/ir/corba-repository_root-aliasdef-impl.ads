------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  C O R B A . R E P O S I T O R Y _ R O O T . A L I A S D E F . I M P L   --
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

with CORBA.Repository_Root.IDLType;
with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.TypedefDef.Impl;

package CORBA.Repository_Root.AliasDef.Impl is

   type Object is
     new CORBA.Repository_Root.TypedefDef.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  To transform a forward_ref in impl.object_ptr.
   function To_Object (Fw_Ref : AliasDef_Forward.Ref)
                       return Object_Ptr;

   --  To transform an object_ptr into Forward_ref
   function To_Forward (Obj : Object_Ptr)
                        return AliasDef_Forward.Ref;

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
      Original_Type_Def : CORBA.Repository_Root.IDLType.Ref);

   --  overload the get_type from IDLType
   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object;

   function get_original_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref;

   procedure set_original_type_def
     (Self : access Object;
      To : CORBA.Repository_Root.IDLType.Ref);

private

   type Object is new CORBA.Repository_Root.TypedefDef.Impl.Object with record
      Original_Type_Def : CORBA.Repository_Root.IDLType.Ref;
   end record;

end CORBA.Repository_Root.AliasDef.Impl;
