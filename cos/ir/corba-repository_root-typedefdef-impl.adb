------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.TYPEDEFDEF.IMPL                   --
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

pragma Style_Checks (Off);

with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.TypedefDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.TypedefDef.Skel);

package body CORBA.Repository_Root.TypedefDef.Impl is

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
     IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
      IDLType.Impl.Init (IDLType_View,
                         Real_Object,
                         Def_Kind);
      Self.IDLType_View := IDLType_View;
   end Init;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------

   function Get_IDLType_View (Self : access Object)
     return CORBA.Repository_Root.IDLType.Impl.Object_Ptr is
   begin
      return Self.IDLType_View;
   end Get_IDLType_View;

   -----------------------------
   --  Inherited from IDLType --
   -----------------------------
   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
   begin
      return IDLType.Impl.Get_Type (Self.IDLType_View);
   end get_type;

   ----------------
   --  Describe  --
   ----------------
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
     is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.TypeDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Get_Defined_In (Self),
               Version => Get_Version (Self),
               IDL_Type => IDLType.Impl.Get_Type (Self.IDLType_View));
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end describe;

end CORBA.Repository_Root.TypedefDef.Impl;
