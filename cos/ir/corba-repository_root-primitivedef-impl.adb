------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 CORBA.REPOSITORY_ROOT.PRIMITIVEDEF.IMPL                  --
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

with CORBA.Repository_Root.PrimitiveDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.PrimitiveDef.Skel);

package body CORBA.Repository_Root.PrimitiveDef.Impl is

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Type : CORBA.TypeCode.Object;
                   Kind : CORBA.Repository_Root.PrimitiveKind) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind);
      Self.Kind := Kind;
      Self.IDL_Type := IDL_Type;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return Self.IDL_Type;
   end get_type;

   function get_kind
     (Self : access Object)
     return CORBA.Repository_Root.PrimitiveKind
   is
   begin
      return Self.Kind;
   end get_kind;

end CORBA.Repository_Root.PrimitiveDef.Impl;
