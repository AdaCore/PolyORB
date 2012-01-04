------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  CORBA.REPOSITORY_ROOT.WSTRINGDEF.IMPL                   --
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

with CORBA.ORB;

with CORBA.Repository_Root.WstringDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.WstringDef.Skel);

package body CORBA.Repository_Root.WstringDef.Impl is

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Bound : CORBA.Unsigned_Long) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind);
      Self.Bound := Bound;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.Create_Wstring_Tc (Self.Bound);
   end get_type;

   function get_bound
     (Self : access Object)
     return CORBA.Unsigned_Long
   is
   begin
      return Self.Bound;
   end get_bound;

   procedure set_bound
     (Self : access Object;
      To : CORBA.Unsigned_Long) is
   begin
      Self.Bound := To;
   end set_bound;

end CORBA.Repository_Root.WstringDef.Impl;
