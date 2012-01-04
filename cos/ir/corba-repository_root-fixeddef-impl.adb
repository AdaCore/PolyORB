------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  C O R B A . R E P O S I T O R Y _ R O O T . F I X E D D E F . I M P L   --
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

with CORBA.Repository_Root.FixedDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.FixedDef.Skel);

package body CORBA.Repository_Root.FixedDef.Impl is

   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   IDL_Digits : CORBA.Unsigned_Short;
                   Scale : CORBA.Short) is
   begin
      IDLType.Impl.Init (IDLType.Impl.Object_Ptr (Self),
                         Real_Object,
                         Def_Kind);
      Self.IDL_Digits := IDL_Digits;
      Self.Scale := Scale;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.Create_Fixed_Tc (Self.IDL_Digits, Self.Scale);
   end get_type;

   function get_digits
     (Self : access Object)
     return CORBA.Unsigned_Short
   is
   begin
      return Self.IDL_Digits;
   end get_digits;

   procedure set_digits
     (Self : access Object;
      To : CORBA.Unsigned_Short) is
   begin
      Self.IDL_Digits := To;
   end set_digits;

   function get_scale
     (Self : access Object)
     return CORBA.Short
   is
   begin
      return Self.Scale;
   end get_scale;

   procedure set_scale
     (Self : access Object;
      To : CORBA.Short) is
   begin
      Self.Scale := To;
   end set_scale;

end CORBA.Repository_Root.FixedDef.Impl;
