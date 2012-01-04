------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               CORBA.REPOSITORY_ROOT.EXTATTRIBUTEDEF.IMPL                 --
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

with CORBA.Repository_Root.ExceptionDef.Impl;
with CORBA.Repository_Root.ExtAttributeDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.ExtAttributeDef.Skel);

package body CORBA.Repository_Root.ExtAttributeDef.Impl is

   ------------------------
   -- describe_attribute --
   ------------------------

   function describe_attribute
     (Self : access Object)
      return ExtAttributeDescription
   is
   begin
      return
       (name           => get_name (Self),
        id             => get_id (Self),
        defined_in     => get_defined_in (Self),
        version        => get_version (Self),
        IDL_type       => get_type (Self),
        mode           => get_mode (Self),
        get_exceptions => Self.Get_Exceptions,
        put_exceptions => Self.Set_Exceptions);
   end describe_attribute;

   ------------------------
   -- get_get_exceptions --
   ------------------------

   function get_get_exceptions
     (Self : access Object)
      return ExcDescriptionSeq
   is
   begin
      return Self.Get_Exceptions;
   end get_get_exceptions;

   ------------------------
   -- get_set_exceptions --
   ------------------------

   function get_set_exceptions
     (Self : access Object)
      return ExcDescriptionSeq
   is
   begin
      return Self.Set_Exceptions;
   end get_set_exceptions;

   ------------------------
   -- set_get_exceptions --
   ------------------------

   procedure set_get_exceptions
     (Self : access Object;
      To   : ExcDescriptionSeq)
   is
   begin
      Self.Get_Exceptions := To;
   end set_get_exceptions;

   ------------------------
   -- set_set_exceptions --
   ------------------------

   procedure set_set_exceptions
     (Self : access Object;
      To   : ExcDescriptionSeq)
   is
   begin
      Self.Set_Exceptions := To;
   end set_set_exceptions;

   package body Internals is

      ----------
      -- Init --
      ----------

      procedure Init
        (Self           : access Object'Class;
         Real_Object    : IRObject.Impl.Object_Ptr;
         Def_Kind       : DefinitionKind;
         Id             : RepositoryId;
         Name           : Identifier;
         Version        : VersionSpec;
         Defined_In     : Container_Forward.Ref;
         Type_Def       : IDLType.Ref;
         Mode           : AttributeMode;
         Get_Exceptions : ExceptionDefSeq;
         Set_Exceptions : ExceptionDefSeq)
      is
      begin
         AttributeDef.Impl.Init
           (AttributeDef.Impl.Object_Ptr (Self),
            Real_Object,
            Def_Kind,
            Id,
            Name,
            Version,
            Defined_In,
            Type_Def,
            Mode);

         Self.Get_Exceptions :=
           ExceptionDef.Impl.Get_ExcDescriptionSeq (Get_Exceptions);
         Self.Set_Exceptions :=
           ExceptionDef.Impl.Get_ExcDescriptionSeq (Set_Exceptions);
      end Init;

   end Internals;

end CORBA.Repository_Root.ExtAttributeDef.Impl;
