------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            CORBA.REPOSITORY_ROOT.INTERFACEATTREXTENSION.IMPL             --
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

with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.ExtAttributeDef.Impl;
with CORBA.Repository_Root.InterfaceAttrExtension.Skel;
pragma Warnings (Off, CORBA.Repository_Root.InterfaceAttrExtension.Skel);

with PolyORB.CORBA_P.Server_Tools;

package body CORBA.Repository_Root.InterfaceAttrExtension.Impl is

   --------------------------
   -- create_ext_attribute --
   --------------------------

   function create_ext_attribute
     (Self           : access Object;
      id             :        RepositoryId;
      name           :        Identifier;
      version        :        VersionSpec;
      IDL_type       :        IDLType.Ref;
      mode           :        AttributeMode;
      get_exceptions :        ExceptionDefSeq;
      set_exceptions :        ExceptionDefSeq)
      return ExtAttributeDef.Ref
   is
   begin
      Container.Impl.Check_Structure (Self.Real, dk_Attribute);
      Container.Impl.Check_Id (Self.Real, id);
      Container.Impl.Check_Name (Self.Real, name);

      declare
         Result : CORBA.Repository_Root.ExtAttributeDef.Ref;
         Obj    : constant ExtAttributeDef.Impl.Object_Ptr
           := new ExtAttributeDef.Impl.Object;

      begin
         --  Initialize object

         ExtAttributeDef.Impl.Internals.Init
           (Obj,
            IRObject.Impl.Object_Ptr (Obj),
            dk_Attribute,
            id,
            name,
            version,
            Container.Impl.To_Forward (Self.Real),
            IDL_type,
            mode,
            get_exceptions,
            set_exceptions);

         --  Add it to contents of this container

         Container.Impl.Append_To_Contents
           (Self.Real,
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         --  Activate it

         PolyORB.CORBA_P.Server_Tools.Initiate_Servant
           (PortableServer.Servant (Obj), Result);

         return Result;
      end;
   end create_ext_attribute;

   ----------------------------
   -- describe_ext_interface --
   ----------------------------

   function describe_ext_interface
     (Self : access Object)
      return InterfaceAttrExtension.ExtFullInterfaceDescription
   is
      pragma Unreferenced (Self);
      Nil_Ref : InterfaceAttrExtension.ExtFullInterfaceDescription;
      pragma Warnings (Off, Nil_Ref);
      --  XXX this operation need to be implemented
   begin

      --  Insert implementation of describe_ext_interface

      return Nil_Ref;
--      return
--       (name      => get_name (Self.Associated_Interface),
--        id        => get_id (Self.Associated_Interface),
--        defined_in =>
--          Contained.Impl.get_defined_in ((Self.Associated_Interface),
--        version    => get_version (Self.Associated_Interface),
--        operations => XXX
--        attributes => XXX
--        base_interfaces
   end describe_ext_interface;

   package body Internals is

      ----------
      -- Init --
      ----------

      procedure Init
        (Self        : access Object'Class;
         Real_Object :        IRObject.Impl.Object_Ptr)
      is
      begin
         Self.Real := Container.Impl.Object_Ptr (Real_Object);
      end Init;

   end Internals;

end CORBA.Repository_Root.InterfaceAttrExtension.Impl;
