------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            CORBA.REPOSITORY_ROOT.INTERFACEATTREXTENSION.IMPL             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
      id             : in     RepositoryId;
      name           : in     Identifier;
      version        : in     VersionSpec;
      IDL_type       : in     IDLType.Ref;
      mode           : in     AttributeMode;
      get_exceptions : in     ExceptionDefSeq;
      set_exceptions : in     ExceptionDefSeq)
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
         Real_Object : in     IRObject.Impl.Object_Ptr)
      is
      begin
         Self.Real := Container.Impl.Object_Ptr (Real_Object);
      end Init;

   end Internals;

end CORBA.Repository_Root.InterfaceAttrExtension.Impl;
