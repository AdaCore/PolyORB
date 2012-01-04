------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             CORBA.REPOSITORY_ROOT.ABSTRACTINTERFACEDEF.IMPL              --
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

package body CORBA.Repository_Root.AbstractInterfaceDef.Impl is

   package body Internals is

      function To_InterfaceDefSeq
        (Item : AbstractInterfaceDefSeq)
         return InterfaceDefSeq;

      ----------
      -- Init --
      ----------

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
         Base_Interfaces : AbstractInterfaceDefSeq)
      is
      begin
         InterfaceDef.Impl.Init
           (InterfaceDef.Impl.Object_Ptr (Self),
            Real_Object,
            Def_Kind,
            Id,
            Name,
            Version,
            Defined_In,
            Contents,
            Contained_View,
            IDLType_View,
            To_InterfaceDefSeq (Base_Interfaces),
            True);
      end Init;

      ------------------------
      -- To_InterfaceDefSeq --
      ------------------------

      function To_InterfaceDefSeq
        (Item : AbstractInterfaceDefSeq)
         return InterfaceDefSeq
      is
         Result : InterfaceDefSeq;

      begin
         for J in 1 .. Length (Item) loop
            Append
              (Result,
               InterfaceDef.Convert_Forward.To_Forward
               (InterfaceDef.Ref
                (AbstractInterfaceDef.Convert_Forward.To_Ref
                 (Get_Element (Item, J)))));
         end loop;

         return Result;
      end To_InterfaceDefSeq;

   end Internals;

end CORBA.Repository_Root.AbstractInterfaceDef.Impl;
