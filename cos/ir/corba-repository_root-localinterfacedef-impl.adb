------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              CORBA.REPOSITORY_ROOT.LOCALINTERFACEDEF.IMPL                --
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

package body CORBA.Repository_Root.LocalInterfaceDef.Impl is

   package body Internals is

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
         Base_Interfaces : InterfaceDefSeq)
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
            Base_Interfaces,
            False);
      end Init;

   end Internals;

end CORBA.Repository_Root.LocalInterfaceDef.Impl;
