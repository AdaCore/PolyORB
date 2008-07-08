------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . C O R B A _ P . I R _ H O O K S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with CORBA.Object;

package PolyORB.CORBA_P.IR_Hooks is

   type Get_Interface_Definition_Hook is
     access function
     (Id : CORBA.RepositoryId)
      return CORBA.Object.Ref'Class;
   --  A Get_Intrerface_Definition hook returns an InterfaceDef reference
   --  that describes the interface identified by Id. Implementations
   --  must node return a Nil reference.
   --  Instead, the exception INTF_REPOS should be raised, with minor code
   --  1 if the interface repository is unavailable, or minor code 2 if the
   --  interface repository don't contain an entry with the specified
   --  RepositoryId.

   function Default_Get_Interface_Definition
     (Id : CORBA.RepositoryId)
      return CORBA.Object.Ref'Class;
   --  Default implementation of Get_Interface_Definition hook.
   --  Always raises INTF_REPOS (Minor => 1).

   Get_Interface_Definition : Get_Interface_Definition_Hook
     := Default_Get_Interface_Definition'Access;

end PolyORB.CORBA_P.IR_Hooks;
