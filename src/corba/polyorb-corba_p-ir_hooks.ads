------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . C O R B A _ P . I R _ H O O K S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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
