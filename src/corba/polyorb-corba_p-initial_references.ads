------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N I T I A L _ R E F E R E N C E S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

--  Support package for CORBA initial references.

with CORBA.Object;

with PolyORB.Utils.Strings.Lists;

package PolyORB.CORBA_P.Initial_References is

   type Create_Ptr is access function return CORBA.Object.Ref;
   --  Allocator type

   procedure Register_Initial_Reference
     (Id        : Standard.String;
      Allocator : Create_Ptr);
   --  Register (Id, Allocator) tuple

   procedure Register_Initial_Reference
     (Id  : Standard.String;
      Ref : CORBA.Object.Ref);
   --  Register (Id, Ref) tuple

   function Resolve_Initial_References
     (Id : Standard.String)
     return CORBA.Object.Ref;
   --  Return a valid reference to an object if Id has been previously
   --  registred.
   --  If Id has been registred with a CORBA.Object.Ref, then returns it.
   --  If Id has been registred with an allocator, use this allocator
   --  to create a reference.

   function List_Initial_Services
     return PolyORB.Utils.Strings.Lists.List;
   --  List all registered references.

end PolyORB.CORBA_P.Initial_References;
