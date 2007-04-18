------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   C O R B A . R E P O S I T O R Y _ R O O T . I D L T Y P E . I M P L    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

with CORBA.Repository_Root.IRObject.Impl;

package CORBA.Repository_Root.IDLType.Impl is

   type Object is
     new CORBA.Repository_Root.IRObject.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   --  should only be called if the cast is safe!
   function To_IDLType
     (Self : CORBA.Repository_Root.IRObject.Impl.Object_Ptr)
      return Object_Ptr;

   --  method used to initialize recursively the object fields.
--   procedure Init (Self : access Object;
--                   Real_Object :
--                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
--                   Def_Kind : CORBA.Repository_Root.DefinitionKind);

   function get_type (Self : access Object) return CORBA.TypeCode.Object;

private

   type Object is new CORBA.Repository_Root.IRObject.Impl.Object
     with null record;
   --  the Type attribute is computed dynamically in each child,
   --  because it can be changed by setting specific attributes after
   --  the beginning.

end CORBA.Repository_Root.IDLType.Impl;
