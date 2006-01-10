------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . H E L P E R                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

package CORBA.Helper is

   function TC_RepositoryId return CORBA.TypeCode.Object;
   function From_Any (Item : CORBA.Any) return CORBA.RepositoryId;
   function To_Any (Item : CORBA.RepositoryId) return CORBA.Any;

   function TC_Identifier return CORBA.TypeCode.Object;
   function From_Any (Item : CORBA.Any) return CORBA.Identifier;
   function To_Any (Item : CORBA.Identifier) return CORBA.Any;

   function TC_ScopedName return CORBA.TypeCode.Object;
   function From_Any (Item : CORBA.Any) return CORBA.ScopedName;
   function To_Any (Item : CORBA.ScopedName) return CORBA.Any;

   function TC_Visibility return CORBA.TypeCode.Object;
   function From_Any (Item : CORBA.Any) return CORBA.Visibility;
   function To_Any (Item : CORBA.Visibility) return CORBA.Any;

   function TC_PolicyType return CORBA.TypeCode.Object;
   function From_Any (Item : CORBA.Any) return CORBA.PolicyType;
   function To_Any (Item : CORBA.PolicyType) return CORBA.Any;

end CORBA.Helper;
