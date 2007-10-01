------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C O R B A . D O M A I N M A N A G E R . H E L P E R            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with CORBA.Object;

package CORBA.DomainManager.Helper is

   function Unchecked_To_Ref (The_Ref : Object.Ref'Class) return Ref;

   function To_Ref (The_Ref : Object.Ref'Class) return Ref;

   function TC_DomainManager return TypeCode.Object;

   function From_Any (Item : Any) return Ref;

   function To_Any (Item : Ref) return Any;

   --  DomainManager sequence

   function TC_IDL_SEQUENCE_DomainManager return TypeCode.Object;

   function From_Any (Item : Any)
      return IDL_SEQUENCE_DomainManager.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_DomainManager.Sequence)
      return Any;

   function TC_DomainManagersList return TypeCode.Object;

end CORBA.DomainManager.Helper;
