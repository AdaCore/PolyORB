------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 PORTABLEINTERCEPTOR.ORBINITINFO.HELPER                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any;

package PortableInterceptor.ORBInitInfo.Helper is

   pragma Elaborate_Body;

   function Unchecked_To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return Local_Ref;

   function To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return Local_Ref;

   --  ObjectId type

   TC_ObjectId : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return ObjectId;

   function To_Any (Item : in ObjectId) return CORBA.Any;

   --  DuplicateName exception

   TC_DuplicateName : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Except);

   function From_Any (Item : in CORBA.Any) return DuplicateName_Members;

   function To_Any (Item : in DuplicateName_Members) return CORBA.Any;

   procedure Raise_DuplicateName (Members : in DuplicateName_Members);
   pragma No_Return (Raise_DuplicateName);

   --  InvalidName exception

   TC_InvalidName : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Except);

   function From_Any (Item : in CORBA.Any) return InvalidName_Members;

   function To_Any (Item : in InvalidName_Members) return CORBA.Any;

   procedure Raise_InvalidName (Members : in InvalidName_Members);
   pragma No_Return (Raise_InvalidName);

end PortableInterceptor.ORBInitInfo.Helper;
