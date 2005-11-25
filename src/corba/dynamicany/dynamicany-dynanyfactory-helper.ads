------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      D Y N A M I C A N Y . D Y N A N Y F A C T O R Y . H E L P E R       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with PolyORB.Any;

package DynamicAny.DynAnyFactory.Helper is

   --  DynAnyFactory interface

   TC_DynAnyFactory : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Object);

   function Unchecked_To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return Local_Ref;

   function To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return Local_Ref;

   --  InconsistentTypeCode exception

   TC_InconsistentTypeCode : CORBA.TypeCode.Object :=
      CORBA.TypeCode.Internals.To_CORBA_Object
      (PolyORB.Any.TypeCode.TC_Except);

   function From_Any (Item : in CORBA.Any) return InconsistentTypeCode_Members;

   function To_Any (Item : in InconsistentTypeCode_Members) return CORBA.Any;

   procedure Raise_InconsistentTypeCode
     (Members : in InconsistentTypeCode_Members);
   pragma No_Return (Raise_InconsistentTypeCode);

end DynamicAny.DynAnyFactory.Helper;
