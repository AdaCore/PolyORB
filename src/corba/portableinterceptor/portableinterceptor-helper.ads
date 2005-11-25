------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O R T A B L E I N T E R C E P T O R . H E L P E R            --
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

with CORBA;

package PortableInterceptor.Helper is

   pragma Elaborate_Body;

   --  ForwardRequest exception

   TC_ForwardRequest : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Except);

   function From_Any (Item : in CORBA.Any) return ForwardRequest_Members;

   function To_Any (Item : in ForwardRequest_Members) return CORBA.Any;

   procedure Raise_ForwardRequest (Members : in ForwardRequest_Members);
   pragma No_Return (Raise_ForwardRequest);

   --  ReplyStatus type

   TC_ReplyStatus : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return ReplyStatus;

   function To_Any (Item : in ReplyStatus) return CORBA.Any;

   --  SlotId type

   TC_SlotId : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return SlotId;

   function To_Any (Item : in SlotId) return CORBA.Any;

   --  InvalidSlot exception

   TC_InvalidSlot : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Except);

   function From_Any (Item : in CORBA.Any) return InvalidSlot_Members;

   function To_Any (Item : in InvalidSlot_Members) return CORBA.Any;

   procedure Raise_InvalidSlot (Members : in InvalidSlot_Members);
   pragma No_Return (Raise_InvalidSlot);

   --  ServerId type

   TC_ServerId : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return ServerId;

   function To_Any (Item : in ServerId) return CORBA.Any;

   --  ORBId type

   TC_ORBId : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return ORBId;

   function To_Any (Item : in ORBId) return CORBA.Any;

   --  AdapterManagerId type

   TC_AdapterManagerId : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return AdapterManagerId;

   function To_Any (Item : in AdapterManagerId) return CORBA.Any;

   --  AdapterState type

   TC_AdapterState : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return AdapterState;

   function To_Any (Item : in AdapterState) return CORBA.Any;

end PortableInterceptor.Helper;
