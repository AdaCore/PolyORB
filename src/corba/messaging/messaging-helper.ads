------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     M E S S A G I N G . H E L P E R                      --
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

with CORBA;
with PolyORB.Any;

package Messaging.Helper is

   pragma Elaborate_Body;

   TC_IDL_Sequence_Octet : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any (Item : in CORBA.Any) return IDL_Sequence_Octet.Sequence;

   function To_Any (Item : in IDL_Sequence_Octet.Sequence) return CORBA.Any;

   --  RebindMode type

   TC_RebindMode : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return RebindMode;

   function To_Any (Item : in RebindMode) return CORBA.Any;

   --  SyncScope type

   TC_SyncScope : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return SyncScope;

   function To_Any (Item : in SyncScope) return CORBA.Any;

   --  RoutingType

   TC_RoutingType : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return RoutingType;

   function To_Any (Item : in RoutingType) return CORBA.Any;

   --  Priority type

   TC_Priority : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return Priority;

   function To_Any (Item : in Priority) return CORBA.Any;

   --  Ordering type

   TC_Ordering : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return Ordering;

   function To_Any (Item : in Ordering) return CORBA.Any;

   --  PriorityRange struct

   TC_PriorityRange : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : in CORBA.Any) return PriorityRange;

   function To_Any (Item : in PriorityRange) return CORBA.Any;

   --  RoutingTypeRange struct

   TC_RoutingTypeRange : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : in CORBA.Any) return RoutingTypeRange;

   function To_Any (Item : in RoutingTypeRange) return CORBA.Any;

   --  PolicyValue struct

   TC_PolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : in CORBA.Any) return PolicyValue;

   function To_Any (Item : in PolicyValue) return CORBA.Any;

   --  PolicyValueSeq type

   TC_IDL_Sequence_Messaging_PolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any (Item : in CORBA.Any)
      return IDL_Sequence_Messaging_PolicyValue.Sequence;

   function To_Any
     (Item : in IDL_Sequence_Messaging_PolicyValue.Sequence)
     return CORBA.Any;

   TC_PolicyValueSeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : in CORBA.Any) return PolicyValueSeq;

   function To_Any (Item : in PolicyValueSeq) return CORBA.Any;

end Messaging.Helper;
