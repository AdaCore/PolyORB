------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     M E S S A G I N G . H E L P E R                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with CORBA;
with PolyORB.Any;

package Messaging.Helper is

   pragma Elaborate_Body;

   TC_IDL_SEQUENCE_Octet : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any (Item : CORBA.Any) return IDL_SEQUENCE_Octet.Sequence;

   function To_Any (Item : IDL_SEQUENCE_Octet.Sequence) return CORBA.Any;

   --  RebindMode type

   TC_RebindMode : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return RebindMode;

   function To_Any (Item : RebindMode) return CORBA.Any;

   --  SyncScope type

   TC_SyncScope : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return SyncScope;

   function To_Any (Item : SyncScope) return CORBA.Any;

   --  RoutingType

   TC_RoutingType : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return RoutingType;

   function To_Any (Item : RoutingType) return CORBA.Any;

   --  Priority type

   TC_Priority : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return Priority;

   function To_Any (Item : Priority) return CORBA.Any;

   --  Ordering type

   TC_Ordering : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return Ordering;

   function To_Any (Item : Ordering) return CORBA.Any;

   --  PriorityRange struct

   TC_PriorityRange : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return PriorityRange;

   function To_Any (Item : PriorityRange) return CORBA.Any;

   --  RoutingTypeRange struct

   TC_RoutingTypeRange : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return RoutingTypeRange;

   function To_Any (Item : RoutingTypeRange) return CORBA.Any;

   --  PolicyValue struct

   TC_PolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return PolicyValue;

   function To_Any (Item : PolicyValue) return CORBA.Any;

   --  PolicyValueSeq type

   TC_IDL_SEQUENCE_Messaging_PolicyValue : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any (Item : CORBA.Any)
      return IDL_SEQUENCE_Messaging_PolicyValue.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_Messaging_PolicyValue.Sequence)
     return CORBA.Any;

   TC_PolicyValueSeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
         (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return PolicyValueSeq;

   function To_Any (Item : PolicyValueSeq) return CORBA.Any;

end Messaging.Helper;
