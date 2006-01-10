------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           I O P . H E L P E R                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Any;

package IOP.Helper is

   pragma Elaborate_Body;

   --  ProfileId type

   TC_ProfileId : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return ProfileId;

   function To_Any (Item : ProfileId) return CORBA.Any;

   --  TaggedProfile type

   TC_TaggedProfile : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return TaggedProfile;

   function To_Any (Item : TaggedProfile) return CORBA.Any;

   --  Anonymous TaggedProfile sequence

   TC_IDL_SEQUENCE_IOP_TaggedProfile : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any (Item : CORBA.Any)
      return IDL_SEQUENCE_IOP_TaggedProfile.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_IOP_TaggedProfile.Sequence)
      return CORBA.Any;

   --  IOR type

   TC_IOR : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return IOR;

   function To_Any (Item : IOR) return CORBA.Any;

   --  ComponentId type

   TC_ComponentId : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return ComponentId;

   function To_Any (Item : ComponentId) return CORBA.Any;

   --  TaggedComponent type

   TC_TaggedComponent : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return TaggedComponent;

   function To_Any (Item : TaggedComponent) return CORBA.Any;

   --  Anonymous TaggedComponent sequence

   TC_IDL_SEQUENCE_IOP_TaggedComponent : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any (Item : CORBA.Any)
      return IDL_SEQUENCE_IOP_TaggedComponent.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_IOP_TaggedComponent.Sequence)
      return CORBA.Any;

   --  TaggedComponentSeq type

   TC_TaggedComponentSeq : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return TaggedComponentSeq;

   function To_Any (Item : TaggedComponentSeq) return CORBA.Any;

   --  MultipleComponentProfile type

   TC_MultipleComponentProfile : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return MultipleComponentProfile;

   function To_Any (Item : MultipleComponentProfile) return CORBA.Any;

   --  ServiceId type

   TC_ServiceId : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return ServiceId;

   function To_Any (Item : ServiceId) return CORBA.Any;

   --  ServiceContext type

   TC_ServiceContext : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return ServiceContext;

   function To_Any (Item : ServiceContext) return CORBA.Any;

   --  Anonymous ServiceContext sequence

   TC_IDL_SEQUENCE_IOP_ServiceContext : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Sequence);

   function From_Any (Item : CORBA.Any)
      return IDL_SEQUENCE_IOP_ServiceContext.Sequence;

   function To_Any
     (Item : IDL_SEQUENCE_IOP_ServiceContext.Sequence)
      return CORBA.Any;

   --  ServiceContextList type

   TC_ServiceContextList : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return ServiceContextList;

   function To_Any (Item : ServiceContextList) return CORBA.Any;

   --  EncodingFormat type

   TC_EncodingFormat : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Alias);

   function From_Any (Item : CORBA.Any) return EncodingFormat;

   function To_Any (Item : EncodingFormat) return CORBA.Any;

   --  Encoding type

   TC_Encoding : CORBA.TypeCode.Object
     := CORBA.TypeCode.Internals.To_CORBA_Object
     (PolyORB.Any.TypeCode.TC_Struct);

   function From_Any (Item : CORBA.Any) return Encoding;

   function To_Any (Item : Encoding) return CORBA.Any;

end IOP.Helper;
