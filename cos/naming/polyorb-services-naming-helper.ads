------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . S E R V I C E S . N A M I N G . H E L P E R        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Any;
with PolyORB.References;

package PolyORB.Services.Naming.Helper is

   pragma Elaborate_Body;

   --  Istring type.
   TC_Istring : PolyORB.Any.TypeCode.Object := PolyORB.Any.TypeCode.TC_Alias;

   function From_Any (Item : in PolyORB.Any.Any)
      return Istring;

   function To_Any (Item : in Istring)
     return PolyORB.Any.Any;

   --  NameComponent type.
   TC_NameComponent : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Struct;

   function From_Any (Item : in PolyORB.Any.Any)
      return NameComponent;

   function To_Any
     (Item : in NameComponent)
     return PolyORB.Any.Any;

   --  Sequence of NameComponent type.
   TC_SEQUENCE_NameComponent : PolyORB.Any.TypeCode.Object
     := PolyORB.Any.TypeCode.TC_Sequence;

   function From_Any (Item : in PolyORB.Any.Any)
      return SEQUENCE_NameComponent.Sequence;

   function To_Any
     (Item : in SEQUENCE_NameComponent.Sequence)
     return PolyORB.Any.Any;

   --  Name type.
   TC_Name : PolyORB.Any.TypeCode.Object := PolyORB.Any.TypeCode.TC_Alias;

   function From_Any (Item : in PolyORB.Any.Any)
      return Name;

   function To_Any (Item : in Name)
     return PolyORB.Any.Any;

   --  BindingType type.
   TC_BindingType : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Enum;

   function From_Any (Item : in PolyORB.Any.Any)
      return BindingType;

   function To_Any (Item : in BindingType)
     return PolyORB.Any.Any;

   --  Binding type.
   TC_Binding : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Struct;

   function From_Any (Item : in PolyORB.Any.Any)
      return Binding;

   function To_Any (Item : in Binding)
     return PolyORB.Any.Any;

   --  Sequence Binding type.
   TC_SEQUENCE_Binding : PolyORB.Any.TypeCode.Object
     := PolyORB.Any.TypeCode.TC_Sequence;

   function From_Any (Item : in PolyORB.Any.Any)
      return SEQUENCE_Binding.Sequence;

   function To_Any (Item : in SEQUENCE_Binding.Sequence)
     return PolyORB.Any.Any;

   --  BindingList type.
   TC_BindingList : PolyORB.Any.TypeCode.Object
     := PolyORB.Any.TypeCode.TC_Alias;

   function From_Any (Item : in PolyORB.Any.Any)
      return BindingList;

   function To_Any
     (Item : in BindingList)
     return PolyORB.Any.Any;

   TC_Object : PolyORB.Any.TypeCode.Object
     := PolyORB.Any.TypeCode.TC_Object;

   function To_Any (Item : in PolyORB.References.Ref)
                    return PolyORB.Any.Any;

end PolyORB.Services.Naming.Helper;
