------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.SERVICES.NAMING.NAMINGCONTEXT.HELPER                --
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

with PolyORB.Any;

package PolyORB.Services.Naming.NamingContext.Helper is

   pragma Elaborate_Body;

   function Unchecked_To_Ref
     (The_Ref : in PolyORB.References.Ref)
     return PolyORB.Services.Naming.NamingContext.Ref;

   function To_Ref
     (The_Ref : in PolyORB.References.Ref)
     return PolyORB.Services.Naming.NamingContext.Ref;

   --  NamingContext Type.
   TC_NamingContext : PolyORB.Any.TypeCode.Object
     := PolyORB.Any.TypeCode.TC_Object;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.Ref;

   function To_Any
     (Item : in NamingContext.Ref)
     return PolyORB.Any.Any;

   --  NotFound exception.
   TC_NotFoundReason : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Enum;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.NotFoundReason;

   function To_Any
     (Item : in NamingContext.NotFoundReason)
     return PolyORB.Any.Any;

   TC_NotFound : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Except;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.NotFound_Members;

   function To_Any
     (Item : in NamingContext.NotFound_Members)
     return PolyORB.Any.Any;

   procedure Raise_NotFound_From_Any
     (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_NotFound_From_Any);

   --  CannotProceed exception.
   TC_CannotProceed : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Except;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.CannotProceed_Members;

   function To_Any
     (Item : in NamingContext.CannotProceed_Members)
     return PolyORB.Any.Any;
   procedure Raise_CannotProceed_From_Any
     (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_CannotProceed_From_Any);

   --  InvalidName exception.
   TC_InvalidName : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Except;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.InvalidName_Members;

   function To_Any
     (Item : in NamingContext.InvalidName_Members)
     return PolyORB.Any.Any;
   procedure Raise_InvalidName_From_Any
     (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_InvalidName_From_Any);

   --  AlreadyBound exception.
   TC_AlreadyBound : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Except;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.AlreadyBound_Members;

   function To_Any
     (Item : in NamingContext.AlreadyBound_Members)
     return PolyORB.Any.Any;
   procedure Raise_AlreadyBound_From_Any
     (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_AlreadyBound_From_Any);

   --  NotEmpty exception.
   TC_NotEmpty : PolyORB.Any.TypeCode.Object :=
      PolyORB.Any.TypeCode.TC_Except;

   function From_Any (Item : in PolyORB.Any.Any)
      return NamingContext.NotEmpty_Members;

   function To_Any
     (Item : in NamingContext.NotEmpty_Members)
     return PolyORB.Any.Any;

   procedure Raise_NotEmpty_From_Any
     (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_NotEmpty_From_Any);

end PolyORB.Services.Naming.NamingContext.Helper;
