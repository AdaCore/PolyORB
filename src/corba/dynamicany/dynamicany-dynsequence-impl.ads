------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          D Y N A M I C A N Y . D Y N S E Q U E N C E . I M P L           --
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

with DynamicAny.DynAny.Impl;

package DynamicAny.DynSequence.Impl is

   type Object is new DynamicAny.DynAny.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   function Get_Length (Self : access Object) return CORBA.Unsigned_Long;

   procedure Set_Length
     (Self : access Object;
      Len  : CORBA.Unsigned_Long);

   function Get_Elements (Self : access Object) return AnySeq;

   procedure Set_Elements
     (Self  : access Object;
      Value : AnySeq);

   function Get_Elements_As_Dyn_Any (Self : access Object) return DynAnySeq;

   procedure Set_Elements_As_Dyn_Any
     (Self  : access Object;
      Value : DynAnySeq);

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean;

   package Internals is

      procedure Initialize
        (Self   : access Object'Class;
         Value  : PolyORB.Any.Any;
         Parent : DynAny.Impl.Object_Ptr);

      procedure Initialize
        (Self     : access Object'Class;
         IDL_Type : PolyORB.Any.TypeCode.Object);

      function Create
        (Value  : PolyORB.Any.Any;
         Parent : DynAny.Impl.Object_Ptr)
         return DynAny.Local_Ref;

      function Create
        (Value : PolyORB.Any.TypeCode.Object)
         return DynAny.Local_Ref;

   end Internals;

private

   type Object is new DynamicAny.DynAny.Impl.Object with record
      null;
   end record;

end DynamicAny.DynSequence.Impl;
