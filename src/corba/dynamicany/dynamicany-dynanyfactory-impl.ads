------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        D Y N A M I C A N Y . D Y N A N Y F A C T O R Y . I M P L         --
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

with CORBA.Local;

package DynamicAny.DynAnyFactory.Impl is

   type Object is new CORBA.Local.Object with private;

   type Object_Ptr is access all Object'Class;

   function Create_Dyn_Any
     (Self  : access Object;
      Value : CORBA.Any)
      return DynAny.Local_Ref;

   function Create_Dyn_Any_From_Type_Code
     (Self     : access Object;
      IDL_Type : CORBA.TypeCode.Object)
      return DynAny.Local_Ref;

   function Create_Dyn_Any_Without_Truncation
     (Self  : access Object;
      Value : CORBA.Any)
      return DynAny.Local_Ref;

   function Create_Multiple_Dyn_Anys
     (Self           : access Object;
      Values         : AnySeq;
      Allow_Truncate : CORBA.Boolean)
      return DynamicAny.DynAnySeq;

   function Create_Multiple_Anys
     (Self   : access Object;
      Values : DynAnySeq)
      return AnySeq;

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean;

private

   type Object is new CORBA.Local.Object with null record;

end DynamicAny.DynAnyFactory.Impl;
