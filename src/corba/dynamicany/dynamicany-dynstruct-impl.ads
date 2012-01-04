------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            D Y N A M I C A N Y . D Y N S T R U C T . I M P L             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any;

with DynamicAny.DynAny.Impl;

package DynamicAny.DynStruct.Impl is

   type Object is new DynamicAny.DynAny.Impl.Object with private;

   type Object_Ptr is access all Object'Class;

   function Current_Member_Name (Self : access Object) return FieldName;

   function Current_Member_Kind (Self : access Object) return CORBA.TCKind;

   function Get_Members (Self : access Object) return NameValuePairSeq;

   procedure Set_Members
     (Self  : access Object;
      Value : NameValuePairSeq);

   function Get_Members_As_Dyn_Any
     (Self : access Object)
      return NameDynAnyPairSeq;

   procedure Set_Members_As_Dyn_Any
     (Self  : access Object;
      Value : NameDynAnyPairSeq);

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean;

   package Internals is

      procedure Initialize
        (Self   : access Object'Class;
         Value  : PolyORB.Any.Any;
         Parent : DynAny.Impl.Object_Ptr);

      procedure Initialize
        (Self     : access Object'Class;
         IDL_Type : PolyORB.Any.TypeCode.Local_Ref);

      function Create
        (Value  : CORBA.Any;
         Parent : DynAny.Impl.Object_Ptr) return DynAny.Local_Ref;

      function Create
        (Value : PolyORB.Any.TypeCode.Local_Ref) return DynAny.Local_Ref;

   end Internals;

private

   type Object is new DynamicAny.DynAny.Impl.Object with record
      null;
   end record;

end DynamicAny.DynStruct.Impl;
