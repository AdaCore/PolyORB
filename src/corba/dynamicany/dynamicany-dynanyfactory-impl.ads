------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        D Y N A M I C A N Y . D Y N A N Y F A C T O R Y . I M P L         --
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
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean;

private
   type Object is new CORBA.Local.Object with null record;
end DynamicAny.DynAnyFactory.Impl;
