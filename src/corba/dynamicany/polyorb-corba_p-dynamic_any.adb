------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . C O R B A _ P . D Y N A M I C _ A N Y           --
--                                                                          --
--                                 B o d y                                  --
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

with DynamicAny.DynAnyFactory.Helper;
with DynamicAny.DynArray.Impl;
with DynamicAny.DynEnum.Impl;
with DynamicAny.DynFixed.Impl;
with DynamicAny.DynSequence.Impl;
with DynamicAny.DynStruct.Impl;
with DynamicAny.DynUnion.Impl;
with DynamicAny.DynValue.Impl;
with DynamicAny.DynValueBox.Impl;

package body PolyORB.CORBA_P.Dynamic_Any is

   use PolyORB.Any;
   use PolyORB.Any.TypeCode;

   ------------
   -- Create --
   ------------

   function Create
     (IDL_Type : CORBA.TypeCode.Object)
      return DynamicAny.DynAny.Local_Ref
   is
      Neutral : constant PolyORB.Any.TypeCode.Local_Ref :=
                  CORBA.TypeCode.Internals.To_PolyORB_Object (IDL_Type);

   begin
      case Kind (Unwind_Typedefs (Neutral)) is
         when Tk_Null
           | Tk_Void
           | Tk_Boolean
           | Tk_Octet
           | Tk_Char
           | Tk_Short
           | Tk_Ushort
           | Tk_Long
           | Tk_Ulong
           | Tk_Float
           | Tk_Double
           | Tk_String
           | Tk_TypeCode
           | Tk_Longlong
           | Tk_Ulonglong
           | Tk_Longdouble
           | Tk_Widechar
           | Tk_Wstring
           | Tk_Any
           | Tk_Objref
           | Tk_Abstract_Interface
         =>
            return DynamicAny.DynAny.Impl.Internals.Create (Neutral);

         when Tk_Array =>
            return DynamicAny.DynArray.Impl.Internals.Create (Neutral);

         when Tk_Enum =>
            return DynamicAny.DynEnum.Impl.Internals.Create (Neutral);

         when Tk_Fixed =>
            return DynamicAny.DynFixed.Impl.Internals.Create (Neutral);

         when Tk_Sequence =>
            return DynamicAny.DynSequence.Impl.Internals.Create (Neutral);

         when Tk_Struct
           | Tk_Except
         =>
            return DynamicAny.DynStruct.Impl.Internals.Create (Neutral);

         when Tk_Union =>
            return DynamicAny.DynUnion.Impl.Internals.Create (Neutral);

         when Tk_Value =>
            return DynamicAny.DynValue.Impl.Internals.Create (Neutral);

         when Tk_Valuebox =>
            return DynamicAny.DynValueBox.Impl.Internals.Create (Neutral);

         when Tk_Principal
           | Tk_Native
         =>
            DynamicAny.DynAnyFactory.Helper.Raise_InconsistentTypeCode
              ((CORBA.IDL_Exception_Members with null record));

         when Tk_Local_Interface
           | Tk_Component
           | Tk_Home
           | Tk_Event
         =>
            --  XXX Not yet implemented

            raise Program_Error;

         when Tk_Alias =>
            --  This should never happen

            raise Program_Error;

      end case;
   end Create;

   function Create
     (Value          : CORBA.Any;
      Allow_Truncate : Boolean;
      Parent         : DynamicAny.DynAny.Impl.Object_Ptr)
      return DynamicAny.DynAny.Local_Ref
   is
   begin
      case Kind (Get_Unwound_Type (PolyORB.Any.Any (Value))) is
         when Tk_Null
           | Tk_Void
           | Tk_Boolean
           | Tk_Octet
           | Tk_Char
           | Tk_Short
           | Tk_Ushort
           | Tk_Long
           | Tk_Ulong
           | Tk_Float
           | Tk_Double
           | Tk_String
           | Tk_TypeCode
           | Tk_Longlong
           | Tk_Ulonglong
           | Tk_Longdouble
           | Tk_Widechar
           | Tk_Wstring
           | Tk_Any
           | Tk_Objref
           | Tk_Abstract_Interface
         =>
            return DynamicAny.DynAny.Impl.Internals.Create (Value, Parent);

         when Tk_Array =>
            return DynamicAny.DynArray.Impl.Internals.Create (Value, Parent);

         when Tk_Enum =>
            return DynamicAny.DynEnum.Impl.Internals.Create (Value, Parent);

         when Tk_Fixed =>
            return DynamicAny.DynFixed.Impl.Internals.Create (Value, Parent);

         when Tk_Sequence =>
            return
              DynamicAny.DynSequence.Impl.Internals.Create (Value, Parent);

         when Tk_Struct
           | Tk_Except
         =>
            return
              DynamicAny.DynStruct.Impl.Internals.Create (Value, Parent);

         when Tk_Union =>
            return DynamicAny.DynUnion.Impl.Internals.Create (Value, Parent);

         when Tk_Value =>
            return
              DynamicAny.DynValue.Impl.Internals.Create
              (Value, Allow_Truncate, Parent);

         when Tk_Valuebox =>
            return
              DynamicAny.DynValueBox.Impl.Internals.Create (Value, Parent);

         when Tk_Principal
           | Tk_Native
         =>
            DynamicAny.DynAnyFactory.Helper.Raise_InconsistentTypeCode
              ((CORBA.IDL_Exception_Members with null record));

         when Tk_Local_Interface
           | Tk_Component
           | Tk_Home
           | Tk_Event
         =>
            --  XXX Not yet implemented

            raise Program_Error;

         when Tk_Alias =>
            --  This should never happen

            raise Program_Error;

      end case;
   end Create;

end PolyORB.CORBA_P.Dynamic_Any;
