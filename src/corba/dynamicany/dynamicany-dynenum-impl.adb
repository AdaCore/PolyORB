------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              D Y N A M I C A N Y . D Y N E N U M . I M P L               --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Smart_Pointers;
with PolyORB.Types;

with DynamicAny.DynAny.Helper;

package body DynamicAny.DynEnum.Impl is

   use PolyORB.Any;
   use PolyORB.Any.TypeCode;
   use type PolyORB.Types.Identifier;
   use type PolyORB.Types.Unsigned_Long;

   function Is_Destroyed
     (Self : access DynAny.Impl.Object'Class)
      return Boolean
      renames DynAny.Impl.Internals.Is_Destroyed;

   -------------------
   -- Get_As_String --
   -------------------

   function Get_As_String (Self : access Object) return CORBA.String is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Value : constant PolyORB.Any.Any
           := DynAny.Impl.Internals.Get_Value (Self);
         TC    : constant PolyORB.Any.TypeCode.Object := Get_Type (Value);
         Index : constant PolyORB.Types.Unsigned_Long
           := From_Any
           (Get_Aggregate_Element (Value, PolyORB.Any.TC_Unsigned_Long, 0));

      begin
         return CORBA.String (Member_Name (TC, Index));
      end;
   end Get_As_String;

   ------------------
   -- Get_As_ULong --
   ------------------

   function Get_As_ULong (Self : access Object) return CORBA.Unsigned_Long is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Value : constant PolyORB.Any.Any
           := DynAny.Impl.Internals.Get_Value (Self);

      begin
         return
           CORBA.Unsigned_Long
           (PolyORB.Types.Unsigned_Long'
            (From_Any
             (Get_Aggregate_Element
              (Value, PolyORB.Any.TC_Unsigned_Long, 0))));
      end;
   end Get_As_ULong;

   ---------------
   -- Internals --
   ---------------

   package body Internals is

      ------------
      -- Create --
      ------------

      function Create
        (Value  : PolyORB.Any.Any;
         Parent : DynAny.Impl.Object_Ptr)
         return DynAny.Local_Ref
      is
         Obj    : constant Object_Ptr := new Object;

         Result : DynAny.Local_Ref;

      begin
         pragma Assert (Kind (Get_Type (Value)) = Tk_Enum);

         Initialize (Obj, Value, Parent);

         DynAny.Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Obj));

         return Result;
      end Create;

      function Create
        (Value : PolyORB.Any.TypeCode.Object)
         return DynAny.Local_Ref
      is
         Obj    : constant Object_Ptr := new Object;

         Result : DynAny.Local_Ref;

      begin
         pragma Assert (Kind (Value) = Tk_Array);

         Initialize (Obj, Value);

         DynAny.Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Obj));

         return Result;
      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self     : access Object'Class;
         IDL_Type : PolyORB.Any.TypeCode.Object)
      is
      begin
         DynAny.Impl.Internals.Initialize (Self, IDL_Type);
      end Initialize;

      procedure Initialize
        (Self   : access Object'Class;
         Value  : PolyORB.Any.Any;
         Parent : DynAny.Impl.Object_Ptr)
      is
      begin
         DynAny.Impl.Internals.Initialize (Self, Value, Parent);
      end Initialize;

   end Internals;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);

   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         DynamicAny.DynEnum.Repository_Id)
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         DynamicAny.DynAny.Repository_Id);
   end Is_A;

   -------------------
   -- Set_As_String --
   -------------------

   procedure Set_As_String
     (Self  : access Object;
      Value : CORBA.String)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Val        : constant PolyORB.Any.Any
           := DynAny.Impl.Internals.Get_Value (Self);
         TC         : constant PolyORB.Any.TypeCode.Object := Get_Type (Val);
         Enum_Value : PolyORB.Any.Any
           := Get_Aggregate_Element (Val, PolyORB.Any.TC_Unsigned_Long, 0);

      begin
         for J in 0 .. Member_Count (TC) - 1 loop
            if Member_Name (TC, J) = PolyORB.Types.Identifier (Value) then
               Set_Any_Value (J, Get_Container (Enum_Value).all);
               return;
            end if;
         end loop;
      end;

      DynAny.Helper.Raise_InvalidValue
        ((CORBA.IDL_Exception_Members with null record));
   end Set_As_String;

   ------------------
   -- Set_As_ULong --
   ------------------

   procedure Set_As_ULong
     (Self  : access Object;
      Value : CORBA.Unsigned_Long)
   is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Val        : constant PolyORB.Any.Any
           := DynAny.Impl.Internals.Get_Value (Self);
         TC         : constant PolyORB.Any.TypeCode.Object := Get_Type (Val);
         Enum_Value : PolyORB.Any.Any
           := Get_Aggregate_Element (Val, PolyORB.Any.TC_Unsigned_Long, 0);

      begin
         if PolyORB.Types.Unsigned_Long (Value) < Member_Count (TC) then
            Set_Any_Value
              (PolyORB.Types.Unsigned_Long (Value),
               Get_Container (Enum_Value).all);

         else
            DynAny.Helper.Raise_InvalidValue
              ((CORBA.IDL_Exception_Members with null record));
         end if;
      end;
   end Set_As_ULong;

end DynamicAny.DynEnum.Impl;
