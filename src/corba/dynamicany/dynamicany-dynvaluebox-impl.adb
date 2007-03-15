------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          D Y N A M I C A N Y . D Y N V A L U E B O X . I M P L           --
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

with DynamicAny.DynValueCommon;

package body DynamicAny.DynValueBox.Impl is

   use PolyORB.Any;
   use PolyORB.Any.TypeCode;

   function Is_Destroyed
     (Self : access DynAny.Impl.Object'Class)
      return Boolean
      renames DynAny.Impl.Internals.Is_Destroyed;

   ---------------------
   -- Get_Boxed_Value --
   ---------------------

   function Get_Boxed_Value (Self : access Object) return CORBA.Any is
      Result : CORBA.Any;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Boxed_Value;

   --------------------------------
   -- Get_Boxed_Value_As_Dyn_Any --
   --------------------------------

   function Get_Boxed_Value_As_Dyn_Any
     (Self : access Object)
      return DynAny.Local_Ref
   is
      Result : DynamicAny.DynAny.Local_Ref;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Boxed_Value_As_Dyn_Any;

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
         pragma Assert (Kind (Get_Type (Value)) = Tk_Valuebox);

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
         pragma Assert (Kind (Value) = Tk_Valuebox);

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
         DynValueCommon.Impl.Internals.Initialize (Self, IDL_Type);
      end Initialize;

      procedure Initialize
        (Self   : access Object'Class;
         Value  : PolyORB.Any.Any;
         Parent : DynAny.Impl.Object_Ptr)
      is
      begin
         DynValueCommon.Impl.Internals.Initialize (Self, Value, Parent);
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
         DynamicAny.DynValueBox.Repository_Id)
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         DynamicAny.DynValueCommon.Repository_Id)
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         DynamicAny.DynAny.Repository_Id);
   end Is_A;

   ---------------------
   -- Set_Boxed_Value --
   ---------------------

   procedure Set_Boxed_Value
     (Self  : access Object;
      Boxed : CORBA.Any)
   is
      pragma Unreferenced (Boxed);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Set_Boxed_Value;

   --------------------------------
   -- Set_Boxed_Value_As_Dyn_Any --
   --------------------------------

   procedure Set_Boxed_Value_As_Dyn_Any
     (Self  : access Object;
      Boxed : DynAny.Local_Ref)
   is
      pragma Unreferenced (Boxed);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Set_Boxed_Value_As_Dyn_Any;

end DynamicAny.DynValueBox.Impl;
