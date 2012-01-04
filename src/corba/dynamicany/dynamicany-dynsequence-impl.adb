------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          D Y N A M I C A N Y . D Y N S E Q U E N C E . I M P L           --
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

with PolyORB.Smart_Pointers;

package body DynamicAny.DynSequence.Impl is

   use PolyORB.Any;
   use PolyORB.Any.TypeCode;

   function Is_Destroyed
     (Self : access DynAny.Impl.Object'Class)
      return Boolean
      renames DynAny.Impl.Internals.Is_Destroyed;

   ------------------
   -- Get_Elements --
   ------------------

   function Get_Elements (Self : access Object) return AnySeq is
      Result : DynamicAny.AnySeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Elements;

   -----------------------------
   -- Get_Elements_As_Dyn_Any --
   -----------------------------

   function Get_Elements_As_Dyn_Any (Self : access Object) return DynAnySeq is
      Result : DynamicAny.DynAnySeq;

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return Result;
   end Get_Elements_As_Dyn_Any;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Self : access Object) return CORBA.Unsigned_Long is
   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
      return 0;
   end Get_Length;

   ---------------
   -- Internals --
   ---------------

   package body Internals is

      ------------
      -- Create --
      ------------

      function Create
        (Value  : CORBA.Any;
         Parent : DynAny.Impl.Object_Ptr) return DynAny.Local_Ref
      is
         Obj    : constant Object_Ptr := new Object;
         Result : DynAny.Local_Ref;

      begin
         pragma Assert (Kind (CORBA.Get_Type (Value)) = Tk_Sequence);

         Initialize (Obj, PolyORB.Any.Any (Value), Parent);

         DynAny.Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Obj));

         return Result;
      end Create;

      function Create
        (Value : PolyORB.Any.TypeCode.Local_Ref)
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
         IDL_Type : PolyORB.Any.TypeCode.Local_Ref)
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
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, DynamicAny.DynSequence.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, DynamicAny.DynAny.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   ------------------
   -- Set_Elements --
   ------------------

   procedure Set_Elements
     (Self  : access Object;
      Value : AnySeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Set_Elements;

   -----------------------------
   -- Set_Elements_As_Dyn_Any --
   -----------------------------

   procedure Set_Elements_As_Dyn_Any
     (Self  : access Object;
      Value : DynAnySeq)
   is
      pragma Unreferenced (Value);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Set_Elements_As_Dyn_Any;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
     (Self : access Object;
      Len  : CORBA.Unsigned_Long)
   is
      pragma Unreferenced (Len);

   begin
      if Is_Destroyed (Self) then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      raise Program_Error;
   end Set_Length;

end DynamicAny.DynSequence.Impl;
