------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        D Y N A M I C A N Y . D Y N A N Y F A C T O R Y . I M P L         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
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

with PolyORB.Initial_References;
with PolyORB.Initialization;
with PolyORB.References;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

with PolyORB.CORBA_P.Dynamic_Any;

package body DynamicAny.DynAnyFactory.Impl is

   procedure Deferred_Initialization;

   --------------------
   -- Create_Dyn_Any --
   --------------------

   function Create_Dyn_Any
     (Self  : access Object;
      Value : CORBA.Any)
      return DynAny.Local_Ref
   is
      pragma Unreferenced (Self);

   begin
      return PolyORB.CORBA_P.Dynamic_Any.Create (Value, True, null);
   end Create_Dyn_Any;

   -----------------------------------
   -- Create_Dyn_Any_From_Type_Code --
   -----------------------------------

   function Create_Dyn_Any_From_Type_Code
     (Self     : access Object;
      IDL_Type : CORBA.TypeCode.Object)
      return DynAny.Local_Ref
   is
      pragma Unreferenced (Self);

   begin
      return PolyORB.CORBA_P.Dynamic_Any.Create (IDL_Type);
   end Create_Dyn_Any_From_Type_Code;

   ---------------------------------------
   -- Create_Dyn_Any_Without_Truncation --
   ---------------------------------------

   function Create_Dyn_Any_Without_Truncation
     (Self  : access Object;
      Value : CORBA.Any)
      return DynAny.Local_Ref
   is
      pragma Unreferenced (Self);

   begin
      return PolyORB.CORBA_P.Dynamic_Any.Create (Value, False, null);
   end Create_Dyn_Any_Without_Truncation;

   --------------------------
   -- Create_Multiple_Anys --
   --------------------------

   function Create_Multiple_Anys
     (Self   : access Object;
      Values : DynAnySeq)
      return AnySeq
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Values);

      Result : DynamicAny.AnySeq;

   begin
      raise Program_Error;
      return Result;
   end Create_Multiple_Anys;

   ------------------------------
   -- Create_Multiple_Dyn_Anys --
   ------------------------------

   function Create_Multiple_Dyn_Anys
     (Self           : access Object;
      Values         : AnySeq;
      Allow_Truncate : CORBA.Boolean)
      return DynamicAny.DynAnySeq
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Values);
      pragma Unreferenced (Allow_Truncate);

      Result : DynamicAny.DynAnySeq;

   begin
      raise Program_Error;
      return Result;
   end Create_Multiple_Dyn_Anys;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
      Factory_Ref : PolyORB.References.Ref;
   begin
      Factory_Ref.Set (PolyORB.Smart_Pointers.Entity_Ptr'(new Object));
      PolyORB.Initial_References.Register_Initial_Reference
        ("DynAnyFactory", Factory_Ref);
   end Deferred_Initialization;

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
          (Logical_Type_Id, DynamicAny.DynAnyFactory.Repository_Id)
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"dynamicany.dynanyfactory",
          Conflicts => Empty,
          Depends   => +"initial_references",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;
end DynamicAny.DynAnyFactory.Impl;
