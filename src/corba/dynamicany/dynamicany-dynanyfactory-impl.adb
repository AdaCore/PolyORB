------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        D Y N A M I C A N Y . D Y N A N Y F A C T O R Y . I M P L         --
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

with PolyORB.CORBA_P.Initial_References;
with PolyORB.Initialization;
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
      Factory_Object : constant Object_Ptr := new Object;
      Factory_Ref    : CORBA.Object.Ref;
   begin
      CORBA.Object.Set
        (Factory_Ref,
         PolyORB.Smart_Pointers.Entity_Ptr (Factory_Object));

      PolyORB.CORBA_P.Initial_References.Register_Initial_Reference
        ("DynAnyFactory",
         Factory_Ref);
   end Deferred_Initialization;

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
      return
        CORBA.Is_Equivalent
        (Logical_Type_Id,
         DynamicAny.DynAnyFactory.Repository_Id)
        or else
          CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0");
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
          Depends   => +"corba.initial_references",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;
end DynamicAny.DynAnyFactory.Impl;
