------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             D Y N A M I C A N Y . D Y N A N Y F A C T O R Y              --
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

with PolyORB.Exceptions;
with DynamicAny.DynAnyFactory.Impl;

package body DynamicAny.DynAnyFactory is

   --------------------
   -- Create_Dyn_Any --
   --------------------

   function Create_Dyn_Any
     (Self  : Local_Ref;
      Value : CORBA.Any)
      return DynAny.Local_Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Create_Dyn_Any (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Create_Dyn_Any;

   -----------------------------------
   -- Create_Dyn_Any_From_Type_Code --
   -----------------------------------

   function Create_Dyn_Any_From_Type_Code
     (Self     : Local_Ref;
      IDL_Type : CORBA.TypeCode.Object)
      return DynAny.Local_Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        Impl.Create_Dyn_Any_From_Type_Code
        (Impl.Object_Ptr (Entity_Of (Self)), IDL_Type);
   end Create_Dyn_Any_From_Type_Code;

   ---------------------------------------
   -- Create_Dyn_Any_Without_Truncation --
   ---------------------------------------

   function Create_Dyn_Any_Without_Truncation
     (Self  : Local_Ref;
      Value : CORBA.Any)
      return DynAny.Local_Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        Impl.Create_Dyn_Any_Without_Truncation
        (Impl.Object_Ptr (Entity_Of (Self)), Value);
   end Create_Dyn_Any_Without_Truncation;

   --------------------------
   -- Create_Multiple_Anys --
   --------------------------

   function Create_Multiple_Anys
     (Self   : Local_Ref;
      Values : DynAnySeq)
      return AnySeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        Impl.Create_Multiple_Anys
        (Impl.Object_Ptr (Entity_Of (Self)), Values);
   end Create_Multiple_Anys;

   ------------------------------
   -- Create_Multiple_Dyn_Anys --
   ------------------------------

   function Create_Multiple_Dyn_Anys
     (Self           : Local_Ref;
      Values         : AnySeq;
      Allow_Truncate : CORBA.Boolean)
      return DynAnySeq
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        Impl.Create_Multiple_Dyn_Anys
        (Impl.Object_Ptr (Entity_Of (Self)), Values, Allow_Truncate);
   end Create_Multiple_Dyn_Anys;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out InconsistentTypeCode_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

end DynamicAny.DynAnyFactory;
