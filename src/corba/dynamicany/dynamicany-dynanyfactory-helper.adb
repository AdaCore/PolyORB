------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      D Y N A M I C A N Y . D Y N A N Y F A C T O R Y . H E L P E R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

with CORBA.Object;
with PolyORB.Any;
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body DynamicAny.DynAnyFactory.Helper is

   procedure Deferred_Initialization;

   procedure Raise_InconsistentTypeCode_From_Any (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_InconsistentTypeCode_From_Any);

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("InconsistentTypeCode");
         Id   : CORBA.String
           := CORBA.To_CORBA_String
           ("IDL:omg.org/DynamicAny/DynAnyFactory/InconsistentTypeCode:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_InconsistentTypeCode, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_InconsistentTypeCode, CORBA.To_Any (Id));
      end;
      PolyORB.Exceptions.Register_Exception
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_InconsistentTypeCode),
         Raise_InconsistentTypeCode_From_Any'Access);
   end Deferred_Initialization;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : in CORBA.Any)
      return InconsistentTypeCode_Members
   is
      pragma Unreferenced (Item);

      Result : InconsistentTypeCode_Members;

   begin
      return Result;
   end From_Any;

   --------------------------------
   -- Raise_InconsistentTypeCode --
   --------------------------------

   procedure Raise_InconsistentTypeCode
     (Members : in InconsistentTypeCode_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InconsistentTypeCode'Identity,
         Members);
   end Raise_InconsistentTypeCode;

   -----------------------------------------
   -- Raise_InconsistentTypeCode_From_Any --
   -----------------------------------------

   procedure Raise_InconsistentTypeCode_From_Any (Item : in PolyORB.Any.Any) is
      Members : constant InconsistentTypeCode_Members
        := From_Any (CORBA.Internals.To_CORBA_Any (Item));

   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InconsistentTypeCode'Identity,
         Members);
   end Raise_InconsistentTypeCode_From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in InconsistentTypeCode_Members) return CORBA.Any is
      pragma Unreferenced (Item);

      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (TC_InconsistentTypeCode);

   begin
      return Result;
   end To_Any;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, Repository_Id)
      then
         return Unchecked_To_Local_Ref (The_Ref);
      end if;

      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return Local_Ref
   is
      Result : DynamicAny.DynAnyFactory.Local_Ref;

   begin
      Set (Result, CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Local_Ref;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"DynamicAny.DynAnyFactory.Helper",
          Conflicts => Empty,
          Depends   =>
                  Empty
                  & "exceptions"
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end DynamicAny.DynAnyFactory.Helper;
