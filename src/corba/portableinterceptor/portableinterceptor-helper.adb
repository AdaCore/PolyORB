------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O R T A B L E I N T E R C E P T O R . H E L P E R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with CORBA.Object.Helper;

with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PortableInterceptor.Helper is

   procedure Deferred_Initialization;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      declare
         Name             : constant CORBA.String
           := CORBA.To_CORBA_String ("ForwardRequest");
         Id               : constant CORBA.String
           := CORBA.To_CORBA_String (ForwardRequest_Repository_Id);
         Arg_Name_Forward : constant CORBA.String
           := CORBA.To_CORBA_String ("forward");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ForwardRequest, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ForwardRequest, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_ForwardRequest, CORBA.To_Any (CORBA.Object.Helper.TC_Object));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_ForwardRequest, CORBA.To_Any (Arg_Name_Forward));
      end;
      PolyORB.Exceptions.Register_Exception
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_ForwardRequest),
         Raise_ForwardRequest_From_Any'Access);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ReplyStatus");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String (ReplyStatus_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ReplyStatus, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ReplyStatus, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_ReplyStatus, CORBA.To_Any (CORBA.TC_Short));
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("SlotId");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String (SlotId_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_SlotId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_SlotId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_SlotId, CORBA.To_Any (CORBA.TC_Unsigned_Long));
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("InvalidSlot");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String (InvalidSlot_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_InvalidSlot, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_InvalidSlot, CORBA.To_Any (Id));
      end;
      PolyORB.Exceptions.Register_Exception
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_InvalidSlot),
         Raise_InvalidSlot_From_Any'Access);

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ServerId");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String (ServerId_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServerId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServerId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_ServerId, CORBA.To_Any (CORBA.TC_String));
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ORBId");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String (ORBId_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ORBId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ORBId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_ORBId, CORBA.To_Any (CORBA.TC_String));
      end;

      declare
         Name : constant CORBA.String
           := CORBA.To_CORBA_String ("AdapterManagerId");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String (AdapterManagerId_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_AdapterManagerId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AdapterManagerId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_AdapterManagerId, CORBA.To_Any (CORBA.TC_String));
      end;

      declare
         Name : constant CORBA.String
           := CORBA.To_CORBA_String ("AdapterState");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String (AdapterState_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AdapterState, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_AdapterState, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_AdapterState, CORBA.To_Any (CORBA.TC_Short));
      end;
   end Deferred_Initialization;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return AdapterManagerId is
      Result : constant CORBA.String := CORBA.From_Any (Item);
   begin
      return AdapterManagerId (Result);
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return AdapterState is
      Result : constant CORBA.Short := CORBA.From_Any (Item);
   begin
      return AdapterState (Result);
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return ForwardRequest_Members is
      Index          : CORBA.Any;
      Result_Forward : CORBA.Object.Ref;
   begin
      Index := CORBA.Get_Aggregate_Element (Item,
                                            CORBA.Object.Helper.TC_Object,
                                            CORBA.Unsigned_Long (0));
      Result_Forward := CORBA.Object.Helper.From_Any (Index);
      return (Forward => Result_Forward);
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return InvalidSlot_Members is
      pragma Unreferenced (Item);
      Result : InvalidSlot_Members;
   begin
      return Result;
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return ORBId is
      Result : constant CORBA.String := CORBA.From_Any (Item);
   begin
      return ORBId (Result);
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return ReplyStatus is
      Result : constant CORBA.Short := CORBA.From_Any (Item);
   begin
      return ReplyStatus (Result);
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return ServerId is
      Result : constant CORBA.String := CORBA.From_Any (Item);
   begin
      return ServerId (Result);
   end From_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return SlotId is
      Result : constant CORBA.Unsigned_Long := CORBA.From_Any (Item);
   begin
      return SlotId (Result);
   end From_Any;

   -----------------------------------
   -- Raise_ForwardRequest_From_Any --
   -----------------------------------

   procedure Raise_ForwardRequest_From_Any (Item : in PolyORB.Any.Any) is
      Members : constant ForwardRequest_Members
        := From_Any (CORBA.Internals.To_CORBA_Any (Item));
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (ForwardRequest'Identity,
         Members);
   end Raise_ForwardRequest_From_Any;

   --------------------------------
   -- Raise_InvalidSlot_From_Any --
   --------------------------------

   procedure Raise_InvalidSlot_From_Any (Item : in PolyORB.Any.Any) is
      Members : constant InvalidSlot_Members
        := From_Any (CORBA.Internals.To_CORBA_Any (Item));
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidSlot'Identity,
         Members);
   end Raise_InvalidSlot_From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in AdapterManagerId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Set_Type (Result, TC_AdapterManagerId);
      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in AdapterState) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Set_Type (Result, TC_AdapterState);
      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in ForwardRequest_Members) return CORBA.Any is
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_ForwardRequest);
   begin
      CORBA.Add_Aggregate_Element
         (Result, CORBA.Object.Helper.To_Any (Item.Forward));
      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in InvalidSlot_Members) return CORBA.Any is
      pragma Unreferenced (Item);
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_InvalidSlot);
   begin
      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in ORBId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Set_Type (Result, TC_ORBId);
      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in ReplyStatus) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Set_Type (Result, TC_ReplyStatus);
      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in ServerId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Set_Type (Result, TC_ServerId);
      return Result;
   end To_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in SlotId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Unsigned_Long (Item));
   begin
      CORBA.Set_Type (Result, TC_SlotId);
      return Result;
   end To_Any;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"PortableInterceptor.Helper",
          Conflicts => Empty,
          Depends   =>
                  Empty
                  & "corba.object"
                  & "exceptions",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end PortableInterceptor.Helper;
