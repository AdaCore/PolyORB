------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O R T A B L E I N T E R C E P T O R . C U R R E N T           --
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

with CORBA.Object;
with PolyORB.CORBA_P.Initial_References;
with PolyORB.Initialization;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings.Lists;
with PortableInterceptor.Current.Impl;

package body PortableInterceptor.Current is

   function Create return CORBA.Object.Ref;

   procedure Deferred_Initialization;

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref is
      Result  : Local_Ref;
      Current : constant PolyORB.Smart_Pointers.Entity_Ptr := new Impl.Object;
   begin
      Set (Result, Current);

      return CORBA.Object.Ref (Result);
   end Create;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      PolyORB.CORBA_P.Initial_References.Register_Initial_Reference
        ("PICurrent", Create'Access);
   end Deferred_Initialization;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (Self : in Local_Ref;
      Id   : in SlotId)
      return CORBA.Any
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.Current.Impl.Get_Slot
        (PortableInterceptor.Current.Impl.Object_Ptr (Entity_Of (Self)),
         Id);
   end Get_Slot;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (Self : in Local_Ref;
      Id   : in SlotId;
      Data : in CORBA.Any)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.Current.Impl.Set_Slot
        (PortableInterceptor.Current.Impl.Object_Ptr (Entity_Of (Self)),
         Id,
         Data);
   end Set_Slot;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Utils.Strings;
      use PolyORB.Utils.Strings.Lists;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"portableinterceptor.current",
          Conflicts => Empty,
          Depends   => +"corba.initial_references",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end PortableInterceptor.Current;
