------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O R T A B L E I N T E R C E P T O R . C U R R E N T . I M P L      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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
with PolyORB.CORBA_P.Interceptors_Slots;

with PolyORB.Annotations;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Initialization;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings.Lists;

package body PortableInterceptor.Current.Impl is

   use PolyORB.Annotations;
   use PolyORB.CORBA_P.Interceptors_Slots;
   use PolyORB.Tasking.Threads.Annotations;

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
     (Self : access Object;
      Id   : SlotId)
      return CORBA.Any
   is
      pragma Unreferenced (Self);

      Npad : Notepad_Access;
      Note : Slots_Note;
   begin
      if not PolyORB.CORBA_P.Interceptors_Slots.ORB_Initializer_Done then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 10,
                                        Completed => CORBA.Completed_No));
      end if;

      Npad := Get_Current_Thread_Notepad;

      Get_Note (Npad.all, Note, Invalid_Slots_Note);

      --  If the slot table is not allocated for this thread then
      --  allocate it

      if not Is_Allocated (Note) then
         Allocate_Slots (Note);
         Set_Note (Npad.all, Note);
      end if;

      return Get_Slot (Note, Id);
   end Get_Slot;

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
         PortableInterceptor.Current.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
           (Logical_Type_Id,
            "IDL:omg.org/CORBA/Current:1.0");
   end Is_A;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (Self : access Object;
      Id   : SlotId;
      Data : CORBA.Any)
   is
      pragma Unreferenced (Self);

      Npad : Notepad_Access;
      Note : Slots_Note;
   begin
      if not PolyORB.CORBA_P.Interceptors_Slots.ORB_Initializer_Done then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 10,
                                        Completed => CORBA.Completed_No));
      end if;

      Npad := Get_Current_Thread_Notepad;

      Get_Note (Npad.all, Note, Invalid_Slots_Note);

      --  If the slot table is not allocated for this thread then
      --  allocate it.

      if not Is_Allocated (Note) then
         Allocate_Slots (Note);
      end if;

      Set_Slot (Note, Id, Data);
      Set_Note (Npad.all, Note);
   end Set_Slot;

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
       Init      => Deferred_Initialization'Access,
       Shutdown  => null));
end PortableInterceptor.Current.Impl;
