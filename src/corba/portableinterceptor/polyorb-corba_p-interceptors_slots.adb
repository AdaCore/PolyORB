------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S _ S L O T S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PortableInterceptor.Helper;

package body PolyORB.CORBA_P.Interceptors_Slots is

   use Any_Sequences;
   use PortableInterceptor;
   use PortableInterceptor.Helper;

   Last_Allocated_Slot_Id : PortableInterceptor.SlotId := 0;

   ----------------------
   -- Allocate_Slot_Id --
   ----------------------

   function Allocate_Slot_Id
      return SlotId
   is
   begin
      Last_Allocated_Slot_Id := Last_Allocated_Slot_Id + 1;

      return Last_Allocated_Slot_Id;
   end Allocate_Slot_Id;

   --------------------
   -- Allocate_Slots --
   --------------------

   procedure Allocate_Slots (Note : in out Slots_Note) is
      Empty : constant CORBA.Any :=
                CORBA.Internals.Get_Empty_Any (CORBA.TC_Null);

   begin
      Note.Slots := Null_Sequence;

      for J in 1 .. Last_Allocated_Slot_Id loop
         Append (Note.Slots, Empty);
      end loop;

      Note.Allocated := True;
   end Allocate_Slots;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (Note : Slots_Note;
      Id   : SlotId)
      return CORBA.Any
   is
   begin
      pragma Assert (Note.Allocated);

      if Id not in 1 .. SlotId (Length (Note.Slots)) then
         Raise_InvalidSlot ((null record));
      end if;

      return Get_Element (Note.Slots, Integer (Id));
   end Get_Slot;

   ------------------------
   -- Invalid_Slots_Note --
   ------------------------

   function Invalid_Slots_Note return Slots_Note is
      Aux : constant Slots_Note
        := (PolyORB.Annotations.Note with False, Any_Sequences.Null_Sequence);
   begin
      return Aux;
   end Invalid_Slots_Note;

   ------------------
   -- Is_Allocated --
   ------------------

   function Is_Allocated (Note : Slots_Note) return Boolean is
   begin
      return Note.Allocated;
   end Is_Allocated;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (Note : in out Slots_Note;
      Id   : SlotId;
      Data : CORBA.Any)
   is
   begin
      pragma Assert (Note.Allocated);

      if Id not in 1 .. SlotId (Length (Note.Slots)) then
         Raise_InvalidSlot ((null record));
      end if;

      Replace_Element (Note.Slots, Integer (Id), Data);
   end Set_Slot;

end PolyORB.CORBA_P.Interceptors_Slots;
